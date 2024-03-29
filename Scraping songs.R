library(tidyverse)
library(magrittr)
library(rvest)
library(xml2)
library(stringr)
library(spotifyr)

# Get the top 100 songs on wikipedia

yr <- c(1958:2018)
top_100_df <- NULL

for(year in seq_along(yr)) {
  
  url <- read_html(paste("https://en.wikipedia.org/wiki/Billboard_Year-End_Hot_100_singles_of_",yr[year], sep = ""))
  if(nrow(html_table(url, fill = TRUE)[[1]]) > 10) {
    url %>%
      html_table(fill = TRUE) %>% 
      .[[1]] %>% 
      mutate(Year = yr[year],
             Top_100 = 'Yes') %>%
      set_names(c("Position", "Song", "Artist", "Year", "Top_100")) ->
      data
    
    top_100_df <- rbind(top_100_df, data)
    
  } else {
    
    url %>%
      html_table(fill = TRUE) %>% 
      .[[2]] %>% 
      mutate(Year = yr[year],
             Top_100 = 'Yes') %>%
      set_names(c("Position", "Song", "Artist", "Year", "Top_100")) ->
      data
    
    top_100_df <- rbind(top_100_df, data)
  }
  
}

# The song names come as untidy string, cleaning them up.

top_100_df$Song %>%
  lapply(function(x)
    x %>%
      str_split("\"") %>%
      .[[1]] %>%
      .[2] 
  ) %>%
  unlist ->
  top_100_songs 

top_100_df %<>%
  mutate(Song = top_100_songs)


# get all artist names. Note its common that artists have more than a few song in the top 100. Use unique().

top_100_artists <- top_100_df$Song %>% unique 

# Set spotify client Id and get access

Sys.setenv(SPOTIFY_CLIENT_ID = '47cc92a7e22b4897944a0a391cfd0135')  
Sys.setenv(SPOTIFY_CLIENT_SECRET = '15bac28b34c0426289f0c44c50b21ce2')  
access_token <- get_spotify_access_token()

# download the song features in spotify for all artist with a song in the top 100. 

top_100_artist_all_songs <- NULL

for(artist in setdiff(top_100_artists, unique(top_100_artist_all_songs$artist_name))) {
  tryCatch(
    {
      artist %>%
        get_artist_audio_features(include_groups = c("album")) ->
        data

      top_100_artist_all_songs <- rbind(top_100_artist_all_songs, data)

    },
    error=function(e) {
      X = print("Error")
    }
    )

}

# save the files (exluded here)

# saveRDS(top_100_artist_all_songs, file = "top_100_artist_all_songs")
# top_100_artist_all_songs <- readRDS("top_100_artist_all_songs")

# We get a lot of output. Only interested in the following audio features.

audio_features <- c("track_id", "track_name", "artist_name", "album_name", "album_release_year", "album_release_date_precision", "album_type", "album_release_date", "track_number", "explicit", "type", "disc_number", 
                    "time_signature", "tempo", "available_markets", "duration_ms", "key_name", "mode_name", "key_mode", "danceability", 
                    "energy", "key", "loudness", "mode", "speechiness", "acousticness", "instrumentalness", "liveness", "valence")


# Join the dataset with the one from wikipedia.

top_100_artist_all_songs %>%
  select(audio_features) %>%
  inner_join(
    top_100_df %>% 
      rename(track_name = 'Song',
             artist_name = 'Artist')
  ) %>% tbl_df %>%
  arrange(track_name) -> 
  top_100_artist_songs

# Looks like there are duplicated songs. Very common in spotify to have same song with multiple Ids. Identify and exclude.

top_100_artist_songs %>%
  select(track_name, Year) %>%
  duplicated %>%
  which -> 
  duplicate_songs

top_100_artist_songs[-duplicate_songs, ] -> 
  billboard_songs_df

# Identify non top 100 songs. To facilitate the building of models. Taking a random sample.

setdiff(top_100_artist_all_songs$track_name, billboard_songs_df$track_name) ->
  other_songs

set.seed(1234)
top_100_artist_all_songs %>%
  filter(track_name %in% other_songs, album_release_year %in% c(1958:2018)) %>%
  sample_n(10000 - nrow(billboard_songs_df)) %>%
  tbl_df %>%
  select(audio_features) %>%
  mutate(Position = NA,
         Year = NA,
         Top_100 = "No") ->
  other_songs_df

# concatenate the two datasets and save the final file.

df <- rbind(billboard_songs_df, other_songs_df)

write.csv(df %>% select(-available_markets), file = "10000_Songs_df.csv", row.names = F)


