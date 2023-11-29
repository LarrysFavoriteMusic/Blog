library(spotifyr)
library(tidyverse)
library(kableExtra)


#Sys.setenv(SPOTIFY_CLIENT_ID)
#Sys.setenv(SPOTIFY_CLIENT_SECRET)
access_token <- get_spotify_access_token()

source("AlternateAudioArtists.R")

AlbumList<-read_csv('TopAlbumsDataBase.csv')

DataFrameList<-pmap(list(AlbumList$Album, AlbumList$Artist, AlbumList$Selection_Number),
                    function(Album,Artist, Number){
                      print(Artist)
                      if(Artist == "Nilufer Yanya"){
                        Artist <-"Nilüfer Yanya"
                      }
                      if(Number != 1){
                        df<-alternateartistid(artist = Artist, include_groups = 'album', return_closest_artist = FALSE,
                                              selection = Number)|>
                          filter(album_name == Album)
                      }
                      else{
                        df<-get_artist_audio_features(artist = Artist, include_groups = 'album', return_closest_artist = TRUE)|>
                          filter(album_name == Album)
                      }
                      
                    })


get_genre_artists('wonky')

get_my_top_artists_or_tracks(type = "artists", limit = 20, offset = 0,
                             time_range = "medium_term",
                             authorization = get_spotify_authorization_code(),
                             include_meta_info = FALSE)



get_my_recently_played(limit = 5) %>% 
  mutate(
    artist.name = map_chr(track.artists, function(x) x$name[1]),
    played_at = as_datetime(played_at)
  ) %>% 
  select(
    all_of(c("track.name", "artist.name", "track.album.name", "played_at"))
  ) %>% 
  kable()

get_spotify_authorization_code(scope = scopes()[c(1
                                                  )])

auth_object <- get_spotify_authorization_code(scope = scopes()[c(7,8,9,10,14,15)])
get_show("3kDS5MRlBl53tA3MRGqLzx", authorization = auth_object)


