
library(spotifyr)
library(tidyverse)
library(plotly)
library(mgcv)
library(tidymodels)
library(nnet)
library(VGAM)
library(stringr)

Sys.setenv(SPOTIFY_CLIENT_ID = 'bf03179b875d4cfeaee4754331640464')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '693bd8637b45414599a35bb722b51e94')
access_token <- get_spotify_access_token()



AlbumList<-read_csv('TopAlbumsDataBase.csv')

PaperQuality<-function(...){
  theme_bw()%+replace%
    theme(axis.title.x = element_text(size = 22), axis.title.y = element_text(size = 14, angle = 90),
          axis.text = element_text(size = 14), title = element_text(size = 24, face = "bold"),
          legend.text = element_text(size = 18), legend.title = element_blank(),
          legend.position = "bottom")
}

#get_artist_audio_features(artist = 'Lawn', 
 #                         include_groups = 'album', return_closest_artist = TRUE, dedupe_albums = FALSE)
#readline(1)
#  filter(album_name == "Blood on the Tracks")

source("AlternateAudioArtists.R")

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

DataFrameList<-DataFrameList[sapply(DataFrameList, nrow)!=0]

AlbumDF<-bind_rows(DataFrameList, get_artist_audio_features("Ethel Cain")|> ## Ethel Cain Album is wierd
                     filter(album_name == "Preacher’s Daughter")|>
                     mutate(album_name = "Preacher's Daughter"))|>
  rename(Artist = artist_name)

AlbumDF<-AlbumDF|>
  mutate(Year = lubridate::year(as.Date(album_release_date)),
         Album = album_name)


setdiff(unique(AlbumList$Album), 
        unique(AlbumDF$Album)) ## Check to the I'm not missing albums


AlbumAll<-merge(AlbumList, AlbumDF,  by = c("Album"))|>
  mutate(track_name = str_to_title(track_name),
         Rank = as.factor(Rank))|>
  dplyr::distinct(track_name, track_number, .keep_all = TRUE)## combined ranks with audio data

write_csv(AlbumAll, "SpotifyAudioData.csv")

AlbumMean<-AlbumAll|>
  group_by(Album, Rank)|>
  summarise(across(where(is.numeric), mean, na.rm = TRUE))

model<-multinom(Rank~tempo+valence+loudness+energy+acousticness,data=AlbumMean, model = TRUE)

model2<-vglm(Rank~valence+loudness+energy+acousticness, family = multinomial(refLevel = 1),
             data =AlbumAll)

covariate <- sample(0:1, 100, replace=TRUE)
exposure  <- runif(100,0,1)+(0.3*covariate)
outcome   <- 2.0+(0.5*exposure)+(0.25*covariate)

lm(outcome~exposure+covariate)

#group_by(Year)%>%
#summarise(across(where(is.numeric), mean, na.rm = TRUE))%>%
valenceplot<-ggplot(AlbumAll)+
  geom_jitter(aes(x = Year, y = valence), alpha =0.6)+
  geom_smooth(aes(x = Year, y = valence), method = 'loess')+
  geom_boxplot(aes(x = Year, y= valence, group = Year), fill ='blue', alpha = 0.6)+
  PaperQuality()
ggplotly(valenceplot)

ggplot(AlbumAll)+
  geom_point(aes(x =energy, y = valence, fill = as.factor(Rank)), shape =21, size =3)+
  facet_wrap(~Year)+
  geom_smooth(aes(x = energy, y = valence), method =lm)

AlbumAll|>
  group_by(Year, Album)|>
  summarise(AlbumTime = sum(duration_ms/(1000*60)))|>
  group_by(Year)|>
  summarise(across(where(is.numeric), mean))|>
  ggplot()+
  geom_line(aes(x = Year, y = AlbumTime), size =3, color = 'red')+
  geom_point(aes(Year, y= AlbumTime), fill= 'red', shape =21, size =3)+
  theme_bw()

valenceplot<-ggplot(AlbumAll|>
                      mutate(Rank = as.factor(Rank)))+
  # geom_jitter(aes(x = Year, y = valence), alpha =0.6)+
  geom_smooth(aes(x = Rank, y = valence, group =1), method = 'loess')+
  geom_boxplot(aes(x = Rank, y= valence, group = Rank), fill ='blue', alpha = 0.6)+
  PaperQuality()#+facet_wrap(~Year)
ggplotly(valenceplot)

ggplot(AlbumAll)+
  geom_point(aes(x = as.factor(Rank), y= valence, fill = as.factor(Year)), shape =21)+
  geom_smooth(aes(x = as.factor(Rank), y= valence), method =lm)+
  PaperQuality()+facet_wrap(~Year)

ggplot(AlbumAll)+
  geom_jitter(aes(x = energy, y = acousticness, fill = as.factor(Rank)),
              shape =21)+
  geom_smooth(aes(x = energy, y = acousticness), method = 'gam')+
  facet_wrap(~Rank)

ggplot(AlbumAll|>
         mutate(Year = as.factor(Year)))+
  geom_bar(aes(x = key_name, fill = Year), color = 'black',
           position = "dodge")+
  PaperQuality()+
  facet_wrap(~Year)

ggplot(AlbumAll|>
         mutate(Year = as.factor(Year)))+
  geom_bar(aes(x = key_name), color = 'black',
           position = "dodge")

PercentPlot<-AlbumAll|>
  group_by(Year)|>
  count(key_mode)|>
  mutate(percent = n/sum(n))|>
  ggplot()+
  geom_line(aes(x = Year, y = 100*percent, color = key_mode))+
  geom_point(aes(x = Year, y = 100*percent, color = key_mode))+
  PaperQuality()+
  facet_wrap(~key_mode)+
  theme(legend.position = "none")

ggplot(AlbumAll)+
  geom_boxplot(aes(x = key_name, y = Rank))

testnomial<-multinom(Rank~valence+tempo+, AlbumAll)

library(GGally)

AlbumAll|>
  dplyr::select(Year,liveness,acousticness,speechiness,tempo,valence, loudness,energy,danceability)|>
  mutate(Year = as.factor(Year))|>
  ggpairs()+
  theme_bw()

lmrank<-lm(Rank~valence, data = AlbumAll)

gamrank<-gam(Rank~s(valence), data = AlbumAll)

set.seed(1492)
ggplot(data.frame(x = rnorm(100)), aes(x)) +  
  stat_smooth(,formula=y~poly(x,2), method =lm)
