library(ggridges)


AlbumAll<-read_csv('SpotifyAudioData.csv')|>
  rename(Artist = Artist.x, Year = Year.x)



## Use search spotify to get genre

ArtistIDs<-AlbumAll|>
  select(Artist, artist_id)
  
SearchArtistFunction<-function(Artist, ID){
  SearchDF<-search_spotify(Artist, "artist")|>
    filter(id == ID)|>
    select(id,genres, popularity, followers.total)|>
    rename(artist_id = id)
  
}

#Test<-SearchArtistFunction(ArtistIDs$Artist, ArtistIDs$artist_id)

Test<-bind_rows(pmap(list(ArtistIDs$Artist, ArtistIDs$artist_id), .f = SearchArtistFunction))



BindTest<-merge(AlbumAll, Test,by = 'artist_id')
BindTest<-BindTest|>
  group_by(artist_id, track_name)|>
  slice(1)



BindTest|>
  filter(Rank<11)|>
  group_by(Year)|>
  summarise(across(where(is.numeric), mean))|>
  ggplot()+
  geom_line(aes(x = Year, y = popularity))+
  geom_point(aes(x = Year,y = popularity))


ggplot(BindTest|>
         filter(Rank<11))+
  geom_boxplot(aes(x = Year, group = Year,y = popularity))


testlm<-lm(Rank = )

BindTest|>
  filter(Rank <11)|>
#  group_by(Artist)|>
  group_by(artist_id)|>
  slice(1)|>
  arrange(popularity)|>
  ungroup()|>
  slice(1:10)
  ggplot()+
  geom_bar(aes(x =))



ggplot(BindTest)+
  geom_point(aes(x = followers.total, y = popularity))+
  geom_smooth(aes(x = followers.total, y = popularity), method = "lm")+
#  geom_label(aes(x = followers.total, y = popularity, label = Artist))+
  scale_x_log10()+
  ggpubr::stat_regline_equation(aes(x = followers.total, y = popularity,
                                label = paste(..eq.label.., ..rr.label.., sep = "~~~~")))+
  theme_bw()
  
  
AlbumAll|>
  filter(Rank <11)|>
  #group_by(Album, Year)|>
  #summarise(across(where(is.numeric), median))|>
  ggplot()+
  geom_boxplot(aes(group = as.factor(Year),x = Year, y = valence, fill = as.factor(Year)))+
  stat_summary(aes(x = Year, y = valence), 
               fun = mean,
               geom = "point", shape =21, size =4)


ggplot(AlbumAll)+
  geom_point(aes(x = energy, y = tempo, color = loudness))+
  facet_wrap(~Year)+
  scale_color_gradientn(colors = rev(rainbow(6)))

AlbumAveraged<-AlbumAll|>
  group_by(Album, Year)|>
  summarise(across(where(is.numeric), mean))

PRCOMP<-prcomp(AlbumAll|>
         select(energy, tempo, loudness, danceability, valence,liveness,instrumentalness,
                acousticness,speechiness), scale. = TRUE)
library(psych)
library(factoextra)

AlbumScaled<-as.data.frame(scale(AlbumAveraged|>
                                   ungroup()|>
                     select(energy, tempo, loudness, danceability, valence,liveness,instrumentalness,
                            acousticness,speechiness, duration_ms)))

test<-factanal(AlbumScaled,factors = 5)


factanal(AlbumScaled, factors = 5)


Test<-glm(Rank~tempo+loudness+danceability+valence+liveness+instrumentalness, data = AlbumAveraged|>
            filter(Rank < 11)|>
      mutate(Rank = as.factor(Rank)),
      family ="binomial")



ggplot(AlbumAveraged|>filter(Rank <11))+
  geom_bar(aes(x = Album, y = valence), stat = 'identity')


AlbumAll|>
  filter(Rank < 11)|>
  group_by(Year)|>
  summarise(across(where(is.numeric), mean, na.rm = TRUE))|>
  ggplot()+
  geom_line(aes(x = Year, y= loudness))


Plot<-ggplot(AlbumAll|>
         filter(Rank < 11))+
  geom_density_ridges2(aes(x = valence, y = as.factor(Year), 
                           fill = as.factor(Year)), color = 'black', alpha = 1,
                       quantile_lines = TRUE, quantiles =2, jittered_points = TRUE)+
  #geom_jitter(aes(x = loudness, y= as.factor(Year)))+
  PaperQuality()

ggplotly(Plot)


AlbumAll|>
  mutate(Album = as.factor())
  #filter(Rank < 11)|>
  group_by(Year)|>
  arrange(loudness)|>
#  slice_max(n =10, order_by = loudness)|>
  ggplot()+
  geom_bar(aes(x = reorder(Album, -loudness), y = loudness), stat = 'identity')+
  theme(axis.text.x = element_text(angle = 90))


  facet_wrap(~Year)
  
  
  
  geom_density_ridges2(aes(x = loudness, y = Album), 
                       color = 'black', alpha = 0.5,
                       quantile_lines = TRUE, quantiles =2)+
  PaperQuality()+
  facet_wrap(~Album)
  








