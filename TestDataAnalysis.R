library(ggridges)


AlbumAll<-read_csv('SpotifyAudioData.csv')|>
  rename(Artist = Artist.x, Year = Year.x)


AlbumAll|>
  #group_by(Album, Year)|>
  #summarise(across(where(is.numeric), median))|>
  ggplot()+
  geom_boxplot(aes(x = Album, y = energy))


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
  filter(Rank < 11)|>
  group_by(Year)|>
  arrange(loudness)|>
  slice_max(n =10, order_by = loudness)|>
  ggplot()+
  geom_density_ridges2(aes(x = loudness, y = Album), 
                       color = 'black', alpha = 0.5,
                       quantile_lines = TRUE, quantiles =2)+
  PaperQuality()+
  facet_wrap(~Album)
  








