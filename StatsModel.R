library(ordinal)
library(VGAM)
library(tidymodels)
library(nnet)



RankModel<-multinom(Rank~valence*acousticness,data=AlbumMean, model = TRUE)



AlbumMean<-ungroup(AlbumMean)

var<-quo(valence)



ggplot(AlbumAll)+
  geom_boxplot(aes(x = Rank, group = Rank, y = valence))+
  geom_smooth(aes(x = Rank, y = valence, group =1))


var<-quo(valence)

plotfunction<-function(df,var){
  #var<-quo(var)
  ggplot(df)+
    geom_boxplot(aes(x = Rank, y = {{var}}, group = Rank), fill = 'blue', alpha = 0.5)+
    geom_smooth(aes(x = Rank, y ={{var}}, group = 1))+
    theme_bw()
}

plotfunction(AlbumAll, )


MeanRankplotfunction<-function(df,var){
  
  dfmean<-df|>
    mutate(Rank = as.factor(Rank))|>
    group_by(Year, Rank)|>
    summarise(across(where(is.numeric), median, na.rm = TRUE))
  
  ggplot(dfmean)+
    geom_line(aes(x = Year, y = {{var}}, color = Rank), size =3)+
    geom_point(aes(x = Year, y= {{var}}, fill = Rank), shape =21, size =3)+
    theme_bw()+
    facet_wrap(~Rank)
}

MeanRankplotfunction(AlbumAll, danc)









AlbumMean|>
  group_by(Year)|>
  summarise(across(where(is.numeric), median, na.rm = TRUE))|>
  ggplot()+
  geom_line(aes(x = Year, y = tempo))+
  geom_point(aes(x = Year, y = tempo))+
  theme_bw()



x_s<-select(AlbumMean, !!var)
x_c<-select(AlbumMean, -!!var)

grid<-crossing(x_s,x_c)

library(broom)

augment.multinom <- function(object, newdata) {
  newdata <- as_tibble(newdata)
  class_probs <- predict(object, newdata, type = "prob")
  bind_cols(newdata, as_tibble(class_probs))
}

au <- augment(RankModel, grid)

pd <- au|>
  pivot_longer(cols = 20:29, names_to = "Rank_Model",values_to = "prob")|>
  group_by(Rank_Model, !!var)|>
  summarise(marginal_prob = mean(prob))|>
  mutate(Rank_Model = as.numeric(Rank_Model))|>
  dplyr::arrange(Rank_Model)
  
  


ggplot(pd)+
  geom_line(aes(x = !!var, y =marginal_prob, color = as.factor(Rank_Model)),
            size =2)+
  facet_wrap(~Rank_Model)



partial_dependence <- function(predictor, data) {
  
  var <- ensym(predictor)
  x_s <- select(data, !!var)
  x_c <- select(data, -!!var)
  grid <- crossing(x_s, x_c)
  
  augment(fit, grid) %>% 
    gather(class, prob, setosa, versicolor, virginica) %>% 
    group_by(class, !!var) %>%
    summarize(marginal_prob = mean(prob))
}

all_dependencies <- names(select(AlbumMean, valence, energy, loudness, acousticness))|> 
  map_dfr(partial_dependence, data = AlbumMean)|> 
  gather(feature, feature_value, -class, -marginal_prob)|>
  na.omit()




library(tidyverse)
library(skimr)

data <- as_tibble(iris)
glimpse(data)

fit <- multinom(Species ~ ., data, trace = FALSE)
fit

var <- quo(Sepal.Length)

x_s <- select(data, !!var)   # grid where we want partial dependencies
x_c <- select(data, -!!var)  # other predictors

grid <- crossing(x_s, x_c)


augment.multinom <- function(object, newdata) {
  newdata <- as_tibble(newdata)
  class_probs <- predict(object, newdata, type = "prob")
  bind_cols(newdata, as_tibble(class_probs))
}

au <- augment(fit, grid)
au

pd <- au %>%
  gather(class, prob, setosa, versicolor, virginica) %>% 
  group_by(class, !!var) %>%
  summarize(marginal_prob = mean(prob))
pd

pd %>%
  ggplot(aes(!!var, marginal_prob, color = class)) +
  geom_point(size = 1) +
  scale_color_viridis_d() +
  labs(title = paste("Partial dependence plot for", quo_name(var)),
       y = "Average class probability across all other predictors",
       x = quo_name(var)) +
  theme_classic()


