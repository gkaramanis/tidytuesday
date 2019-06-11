library(tidyverse)
library(sp)
library(rworldmap)

meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")

# https://stackoverflow.com/questions/21708488/get-country-and-continent-from-longitude-and-latitude-point-in-r
coords2continent = function(points)
{  
  countriesSP <- getMap(resolution='low')
  pointsSP = SpatialPoints(points, proj4string = CRS(proj4string(countriesSP)))  
  indices = over(pointsSP, countriesSP)
  # indices$REGION # returns continent
  indices$ADMIN  #returns country name
}

fellMeteo <- meteorites %>%
  drop_na() %>%
  filter(fall == "Fell") %>% 
  select(long, lat, mass, name) %>% 
  mutate(country = coords2continent(points)) %>% 
  drop_na() %>%
  group_by(country) %>% 
  mutate(medianMass = median(mass),
         sumFell = sum(n()))

fellMeteo %>% 
  ggplot() + 
  geom_point(aes(long, lat, size = medianMass), alpha = 0.5) +
  theme_minimal()

ggsave("./week-24/meteorites.png", width = 7, height = 4)
