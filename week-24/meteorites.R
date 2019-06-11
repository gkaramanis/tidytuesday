library(tidyverse)
library(sp)
library(ggmap)
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
  select(long, lat) %>% 
  mutate(country = coords2continent(.)) %>% 
  drop_na() %>% 
  left_join(., meteorites) %>% 
  group_by(country) %>% 
  mutate(medianMass = median(mass),
         sumFell = sum(n()),
         countryLat = mean(lat),
         countryLong = mean(long)) %>% 
  distinct(country, medianMass, sumFell, countryLat, countryLong)

worldmap <- borders("world", colour = "gray60", fill = "gray80", size = 0.05)

fellMeteo %>% 
  ggplot(aes(countryLong, countryLat)) + 
  worldmap +
  coord_cartesian(xlim = c(-170, 180), ylim = c(-55, 85)) +
  geom_point(aes(size = medianMass), alpha = 0.5) +
  geom_curve(aes(xend = countryLong - sumFell,
                 yend = countryLat + sumFell), curvature = 0.2) +
  theme_void() +
  theme(
    legend.position = "bottom"
  )

ggsave("./week-24/meteorites.png", width = 5, height = 3)
