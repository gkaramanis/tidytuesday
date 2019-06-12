library(tidyverse)
#library(sp)
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
  mutate(country = str_replace_all(country, "United States of America", "U.S.A.")) %>% 
  drop_na() %>% 
  left_join(., meteorites) %>% 
  group_by(country) %>% 
  mutate(medianMass = median(mass)/1000,
         sumFell = sum(n()),
         countryLat = mean(lat),
         countryLong = mean(long)) %>% 
  filter(sumFell > 21) %>% 
  mutate(countryNr = group_indices())



fellMeteo %>% 
  ggplot(aes(countryNr + lat/100, 3)) +
  geom_segment(aes(xend = countryNr + lat/100,
                   yend = 15 - sumFell/100,
                   size = 1),
               alpha = 0.5,
               color = "orange") +
  geom_point(aes(size = mass),
             color = "orangered",
             alpha = 0.5) +
  theme_minimal() +
  ylim(0, 15) +
  coord_polar() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1)
  ) +

ggsave("./week-24/meteorites.png")




