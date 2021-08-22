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

countriesList <- fellMeteo %>%
  distinct(country, countryNr, countryLat)

fellMeteo %>% 
  ggplot(aes(countryNr + lat/50, 3)) +
  geom_rect(aes(xmin = 0, ymin = 0, xmax = 29, ymax = 3)) +
  geom_segment(aes(xend = countryNr + lat/50,
                   yend = 3 + (2020 - year)/100),
                   size = 0.25, alpha = 0.5, color = "orange") +
  geom_point(aes(size = mass),
             color = "orangered", fill = "red",
             alpha = 0.2, shape = 21) +
  geom_text(data = countriesList, aes(countryNr + countryLat/50, 2.8, label = country),
             color = "white", hjust = 1, size = 1) +    
  scale_size(range = c(0, 10)) +                 
  theme_minimal() +
  xlim(0, 29) +
  ylim(0, 15) +
  coord_polar(start = -pi/2.4) +
  theme_void() +
  theme(
    legend.position = "top",
    panel.background = element_rect(fill = "midnightblue"),
    plot.margin = margin(0, 0, -27, 0, "cm")
  ) 

ggsave("./week-24/meteorites.png", dpi = 600)




