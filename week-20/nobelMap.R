library(tidyverse)
library(ggmap)

# Load data
nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")
countriesCoord <- readr::read_csv("http://worldmap.harvard.edu/download/wfs/34645/csv?outputFormat=csv&service=WFS&request=GetFeature&format_options=charset%3AUTF-8&typename=geonode%3Acountry_centroids_az8&version=1.0.0")

nobelCountries <- nobel_winners %>%
  select(c("birth_country", "death_country")) %>% 
  # remove NA death countries
  filter(!is.na(death_country)) %>%
  # Clean birth countries
  mutate(birth_con = str_extract(birth_country, "\\(([^()]*)\\)")) %>%
  mutate(birth_country = 
           if_else(
             is.na (birth_con),
             birth_country,
             gsub("[()]", "", birth_con)
           )) %>% 
  mutate(death_con = str_extract(death_country, "\\(([^()]*)\\)")) %>%
  mutate(death_country = 
           if_else(
             is.na (death_con),
             death_country,
             gsub("[()]", "", death_con)
           )
         
  ) %>% 
  mutate(
    colour = case_when(
      death_country == "United States of America" ~ "#FF2B4F",
      death_country == "Germany" ~ "#fcab27",
      death_country == "United Kingdom" ~ "#3686d3",
      death_country == "France" ~ "#88398a",
      death_country == "Switzerland" ~ "#20d4bc",
      T ~ "gray60"
    )
  ) %>% 
  select (-c(birth_con, death_con)) %>% 
# keep only entries with different birth and death country
filter(birth_country != death_country)

countriesLL <- countriesCoord %>%  select(c("name_long", "Longitude", "Latitude"))

nobelLL <- merge(nobelCountries, countriesLL, by.x = "birth_country", by.y = "name_long")
nobelLL <- nobelLL %>% rename(fromLong = Longitude, fromLat = Latitude) 
nobelLL <- merge(nobelLL, countriesLL, by.x = "death_country", by.y = "name_long")
nobelLL <- nobelLL %>% rename(toLong = Longitude, toLat = Latitude) 

ggplot(nobelLL) +
  geom_curve(aes(x = fromLat, y = fromLong, xend = toLat, yend = toLong))

qmplot(data = nobelLL, x = fromLong, y = fromLat, xend = toLong, yend = toLat, maptype = "toner-lite", geom = "curve") +
coord_cartesian() +
ggsave("./week-20/nobelMap.png", width = 5, height = 3)

