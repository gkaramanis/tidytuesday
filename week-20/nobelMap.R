library(tidyverse)
library(ggmap)

# Load data
nobel_winners <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-05-14/nobel_winners.csv")
countriesCoord <- readr::read_csv("http://worldmap.harvard.edu/download/wfs/34645/csv?outputFormat=csv&service=WFS&request=GetFeature&format_options=charset%3AUTF-8&typename=geonode%3Acountry_centroids_az8&version=1.0.0")

nobelCountries <- nobel_winners %>%
  select(c("birth_country", "death_country")) %>% 
  # remove NA death countries
  filter(!is.na(death_country)) %>%
  # Clean birth and death countries (to get current countries in map) 
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
  mutate_at(vars(birth_country, death_country),
            ~ replace(., which(.== "Czechoslovakia"), "Czech Republic")) %>%
  mutate_at(vars(birth_country, death_country),
            ~ replace(., which(.== "Northern Ireland"), "United Kingdom")) %>%
  mutate_at(vars(birth_country, death_country),
            ~ replace(., which(.== "Scotland"), "United Kingdom")) %>%
  mutate_at(vars(birth_country, death_country),
            ~ replace(., which(.== "East Germany"), "Germany")) %>%
  mutate_at(vars(birth_country, death_country),
            ~ replace(., which(.== "Republic of Macedonia"), "Macedonia")) %>%
  mutate_at(vars(birth_country, death_country),
            ~ replace(., which(.== "Serbia"), "Republic of Serbia")) %>%
  mutate_at(vars(birth_country, death_country),
            ~ replace(., which(.== "Guadeloupe Island"), "France")) %>%
  mutate_at(vars(birth_country, death_country),
            ~ replace(., which(.== "Union of Soviet Socialist Republics"), "Russia")) %>%
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

countriesLL <- countriesCoord %>% select(c("admin", "Longitude", "Latitude"))

nobelLL <- merge(nobelCountries, countriesLL, by.x = "birth_country", by.y = "admin", all.x = TRUE)
nobelLL <- nobelLL %>% rename(fromLong = Longitude, fromLat = Latitude) 
nobelLL <- merge(nobelLL, countriesLL, by.x = "death_country", by.y = "admin", all.x = TRUE)
nobelLL <- nobelLL %>% rename(toLong = Longitude, toLat = Latitude) 

worldmap <- borders("world", colour = "gray60", fill = "gray80", size = 0.05)

ggplot(nobelLL, aes(
  x = fromLong, y = fromLat,
  xend = toLong, yend = toLat,
  colour = colour,
  alpha = (colour != "gray60"))) +
  scale_color_identity() +
  scale_alpha_manual(values = c(0.3, 0.5), guide = FALSE) +
  scale_size_manual(values = c(0.05, 0.1), guide = FALSE) +
  labs(title = "Birth and death countries of Nobel laureates that were born and died in different countries",
       subtitle = "Arrow heads show the death country (USA, W. Germany, UK, Germany, France and Switzerland are the most common)", 
       caption = "\nSource: Kaggle | Graphic: Georgios Karamanis / @geokaramanis") +
  worldmap +
  coord_cartesian(ylim = c(-50, 105)) +
  geom_curve(aes(x = fromLong, y = fromLat,
                 xend = toLong, yend = toLat,
                 size = colour != "gray60"),
             arrow = arrow(length = unit(0.009, "npc"))) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F0EFF1", colour = "#F0EFF1"),
    text = element_text(family = "IBM Plex Sans", size = 5),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(vjust = 2),
  ) +
  ggsave("./week-20/nobelMap.png", width = 6, height = 3.5)

nobelLL %>% filter()