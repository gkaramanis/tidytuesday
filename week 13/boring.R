library(ggplot2)
library(dplyr)
library(viridis)
library(png)

catpng <- readPNG('./img/sitcat.png')

seattle_pets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-26/seattle_pets.csv")

seattle_pets %>% group_by(species) %>%
  do(plots=ggplot(data=., aes(x=license_issue_date)) +
  geom_point())

seattle_pets$liceDate <-  as.Date(seattle_pets$license_issue_date, format='%B %d %Y')

lic <-  seattle_pets %>%
  mutate(month = format(liceDate, "%m"),
         year = format(liceDate, "%Y")) %>%
  group_by(year, month) %>%
  tally()

ggplot(lic, aes(month)) +
  geom_bar(aes(y=n, fill = factor(year)),
           width = 1, stat = "identity") +
  scale_fill_viridis(discrete = TRUE, option="viridis") +
  scale_y_sqrt() +
  theme_minimal() +
  labs(
    title = "Number of licenses issued per month",
    subtitle = "since 2003",
    caption = toupper("source: data.seattle.gov")
    ) +
  xlab("Month") +
  ylab("Licenses issued (log)") +
  annotation_raster(catpng, ymin = 4, ymax= 30,
                    xmin = 1, xmax = 3)
