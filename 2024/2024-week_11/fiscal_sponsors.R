library(tidyverse)
library(camcorder)

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

fiscal_sponsor_directory <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-03-12/fiscal_sponsor_directory.csv')

lgbtq_fiscal <- fiscal_sponsor_directory %>% 
  filter(str_detect(eligibility_criteria, "LGBTQ") | str_detect(project_types, "LGBTQ"))

ggplot(lgbtq_fiscal) +
  geom_point(aes(year_501c3, year_fiscal_sponsor, size = n_sponsored), shape = 21) +
  scale_size_area(max_size = 20)
