library(tidyverse)

car_corp <- read_csv("week-42/car-corp.csv")

car_corp_long <- car_corp %>% 
  pivot_longer(brand1:part2, names_to = "ownership", values_to = "brand", values_drop_na = TRUE) %>%
  mutate(
    ownership = replace(ownership, str_detect(ownership, "brand"), "full"),
    ownership = replace(ownership, str_detect(ownership, "part"), "partial")
  )

write_csv(car_corp_long, "week-42/car-corp-long.csv")
