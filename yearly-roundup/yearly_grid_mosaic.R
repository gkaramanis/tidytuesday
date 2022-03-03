# # cd to TidyTuesday directory
# 
# find . -name "*png" | grep -i "plots" | xargs -I{} cp -p "{}" ~/SynologyDrive/R/tidytuesday/yearly-roundup/NEWYEAR/
#   
# # cd to NEWYEAR
#   
# mkdir resized
# 
# sips -Z 1200 *.png --out "resized/"
# 
# montage -density 300 -tile 9x0 -border 0 -background lightgrey *.png year.png

library(dplyr)
library(stringr)

plots <- list.files(c("2021", "2022"), pattern = "png", recursive = TRUE, full.names = TRUE) %>% 
  as_tibble() %>% 
  rename("from" = "value") %>% 
  filter(str_detect(from, "plots")) %>% 
  mutate(to = paste0("yearly-roundup/third-year/", str_replace_all(from, "/", "_")))

file.copy(plots$from, plots$to)

dir.create("yearly-roundup/third-year/resized/")

# Remove unneeded images before next step

system('sips -Z 800 yearly-roundup/third-year/*.png --out \"yearly-roundup/third-year/resized/\"')

system('/opt/homebrew/bin//montage -density 300 -tile 9x0 -geometry 400x400+10+10 -border 0 -background lightgrey yearly-roundup/third-year/resized/*.png yearly-roundup/third-year/third-year.png')
