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


Change year folder!
  
  
library(dplyr)
library(stringr)

plots <- list.files(c("2022", "2023"), pattern = "png", recursive = TRUE, full.names = TRUE) %>% 
  as_tibble() %>% 
  rename("from" = "value") %>% 
  filter(str_detect(from, "plots")) %>% 
  mutate(to = paste0("yearly-roundup/fourth-year/", str_replace_all(from, "/", "_")))

file.copy(plots$from, plots$to)

dir.create("yearly-roundup/fourth-year/resized/")

# Remove unneeded images before next step

system('sips -Z 800 yearly-roundup/fourth-year/*.png --out \"yearly-roundup/fourth-year/resized/\"')

system('/opt/homebrew/bin//montage -density 300 -tile 9x0 -geometry 400x400+10+10 -border 0 -background lightgrey yearly-roundup/fourth-year/resized/*.png yearly-roundup/fourth-year/fourth-year.png')
