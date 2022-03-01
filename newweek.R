library(lubridate)

newscript <- readline(prompt = "Name of script: ")

# Get date of next Tuesday
i = wday(Sys.Date(), week_start = 1)
next_tuesday <- Sys.Date() + (7 - i + 2) %% 7

# Get ISO week, create new week folder and plot subfolder
which_week <- isoweek(next_tuesday)
which_year <- isoyear(next_tuesday)

folder <- paste0(paste0(which_year, "/"), paste0(which_year, "-week_"), formatC(which_week, width = 2, flag = "0")) 
dir.create(file.path(paste0(folder, "/plots")), recursive = TRUE)

# Create README 
readme <- paste0(folder, "/README.md")
file.create(readme)
readme_text <- paste0(
  "https://github.com/rfordatascience/tidytuesday/tree/master/data/", which_year, "/", next_tuesday, "\n\n![](plots/",
  newscript,
  ".png)")
write(as.character(readme_text), file(readme))

# Create script file
script_file <- paste0(folder, "/", newscript, ".R")
file.create(script_file)
script_text <- paste0(
  'library(tidyverse)', '\n',
  'library(camcorder)',
  '\n\n',
  'gg_record(dir = "temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)',
  '\n\n'

)
write(as.character(script_text), file(script_file))

# Open script and start having fun!
file.edit(script_file)

closeAllConnections()
