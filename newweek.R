library(lubridate)

newscript <- readline(prompt = "Name of script: ")
# which_week <- menu(c("Current", "Next"), title = "Which week?") - 1

# Get date of next Tuesday
i = wday(Sys.Date(), week_start = 1)
next_tuesday <- Sys.Date() + (7 - i + 2) %% 7

# Get ISO week, create new week folder and plot subfolder
which_week <- isoweek(next_tuesday)
folder <- paste0("2020-week", which_week) 
dir.create(file.path(folder, "plots/temp"), recursive = TRUE)

# Create README 
readme <- paste0(folder, "/README.md")
file.create(readme)
readme_text <- paste0(
  "https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/", next_tuesday, "\n\n![](plots/",
  newscript,
  ".png)")
write(as.character(readme_text), file(readme))

# Create script file
script_file <- paste0(folder, "/", newscript, ".R")
file.create(script_file)
script_text <- paste0(
  'library(tidyverse)',
  '\n\n',
  'ggsave(here::here(\"',
  folder,
  '\", \"plots\", \"temp\", paste0(\"',
  newscript,
  '-\", format(Sys.time(), \"%Y%m%d_%H%M%S\"), \".png\")), dpi = 320)'
)
write(as.character(script_text), file(script_file))

# Open files to edit
file.edit(script_file)
file.edit(readme)
