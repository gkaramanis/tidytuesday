library(lubridate)

newscript <- readline(prompt = "Name of script: ")

# Get current ISO week, create new week folder and plot subfolder
folder <- paste0("2020-week", isoweek(Sys.Date())) 
dir.create(file.path(folder, "plots"), recursive = TRUE)

# Create README 
readme <- paste0(folder, "/README.md")
file.create(readme)
readme_text <- paste0(
  "https://github.com/rfordatascience/tidytuesday/tree/master/data/2020/2020-\n\n![](plots/",
  newscript,
  ".png)")
write(as.character(readme_text), file(readme))

# Create script file
filename <- paste0(folder, "/", newscript, ".R")
file.create(filename)

# Open files to edit
file.edit(filename)
file.edit(readme)
