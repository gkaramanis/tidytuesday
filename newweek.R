library(lubridate)

# Get current ISO week, create new week folder and plot subfolder
folder <- paste0("2020-week", isoweek(Sys.Date())) 
dir.create(file.path(folder, "plots"), recursive = TRUE)

# Create README and script file
readme <- paste0(folder, "/README.md")
file.create(readme)
newscript <- readline(prompt = "Name of script: ")
filename <- paste0(folder, "/", newscript, ".R")
file.create(filename)

file.edit(filename)
file.edit(readme)
