library(lubridate)

newscript <- readline(prompt = "Name of script: ")

# Get date of next Tuesday
i = wday(Sys.Date(), week_start = 1)
next_tuesday <- Sys.Date() + (7 - i + 2) %% 7

# Get ISO week, create new week folder and plot subfolder
which_week <- isoweek(next_tuesday)
folder <- paste0("2021/", "2021-week", formatC(which_week, width = 2, flag = "0")) 
dir.create(file.path(paste0(folder, "/plots")), recursive = TRUE)

# Create README 
readme <- paste0(folder, "/README.md")
file.create(readme)
readme_text <- paste0(
  "https://github.com/rfordatascience/tidytuesday/tree/master/data/2021/", next_tuesday, "\n\n![](plots/",
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
  '\n\n',
  '# export gif
  # gg_playback(frame_duration = 0.15, image_resize = 1080)
  # convert to mp4 in terminal
  # ffmpeg -i 2021_08_01_14_53_40.gif -movflags faststart -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" olympics_makingof.mp4',
  '\n\n',
  '# ggsave(here::here(',
  '\"temp\", paste0(\"',
  newscript,
  '-\", format(Sys.time(), \"%Y%m%d_%H%M%S\"), \".png\")), dpi = 320)',
  '\n'
)
write(as.character(script_text), file(script_file))

# Update script-of-the-week
sotw <- file("script-of-the-week.R", "wt")
sotw_text <- paste0(
  'eval(parse(text="source(\\"./',
  script_file,
  '\\")"))'
)
writeLines(sotw_text, sotw)

# Open script and start having fun!
file.edit(script_file)
