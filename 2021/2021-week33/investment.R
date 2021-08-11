library(tidyverse)library(camcorder)

gg_record(dir = "temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

# export gif
  # gg_playback(frame_duration = 0.15, image_resize = 1080)
  # convert to mp4 in terminal
  # ffmpeg -i 2021_08_01_14_53_40.gif -movflags faststart -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" olympics_makingof.mp4

# ggsave(here::here("temp", paste0("investment-", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")), dpi = 320)

