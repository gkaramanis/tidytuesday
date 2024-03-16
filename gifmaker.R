library(camcorder)

# export gif
gg_record(dir = "tidytuesday-temp")
gg_playback(frame_duration = 0.15, image_resize = 1080, name = "tidytuesday-temp/animated.gif", playback = FALSE)

# convert to mp4
system('/opt/homebrew/bin//ffmpeg -i ./tidytuesday-temp/animated.gif -movflags faststart -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" -vf tpad=stop_mode=clone:stop_duration=0.1 ./tidytuesday-temp/making-of.mp4')