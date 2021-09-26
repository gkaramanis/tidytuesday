library(camcorder)

# export gif
gg_record(dir = "temp")
gg_playback(frame_duration = 0.15, image_resize = 1080, name = "animated.gif", playback = FALSE)

# convert to mp4
system('ffmpeg -i ./temp/animaged.gif -movflags faststart -pix_fmt yuv420p -vf "scale=trunc(iw/2)*2:trunc(ih/2)*2" ./temp/making-of.mp4')