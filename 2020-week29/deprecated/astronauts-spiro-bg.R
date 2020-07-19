# Modified from
# https://stackoverflow.com/questions/46354513/diagonal-gradient-in-a-ggplot-background

xColor <- dnorm(seq(-1, 1, length.out = 100), 0, 0.5) # scale of color for x and y, 
yColor <- dnorm(seq(-1, 1, length.out = 100), 0, 0.5)
x <- seq(-190, 190, length.out = 100) # on respective x and y axis
y <- seq(-190, 190, length.out = 100)
df <- cbind(expand.grid(x = xColor, y = yColor), expand.grid(x = x, y = y)) #grid for colors
colnames(df) <- c("xColor", "yColor", "x", "y")
df$zColor <- (df$xColor ^ 2 + df$yColor ^ 2) # the color factor for radius

