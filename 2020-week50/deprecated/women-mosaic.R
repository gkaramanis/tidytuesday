library(RsimMosaic)

createTiles(inPath = here::here("2020-week50", "img"), outPath = here::here("2020-week50", "img", "tiles"), tileHeight = 40, verbose = TRUE)

composeMosaicFromImageRandomOptim(
  originalImageFileName = "/Users/georgios/Documents/Projects/R/tidytuesday/2020-week50/img/unsung-hero.jpeg",
  outputImageFileName = "/Users/georgios/Documents/Projects/R/tidytuesday/2020-week50/plots/unsung-gradient.png",
  imagesToUseInMosaic = "/Users/georgios/Documents/Projects/R/tidytuesday/2020-week50/img/tiles/"
)
