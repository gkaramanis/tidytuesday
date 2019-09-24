library(visdat)
library(sugrrants)
# devtools::install_github("EmilHvitfeldt/prismatic")
library(prismatic)
library(tidyverse)

# https://twitter.com/infobeautiful/status/1176163067631218688?s=20

school_diversity <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-24/school_diversity.csv")
