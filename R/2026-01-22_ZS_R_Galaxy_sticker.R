################################################################################
# LOGO GALAXYR
################################################################################

# install specific fork
pak::pak("emilioxavier/hexSticker")

# change working dir
setwd("./inst/figures")

# load packages
library(hexSticker)
library(showtext)
library(ggplot2)

font_add_google(
  name = "Comfortaa",
  family = "Comfortaa")
showtext_auto()

# load image
imgurl <- "galaxyr_transparent.png"

# make sticker
s <- sticker(
  imgurl, s_x = 1, s_y = 1.15, s_width = .7,
  package = "GalaxyR", p_size = 20, p_x = 1, p_y = 0.6, p_family = "Comfortaa", p_fontface = "bold",
  h_size = 2,  h_color = "#276ABE", h_fill = "#22272E",
  filename = "logo_galaxyr.png")
plot(s)

################################################################################
