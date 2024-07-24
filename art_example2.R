# PROMPT: draw random values from some kind of distribution to create texture


# libraries ---------------------------------------------------------------


library(tidyverse)
library(here)


# dataset parameters ------------------------------------------------------


set.seed(02141)

colour_palette <- c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51")

offsets <- list(
  c(-2,2),
  c(0,2),
  c(2,2),
  c(-2,0),
  c(0,0),
  c(2,0),
  c(-2,-2),
  c(0,-2),
  c(2,-2)
)


# generate dataset --------------------------------------------------------


art_maker <- function(seed, n_points_avg, n_points_sd, colour_palette, background_colour) {
  set.seed(seed)
  
  data <- map(offsets, \(.offset) {
    colour <- sample(colour_palette, size = 1)
    n_points <- rnorm(1, mean = n_points_avg, sd = n_points_sd) |> as.integer() |> abs()
    
    tibble(
      x = rnorm(n_points, mean = .offset[1], sd = 0.3),
      y = rnorm(n_points, mean = .offset[2], sd = 0.3),
      size = runif(n_points, min = 0.2, max = 4),
      colour = colour
    )
  }) |> 
    list_c()
  
  ggplot(data = data, mapping = aes(x = x, y = y, colour = colour, size = size)) + 
    geom_point(pch = 1) + 
    scale_colour_identity() +
    scale_size_identity() +
    lims(
      x = c(-3,3),
      y = c(-3,3)
    ) +
    theme_void() + 
    theme(
      plot.background = element_rect(fill = background_colour, colour = "black")
    )
}


# plot --------------------------------------------------------------------


art_maker(02141, 100, 50, c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51"), "#eaf4f4")

art_maker(6083, 70, 40, c("#ffbe0b", "#fb5607", "#ff006e", "#8338ec", "#3a86ff"), "#eaf4f4")



# save --------------------------------------------------------------------


ggsave("my_art2.png", path = here("viz"), height = 5, width = 5)


# -------------------------------------------------------------------------