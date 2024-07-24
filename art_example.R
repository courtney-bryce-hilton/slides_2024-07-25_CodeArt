# PROMPT: create a piece of generative aRt consisting of 5 points / objects and a line
# each point / object should be a different colour and size

# libraries ---------------------------------------------------------------


library(tidyverse)
library(here)


# time to make art --------------------------------------------------------


data_for_points <- tibble(
  x = seq(1,5),
  y = seq(1,5),
  sizes = seq(1,5),
  colours = c("#003049", "#d62828", "#f77f00", "#fcbf49", "#eae2b7")
)

data_for_line <- tibble(
  x = seq(1,5),
  y = seq(1,5)
)


ggplot() + 
  geom_line(data = data_for_line, mapping = aes(x = x, y = y)) +
  geom_point(data = data_for_points, mapping = aes(x = x, y = y, size = sizes, colour = colours)) + 
  scale_x_continuous(expand = c(.2,.2)) +
  scale_y_continuous(expand = c(.2,.2)) +
  theme_void() + 
  theme(
    aspect.ratio = 1,
    legend.position = "none",
    plot.background = element_rect(fill = "white", colour = "black")
  )


# export your aRt ---------------------------------------------------------


ggsave("my_art.png", path = here("viz"), height = 5, width = 5)


# hmm ---------------------------------------------------------------------


x <- seq(0, 5, by = 0.1)
y <- x + sin(x * 2 * pi * 5 / max(x)) * x / 2  # 5 wiggles
data <- data.frame(x, y)

ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "blue") +
  geom_point(data = data_for_points, mapping = aes(x = x, y = y, size = sizes, colour = colours)) + 
  scale_x_continuous(limits = c(0,7), expand = c(.2,.2)) +
  scale_y_continuous(limits = c(0,7), expand = c(.2,.2)) +
  theme_void() + 
  theme(
    aspect.ratio = 1,
    legend.position = "none"
  )


# -------------------------------------------------------------------------