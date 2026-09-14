install.packages("palmerpenguins")

library(palmerpenguins)
library(tidyverse)
library(penguins)

ggplot(data = penguins, 
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm)) + 
  geom_point() +
  labs(title = "Bill depth and length",
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x = "Bill depth (mm)", y = "Bill length (mm)",
       color = "Species",
       caption = "Source: Palmer Station LTER / palmerpenguins package") +
  scale_color_viridis_d()
                     