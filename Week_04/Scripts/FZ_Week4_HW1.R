## Fabiola Gonzalez Zamora ##
### Week 4 Homework Pt. 1 ###
### Updated 2026-09-19 ###

#### Load Libraries ####
library(palmerpenguins)
library(tidyverse)
library(here)
library(dplyr)
library(ggplot2)
library(peRReo)
library(dadjokeapi)


### Homework Pt. 1 ###
# Calculate mean and variance of body mass by species, island, and sex without any NAs
penguins |>
  drop_na(species, island, sex) |>
  group_by(species, island, sex) |>
  summarise(mean_mass_g = mean(body_mass_g, na.rm = TRUE),
            variance_mass_g = sd(body_mass_g, na.rm = TRUE),
            .groups = "drop")

# Filters out (i.e. excludes) male penguins, then calculates the log body mass, then selects only the columns for species, island, sex, and log body mass, then use these data to make any plot
# Make sure the plot has clean and clear labels and follows best practices
# Save the plot in the correct output folder

# Get the color palette going here
isl = latin_palette('natti', 3)
spec = latin_palette('aventura', 3)

penguins |>
  filter(sex != "male") |> # Exclude male penguins
  mutate(log_mass = log(body_mass_g)) |> # Add column and calculate log body mass
  select(species, island, sex, log_mass) |> # Select columns
  ggplot(aes(x = island, 
             y = log_mass,
             fill = island)) +
  geom_violin(trim = FALSE, # Make violin plot
              alpha = 0.7,
              show.legend = FALSE) +
  geom_boxplot(width = 0.1, # Add box plot overlay 
               color = "black",
               show.legend = FALSE) +
  geom_jitter(aes(color = species), # Add points across penguin species to show relative contributions
              alpha = 0.9,
              width = 0.05,
              height = 0,
              size = 1.5,
              shape = 17, # 17 = triangles
              show.legend = TRUE) + # Here, I only want the legend to clarify what the different colored points represent
  stat_summary(fun = "mean", # Map the mean of each island
               geom = "point", 
               color = "black", 
               size = 2,
               show.legend = FALSE) +
  labs(title = "Log Mass of Female Penguins by Island",
       x = "Island",
       y = "log(Mass in grams)",
       color = "Species") +
  scale_fill_manual(values = isl, guide = "none") + # Guide = "none" keeps the colors but removes the legend
  scale_color_manual(values = spec) +
  theme_classic() +
  theme(plot.title = element_text(size = 20),
        axis.title = element_text(size = 13),
        legend.background = element_rect(fill = "linen")) # Add rectangle for legend for improved clarity

# Save the plot
ggsave(here("Week_04", "Output","logmassfemalepenguins.png"))
