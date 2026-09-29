### Fabiola Gonzalez Zamora ###
#### Week 5 ####
### Updated 2026-09-22 ###

#### Load Libraries ####
library(tidyverse)
library(here)
library(dplyr)
library(ggplot2)
library(peRReo)
library(lubridate)

#### Homework Pt. 1 ####
### Read in conductivity data ###

CondData <- read_csv(here("Week_05", "Data", "CondData.csv")) |>
  mutate(date = mdy_hms(date), 
         date = round_date(date, unit = "10 seconds")) 
glimpse(CondData)

# mdy_hms() = 2021-02-24 10:22:20 PM
# round_date(date, unit = "10 seconds) rounds time to 10 seconds

### Read in depth data ###
DepthData <- read_csv(here("Week_05", "Data", "DepthData.csv"))

glimpse(DepthData)

### Join the two dataframes using inner_join() (only exact matches) ###
# inner_join() keeps only rows that exist in both dataframes

inner_join(CondData, DepthData)

# Output: Joining with `by = join_by(date)`
# Tibble
# date                Temperature Serial Salinity AbsPressure  Depth
# 2021-01-15 09:54:30        29.2    316     34.9        102. -0.009

### Calculate averages of date, depth, temperature, and salinity by minute ###
AvgData <- CondData |>
  inner_join(DepthData,
             by = "date") |>
  mutate(
    date = floor_date(date, unit = "minute")) |>
  group_by(date) |>
  summarise(
    average_depth = mean(Depth, na.rm = TRUE),
    average_temperature = mean(Temperature, na.rm = TRUE),
    average_salinity = mean(Salinity, na.rm = TRUE),
    .groups = "drop"
  )

### Export Average Data ###

write_csv(AvgData, here("Week_05", "Output", "averages_by_minute.csv"))

### Make a plot using the averaged data ###

# But first, gotta make the colors pretty

hola = latin_palette('aventura', 3)

# Reshape data and make the line plot showing the relative averages for each measurement

AvgPlot <- AvgData |>
  pivot_longer( # Reshape the data frame long format for the plot
    cols = c(
      average_depth,
      average_temperature,
      average_salinity),
    names_to = "measurement",
    values_to = "average") |>
  ggplot(
    aes(
      x = date,
      y = average,
      color = measurement)) + 
  geom_line(linewidth = 0.6,
            show.legend = FALSE) + # No need for the legend
  facet_wrap(
    ~ measurement, # Facet via measurement
    scales = "free_y",
    ncol = 1,
    labeller = as_labeller( # Labeller function allows you to alter grid titles for facet-wrapped figures
      c(
        average_depth = "Average Depth (m)",
        average_temperature = "Average Temperature (Cº)",
        average_salinity = "Average Salinity"))) +
  scale_color_manual(values = hola) +
  labs(
    title = "Average Conditions by Minute",
    x = "Date and Time",
    y = "Average value") +
    theme_gray()

AvgPlot

### Export Plot ###

ggsave(filename = here("Week_05", "Output", "average_conditions_by_minute.png"),
       plot = AvgPlot)

