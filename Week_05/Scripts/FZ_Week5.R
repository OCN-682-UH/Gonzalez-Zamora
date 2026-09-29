### Fabiola Gonzalez Zamora ###
#### Week 5, Part 1 ####
### Updated 2026-09-22 ###

#### Load Libraries ####
library(tidyverse)
library(here)
library(dplyr)
library(ggplot2)
library(peRReo)
library(dadjokeapi)


#### Joins & Dates with Lubridate ####
# Multiple hierarchies of data, want to join into big data set
# Crucially, needs to be column name that is consistent across data frames

T1 <- tibble(
  Site.ID = c("A", "B", "C", "D"), 
  Temperature = c(14.1, 16.7, 15.3, 12.8)
)

T1

T2 <- tibble(
  Site.ID = c("A", "B", "D", "E"),
  pH = c(7.3, 7.8, 8.1, 7.9)
)

T2

### left_join ###
# left_join() keeps all rows from the left (first) dataframe and adds matching rows from the right

left_join(T1, T2) # Which ever is on left is what stays

### right_join ###
# right_join() is opposite

right_join(T1, T2)

### inner_join ###
# inner_join() keeps only rows that exist in both dataframes
 
inner_join(T1, T2) # Drops sites C and E becuase A, B, D are the only rows that exist in both

### full_join###
# full_join() keeps errything

full_join(T1, T2)

### semi_join ###
# semi_join() keeps rows from first where there are matches in the second
# Only returns with columns from the first

semi_join(T1, T2)

### anti_join ###
# anti_join() returns rows in the first dataframe that do not match the second

anti_join(T1, T2) # Only Site C is returned since it has no match in T2

### Different or multiple column names ###

T3 <- tibble(
  SiteID = c("A", "B", "C", "D"),  # Note: different name!
  Chlorophyll = c(2.3, 3.1, 1.9, 2.8)
)

T3

# Joining with by = () argument

left_join(T1, T3, by = c("Site.ID" = "SiteID"))

T4 <- tibble(
  Site.ID = c("A", "A", "B", "B"),
  Year = c(2020, 2021, 2020, 2021),
  Biomass = c(12.5, 15.3, 18.2, 16.9)
)

T5 <- tibble(
  SiteID = c("A", "A", "B"),
  Year = c(2020, 2021, 2021),
  Nutrients = c(8.2, 7.9, 9.1)
)

left_join(T4, T5, by = c("Site.ID" = "SiteID", "Year" = "Year")) # Even though Year appears with same name, still need to include in the by argument when joining by multiple columns

### Naming Conflicts ###

T6 <- tibble(
  Site.ID = c("A", "B", "C"),
  Notes = c("pristine", "degraded", "moderately impaired")
)

T7 <- tibble(
  Site.ID = c("A", "B", "D"),
  Notes = c("sunny", "shaded", "partially shaded"),
  Quality = c("good", "fair", "poor")
)

# The .x and .y suffixes on the notes can be problematic when joining

T6_renamed <- T6 |> 
  rename(Condition_Notes = Notes)

T7_renamed <- T7 |> 
  rename(Habitat_Notes = Notes)

left_join(T6_renamed, T7_renamed, by = "Site.ID")


#### Dates & Times with Lubridate ####

### What time is it? ###

now()

[1] "2026-09-22 13:37:21 HST"

now(tzone = "US/Hawaii")

[1] "2026-09-22 13:37:57 HST"

### Date ###

today()
[1] "2026-09-22"

### Time Checks ###

am(now())
[1] FALSE

leap_year(now())
[1] FALSE

### Date specifications ###
# lubridate() guesses date format from character strings using year, month, day

# 2021-02-24        = ymd()
# 02/24/2021        = mdy()
# February 24 2021  = mdy()
# 24/02/2021        = dmy()

# Make sure that date column is a character, not a factor

### Date & time ###

# 2021-02-24 10:22:20 PM          = ymd_hms()
# 02/24/2021 22:22:20             = mdy_hms()
# February 24 2021 10:22 PM       = mdy_hm()

### Create vector of datetimes ###
datetimes <- c(
  "02/24/2021 22:22:20",
  "02/25/2021 11:21:10",
  "02/26/2021 8:01:52"
)

datetimes

# Convert vector to datetime objects

datetimes <- mdy_hms(datetimes)

datetimes # Output now shows POSIXct dates with times

### Extract month ###
# As number

month(datetimes)

# Abbreviated label

month(datetimes, label = TRUE)

# Full name

month(datetimes, label = TRUE, abbr = FALSE)

# Day of the month

day(datetimes)

# Day of the week

wday(datetimes, label = TRUE)

# Extract hour, minute, second

hour(datetimes)
minute(datetimes)
second(datetimes)

# Adding time intervals: add 4 hours

datetimes + hours(4)

# hour() extracts hour component
# hours() adds hours to datetime

# Same pattern applies to all time units 

### Rounding dates: Round to nearest minute ###

round_date(datetimes, "minute")

### Timezone awareness ###

datetime_naive <- mdy_hms("02/24/2021 10:22:20")
datetime_naive # No timezones = BAD

# with_tz() to interprest the same clock time in different timezone
# Assuming naive time is in Hawaii

hawaii_time <- with_tz(datetime_naive, tzone = "US/Hawaii")
hawaii_time

# Same moment, viewed from EST
est_time <- with_tz(hawaii_time, tzone = "EST")
est_time

# force_tz() --> change timezone label
# use force_tz() to reassign a naive dataframe to a specific timezone

# Claim this was collected in Hawaii (though it was naive)
force_hawaii <- force_tz(datetime_naive, tzone = "US/Hawaii")
force_hawaii

# Now convert to EST (this changes the clock time!)
with_tz(force_hawaii, tzone = "EST")

#### Challenge ####
CondData <- read_csv(here("Week_05", "Data", "CondData.csv"))

head(CondData)

CondData <- read_csv(here("Week_05", "Data", "CondData.csv")) |>
  mutate(datetime = mdy_hms(date))

head(CondData)

SiteCharac <- read_csv(here("Week_05", "Data", "site.characteristics.data.csv"))
Topt <- read_csv(here("Week_05", "Data", "Topt_data.csv"))

# Pivot Wider
Wide_Site <- SiteCharac |>
  pivot_wider(names_from = "parameter.measured",
              values_from = "values")

Joined <- full_join(Wide_Site, Topt)
view(Joined)

### Fabiola Gonzalez Zamora ###
#### Week 5, Part 2 ####
### Updated 2026-09-27 ###

#### Load Libraries ####
library(patchwork)  # for bringing plots together
library(ggrepel)    # for repelling labels
library(gganimate)  # smooth animations
library(gifski)     # for saving gifs
library(plotly)     # for interactive animations
library(magick)     # for images

renv::install("gifski", rebuild = TRUE)
#### Patchwork ####
# patchwork() makes it easy to bring plots together
# Let's make two plots for an easy example

p1 <- penguins |>
  ggplot(aes(x = body_mass_g, 
             y = bill_length_mm, 
             color = species)) +
  geom_point()

p1

p2 <- penguins |>
  ggplot(aes(x = sex, 
             y = body_mass_g, 
             color = species)) +
  geom_jitter(width = 0.2)

p2

# Combining Plots 

p1 + p2 + # Combines plots
  plot_layout(guides = "collect") + # Collect legends
  plot_annotation(tag_levels = "A") + # Add labels

# Stack plots vertically with / 

p1 / p2 +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "")

#### ggreppel ####
# ggrepel makes it easy to add clear, non-overlapping labels to plots

head(mtcars)
  
# Ugly plot, how make better?
ggplot(mtcars, aes(x = wt, 
                   y = mpg, 
                   label = rownames(mtcars))) +
  geom_text() +
  geom_point(color = 'red')

# Repel labels with geom_text_repel()
ggplot(mtcars, aes(x = wt, 
                   y = mpg, 
                   label = rownames(mtcars))) +
  geom_text_repel() +
  geom_point(color = 'red')

# Use geom_label_repel() for boxes
ggplot(mtcars, aes(x = wt, 
                   y = mpg, 
                   label = rownames(mtcars))) +
  geom_label_repel() +
  geom_point(color = 'red')

#### gganimate ####
# gganimate() lets you create animations

# Static... BORING
penguins |>
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point()

# Add transition with transition_states()
penguins |>
  drop_na(body_mass_g, bill_depth_mm, species, year) |>
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point() +
  transition_states(
    year,
    transition_length = 2,
    state_length = 1
  )

# Add a dynamic title with labs()
penguins |>
  drop_na(body_mass_g, bill_depth_mm, species, year) |>
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point() +
  transition_states(year, 
                    transition_length = 2, 
                    state_length = 1) +
  labs(title = 'Year: {closest_state}') # The placeholder {closest_state} updates with each frame

# Save animation as GIF with anim_save
p <- penguins |>
  drop_na((body_mass_g, bill_depth_mm, species, year) |>
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point() +
  transition_states(year, 
                    transition_length = 2, 
                    state_length = 1) +
  ease_aes("sine-in-out") +
  labs(title = 'Year: {closest_state}') 
anim_save(here("Week_05", "Output", "penguin_animation.gif"), animation = p)

#### plotly ####
# plotky creates interactive plots with built-in animation

# Create interactive scatter plot
penguins |>
  plot_ly(x = ~body_mass_g,
          y = ~bill_depth_mm,
          color = ~species,
          type = "scatter",
          mode = "markers") |>
  layout(title = "Penguin Body Mass vs Bill Depth",
         xaxis = list(title = "Body Mass (g)"),
         yaxis = list(title = "Bill Depth (mm)"))

# Animate species with frame
penguins |>
  plot_ly(x = ~body_mass_g,
          y = ~bill_depth_mm,
          frame = ~species,
          color = ~species,
          type = "scatter",
          mode = "markers",
          marker = list(size = 8)) |>
  layout(title = "Penguin Characteristics",
         xaxis = list(title = "Body Mass (g)"),
         yaxis = list(title = "Bill Depth (mm)"))

## Advantages of plotly over gganimate ##
# Interactive   -> hover, zoom, pan, click to explore
# Easier syntax -> less code than gganimate
# Web-ready     -> works in HTML, Shiny, dashboards
# 3D capable    -> can create 3D scatter plots and surfaces
# No rendering  -> animations happen in the browser, not rendered as GIFs

#### magick ####
# magick lets you read, process, and composite images programmatically

# Read an image with image_read()
penguin <- image_read("https://pngimg.com/uploads/penguin/pinguin_PNG9.png")

penguin

# Save a plot as an image
penguinplot<-penguins |>
  ggplot(aes(x = body_mass_g, 
             y = bill_depth_mm, 
             color = species)) +
  geom_point() 
ggsave(here("Week_05", "Output", "penguinplot.png"))
penguinplot

# Composite images with image_composite()
penplot <- image_read(here("Week_05", "Output", "penguinplot.png"))
out <- image_composite(image = penplot,       composite_image = penguin, offset = "+70+30")
out

# Animate composite images
pengif <- image_read("https://media3.giphy.com/media/H4uE6w9G1uK4M/giphy.gif")
outgif <- image_composite(penplot, pengif, gravity = "center")
animation <- image_animate(outgif, fps = 10, optimize = TRUE)
animation

#### Sourdough Package! ####
remotes::install_github("andrewheiss/sourrr")
library(sourrr)
build_recipe(final_weight = 900, hydration = 0.75)


#### Uploading on Repository ####

# cd "/Users/fabizamora/Desktop/MBIO612/Repositories/Fall_2026/Fall_2026/Gonzalez-Zamora/Gonzalez-Zamora"
# git add Week_05/Scripts/FZ_Week5.R
# git add Week_05/Scripts/FZ_Week5_HW.R
# git add Week_05/Data/
# git add Week_05/Data/
# git add Week_05/Output/chemistry_summary.csv
# git add Week_05/Output/nutrient_concentrations_site_w.png
# git commit -m "Add Week 5 chemistry homework"
# git push

