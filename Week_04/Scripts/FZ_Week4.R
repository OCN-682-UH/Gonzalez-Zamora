## Fabiola Gonzalez Zamora ##
### Week 4 ###
### Updated 2026-09-15 ###

#### Load Libraries ####
library(palmerpenguins)
library(tidyverse)
library(here)
library(dplyr)
library(dadjokeapi)

#### Data Wranging: dplyr ####
# Extract rows with filter()
# Extract columns with select()
# Arrange/sort rows with arrange()
# New columns with mutate()
# Group summaries with group_by() %>% summrize()

filter(.data = DATA,...)
# Data = data frame to transform
# ... = One or more conditions
# filter() returns each row for which condition is true

head(penguins)
filter(.data = penguins, sex =="female") 
penguins |> filter(sex == "female")     # Does same as above

# When you use |>, the dataframe is passed automatically (i.e don't have to write .data = explicitly)

filter(penguins, year == "2008")       # Penguins measured in 2008
filter(penguins, body_mass_g > "5000") # Penguins weight more than 5000 grams

#### Boolean Operators ####
fat_females <- filter(penguins, sex == "female", body_mass_g > 5000)

eightnine <- filter(penguins, year == 2008 | year == 2009)

nightmare <- filter(penguins, island != "Dream") # Basically a not equal to sign

adandgent <- filter(penguins, species %in% c("Gentoo", "Adelie")) # We can use %in% instead of multiple tests

#### Mutate ####
# Add a new column converting body mass in grams to kilograms
penguins <- mutate(penguins,
       body_mass_kg = body_mass_g / 1000,
       bill_length_depth = bill_length_mm / bill_length_mm)

#### Conditional Statements ####
condition1 <- mutate(penguins,
       after_2008 = if_else(year > 2008, "After 2008", "Before 2008"))

# New column + Condition
# Adding flipper length to body mass as a new column
penguins <- mutate(penguins,
                   flipmass = flipper_length_mm + body_mass_g)

# Adding column whose condition is whether body mass is greater than 4000 grams
penguins <- mutate(penguins,
                     chunk = if_else(body_mass_g > 4000, "Big", "Small"))

#### Pipe Operator ####
# Filter only female penguins and add a new column that calculates the log body mass
# Using the |> the dataframe carries over
penguins |>
  filter(sex == "female") |>
  mutate(log_mass = log(body_mass_g))

# Select
# Use select() to select certain columns to remains in the dataframe
penguins |>
  filter(sex == "female") |>
  mutate(log_mass = log(body_mass_g)) |>
  select(species, island, sex, log_mass)

# Can use select() to rename columns
penguins |>
  filter(sex == "female") |>
  mutate(log_mass = log(body_mass_g)) |>
  select(Species = species, island, sex, log_mass)

# Arange
# Use arrange() to sort rows by a column, ascending by default
penguins |>
  arrange(body_mass_g) 

penguins |>
  arrange(desc(body_mass_g)) # Use desc() to sort in descending order

# Summarise
# Compute a table of summarized data
# Calculate mean and min flipper length and exclude any NAs
penguins |>
  summarise(mean_flipper = mean(flipper_length_mm, na.rm = TRUE),
            min_flipper = min(flipper_length_mm, na.rm = TRUE)) # na.rm removes NAs


# Group_by
# Summarize values by certain groups
# group_by() does not do anything, but powerful when put before summarise()
penguins |>
  group_by(island) |>
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_bill_length  = max(bill_length_mm, na.rm = TRUE),
            n                = n())
# Example Output
# A tibble: 3 × 4
# island    mean_bill_length    max_bill_length     n
# <fct>                 <dbl>            <dbl>   <int>
#  1 Biscoe               45.3            59.6    168
#  2 Dream                44.2            58      124
#  3 Torgersen            39.0            46       52

# Count
# count() is quick shortcut for counting rows per group
# Can do multiple variables at once
penguins |>
  count(species, island)

# Remove NAs
# drop_na() drops rows with NAs from a specific column
# Drop all the rows that are missing data on sex, then calculate mean bill length by island and sex
penguins |>
  drop_na(sex) |>
  group_by(island, sex) |>
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE))

#### Pipe into ggplot ####
# Can connect wrangling to a plot with a pipe
# Will not need to call the dataframe in ggplot if you pipe it
# Drop NAs from sex, then plot boxplots of flipper length by sex
penguins |>
  drop_na(sex) |>
  ggplot(aes(x = sex, y = flipper_length_mm)) +
  geom_boxplot()

groan()
# Why do trees seem suspicious on sunny days? Dunno, they're just a bit shady.
