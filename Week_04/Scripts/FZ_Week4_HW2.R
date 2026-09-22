### Fabiola Gonzalez Zamora ###
## Week 4 Homework Pt. 2 ##
## Updated 2026-09-21 ##

#### Load Libraries ####
library(tidyverse)
library(here)
library(dplyr)
library(ggplot2)
library(peRReo)
library(cowsay)

### Homework Pt. 2 ###
# Using chemistry data:
  # Remove NAs
  # Separate Tide_time column
  # Filter out subset
  # Use either pivot_longer() or pivot_wider()
  # Calculate summary statistics and export csv into "Output"
  # Make a plot (not a boxplot) and export to "Output"

### Load data ###
ChemData <- read_csv(here("Week_04", "Data", "chemicaldata_maunalua.csv"))
glimpse(ChemData)

ChemDictionary <- read_csv(here("Week_04", "Data", "chemicaldata_maunalua.csv"))
glimpse(ChemDictionary)

# Clean the data
ChemData_clean <- ChemData |>
  drop_na() |> # Drop NAs       
  separate_wider_delim(cols = Tide_time, 
                       delim = "_",  
                       names = c("Tide", "Time"),
                       cols_remove = FALSE) |>
  filter(Site == "W") |> # Filter only observations at Site W
  pivot_longer(cols = c(Phosphate, Silicate, NN), # Select nutrients to pivot
               names_to = "Nutrient", # New column for old column names
               values_to = "Concentration") 

head(ChemData_clean)

### Summary Statistics ###
ChemSummary <- ChemData_clean |>
  group_by(Nutrient, Tide, Time) |>
  summarise(mean_concentration = mean(Concentration), # Mean of concentration
            var_concentration = var(Concentration),   # Variance
            sd_concentration = sd(Concentration),     # Standard deviation 
            sample_size = n(),
            sd_error = sd_concentration / sqrt(sample_size), # Standard error
            .groups = "drop") # removes all grouping after the summary is calculated

head(ChemSummary)

### Export summary statistics ###
write_csv(ChemSummary,
          here("Week_04", "Output", "chemistry_summary.csv"))

### PLOT TIME ###
# Let's get the palette going, peRReo palette is the goat
niteowl <- latin_palette("shakira", 2)

ChemPlot <- ggplot(
  data = ChemSummary,
  mapping = aes(
    x = Tide,
    y = mean_concentration,
    fill = Time)) +
  geom_col( 
    position = position_dodge(width = 1), # Using position_dodge() positions adjacent bars 
    width = 0.8, # Assign width of the bars themselves
    alpha = 0.7) + # Make bars a bit transparent for error bars to stand out
  geom_errorbar(aes(
    ymin = mean_concentration - sd_error, # Format error bars using the summary we calculated above
    ymax = mean_concentration + sd_error, 
    color = Time),
    position = position_dodge(width = 1), # Using position_dodge() same as above  
    width = 0.8, 
    show.legend = FALSE) +
  facet_wrap(~Nutrient,
             scales = "free_y") + # scales = "free" releases y-axis
  scale_fill_manual(values = niteowl) + # assign color palette to concentration columns
  scale_color_manual(values = niteowl) + # assign color palette to error bars
  labs(
    title = "Nutrient Concentration at Site 'W'",
    subtitle = "Mean Nutrient Concentration by Tide and Time of Day",
    x = "Tide",
    y = "Mean concentration (umol/L)",
    fill = "Time of Day") +
  theme_classic() + 
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "linen")
  )

ChemPlot


### Export Plot ### 
ggsave(here("Week_04", "Output","nutrient_concentrations_site_w.png"))



