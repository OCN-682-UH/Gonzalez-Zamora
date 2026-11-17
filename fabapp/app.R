#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

## Load Libraries -------------------------------------------------------------
library(shiny)
library(readr)
library(tidyverse)
library(here)
library(ggplot2)
library(dplyr)
library(here)
library(peRReo)
library(shinyWidgets)
library(bslib)


## Read in Data ---------------------------------------------------------------
edible_plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-02-03/edible_plants.csv')

glimpse(edible_plants)


## Choices for the "Compare by" dropdown --------------------------------------
# NAMES are what the user sees; VALUES are the real column names

numeric_vars <- c(
  "Preferred pH (lower)"        = "preferred_ph_lower",
  "Preferred pH (upper)"        = "preferred_ph_upper",
  "Days to germination"         = "days_germination",
  "Germination temperature (°C)" = "temperature_germination",
  "Energy (kcal)"               = "energy"
)


## Make the palette -----------------------------------------------------------
pal <- setNames(
  latin_palette("natti", length(numeric_vars)),
  numeric_vars)

## UI - User View -------------------------------------------------------------
#
# Every inputId creates a value the server reads as input$<id>.
# Every *Output() reserves a space the server fills via output$<id>.

ui <- fluidPage(
  titlePanel("Edible Plants"),
  
 # sidebarLayout(
 #   sidebarPanel(
 #   selectInput(
 #      inputId = "plant", # creates input$plant
 #      label = "Choose a plant", 
 #     choices = sort(unique(edible_plants$common_name))) # options
 #   ),
    
    mainPanel(
      selectInput(
        inputId = "plants",
        label = "Choose plants to compare",
        choices = sort(unique(edible_plants$common_name)),
        selected = c("Cauliflower", "Beans (Runner)", "Beetroot"),
        multiple = TRUE),
      
      selectInput(
        inputId = "variable",
        label = "Compare by",
        choices = numeric_vars
      )
    ),
 
 hr(),
 
 actionButton("plant_garden", 
              "Plant a random garden",
              icon = icon("seedling")),
 
 mainPanel(
   plotOutput("compare_plot"),   # filled by output$compare_plot
   tableOutput("compare_table"), # filled by output$compare_table
   hr(),   
   h4("Your Random Garden"),     # filled by output$garden_plot
   plotOutput("garden_plot")
 ),

  theme = bs_theme(bootswatch = "morph")
)
  
## SERVER ---------------------------------------------------------------------

server <- function(input, output, session){
  
# ---- Shared computation -----------------------------------------------------
# One reactive, read by both the plot and the table
    chosen <- reactive({
    req(input$plants)
    edible_plants |>
      filter(common_name %in% input$plants) |> # %in%, because it's a vector
      select(taxonomic_name,
             common_name,
             cultivation,
             water,
             sunlight,
             value = all_of(input$variable)) |>
      # Three of these columns are character, not numeric
      # as.numeric() turns all of those into NA
      # filter below drops them, otherwise NA appears as a bar on chart
      mutate(value = as.numeric(value)) |>
      filter(!is.na(value))
  })
 
# ---- Output 1: Dynamic Figure ----------------------------------------- 
# renderPlot() is reactive: Shiny sees input$variable and chosen() inside it
    
    output$compare_plot <- renderPlot({
    
    var_label <- names(numeric_vars)[numeric_vars == input$variable]
    
    # Stuff is missing for a bunch of plants
    validate(need(nrow(chosen()) > 0,
                  "None of the selected plants have data for this variable."))
    
    chosen() |>
      # Sorts bars by value. .desc = TRUE because factor level 1 sits on the
      # LEFT of a vertical chart, so ascending would put the smallest first
      mutate(common_name = fct_reorder(
        common_name, 
        value,
        .desc = TRUE)) |>
      ggplot(aes(
        x = common_name,
        y = value)) +
      # color changes when the user switches variable
      geom_col(fill = pal[[input$variable]],
               alpha = 0.65) +
      labs(
        title = var_label,
        x = NULL,
        y = var_label) +
      theme_gray()

  })
  
# ---- Output 2: Table ---------------------------------------------------
  output$compare_table <- renderTable({
  #  chosen() |>
  #    mutate(across(everything(), as.character)) |>
  #    pivot_longer(everything(),
  #                 names_to = "Field",
  #                 values_to = "Value") |>
  #    filter(!is.na(Value)) # hide fields that a particular plant has
    
    chosen() |>
      rename("Plant" = common_name,
             "Scientific Name" = taxonomic_name,
             "Cultivation" = cultivation,
             "Water" = water,
             "Sunlight" = sunlight,
             "Value" = value)
    
    })
  # ---- Output 3: Random Garden -------------------------------------------
  # eventReactive() is like reactive(), reruns when first argument changes
  # Only changes when user asks for new garden
  garden <- eventReactive(input$plant_garden,{
    edible_plants |>
      slice_sample(n = 9) |>          # nine plants, one bed
      mutate(
        bed_row = rep(1:3, each = 3), # 3x3 grid, new mutate() function! 
        bed_col = rep(1:3, times = 3)
      )
  }, ignoreNULL = FALSE)              # ignoreNULL = FALSE so runs once @ start
  
  output$garden_plot <- renderPlot({
    garden() |>
      ggplot(aes(x = bed_col, y = bed_row, fill = cultivation)) +
      geom_tile(colour = "white", linewidth = 2) +
      # str_wrap stops long names overflowing their tile
      geom_text(aes(label = str_wrap(common_name, 12)),
                size = 3.4, lineheight = 0.95, colour = "grey15") +
  
      # both axes meaningless, removed
      scale_fill_brewer(palette = "Greens", name = "Group") +
      scale_y_reverse() +                         # row 1 at the top
      labs(title = NULL,
           subtitle = "Press the button for a new garden",
           x = NULL,
           y = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
  })
}


## LAUNCH  --------------------------------------------------------------------    
shinyApp(ui = ui, server = server)
