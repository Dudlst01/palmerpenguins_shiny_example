# Load packages
library(shiny)
library(tidyverse)
library(ggplot2)
library(janitor)
library(plotly)
library(palmerpenguins)

# Pre-processing Data
penguins_edited <- as.data.frame(penguins_raw)
class(penguins_edited)

penguins_edited <- penguins_edited %>%
  rename(study_name = studyName,
         delta_15_n = "Delta 15 N (o/oo)",
         delta_13_c = "Delta 13 C (o/oo)")

penguins_edited <- clean_names(penguins_edited)

penguins_edited <- penguins_edited %>%
  mutate(species = case_when(
    species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
    species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
    species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo",
    TRUE ~ species # This line keeps all other species as they are
  ))

# Remove unneeded columns and adding year
penguins_edited <- penguins_edited %>%
  select(-study_name, -region, -comments) %>%
  mutate(year = year(penguins_edited$date_egg)) %>%
  drop_na() # Lost 20 rows

# Change clutch_completion from character to factor
class(penguins_edited$clutch_completion)
penguins_edited$clutch_completion <- as.factor(penguins_edited$clutch_completion)
class(penguins_edited$clutch_completion)

head(penguins_edited)

# Change sample_number from double to integer
class(penguins_edited$sample_number)
penguins_edited$sample_number <- as.integer(penguins_edited$sample_number)
class(penguins_edited$sample_number)

# Checking equal amount of days by year
penguins_edited %>%
  group_by(year = year(penguins_edited$date_egg)) %>%
  summarise(count = n_distinct(date_egg))


################################################################################
# Adjust UI
# Assuming penguins_edited is loaded and the necessary libraries are included

ui <- fluidPage(
  titlePanel("Penguin Data Exploration"),
  sidebarLayout(
    sidebarPanel(
      selectInput("species", "Select a Species:", 
                  choices = c("All", unique(penguins_edited$species))),
      selectInput("island", "Select Island:", 
                  choices = c("All", unique(penguins_edited$island))),
      selectInput("gender", "Select Gender:", 
                  choices = c("All", unique(penguins_edited$sex))),
      sliderInput("yearRange", "Select Year Range:",
                  min = min(penguins_edited$year, na.rm = TRUE), 
                  max = max(penguins_edited$year, na.rm = TRUE), 
                  value = c(min(penguins_edited$year, na.rm = TRUE), max(penguins_edited$year, na.rm = TRUE)),
                  step = 1),
      actionButton("update", "Update View"),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Culmen Dimensions", plotlyOutput("culmenPlot")),
        tabPanel("Body Mass Distribution", plotlyOutput("massPlot")),
        tabPanel("Flipper Length", plotlyOutput("flipperPlot"))
      ),
    )
  )
)

server <- function(input, output, session) {
  # Define reactive expression for filtered data
  filtered_data <- reactive({
    data <- penguins_edited %>%
      filter(
        (species == input$species | input$species == "All") &
        (year >= input$yearRange[1] & year <= input$yearRange[2]) &
        (island == input$island | input$island == "All") &
        (sex == input$gender | input$gender == "All")
      )
    data
  })
  
  # Keep the colors consistent across visuals
  species_colors <- c("Adelie" = "steelblue", "Chinstrap" = "lightgreen", "Gentoo" = "orange")
  
  observeEvent(input$update, {
    # Now this block is used only to trigger the reactivity
  })
  
  # Function to create a blank plot with a message
  blank_plot_with_message <- function(message) {
    ggplot() +
      geom_blank() +
      theme_void() +
      labs(title = message)
  }
  
  # Update culmenPlot output based on filtered_data()
  output$culmenPlot <- renderPlotly({
    if (nrow(filtered_data()) == 0) {
      ggplotly(blank_plot_with_message("No data available with the current filters"))
    } else {
      p <- ggplot(filtered_data(), aes(x = culmen_length_mm, y = culmen_depth_mm, color = species)) +
        geom_point() +
        scale_color_manual(values = species_colors) +
        labs(title = "Culmen Length vs Depth by Species", x = "Culmen Length (mm)", y = "Culmen Depth (mm)") +
        theme_classic() +
        theme(legend.title = element_text(face = "bold"), legend.position = "right")
      ggplotly(p)
    }
  })
  
  # Update massPlot output based on filtered_data()
  output$massPlot <- renderPlotly({
    if (nrow(filtered_data()) == 0) {
      ggplotly(blank_plot_with_message("No data available with the current filters"))
    } else {
      p <- ggplot(filtered_data(), aes(x = body_mass_g, fill = species)) +
        geom_histogram(binwidth = 100, color = "black") +
        scale_fill_manual(values = species_colors) +
        facet_wrap(~species) +
        labs(title = "Body Mass Distribution by Species", x = "Body Mass (g)", y = "Frequency") +
        theme_classic() +
        theme(legend.title = element_text(face = "bold"), legend.position = "none")
      ggplotly(p)
    }
  })
  
  # Update flipperPlot output based on filtered_data()
  output$flipperPlot <- renderPlotly({
    if (nrow(filtered_data()) == 0) {
      ggplotly(blank_plot_with_message("No data available with the current filters"))
    } else {
      p <- ggplot(filtered_data(), aes(x = flipper_length_mm, fill = species)) +
        geom_histogram(binwidth = 5, color = "black") +
        scale_fill_manual(values = species_colors) +
        labs(title = "Flipper Length Distribution", x = "Flipper Length (mm)", y = "Frequency") +
        theme_classic() +
        facet_wrap(~species)
      ggplotly(p)
    }
  })
}

shinyApp(ui = ui, server = server)

