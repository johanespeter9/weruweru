library(shiny)
library(bslib)
library(tidyverse)
library(sf)
library(tmap)
library(stringr)
library(RColorBrewer)
library(DT)

# Set tmap mode to view
tmap_mode("view")

# UI
ui <- page_sidebar(
  title = "Weruweru River Catchment Interactive Map",
  sidebar = sidebar(
    # Layer controls
    checkboxGroupInput("layers", "Select Layers to Display:",
                       choices = c("Furrows" = "furrow",
                                   "Abstraction Points" = "abstraction",
                                   "Intake Measurements" = "intake",
                                   "Stakeholder Farms" = "farms"),
                       selected = c("furrow", "abstraction", "intake", "farms")),
    
    # Base map selector
    selectInput("basemap", "Select Base Map:",
                choices = c("Satellite" = "Esri.WorldImagery",
                            "OpenStreetMap" = "OpenStreetMap",
                            "Terrain" = "Stamen.Terrain"),
                selected = "Esri.WorldImagery"),
    
    # Additional controls
    checkboxInput("labels", "Show Labels", TRUE),
    sliderInput("transparency", "Layer Transparency:",
                min = 0, max = 1, value = 0.7, step = 0.1),
    
    # Select feature type for table
    selectInput("tableType", "Select Feature Information to Display:",
                choices = c("Furrows" = "furrow",
                            "Abstraction Points" = "abstraction",
                            "Intake Measurements" = "intake",
                            "Stakeholder Farms" = "farms"),
                selected = "furrow"),
    
    # Information box
    card(
      card_header("Dataset Information"),
      p("This interactive map displays various features of the Weruweru River Catchment:"),
      tags$ul(
        tags$li("Furrow systems and irrigation infrastructure"),
        tags$li("Water abstraction points"),
        tags$li("Stakeholder farms and properties"),
        tags$li("Intake measurement locations")
      )
    )
  ),
  
  # Main panel with map and data tables
  layout_columns(
    col_widths = c(8, 4),
    card(
      card_header("Interactive Map"),
      tmapOutput("map", height = 600)
    ),
    layout_column_wrap(
      width = 1,
      card(
        card_header("Feature Information"),
        DTOutput("featureTable")
      ),
      card(
        card_header("Project Objectives"),
        tags$div(
          style = "height: 300px; overflow-y: auto;",
          tags$h5("Main Objective"),
          p("To develop an overview map of the Weruweru River Catchment, supported by detailed GIS data and analysis."),
          tags$h5("Specific Objectives"),
          tags$ul(
            tags$li(tags$b("GIS Data Compilation and Analysis:"), 
                    "Compile, assess, and standardize relevant GIS datasets for evidence-based decision-making."),
            tags$li(tags$b("Accurate Mapping:"), 
                    "Create detailed GIS shapefiles of hydrological features, including rivers, tributaries, and furrows."),
            tags$li(tags$b("Water Abstraction Mapping:"), 
                    "Identify and map all surface and groundwater abstraction points within the catchment."),
            tags$li(tags$b("Data-Driven Reporting:"), 
                    "Deliver comprehensive reporting with visual aids for stakeholder understanding."),
            tags$li(tags$b("Stakeholder Engagement:"), 
                    "Foster participation through workshops and community-driven approaches."),
            tags$li(tags$b("Post-Project Integration:"), 
                    "Ensure compatibility with existing systems and provide ongoing support.")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Read data
  furrow <- reactive({
    st_read("Furrows.gpkg") %>% 
      st_make_valid() %>% 
      janitor::clean_names()
  })
  
  abstraction_point <- reactive({
    st_read("Abstraction_Points.gpkg") %>% 
      st_make_valid() %>% 
      janitor::clean_names()
  })
  
  intake_measurement <- reactive({
    st_read("Intake _Measurement_Furrow.gpkg") %>% 
      st_make_valid() %>% 
      janitor::clean_names()
  })
  
  stakeholders_farms <- reactive({
    st_read("Stakeholders _Farms.gpkg") %>% 
      st_make_valid() %>% 
      janitor::clean_names()
  })
  
  # Observer to update tableType based on layers selection
  observe({
    selected_layers <- input$layers
    if (length(selected_layers) == 1) {
      updateSelectInput(session, "tableType", selected = selected_layers[1])
    }
  })
  
  # Create interactive map
  output$map <- renderTmap({
    tm <- tm_basemap(input$basemap)
    
    # Add layers based on user selection
    if ("furrow" %in% input$layers) {
      tm <- tm + tm_shape(furrow()) +
        tm_lines(col = "furrow_name",
                 popup.vars = TRUE,
                 lwd = 2) +
        if(input$labels) tm_text(text = "furrow_name", size = 0.7, col = "white")
    }
    
    if ("farms" %in% input$layers) {
      tm <- tm + tm_shape(stakeholders_farms()) +
        tm_polygons(col = "name",
                    border.col = "green",
                    alpha = input$transparency,
                    popup.vars = c("Company" = "name", 
                                   "Furrow" = "name_furrow", 
                                   "Area Ha" = "area")) +
        if(input$labels) tm_text(text = "name", size = 0.7, col = "white")
    }
    
    if ("intake" %in% input$layers) {
      tm <- tm + tm_shape(intake_measurement()) +
        tm_dots(col = "pointypes",
                popup.vars = TRUE)
    }
    
    if ("abstraction" %in% input$layers) {
      tm <- tm + tm_shape(abstraction_point()) +
        tm_dots(col = "abstraction_points",
                popup.vars = TRUE,
                title = "Point Features")
    }
    
    tm
  })
  
  # Create feature information table
  output$featureTable <- renderDT({
    # Get data based on selected feature type
    selected_data <- switch(input$tableType,
                            "furrow" = {
                              if ("furrow" %in% input$layers) {
                                furrow() %>%
                                  st_drop_geometry() %>%
                                  select(furrow_name, everything())
                              }
                            },
                            "abstraction" = {
                              if ("abstraction" %in% input$layers) {
                                abstraction_point() %>%
                                  st_drop_geometry() %>%
                                  select(abstraction_points, everything())
                              }
                            },
                            "intake" = {
                              if ("intake" %in% input$layers) {
                                intake_measurement() %>%
                                  st_drop_geometry() %>%
                                  select(pointypes, everything())
                              }
                            },
                            "farms" = {
                              if ("farms" %in% input$layers) {
                                stakeholders_farms() %>%
                                  st_drop_geometry() %>%
                                  select(name, name_furrow, area, everything())
                              }
                            }
    )
    
    # Create table with appropriate caption
    if (!is.null(selected_data)) {
      caption <- switch(input$tableType,
                        "furrow" = "Furrow Information",
                        "abstraction" = "Abstraction Points Information",
                        "intake" = "Intake Measurements Information",
                        "farms" = "Stakeholder Farms Information"
      )
      
      datatable(selected_data,
                options = list(pageLength = 5),
                caption = caption)
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)