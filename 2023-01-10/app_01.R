#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rnaturalearth)
library(tidyterra)

loc_info <- read_csv("data/loc_info.csv")

coasts <- ne_load(type = "coastline",
                      category = "physical",
                      returnclass = "sf",
                  destdir = "maps/")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Project FeederWatch Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("long",
                        "Longitude: ",
                        min = -180,
                        max = 180,
                        value = c(-170, -50),
                        dragRange = TRUE),
            sliderInput("lat",
                        "Latitude: ",
                        min = -90,
                        max = 90,
                        dragRange = TRUE,
                        value = c(10, 65))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("Map")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$Map <- renderPlot({
        long_min <- input$long[[1]]
        long_max <- input$long[[2]]
        lat_min <- input$lat[[1]]
        lat_max <- input$lat[[2]]
        ggplot() +
          geom_spatvector(data = coasts) +
          geom_point(data = loc_info,
                     aes(x = longitude,
                         y = latitude,
                         color = yard_type),
                     alpha = 0.5,
                     size = 1) +
          scale_color_brewer(palette = "Dark2",
                             guide = guide_legend(title = "Yard Type")) +
          lims(x = c(long_min, long_max),
               y = c(lat_min, lat_max)) +
          labs(x = "Longitude",
               y = "Latitude") +
          theme_classic()
    })
}

# Run the application
shinyApp(ui = ui, server = server)
