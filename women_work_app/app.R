#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)
library(scales)
library(countrycode)
library(ggplot2)
library(plotly)
library(dplyr)
theme_set(theme_minimal())


jobs_gender <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-05/jobs_gender.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Gender disparity expored 2016"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("major_category",
                        "Ocupation Category",
                        choices = unique(jobs_gender$major_category))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("jobs_scatter", height = "700px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$jobs_scatter <- renderPlotly({
      plotly_graph_1 <-jobs_gender %>% 
        filter(year == "2016", major_category == input$major_category, total_workers > 20000) %>% 
        arrange(desc(wage_percent_of_male)) %>% 
        mutate(percent_female = workers_female / total_workers,
               wage_percent_female = total_earnings_female / total_earnings_male) %>% 
        ggplot(aes(
          percent_female,
          wage_percent_female,
          color = minor_category,
          label = occupation,
          size = total_workers
        ))+
        geom_point(show.legend = FALSE)+
        scale_size_continuous(range = c(1,10))+
        labs(
          x = " % of workforce reported as female",
          y = " % of median salary female",
          title = "Gender disparity and pay gap in 2016",
          subtitle = "Only workforce women"
        )+
        scale_x_continuous(labels = scales::percent_format())+
        scale_y_continuous(labels = scales::percent_format())+
        theme(legend.position = "bottom")
      
      ggplotly(plotly_graph_1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
