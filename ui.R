#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(DT)

headings <- data.frame(heading=c("North","Northeast","East","Southeast","South","Southwest","West","Northwest"),
                       due_r=c("East","Southeast","South","Southwest","West","Northwest","North","Northeast"),
                       due_l=c("West","Northwest","North","Northeast","East","Southeast","South","Southwest"),
                       stringsAsFactors = F
)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("Hurricane Flood Predictions"),
  tabsetPanel(
    tabPanel("Precition Coverage and Performance Metrics",fluid = TRUE,
             basicPage(leafletOutput("coverageMap"),DT::dataTableOutput("buoyPerformance"))
    ),
    tabPanel("Effects of Irma on FL Surge Levels",fluid = TRUE,
             basicPage(htmlOutput("animatedMap"))
    ),
    tabPanel("What-If",fluid = TRUE,
           sidebarLayout(
      sidebarPanel(
        uiOutput("AllBuoys"),
        uiOutput("scenarios"),
        uiOutput("stormHeading"),
        checkboxInput("est_sustained_wind_speed", "Use Estimated Sustained Wind Speed", FALSE)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        leafletOutput("forecastInundationPlot"),
        plotOutput("forecastPlot")
      )
    )
  ),
  tabPanel("Previous Storms",fluid = TRUE,
  sidebarLayout(
    sidebarPanel(
      uiOutput("Hurricanes"),
      uiOutput("Buoys")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot"),
       plotOutput("inundationPlot")
    )
  )
)
  )
)
)
