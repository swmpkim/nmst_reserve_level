# Shiny app to export CDMO-formatted data
# from NMST-formatted file

#### INSTRUCTIONS ############################################################

# 1:
# Re-start your R session to make sure there's no interference:
# From the menu bar, select 'Session', then 'Restart R'
# Windows keyboard shortcut is Ctrl + Shift + F10


# 2:
# In the upper right-hand corner of this (source) window,
# there is a button that says "Run App".
# Push it.
# Make sure pop-ups are enabled; the app comes up in a browser window.

##############################################################################


# load packages and function script
library(shiny)
library(dplyr)
library(readr)
library(tidyr)
library(readxl)
library(ggplot2)
library(here)
library(plotly)
library(DT)
library(janitor)
source(here::here('R', 'sourced', 'functions.R'))

# UI ----  
ui <- fluidPage(
    h3("Namaste Veg File Exploration"),
    selectInput("file", "Which file would you like to use?", 
                dir(here::here("data"))),
    "Some key features of this file are:", 
    verbatimTextOutput("summary"),
    "And here is a preview:", 
    tableOutput("preview")
)


# Server ----
server <- function(input, output, session) {
    
    # READ IN DATA -----
    dat <- reactive({
        req(input$file)
        get_data(here::here("data", input$file), cover_only = TRUE)
    })
    
    dat_summ <- reactive({
        out <- list("Sites" = unique(dat()$SiteID),
                    "First Few Species" = names(dat())[(which(names(dat()) == "Total") + 1):20],
                    "Years Sampled" = unique(dat()$Year))
        out
    })
    
    
    # OUTPUTS  
    output$preview <- renderTable(dat()[1:10, 1:10])
    output$summary <- renderPrint(dat_summ())
}

# Call app ----
shinyApp(ui, server)
