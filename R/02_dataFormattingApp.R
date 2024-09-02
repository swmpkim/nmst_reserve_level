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
    
    tabsetPanel(type = "tabs", id = "tabselected",
                
                tabPanel("1. Review data",
                         value = 1,
                         br(),
                         "Some key features of this file are:",
                         br(),
                         verbatimTextOutput("summary"),
                         br(),
                         "And here is a preview:", 
                         tableOutput("preview"),
                         br()
                         ),
                
                tabPanel("2. Choose options",
                         value = 2,
                         br(),
                         selectInput("unvegspps", 
                                     label = "What species should be included in 'Unvegetated'?",
                                     choices = NULL,
                                     multiple = TRUE),
                         br(),
                         selectInput("yrchoice", 
                                     label = "What year(s) to work with?",
                                     choices = NULL,
                                     multiple = TRUE),
                         br(),
                         radioButtons("htdens", 
                                      label = "Include Height and Density?",
                                      choices = c("yes", "no"))),
                
                tabPanel("3. Preview CDMO layout",
                         value = 3,
                         h4("Preview of CDMO-shaped data"),
                         tableOutput("long")),
                
                tabPanel("4. Download CDMO layout",
                         value = 4,
                         h4("Click the button below to download"),
                         downloadButton("download_cdmo", 
                                        label = "Download")
                         )
    
    )
)


# Server ----
server <- function(input, output, session) {
    
    # READ IN DATA -----
    dat <- reactive({
        req(input$file)
        get_data(here::here("data", input$file), 
                 cover_only = ifelse(input$htdens == "yes",
                                     FALSE,
                                     TRUE))
    })
    
    # Observe when the data frame changes and update the selectInput choices
    observe({
        req(dat())  
        dat_yrs <- unique(dat()$Year)  # Extract unique categories
        updateSelectInput(session, "yrchoice", choices = dat_yrs)
        
        dat_spps <- names(dat())[(which(names(dat()) == "Total") + 1):20]
        updateSelectInput(session, "unvegspps", choices = dat_spps)
    })
    
    # GENERATE PREVIEW FOR SUMMARY
    dat_summ <- reactive({
        out <- list("Sites" = unique(dat()$SiteID),
                    "First Few Species" = names(dat())[(which(names(dat()) == "Total") + 1):20],
                    "Years Sampled" = unique(dat()$Year))
        out
    })
    
    # PIVOT DATA
    dat_cdmo_measurements <- reactive({
        pivot_to_cdmo(dat())
    })
    
    # OUTPUTS  
    output$preview <- renderTable(dat()[1:10, 1:10])
    output$summary <- renderPrint(dat_summ())
    output$long <- renderTable(dat_cdmo_measurements()[1:10, ])
    output$download_cdmo <- downloadHandler(
        filename = function() {
            paste0("download ", as.character(Sys.time()), ".xlsx")
        },
        content = function(file) {
            writexl::write_xlsx(dat_cdmo_measurements(),
                                file)
        }
    )
}

# Call app ----
shinyApp(ui, server)
