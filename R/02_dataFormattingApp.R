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
    
    h3("Namaste to CDMO file shapes"),
    selectInput("file", "Which file would you like to use?", 
                dir(here::here("data"))),
    
    # Sidebar ----
    sidebarLayout(
        sidebarPanel(
            "Choose options for generating CDMO-formatted file, 
            \nand click 'Update' button below when completed",
            br(),
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
                         choices = c("yes", "no")),
            br(),
            actionButton("choices_made", "Update!")
        ),
        
        mainPanel(
            # Review Data tab ----
            tabsetPanel(type = "tabs", id = "tabselected",
                        
                        tabPanel("1. Review data",
                                 value = 1,
                                 br(),
                                 "Some key features of this file are:",
                                 br(),
                                 verbatimTextOutput("summary"),
                                 br(),
                                 h4("Sampling Summary: "),
                                 br(),
                                 "# samples by site/year",
                                 DT::dataTableOutput("sampling_summary"),
                                 br(),
                                 h4("Missing samples: "),
                                 br(),
                                 uiOutput("missing_samples"),
                                 br(),
                                 h4("Preview of raw data file: "),
                                 br(),
                                 DT::dataTableOutput("preview"),
                                 br()
                        ),
                        
                        # CDMO layout tab ----
                        tabPanel("2. Preview CDMO layout",
                                 value = 2,
                                 h4("Preview of CDMO-shaped data"),
                                 DT::dataTableOutput("long")
                        ),
                        
                        # download CDMO format file tab ----
                        tabPanel("3. Download CDMO layout",
                                 value = 3,
                                 h4("Click the button below to download"),
                                 downloadButton("download_cdmo", 
                                                label = "Download")
                        )
            )
        )
    )
)


# Server ----
server <- function(input, output, session) {
    
    # READ IN DATA -----
    dat <- reactive({
        req(input$file)
        tmp <- get_data(here::here("data", input$file), 
                 cover_only = ifelse(input$htdens == "yes",
                                     FALSE,
                                     TRUE))
        tmp <- tmp %>%
            mutate(across(c(Year, Month, Day), as.integer))
        
        tmp
    })
    output$preview <- DT::renderDataTable(dat()[1:10, ])
    
    # Observe when the data frame changes and update the selectInput choices ----
    observe({
        req(dat())  
        dat_yrs <- unique(dat()$Year)  # Extract unique categories
        updateSelectInput(session, "yrchoice", choices = dat_yrs)
        
        dat_spps <- names(dat())[(which(names(dat()) == "Total") + 1):20]
        updateSelectInput(session, "unvegspps", choices = dat_spps)
    })
    
    # GENERATE PREVIEW FOR SUMMARY ----
    dat_summ <- reactive({
        out <- list("Sites" = unique(dat()$SiteID),
                    "First Few Species" = names(dat())[(which(names(dat()) == "Total") + 1):20],
                    "Years Sampled" = unique(dat()$Year))
        out
    })
    output$summary <- renderPrint(dat_summ())
    
    
    # SAMPLING SUMMARY ----
    dat_sampling_summary <- reactive({
        req(dat())
        dat() %>% 
            group_by(SiteID, Year) %>% 
            tally() %>% 
            arrange(Year, SiteID) %>% 
            rename(Site = SiteID) %>% 
            pivot_wider(names_from = Year,
                        values_from = n,
                        values_fill = 0) %>% 
            arrange(Site) 
    })
    output$sampling_summary <- DT::renderDataTable(dat_sampling_summary())

    
    
    # MISSING DATA ----
    unsampds <- reactive({
        req(dat())
        
        tmp <- find_unsampleds(dat())
        
        if (nrow(tmp) == 0) {
            return(NULL)  # No missing data, return NULL
        } else {
            return(tmp)  # Return the data frame with missing data
        }
    })
    # render output, conditionally
    output$missing_samples_table <- DT::renderDataTable({
        req(unsampds())  # Ensure there is data to display
        unsampds()
    })
    output$missing_samples <- renderUI({
        if (is.null(unsampds())) {
            h5("No missing data")
        } else {
            DT::dataTableOutput("missing_samples_table")
        }
    })
    
    
    
    # PIVOT DATA ----
    dat_cdmo_measurements <- eventReactive(input$choices_made, {
        tmp <- dat()
        
        if(!is.null(input$yrchoice)){
            tmp <- tmp %>% 
                dplyr::filter(Year %in% input$yrchoice)
        }
        
        tmp_long <- pivot_to_cdmo(tmp)
        
        if(!("QAQC" %in% names(tmp_long))) tmp_long$QAQC <- NA
        
        if(!is.null(input$unvegspps)){
            # if only one species, all is well
            if(length(input$unvegspps) == 1){
                tmp_long <- tmp_long %>% 
                    dplyr::mutate(Species = case_when(Species == input$unvegspps ~ "Unvegetated",
                                                      .default = Species))
            } else {
                # if multiple, need to add together Cover estimates
                # pull them out and do them separately - 
                # this will end up inserting NAs into the table for other params like
                # height and density, which should be NA
                unveg <- tmp_long %>% 
                    dplyr::filter(Species %in% input$unvegspps) %>% 
                    dplyr::mutate(Species = "Unvegetated") %>% 
                    dplyr::summarize(.by = c(Reserve:Species),
                              Cover = sum(Cover, na.rm = TRUE),
                              QAQC = paste(unique(QAQC), collapse = "; ")) %>% 
                    dplyr::mutate(QAQC = case_when(QAQC == "NA" ~ NA_character_,
                                                   .default = QAQC))
                
                # filter out unvegetateds
                # then bind the unveg df from above, and put in order
                tmp_long <- tmp_long %>% 
                    dplyr::filter(!(Species %in% input$unvegspps)) %>% 
                    bind_rows(unveg) 
            }
            
        }
        
        tmp_long %>% 
            dplyr::mutate(Date2 = lubridate::mdy(Date)) %>% 
            dplyr::arrange(Date2, SiteID, TransectID, PlotID, Species) %>% 
            dplyr::select(-Date2)
    })
    output$long <- DT::renderDataTable(dat_cdmo_measurements())
    
    
    
    # DOWNLOAD DATA ---- 
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
