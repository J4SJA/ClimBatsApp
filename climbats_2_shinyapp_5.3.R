# Script name: climbats_2_shinyapp.R
#                  
# Goal: present data gathered in the climbats WP1
#
# Data source: climbats WP1
#
# Author: Jasja Dekker
#
# Date Created: 2022-12-09
#
# Copyright (c) Jasja Dekker, 2022
# Email: info@jasjadekker.nl
#
# Notes:
# v1.0 basic app
# v2.0 add completeness tab
# v3.0 replace ggplot with plotly , add logo's and about text.
# v4.0 adds select/deselect all species
# v5.0 new dataset, download of full tables only, data type as symbol in completeness graph, add stacked bars for categorical data
# v5.2 improved upon completeness graph

library(data.table)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(tidyverse)
library(plotly)

# font!
fontSize = c(12)

# read data --------------------------------------------------------------
myData <- readRDS("data/combinedData.RDS")
choicesCat <-     unique(myData$TraitCategory)
choices <-        unique(myData$verbatimTraitName)
choicesSpecies <- unique(myData$verbatimScientificName) 
myData$label <- paste(myData$TraitCategory, "-", myData$verbatimTraitName)

# read taxon order
speciesList <- readxl::read_excel("data/Taxon_order.xlsx",  na = "NA")  %>% 
  filter(is.na(taxonID) == FALSE) %>%
  select(verbatimScientificName) 
speciesList$verbatimScientificName <- gsub("_"," ",speciesList$verbatimScientificName)
speciesOrder <- dplyr::pull(speciesList, verbatimScientificName)

# order selection list
choicesSpecies <- choicesSpecies[order(match(choicesSpecies, speciesList$verbatimScientificName))]

# load trait descriptions
#traitDescriptions <- readxl::read_excel("data/List of coauthors, traits & species - EuroBaTrait v1.xlsx")
traitDescriptions <- read.csv("data/Metadata/Trait_description.csv") %>% 
	rename(verbatimTraitName = Name)
	
# make trait label nicer
traitDescriptions$verbatimTraitName <- gsub("([a-z])([A-Z])","\\1 \\2", traitDescriptions$verbatimTraitName)


# Define UI ------------------------------
ui <- fluidPage(title="COST - ClimBats data browser",
  theme=shinytheme("cerulean"),
  useShinyjs(), 
  
  titlePanel(
    fluidRow(
      column(2, img(src="ClimBatslogo.png", height = 100)),
      column(2, h2("ClimBats data browser", align="center")),
      column(2, img(src="COST_LOGO_rgb_lowresolution.jpg", height = 100)),
      column(1, img(src="EN V Funded by the EU_POS.png", width = 100))
    )
  ),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for info ----
    sidebarPanel(id="sidebar",
    						 p("The graph on the 'Data' tab give traits values (+SD)"),
    						 p("You can click on the graph to get info on each dot, zoom in or out and download the graph."),
    		         p(""),
                 p("Select species on tab 'Species'"),
                 p("View which trait-species combinations are missing on the tab 'Completeness of dataset'"),
                 p("Find out more about ClimBats on the tab 'About'"),
                 p(""),
                 selectInput("traitCat","Trait category:",choices=choicesCat),
                 radioButtons("trait","Choose trait:",selected = choices[1], choices=choices)
    ),
    
    # Main panel for displaying outputs or info----
    mainPanel(
      tabsetPanel(
        tabPanel("Data", 
                 p(""),
                 textOutput("description"),
                 p(""),
                 plotlyOutput("distPlot",
                              height="600px"),
                 DT::dataTableOutput("table")
        ),
        tabPanel("Species selection",
                 pickerInput(
                   inputId = "species",
                   label = "Show species:",
                   choices = choicesSpecies,    # provide choices - integers from 1 to 10
                   selected = choicesSpecies,   # select - integers from 1 to 10 
                   options = list(`actions-box` = TRUE,
                                  `selected-text-format` = "count > 2"),   # build buttons for collective selection
                   multiple = T
                 )
                ),
        
        tabPanel("Completeness of dataset",
                 img(src="figure2_final2.png", width=1000)
        ),
        
        tabPanel("Download",
        				 p(""),
        				 p("You can download the full dataset as excel files per table in a zip-file by clicking on this",a("link", href="EuroBaTrait_v1.0.zip"), " (zip file) 2.5 Mb."),
        				 
        				 
        ),
        
          
        tabPanel("Contribute",
        				 p(""),
        				 p("You are welcome to contribute to the EuroBaTrait database at any time. Our objective is to build a living database, and we intend to update the EuroBaTrait database on an annual basis. Benefits of submitting trait data to the database include potential co-authorships in the future release(s)."),
        				 p("If you have less than 30 database entries, please follow this",a("link", href="https://five.epicollect.net/project/eurobatrait/add-entry?form_ref=ceaf670baef44b028b910c8482b81c05_63989bc30240f&parent_form_ref=&branch=&branch_ref=&branch_owner_uuid=&parent_uuid=&uuid=&input_ref=&per_page=50&sort_by=created_at&sort_order=DESC&map_index=0&filter_by=&filter_from=&filter_to=&format=json&headers=true&title=&page=1"), " to enter your data."),
        				 p("Otherwise, please contact Jeremy Froidevaux jeremy.froidevaux(.at.)gmail.com with the following mail object 'EuroBaTrait database CONTRIBUTION'. ") 
        ),
        tabPanel("About",
                 p(""),
                 strong("This is data from Climbats - COST Action CA18107"),
                 p("Bats are sensitive to human-driven habitat alteration, and changes in temperature and water availability 
                           induced by climate change may affect their distribution and survival."),
                 p("Climate change is therefore likely to influence European bat populations and,
                             by affecting insect consumption by bats in farmland, forests and urban areas, there are likely 
                             to be serious consequences for both conservation and the economy. However, little scientific work has addressed this issue, so we lack the knowledge to devise mitigation strategies."),
                 strong("COST"),
                 p("COST (European Cooperation in Science and Technology) is a funding agency for research and  innovation
                  networks. Our Actions help connect research initiatives across Europe and enable scientists
                  to grow their ideas by sharing them with their peers. This boosts their research, career and innovation."),
                 p("More information on COST can be found at www.cost.eu"),
                 img(src="COST_LOGO_rgb_lowresolution.jpg", height = 150),
                 img(src="EN V Funded by the EU_POS.png",   height = 150), 
                 p(""),
                 strong("Climbats data viewer version 5.1. This viewer was made by Jasja Dekker."),
                 p("For questions and remarks please approach me through www.jasjadekker.nl")
        ),
        id="tabset"  
      ),
      id="main"
    )
    
  )
)


server <- function(input, output, session) {
  
  # hide sidebar when completeness or information
  observeEvent(input[["tabset"]], {
    if(input[["tabset"]] %in% c("Completeness of dataset","About")){
      hideElement(selector = "#sidebar")
      removeCssClass("main", "col-sm-8")
      addCssClass("main", "col-sm-12")
    }else{
      showElement(selector = "#sidebar")
      removeCssClass("main", "col-sm-12")
      addCssClass("main", "col-sm-8")
    }
  })
  
  
  # choices <- reactive(myData %>% filter(TraitCategory == input$traitCat) %>%  unique(myData$verbatimTraitName))
  
  observe({
    choicesNew <- pull(myData %>% filter(TraitCategory == input$traitCat) %>% distinct(verbatimTraitName))
    updateRadioButtons(session, "trait",
                       choices = as.list(choicesNew),
                       selected = choicesNew[1]) 
  })
  
  output$description <- renderText({
    paste(
      input$trait,
      ": ",
      myData %>%
        filter(verbatimTraitName == input$trait) %>%
        select(Definition) %>%
        unique() %>%
        pull(),
      sep=""
    )
  }
  )
  
  #   reactive(output$data <- myData %>%
  #   filter(TraitCategory == input$traitCat) %>%
  #   filter(verbatimTraitName %in% input$trait) %>%
  #   filter(verbatimSpeciesName %in% input$species)
  # )
  # 
  
  output$table <- DT::renderDataTable(
    DT::datatable(
      {myData %>%
          #filter(TraitCategory == 'Physiology') %>% 
          filter(verbatimTraitName %in% input$trait) %>%
          #filter(verbatimSpeciesName %in% input$species) %>%
          unite(verbatimTraitName, sep=" ", c("verbatimTraitName", "verbatimTraitUnit")) %>%
          select(verbatimScientificName, Area, verbatimTraitValue,verbatimTraitValueSD, ShortReference) %>%
          #mutate(across("verbatimTraitValue", round, 2)) %>% # round to 3 decimals
          #mutate(across("verbatimTraitValueSD", round, 3)) %>% # round to 3 decimals
          arrange(factor(verbatimScientificName, levels = speciesOrder))%>%
          rename('Species' = verbatimScientificName, 
                 'Study area' = Area,
                 SD = verbatimTraitValueSD,
                 Value = verbatimTraitValue, 
                 Source = ShortReference)},
      
      rownames = FALSE,
      extensions = 'Buttons',
      caption = input$trait,
      options = list(
        paging = FALSE,
        searching = TRUE,
        fixedColumns = TRUE,
        autoWidth = TRUE,
        ordering = TRUE,
        dom = 'tB',
        buttons = c('')
      ),
      class = "display"
    ))
  
  output$distPlot <- renderPlotly({
    graphData <- myData %>%
      filter(TraitCategory == input$traitCat) %>%
      filter(verbatimTraitName %in% input$trait) %>%
      filter(verbatimScientificName %in% input$species) 
    
    graphData$verbatimScientificName <- reorder(graphData$verbatimScientificName, FUN=max, graphData$verbatimTraitValueNum)
    
    yLabel <- paste(graphData$verbatimTraitName[1],graphData$verbatimTraitUnit[1],sep=" ")
    if(graphData$Type[1] %in% c("Quantitative_continuous","Quantitative discrete"))
    {
    plot_ly(data=graphData, x = ~verbatimScientificName, y=~verbatimTraitValueNum,
            type = "scatter",
            mode="markers",
            error_y = list(
              type = "data",
              array=graphData$verbatimTraitValueSD,
              color="#AAAAAA",
              line=list(width=.5)),
            marker = list(size = 10,
                          color = c("#E69F00"),
                          opacity = 0.8,
                          line = list(color = c("#555555"),
                                      width = 0.5
                          )
            )
            
    ) %>%
      layout(xaxis=list(
        title = "",
        # showticklabels = TRUE,
        tickmode="linear",
        type="category"),
        yaxis=list(
        title = yLabel)
      )
    }
    else
    {
      plot_ly(data=graphData %>% group_by(verbatimScientificName, verbatimTraitValue) %>% tally(),
              x = ~verbatimScientificName, y=~n,
              type = "bar",
              name=~verbatimTraitValue, color=~verbatimTraitValue
              
              
              
      ) %>%
        layout(xaxis=list(
                   title = "",
                   # showticklabels = TRUE,
                   tickmode="linear",
                   type="category"),
               yaxis = list(
                   title = 'Count'),
               barmode = 'stack')
    }
    
    
  })
  

}

shinyApp(ui = ui, server = server)