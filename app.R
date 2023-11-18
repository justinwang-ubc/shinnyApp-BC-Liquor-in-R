library(shiny)
library(tidyverse)
bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      radioButtons("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      uiOutput("countryOutput")
    ),
    mainPanel(
      img(src="liquor.png"),
      tabsetPanel( tabPanel("stat",  
                            h2(textOutput("resultCount")),
                            plotOutput("coolplot"),
                            br(), br(),
                           DT::dataTableOutput("results")),
                   tabPanel("Tab 2", "there!")),
     
      )
  )
)

server <- function(input, output) {
  filtered <- reactive({
    if(is.null(input$countryInput)){
      return(NULL)
    }
    bcl %>%
      filter(Price >= input$priceInput[1],
             Price <= input$priceInput[2],
             Type == input$typeInput,
             Country == input$countryInput
      )
  })
  output$countryOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA"
                )
  })
  
  output$resultCount <- renderText({
    
    paste("number of results found:", nrow(filtered()))
  })
  
  output$coolplot <- renderPlot({
    if(is.null(filtered())){
      return()
    }
    ggplot(filtered(), aes(Alcohol_Content)) +
      geom_histogram()
  })
  
  output$results <- DT::renderDataTable({
    filtered()
  }) 
   
  
}

shinyApp(ui = ui, server = server)