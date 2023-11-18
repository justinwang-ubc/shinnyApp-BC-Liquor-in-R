library(shiny)
library(tidyverse)
bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("BC Liquor Store prices"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("priceInput", "Price", 0, 100, c(25, 40), pre = "$"),
      checkboxGroupInput("typeInput", "Product type",
                   choices = c("BEER", "REFRESHMENT", "SPIRITS", "WINE"),
                   selected = "WINE"),
      uiOutput("countryOutput"),
      checkboxInput("sortPrice", "sort the table by price",FALSE)
    ),
    mainPanel(
      img(src="liquor.png"),
      tabsetPanel( tabPanel("histogram graph",  
                            h2(textOutput("resultCount")),
                            plotOutput("coolplot"),
                            br(), br()
                           ),
                   tabPanel("table", 
                            downloadButton("downloadData","Download Data"),
                            DT::dataTableOutput("results"))
                            
                   
                   ),
     
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
                choices = c("All" = "All",sort(unique(bcl$Country))),
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
    data <- filtered()
    if(input$sortPrice){
      data <- data %>% arrange(Price)
    }
    data
  }) 
  
  output$downloadData <- downloadHandler(
    filename = "data.csv",
    content = function(file){
      if (!is.null(filtered())) {
        write.csv(filtered(), file, row.names = FALSE)
      }
    }
  )
   
  
}

shinyApp(ui = ui, server = server)