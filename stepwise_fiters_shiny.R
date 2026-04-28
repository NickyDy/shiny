library(shiny)
library(dplyr)

# Sample dataset
data <- data.frame(
  Category1 = c("A", "A", "B", "B", "C", "C"),
  Category2 = c("X", "Y", "X", "Z", "Y", "Z"),
  Category3 = c("P", "Q", "R", "P", "Q", "R"),
  Value = 1:6
)

# Shiny App
ui <- fluidPage(
  titlePanel("Stepwise Reactive Filters"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("cat1", "Category 1:", choices = unique(data$Category1)),
      selectInput("cat2", "Category 2:", choices = NULL),
      selectInput("cat3", "Category 3:", choices = NULL)
    ),
    mainPanel(
      tableOutput("filtered_data")
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$cat1, {
    updateSelectInput(session, "cat2",
                      choices = unique(data %>% 
                                         filter(Category1 == input$cat1) %>% 
                                         pull(Category2)))
  })
  
  observeEvent(input$cat2, {
    updateSelectInput(session, "cat3",
                      choices = unique(data %>%
                                         filter(Category1 == input$cat1, 
                                                Category2 == input$cat2) %>%
                                         pull(Category3)))
  })
  
  filtered_data <- reactive({
    data %>%
      filter(Category1 == input$cat1,
             Category2 == input$cat2,
             Category3 == input$cat3)
  })
  
  output$filtered_data <- renderTable({
    filtered_data()
  })
}

shinyApp(ui, server)
