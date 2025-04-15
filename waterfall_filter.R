library(shiny)
library(dplyr)
library(bslib)

# Sample data - a dataset with continents, countries, and cities
data <- data.frame(
  continent = rep(c("Africa", "Asia", "Europe", "North America", "South America"), each = 20),
  country = c(
    rep(c("Egypt", "Kenya", "Nigeria", "South Africa", "Morocco"), each = 4),
    rep(c("China", "India", "Japan", "Thailand", "Vietnam"), each = 4),
    rep(c("France", "Germany", "Italy", "Spain", "UK"), each = 4),
    rep(c("Canada", "Mexico", "USA", "Panama", "Cuba"), each = 4),
    rep(c("Argentina", "Brazil", "Chile", "Colombia", "Peru"), each = 4)
  ),
  city = c(
    # Africa
    "Cairo", "Alexandria", "Giza", "Luxor",
    "Nairobi", "Mombasa", "Kisumu", "Nakuru",
    "Lagos", "Abuja", "Kano", "Ibadan",
    "Johannesburg", "Cape Town", "Durban", "Pretoria",
    "Casablanca", "Rabat", "Marrakech", "Fez",
    # Asia
    "Beijing", "Shanghai", "Guangzhou", "Shenzhen",
    "Mumbai", "Delhi", "Bangalore", "Chennai",
    "Tokyo", "Osaka", "Kyoto", "Yokohama",
    "Bangkok", "Phuket", "Chiang Mai", "Pattaya",
    "Hanoi", "Ho Chi Minh City", "Da Nang", "Hue",
    # Europe
    "Paris", "Nice", "Lyon", "Marseille",
    "Berlin", "Munich", "Hamburg", "Frankfurt",
    "Rome", "Milan", "Naples", "Venice",
    "Madrid", "Barcelona", "Valencia", "Seville",
    "London", "Manchester", "Liverpool", "Edinburgh",
    # North America
    "Toronto", "Vancouver", "Montreal", "Ottawa",
    "Mexico City", "Cancun", "Guadalajara", "Monterrey",
    "New York", "Los Angeles", "Chicago", "Miami",
    "Panama City", "Colón", "David", "Santiago",
    "Havana", "Santiago de Cuba", "Camagüey", "Holguín",
    # South America
    "Buenos Aires", "Córdoba", "Rosario", "Mendoza",
    "Rio de Janeiro", "São Paulo", "Brasília", "Salvador",
    "Santiago", "Valparaíso", "Concepción", "Viña del Mar",
    "Bogotá", "Medellín", "Cali", "Cartagena",
    "Lima", "Cusco", "Arequipa", "Trujillo"
  ),
  population = sample(10000:10000000, 100),
  stringsAsFactors = FALSE
)

ui <- page_sidebar(
  title = "Waterfall Filter Example",
  sidebar = sidebar(
    title = "Filters",
    selectInput("continent", "Select Continent", 
                choices = c("All", sort(unique(data$continent)))),
    selectInput("country", "Select Country", choices = NULL),
    selectInput("city", "Select City", choices = NULL)
  ),
  card(
    card_header("Filtered Data"),
    plotOutput("populationPlot"),
    dataTableOutput("filteredData")
  )
)

server <- function(input, output, session) {
  
  # Level 1 filtering: Continent
  filtered_continent <- reactive({
    if (input$continent == "All") {
      data
    } else {
      data %>% filter(continent == input$continent)
    }
  })
  
  # Update country choices based on continent selection
  observe({
    countries <- sort(unique(filtered_continent()$country))
    updateSelectInput(session, "country", 
                      choices = c("All", countries),
                      selected = "All")
  })
  
  # Level 2 filtering: Country
  filtered_country <- reactive({
    if (input$country == "All") {
      filtered_continent()
    } else {
      filtered_continent() %>% filter(country == input$country)
    }
  })
  
  # Update city choices based on country selection
  observe({
    cities <- sort(unique(filtered_country()$city))
    updateSelectInput(session, "city", 
                      choices = c("All", cities),
                      selected = "All")
  })
  
  # Level 3 filtering: City
  filtered_city <- reactive({
    if (input$city == "All") {
      filtered_country()
    } else {
      filtered_country() %>% filter(city == input$city)
    }
  })
  
  # Output the filtered data
  output$filteredData <- renderDataTable({
    filtered_city()
  })
  
  # Create a bar plot of population by city
  output$populationPlot <- renderPlot({
    df <- filtered_city()
    
    if (nrow(df) > 15) {
      # If there are many cities, show top 15 by population
      df <- df %>% 
        arrange(desc(population)) %>%
        head(15)
    }
    
    barplot(
      df$population,
      names.arg = df$city,
      main = "Population by City",
      col = "steelblue",
      las = 2, # Make labels perpendicular to axis
      cex.names = 0.8 # Reduce font size of names
    )
  })
}

shinyApp(ui, server)
