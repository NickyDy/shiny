library(tidyverse)
library(leaflet)
library(shiny)
library(bslib)
library(sf)

und_water <- read_rds("underground_water.rds")
surf_water <- read_rds("surf_water.rds")
#----------------------------------------
mail <- tags$a(icon("envelope"), "Email", 
               href = "mailto:nickydyakov@gmail.com", 
               tagret = "_blank")
github <- tags$a(icon("github"), "Github", 
                 href = "https://github.com/NickyDy", 
                 tagret = "_blank")
#---------------------------------------
ui <- page_sidebar(
  #title = h3("Чистота на водите в България!"), 
  theme = bslib::bs_theme(bootswatch = "darkly"),
  sidebar = sidebar(width = 270, list(
    selectInput("basin", "Водосбор:", 
                choices = unique(und_water$basin)),
    selectInput("oblast", "Област:", choices = NULL),
    selectInput("obshtina", "Община:", choices = NULL),
    selectInput("pokazatel", "Показател:", choices = NULL))),
  navset_pill(
    nav_panel(title = "Чистота на подземната вода", 
              plotOutput("water")),
    nav_panel(title = "Карта (подземна вода)", 
              leafletOutput("map", width = 1600, height = 800)),
    nav_panel(title = "Чистота на повърхностната вода", layout_columns(
              selectInput("surf_basin", "Водосбор:", 
                          choices = unique(surf_water$basin)),
              selectInput("site", "Място на пробовземането:", 
                          choices = NULL),
              selectInput("surf_index", "Показател:", 
                          choices = NULL),
              col_widths = c(2, 5, 2)),
              plotOutput("surf_plot")),
    nav_panel(tags$img(src = "shiny.png", width = 40),
              "Други полезни приложения:",
              tags$a(href = "https://nickydy.shinyapps.io/elections/", br(),
                     "Избори в България!"), br(),
              tags$a(href = "https://nickydy.shinyapps.io/demography/",
                     "Демография на България!"), br(),
              tags$a(href = "https://nickydy.shinyapps.io/climate/",
                     "Климатът на България!"), br(),
              tags$a(href = "https://nickydy.shinyapps.io/inlation/",
                     "Inflation in EU!"), br(),
              # tags$a(href = "https://ndapps.shinyapps.io/bgprices/",
              #        "Сравнение на цените в България!"), br(),
              tags$a(href = "https://ndapps.shinyapps.io/agri/",
                     "Цени на селскостопанска продукция в ЕС!"), br(),
              tags$a(href = "https://nickydy.shinyapps.io/eurostat/",
                     "Евростат за България!"), br()),
    # nav_panel(tags$img(src = "kofi.png", width = 40),
    #           "Ако Ви харесва приложението,
    #            можете да ме подкрепите като направите дарение в евро към
    #            следната сметка:",
    #           br(),
    #           br(),
    #           "Име: Nikolay Dyakov",
    #           br(),
    #           "IBAN: BE89 9670 3038 2685",
    #           br(),
    #           "BIC: TRWIBEB1XXX",
    #           br(),
    #           "Адрес: Rue de Trone 100, 3rd floor,",
    #           br(),
    #           "Brussels,",
    #           br(),
    #           "1050,",
    #           br(),
    #           "Belgium"),
    nav_spacer(),
    nav_menu(
      title = "Links",
      nav_item(mail),
      nav_item(github)
    )
  )
)

server <- function(input, output, session) {
  basin <- reactive({
    filter(und_water, basin == input$basin)
  })
  observeEvent(basin(), {
    freezeReactiveValue(input, "oblast")
    choices <- unique(basin()$oblast)
    updateSelectInput(inputId = "oblast", choices = choices)
  })
  oblast <- reactive({
    req(input$oblast)
    filter(basin(), oblast == input$oblast)
  })
  observeEvent(oblast(), {
    freezeReactiveValue(input, "obshtina")
    choices <- unique(oblast()$obshtina)
    updateSelectInput(inputId = "obshtina", choices = choices)
  })
  obshtina <- reactive({
    req(input$obshtina)
    filter(oblast(), obshtina == input$obshtina)
  })
  observeEvent(obshtina(), {
    freezeReactiveValue(input, "pokazatel")
    choices <- unique(obshtina()$pokazatel)
    updateSelectInput(inputId = "pokazatel", choices = choices)
  })
  pokazatel <- reactive({
    req(input$pokazatel)
    filter(obshtina(), pokazatel == input$pokazatel)
  })
#---------------------------------------------------
 output$water <- renderPlot({
   pokazatel() %>% 
     filter(oblast %in% c(input$oblast),
            obshtina %in% c(input$obshtina),
            pokazatel %in% c(input$pokazatel)) %>% 
     mutate(col = value > standart, 
            date = as.factor(date),
            col = as.factor(col),
            col = fct_recode(col, "Над нормата" = "TRUE", 
                                  "В нормата" = "FALSE")) %>% 
     ggplot(aes(date, value, fill = col)) +
     geom_col() +
     scale_fill_manual(values = c("В нормата" = "blue", "Над нормата" = "red")) +
     geom_hline(aes(yintercept = standart), linewidth = 0.7, lty = 2, color = "red") +
     theme(text = element_text(size = 14), 
           axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
     labs(x = "Дата", y = paste0(pokazatel()$m_edinica), fill = "Легенда:",
          caption = "Източник на данните: ИАОС") +
     facet_wrap(vars(site_name), labeller = labeller(site_name = label_wrap_gen(35)))
 }, height = 800, width = 1600, res = 96)
  
output$map <- renderLeaflet({
  pokazatel() %>%
  st_as_sf(coords = c("long", "lat"), crs = c(4326)) %>%
  leaflet() %>% 
    addProviderTiles(providers$OpenStreetMap) %>% 
    addCircles(weight = 10, color = "red") %>% 
    addLabelOnlyMarkers(label =  ~ site_name)
})
#--------------------------------------------
surf_basin <- reactive({
  filter(surf_water, basin == input$surf_basin)
})
observeEvent(surf_basin(), {
  freezeReactiveValue(input, "site")
  choices <- unique(surf_basin()$site_name)
  updateSelectInput(inputId = "site", choices = choices)
})
site <- reactive({
  req(input$site)
  filter(surf_basin(), site_name == input$site)
})
observeEvent(site(), {
  freezeReactiveValue(input, "surf_index")
  choices <- unique(site()$index)
  updateSelectInput(inputId = "surf_index", choices = choices)
})
surf_index <- reactive({
  req(input$surf_index)
  filter(site(), index == input$surf_index)
})

output$surf_plot <- renderPlot({
  
  surf_index() %>% 
    filter(site_name %in% c(input$site),
           index %in% c(input$surf_index)) %>% 
    mutate(date = as.factor(date),
           col = fct_recode(col, "Извън нормата" = "1", 
                            "В нормата" = "0")) %>% 
    ggplot(aes(date, value, fill = col)) +
    geom_col() +
    scale_fill_manual(values = c("В нормата" = "blue", "Извън нормата" = "red")) +
    geom_hline(aes(yintercept = pdk), linewidth = 0.7, lty = 2, color = "red") +
    theme(text = element_text(size = 14), 
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(x = "Дата", y = paste0(surf_index()$m_edinica), fill = "Легенда:",
         caption = "Източник на данните: ИАОС")
  
}, height = 800, width = 1600, res = 96)
#------------------------------------
  session$onSessionEnded(function() {
    stopApp()
  })
}
shinyApp(ui, server)
