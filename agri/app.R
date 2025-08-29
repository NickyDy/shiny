library(tidyverse)
library(shiny)
library(bslib)

user_base <- read_rds("user_base.rds")
beef_carc <- read_rds("beef_carc.rds") %>% arrange(date)
beef_live <- read_rds("beef_live.rds") %>% arrange(date)
piglets <- read_rds("piglets.rds") %>% arrange(date)
pigmeat_carc <- read_rds("pigmeat_carc.rds") %>% arrange(date)
pigmeat_cuts <- read_rds("pigmeat_cuts.rds") %>% arrange(date) %>% drop_na()
eggs <- read_rds("eggs.rds") %>% arrange(date)
poultry <- read_rds("poultry.rds") %>% arrange(date) %>% drop_na()
sheep_goat <- read_rds("sheep_goat.rds") %>% arrange(date)
raw_milk <- read_rds("raw_milk.rds") %>% arrange(date)
dairy <- read_rds("dairy.rds") %>% arrange(date)
fruit_veg <- read_rds("fruit_veg.rds") %>% arrange(date)
cereals <- read_rds("cereals.rds") %>% 
  filter(!stage_name == "Unknown") %>% 
  arrange(date) %>% drop_na()
oilseeds <- read_rds("oilseeds.rds") %>% arrange(date)
olive_oil <- read_rds("olive_oil.rds") %>%
  mutate(product = str_replace_all(product, c("°" = "%", "," = "."))) %>% 
  arrange(date)
wine <- read_rds("wine.rds") %>% arrange(date)
#-----------------------------------------------------
mail <- tags$a(icon("envelope"), "Email", 
               href = "mailto:nickydyakov@gmail.com", 
               tagret = "_blank")
github <- tags$a(icon("github"), "Github", 
                 href = "https://github.com/NickyDy", 
                 tagret = "_blank")
#-----------------------------------------------------------------
ui <- page_fillable(#h3("Цени на селскостопанска продукция в ЕС!"),
                    theme = bslib::bs_theme(bootswatch = "darkly"),
                    
                    # shinyauthr::loginUI(id = "login"),
                    # div(id = "show-page-content",
                    
                   navset_pill(
                      nav_panel(title = "Телешко (труп)", layout_columns(
                        dateRangeInput("beef_carc_date", "Дата (от/до):", 
                                       language = "bg", weekstart = 1, separator = "до",
                                       start = first(beef_carc$date), 
                                       end = last(beef_carc$date),
                                       min = first(beef_carc$date), 
                                       max = last(beef_carc$date)),
                        selectInput("beef_carc_category", "Категория:",
                                    choices = unique(beef_carc$category)),
                        selectInput("beef_carc_product", "Продукт:",
                                    choices = NULL), col_widths = c(2, 2, 2)),
                        plotOutput("beef_carc_plot")),
                      nav_panel(title = "Телешко", layout_columns(
                        dateRangeInput("beef_live_date", "Дата (от/до):", 
                                       language = "bg", weekstart = 1, separator = "до",
                                       start = first(beef_live$date), 
                                       end = last(beef_live$date),
                                       min = first(beef_live$date), 
                                       max = last(beef_live$date)),
                        selectInput("beef_live_category", "Категория:",
                                    choices = unique(beef_live$category)),
                        selectInput("beef_live_unit", "Мерна единица:",
                                    choices = NULL), col_widths = c(2, 2, 2)),
                        plotOutput("beef_live")),
                      nav_panel("Прасенца",
                                dateRangeInput("piglets_date", "Дата (от/до):", 
                                               language = "bg", weekstart = 1, separator = "до",
                                               start = first(piglets$date), 
                                               end = last(piglets$date),
                                               min = first(piglets$date), 
                                               max = last(piglets$date)),
                                plotOutput("piglets")),
                      nav_panel("Свинско (труп)", layout_columns(
                        dateRangeInput("pigmeat_carc_date", "Дата (от/до):", 
                                       language = "bg", weekstart = 1, separator = "до",
                                       start = first(pigmeat_carc$date), 
                                       end = last(pigmeat_carc$date),
                                       min = first(pigmeat_carc$date), 
                                       max = last(pigmeat_carc$date)),
                        selectInput("pigmeat_carc_product", "Продукт:",
                                    choices = unique(pigmeat_carc$product)), col_widths = c(2, 2)),
                        plotOutput("pigmeat_carc")),
                      nav_panel("Свинско", layout_columns(
                        dateRangeInput("pigmeat_cuts_date", "Дата (от/до):", 
                                       language = "bg", weekstart = 1, separator = "до",
                                       start = first(pigmeat_cuts$date), 
                                       end = last(pigmeat_cuts$date),
                                       min = first(pigmeat_cuts$date), 
                                       max = last(pigmeat_cuts$date)),
                        selectInput("pigmeat_cuts_category", "Категория:",
                                    choices = unique(pigmeat_cuts$category)),
                        selectInput("pigmeat_cuts_price_type", "Тип цена:",
                                    choices = NULL), col_widths = c(2, 2, 2)),
                        plotOutput("pigmeat_cuts")),
                      nav_panel("Яйца", layout_columns(
                        dateRangeInput("eggs_date", "Дата (от/до):", 
                                       language = "bg", weekstart = 1, separator = "до",
                                       start = first(eggs$date), 
                                       end = last(eggs$date),
                                       min = first(eggs$date), 
                                       max = last(eggs$date)),
                        selectInput("farming_method", "Отглеждане:",
                                    choices = unique(eggs$farming_method)), col_widths = c(2, 2)),
                        plotOutput("eggs")),
                      nav_panel("Пилешко", layout_columns(
                        dateRangeInput("poultry_date", "Дата (от/до):", 
                                       language = "bg", weekstart = 1, separator = "до",
                                       start = first(poultry$date), 
                                       end = last(poultry$date),
                                       min = first(poultry$date), 
                                       max = last(poultry$date)),
                        selectInput("poultry_product", "Продукт:",
                                    choices = unique(poultry$product)), col_widths = c(2, 2)),
                        plotOutput("poultry")),
                      nav_panel("Овче и козе месо", layout_columns(
                        dateRangeInput("sheep_goat_date", "Дата (от/до):", 
                                       language = "bg", weekstart = 1, separator = "до",
                                       start = first(sheep_goat$date), 
                                       end = last(sheep_goat$date),
                                       min = first(sheep_goat$date), 
                                       max = last(sheep_goat$date)),
                        selectInput("sheep_goat_category", "Категория:",
                                    choices = unique(sheep_goat$category)), col_widths = c(2, 2)),
                        plotOutput("sheep_goat")),
                      nav_panel("Прясно мляко", layout_columns(
                        dateRangeInput("raw_milk_date", "Дата (от/до):", 
                                       language = "bg", weekstart = 1, separator = "до",
                                       start = first(raw_milk$date), 
                                       end = last(raw_milk$date),
                                       min = first(raw_milk$date), 
                                       max = last(raw_milk$date)),
                        selectInput("raw_milk_product", "Продукт:",
                                    choices = unique(raw_milk$product)), col_widths = c(2, 2)),
                        plotOutput("raw_milk")),
                      nav_panel("Млечни продукти", layout_columns(
                        dateRangeInput("dairy_date", "Дата (от/до):", 
                                       language = "bg", weekstart = 1, separator = "до",
                                       start = first(dairy$date), 
                                       end = last(dairy$date),
                                       min = first(dairy$date), 
                                       max = last(dairy$date)),
                        selectInput("dairy_product", "Продукт:",
                                    choices = unique(dairy$product)), col_widths = c(2, 2)),
                        plotOutput("dairy")),
                      nav_panel("Плодове и зеленчуци", layout_columns(
                        dateRangeInput("fruit_veg_date", "Дата (от/до):", 
                                       language = "bg", weekstart = 1, separator = "до",
                                       start = first(fruit_veg$date), 
                                       end = last(fruit_veg$date),
                                       min = first(fruit_veg$date), 
                                       max = last(fruit_veg$date)),
                        selectInput("fruit_veg_product", "Продукт:", 
                                    choices = unique(fruit_veg$product)),
                        selectInput("fruit_veg_variety", "Разновидност:",
                                    choices = NULL), col_widths = c(2, 2, 4)),
                        plotOutput("fruit_veg")),
                      nav_panel("Зърнени", layout_columns(
                        dateRangeInput("cereals_date", "Дата (от/до):", 
                                       language = "bg", weekstart = 1, separator = "до",
                                       start = first(cereals$date), 
                                       end = last(cereals$date),
                                       min = first(cereals$date), 
                                       max = last(cereals$date)),
                        selectInput("cereals_state", "Държава:",
                                    choices = unique(cereals$state)),
                        selectInput("cereals_stage_name", "Порт:",
                                    choices = NULL),
                        selectInput("cereals_product", "Продукт:",
                                    choices = NULL), col_widths = c(2, 2, 5, 2)),
                        plotOutput("cereals")),
                      nav_panel("Маслодайни", layout_columns(
                        dateRangeInput("oilseeds_date", "Дата (от/до):", 
                                       language = "bg", weekstart = 1, separator = "до",
                                       start = first(oilseeds$date), 
                                       end = last(oilseeds$date),
                                       min = first(oilseeds$date), 
                                       max = last(oilseeds$date)),
                        selectInput("oilseeds_market_stage", "Порт:",
                                    choices = unique(oilseeds$market_stage)),
                        selectInput("oilseeds_state", "Страна:",
                                    choices = NULL),
                        selectInput("oilseeds_product_type", "Тип продукт:",
                                    choices = NULL),
                        selectInput("oilseeds_product", "Продукт:",
                                    choices = NULL), col_widths = c(2, 2, 2, 2, 2)),
                        plotOutput("oilseeds")),
                      nav_panel("Зехтин", layout_columns(
                        dateRangeInput("olive_oil_date", "Дата (от/до):", 
                                       language = "bg", weekstart = 1, separator = "до",
                                       start = first(olive_oil$date), 
                                       end = last(olive_oil$date),
                                       min = first(olive_oil$date), 
                                       max = last(olive_oil$date)),
                        selectInput("olive_state", "Държава:", 
                                    choices = unique(olive_oil$state)),
                        selectInput("olive_oil_product", "Продукт:",
                                    choices = NULL), col_widths = c(2, 2, 3)),
                        plotOutput("olive_oil")),
                      nav_panel("Вино", layout_columns(
                        dateRangeInput("wine_date", "Дата (от/до):", 
                                       language = "bg", weekstart = 1, separator = "до",
                                       start = first(wine$date), 
                                       end = last(wine$date),
                                       min = first(wine$date), 
                                       max = last(wine$date)),
                        selectInput("wine_state", "Държава:",
                                    choices = unique(wine$state)), col_widths = c(2, 2)),
                        plotOutput("wine")),
                      nav_panel(tags$img(src = "shiny.png", width = 40),
                                "Други полезни приложения:",
                                tags$a(href = "https://nickydy.shinyapps.io/elections/", br(),
                                       "Избори в България!"), br(),
                                tags$a(href = "https://nickydy.shinyapps.io/climate/",
                                       "Климатът на България!"), br(),
                                tags$a(href = "https://nickydy.shinyapps.io/demography/",
                                       "Демография на България!"), br(),
                                tags$a(href = "https://nickydy.shinyapps.io/inflation/",
                                       "Inflation in EU"), br(),
                                # tags$a(href = "https://ndapps.shinyapps.io/bgprices/",
                                #        "Сравнение на цените в България!"), br(),
                                tags$a(href = "https://nickydy.shinyapps.io/eurostat/",
                                       "Евростат за България!"), br(),
                                tags$a(href = "https://ndapps.shinyapps.io/und_water/",
                                       "Чистота на водите в България!"), br()),
                      # nav_panel(tags$img(src = "kofi.png", width = 40),
                      #           "Ако Ви харесва приложението,
                      #           можете да ме подкрепите като направите дарение в евро към
                      #           следната сметка:",
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
)# %>% shinyjs::hidden())

server <- function(input, output, session) {
  
  # credentials <- shinyauthr::loginServer(
  #   id = "login",
  #   data = user_base,
  #   user_col = user,
  #   pwd_col = password,
  #   sodium_hashed = TRUE,
  #   log_out = reactive(logout_init()))
  # 
  # observe({
  #   req(credentials()$user_auth)
  #   shinyjs::show(id = "show-page-content")
  # })
  # 
  # logout_init <- shinyauthr::logoutServer(
  #   id = "logout",
  #   active = reactive(credentials()$user_auth))
  # #----------------------------------------------
  beef_carc_category <- reactive({
    filter(beef_carc, category %in% c(input$beef_carc_category))
  })
  
  observeEvent(beef_carc_category(), {
    freezeReactiveValue(input, "beef_carc_product")
    choices <- unique(beef_carc_category()$product)
    updateSelectInput(inputId = "beef_carc_product", choices = choices)
  })
  
  beef_carc_product <- reactive({
    req(input$beef_carc_category)
    filter(beef_carc_category(), product == input$beef_carc_product)
  })
  
  output$beef_carc_plot <- renderPlot({
    beef_carc_product() %>% 
      filter(date >= input$beef_carc_date[1] & date <= input$beef_carc_date[2],
             product %in% c(input$beef_carc_product)) %>% 
      ggplot(aes(date, price / 100)) +
      geom_line() +
      labs(x = NULL, y = "Цена (€/кг)") +
      theme(text = element_text(size = 14), 
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      facet_wrap(vars(state))
  }, height = 800, width = 1800, res = 96)
  #-----------------------------------------
  beef_live_category <- reactive({
    filter(beef_live, category %in% c(input$beef_live_category))
  })
  
  observeEvent(beef_live_category(), {
    freezeReactiveValue(input, "beef_live_unit")
    choices <- unique(beef_live_category()$unit)
    updateSelectInput(inputId = "beef_live_unit", choices = choices)
  })
  
  beef_live_unit <- reactive({
    req(input$beef_live_category)
    filter(beef_live_category(), unit == input$beef_live_unit)
  })
  
  output$beef_live <- renderPlot({
    
    beef_live_unit() %>% 
      filter(date >= input$beef_live_date[1] & date <= input$beef_live_date[2],
             category %in% c(input$beef_live_category),
             unit %in% c(input$beef_live_unit)) %>% 
      ggplot(aes(date, price_eur)) +
      geom_line() +
      labs(x = NULL, y = "Цена (€)/глава") +
      theme(text = element_text(size = 14),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      facet_wrap(vars(state))
    
  }, height = 800, width = 1800, res = 96)
  #---------------------------------------
  output$piglets <- renderPlot({
    
    piglets %>% 
      filter(date >= input$piglets_date[1] & date <= input$piglets_date[2]) %>% 
      ggplot(aes(date, price_100kg_eur / 100)) +
      geom_line() +
      labs(x = NULL, y = "Цена (€/кг)") +
      theme(text = element_text(size = 14),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      facet_wrap(vars(state))
    
  }, height = 800, width = 1800, res = 96)
  #---------------------------------------
  output$pigmeat_carc <- renderPlot({
    
    pigmeat_carc %>% 
      filter(date >= input$pigmeat_carc_date[1] & date <= input$pigmeat_carc_date[2],
             product %in% c(input$pigmeat_carc_product)) %>% 
      ggplot(aes(date, price_100kg_eur / 100)) +
      geom_line() +
      labs(x = NULL, y = "Цена (€/кг)") +
      theme(text = element_text(size = 14),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      facet_wrap(vars(state))
    
  }, height = 800, width = 1800, res = 96)
  #---------------------------------------
  pigmeat_cuts_category <- reactive({
    filter(pigmeat_cuts, category %in% c(input$pigmeat_cuts_category))
  })
  
  observeEvent(pigmeat_cuts_category(), {
    freezeReactiveValue(input, "pigmeat_cuts_price_type")
    choices <- unique(pigmeat_cuts_category()$price_type)
    updateSelectInput(inputId = "pigmeat_cuts_price_type", choices = choices)
  })
  
  pigmeat_cuts_price_type <- reactive({
    req(input$pigmeat_cuts_category)
    filter(pigmeat_cuts_category(), price_type == input$pigmeat_cuts_price_type)
  })
  
  output$pigmeat_cuts <- renderPlot({
    
    pigmeat_cuts_price_type() %>% 
      filter(date >= input$pigmeat_cuts_date[1] & date <= input$pigmeat_cuts_date[2],
             price_type %in% c(input$pigmeat_cuts_price_type)) %>% 
      ggplot(aes(date, price_100kg_eur / 100)) +
      geom_line() +
      labs(x = NULL, y = "Цена (€/кг)") +
      theme(text = element_text(size = 14),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      facet_wrap(vars(state))
    
  }, height = 800, width = 1800, res = 96)
  #---------------------------------------
  output$eggs <- renderPlot({
    
    eggs %>% 
      filter(date >= input$eggs_date[1] & date <= input$eggs_date[2],
             farming_method %in% c(input$farming_method)) %>% 
      ggplot(aes(date, price_100kg_eur / 100)) +
      geom_line() +
      labs(x = NULL, y = "Цена (€/кг)") +
      theme(text = element_text(size = 14),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      facet_wrap(vars(state))
    
  }, height = 800, width = 1800, res = 96)
  #---------------------------------------
  output$poultry <- renderPlot({
    
    poultry %>% 
      filter(date >= input$poultry_date[1] & date <= input$poultry_date[2],
             product %in% c(input$poultry_product)) %>% 
      ggplot(aes(date, price_100kg_eur / 100)) +
      geom_line() +
      labs(x = NULL, y = "Цена (€/кг)") +
      theme(text = element_text(size = 14),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      facet_wrap(vars(state))
    
  }, height = 800, width = 1800, res = 96)
  #---------------------------------------
  output$sheep_goat <- renderPlot({
    
    sheep_goat %>% 
      filter(date >= input$sheep_goat_date[1] & date <= input$sheep_goat_date[2],
             category %in% c(input$sheep_goat_category)) %>%
      group_by(state) %>% 
      ggplot(aes(date, price_100kg_eur / 100)) +
      geom_line() +
      labs(x = NULL, y = "Цена (€/кг)") +
      theme(text = element_text(size = 14),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      facet_wrap(vars(state))
    
  }, height = 800, width = 1800, res = 96)
  #---------------------------------------
  output$raw_milk <- renderPlot({
    
    raw_milk %>% 
      filter(date >= input$raw_milk_date[1] & date <= input$raw_milk_date[2],
             product %in% c(input$raw_milk_product)) %>% 
      ggplot(aes(date, price_100kg_eur / 100)) +
      geom_line() +
      labs(x = NULL, y = "Цена (€/кг)") +
      theme(text = element_text(size = 14),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      facet_wrap(vars(state))
    
  }, height = 800, width = 1800, res = 96)
  #---------------------------------------
  output$dairy <- renderPlot({
    
    dairy %>% 
      filter(date >= input$dairy_date[1] & date <= input$dairy_date[2],
             product %in% c(input$dairy_product)) %>% 
      ggplot(aes(date, price_100kg_eur / 100)) +
      geom_line() +
      labs(x = NULL, y = "Цена (€/кг)") +
      theme(text = element_text(size = 14),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      facet_wrap(vars(state))
    
  }, height = 800, width = 1800, res = 96)
  #---------------------------------------
  fruit_veg_product <- reactive({
    filter(fruit_veg, product %in% c(input$fruit_veg_product))
  })
  
  observeEvent(fruit_veg_product(), {
    freezeReactiveValue(input, "fruit_veg_variety")
    choices <- unique(fruit_veg_product()$variety)
    updateSelectInput(inputId = "fruit_veg_variety", choices = choices)
  })
  
  fruit_veg_variety <- reactive({
    req(input$fruit_veg_product)
    filter(fruit_veg_product(), variety == input$fruit_veg_variety)
  })
  
  output$fruit_veg <- renderPlot({
    
    fruit_veg_variety() %>% 
      filter(date >= input$fruit_veg_date[1] & date <= input$fruit_veg_date[2],
             variety %in% c(input$fruit_veg_variety)) %>% 
      ggplot(aes(date, price_100_kg_eur / 100)) +
      geom_line() +
      labs(x = NULL, y = "Цена (€/кг)") +
      theme(text = element_text(size = 14),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      facet_wrap(vars(state))
    
  }, height = 800, width = 1800, res = 96)
  #---------------------------------------
  cereals_state <- reactive({
    filter(cereals, state %in% c(input$cereals_state))
  })
  
  observeEvent(cereals_state(), {
    freezeReactiveValue(input, "cereals_stage_name")
    choices <- unique(cereals_state()$stage_name)
    updateSelectInput(inputId = "cereals_stage_name", choices = choices)
  })
  
  cereals_stage_name <- reactive({
    req(input$cereals_state)
    filter(cereals_state(), stage_name == input$cereals_stage_name)
  })
  
  observeEvent(cereals_stage_name(), {
    freezeReactiveValue(input, "cereals_product")
    choices <- unique(cereals_stage_name()$product)
    updateSelectInput(inputId = "cereals_product", choices = choices)
  })
  
  cereals_product <- reactive({
    req(input$cereals_stage_name)
    filter(cereals_stage_name(), product == input$cereals_product)
  })
  
  output$cereals <- renderPlot({
    
    cereals_product() %>% 
      filter(date >= input$cereals_date[1] & date <= input$cereals_date[2],
             stage_name %in% c(input$cereals_stage_name),
             product %in% c(input$cereals_product)) %>% 
      ggplot(aes(date, price_tonne_eur)) +
      geom_line() +
      labs(x = NULL, y = "Цена (€/тон)") +
      theme(text = element_text(size = 14),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      facet_wrap(vars(market_name))
    
  }, height = 800, width = 1800, res = 96)
  #---------------------------------------
  oilseeds_market_stage <- reactive({
    filter(oilseeds, market_stage %in% c(input$oilseeds_market_stage))
  })
  
  observeEvent(oilseeds_market_stage(), {
    freezeReactiveValue(input, "oilseeds_state")
    choices <- unique(oilseeds_market_stage()$state)
    updateSelectInput(inputId = "oilseeds_state", choices = choices)
  })
  
  oilseeds_state <- reactive({
    req(input$oilseeds_market_stage)
    filter(oilseeds_market_stage(), state == input$oilseeds_state)
  })
  
  observeEvent(oilseeds_state(), {
    freezeReactiveValue(input, "oilseeds_product_type")
    choices <- unique(oilseeds_state()$product_type)
    updateSelectInput(inputId = "oilseeds_product_type", choices = choices)
  })
  
  oilseeds_product_type <- reactive({
    req(input$oilseeds_state)
    filter(oilseeds_state(), product_type == input$oilseeds_product_type)
  })
  
  observeEvent(oilseeds_product_type(), {
    freezeReactiveValue(input, "oilseeds_product")
    choices <- unique(oilseeds_product_type()$product)
    updateSelectInput(inputId = "oilseeds_product", choices = choices)
  })
  
  oilseeds_product <- reactive({
    req(input$oilseeds_product_type)
    filter(oilseeds_product_type(), product == input$oilseeds_product)
  })
  
  output$oilseeds <- renderPlot({
    
    oilseeds_product() %>% 
      filter(date >= input$oilseeds_date[1] & date <= input$oilseeds_date[2],
             market_stage %in% c(input$oilseeds_market_stage),
             product_type %in% c(input$oilseeds_product_type),
             product %in% c(input$oilseeds_product)) %>% 
      ggplot(aes(date, price_eur)) +
      geom_line() +
      labs(x = NULL, y = "Цена (€)") +
      theme(text = element_text(size = 14),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      facet_wrap(vars(market))
    
  }, height = 800, width = 1800, res = 96)
  #---------------------------------------
  olive_state <- reactive({
    filter(olive_oil, state %in% c(input$olive_state))
  })
  
  observeEvent(olive_state(), {
    freezeReactiveValue(input, "olive_oil_product")
    choices <- unique(olive_state()$product)
    updateSelectInput(inputId = "olive_oil_product", choices = choices)
  })
  
  olive_oil_product <- reactive({
    req(input$olive_state)
    filter(olive_state(), product == input$olive_oil_product)
  })
  
  output$olive_oil <- renderPlot({
    
    olive_oil_product() %>% 
      filter(date >= input$olive_oil_date[1] & date <= input$olive_oil_date[2],
             product %in% c(input$olive_oil_product)) %>% 
      ggplot(aes(date, price_100kg_eur / 100)) +
      geom_line() +
      labs(x = NULL, y = "Цена (€/кг)") +
      theme(text = element_text(size = 14),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      facet_wrap(vars(market))
    
  }, height = 800, width = 1800, res = 96)
  #---------------------------------------
  output$wine <- renderPlot({
    
    wine %>% 
      filter(date >= input$wine_date[1] & date <= input$wine_date[2],
             state %in% c(input$wine_state)) %>% 
      ggplot(aes(date, eur_price_per_hl)) +
      geom_line() +
      labs(x = NULL, y = "Цена (€/hl)") +
      theme(text = element_text(size = 14),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      facet_wrap(vars(wine_description))
    
  }, height = 800, width = 1800, res = 96)
  #---------------------------------------
  session$onSessionEnded(function() {
    stopApp()
  })
}
shinyApp(ui, server)
