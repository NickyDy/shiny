library(tidyverse)
library(shiny)
library(bslib)
library(DT)

pharms <- read_rds("pharm_week31.rds") %>% mutate(price = round(price, 2))
df_2025 <- read_rds("df_2025.rds") %>% filter(!source %in% c("T MARKET")) %>% 
  mutate(date = str_replace(date, "2025-02-06", "2025-02-08"),
                            price = round(price, 2))
#user_base <- read_rds("user_base.rds")

food_levels <- c("Zasiti", "VMV", "Kaufland", "Taraba", "T MARKET",
            "Superbag", "Shop24", "Gladen",
            "BulMag", "Trista", "Морски дар", "Randi", "Наслада",
            "Rusebag", "Бакалийка", "Bestmart")
food_colors <- c("#984ea3", "#4daf4a", "#e41a1c", "#8dd3c7", 
            "#ffed6f", "#bebada", "#fb8072", "darkgreen", 
            "#fdb462", "#b3de69", "blue", "pink", "#bc80bd", 
            "#ccebc5", "midnightblue", "#00FFFF")
pharm_levels <- c("Sopharmacy", "366", "Фрамар", "Remedium",
                  "Gpharm", "Ozone", "Аптеки Лили", "Салвия",
                  "Epharm", "Mypharmacy", "Afya", 
                  "Marvi", "Аптека Промахон", "Аптека Витоша")
pharm_colors <- c("#984ea3", "#4daf4a", "#e41a1c", "#8dd3c7", 
                  "midnightblue", "#bebada", "#fb8072", "darkgreen",
                  "#fdb462", "#b3de69", "blue", "pink", "#bc80bd", 
                  "#00FFFF")
colors_percent <- c("TRUE" = "#00BFC4", "FALSE" = "#F8766D")
#-----------------------------------------------------------
mail <- tags$a(icon("envelope"), "Email", 
               href = "mailto:nickydyakov@gmail.com", 
               tagret = "_blank")
github <- tags$a(icon("github"), "Github", 
                 href = "https://github.com/NickyDy", 
                 tagret = "_blank")
#---------------------------------------------------------
ui <- page_fillable(#h3("Сравнение на цените в България!"), 
                    theme = bslib::bs_theme(bootswatch = "darkly"),
                    
                    # shinyauthr::loginUI(id = "login"),
                    # div(id = "show-page-content",
                    
                    navset_pill(
                      nav_panel(title = "Храни",
                                DTOutput("foods", width = 1850)),
                      nav_panel(title = "Фармация",
                                DTOutput("pharms", width = 1850)),
                      nav_panel(title = "Инфлация (храни)", layout_columns(
                                dateRangeInput("date_range_inf", "Дата (от/до):", language = "bg", 
                                       weekstart = 1, separator = "до",
                                       start = first(df_2025$date), end = last(df_2025$date),
                                       min = first(df_2025$date), max = last(df_2025$date)),
                                selectInput("inf_price_source", "Източник на цените:", 
                                            choices = unique(df_2025$source)),
                                selectInput("inf_location", "Населено място:", choices = NULL),
                                selectInput("inf_type", "Продуктова група:", choices = NULL),
                                selectInput("inf_unit", "Грамаж:", choices = NULL),
                                sliderInput("height", "Височина на графиката:", 
                                            min = 800, max = 7000, value = 800, step = 100),
                                col_widths = c(2, 2, 2, 2, 2, 2)),
                                plotOutput("inf_price_trend")),
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
                                tags$a(href = "https://ndapps.shinyapps.io/agri/",
                                       "Цени на селскостопанска продукция в ЕС!"), br(),
                                tags$a(href = "https://nickydy.shinyapps.io/eurostat/",
                                       "Евростат за България!"), br(),
                                tags$a(href = "https://ndapps.shinyapps.io/und_water/",
                                       "Чистота на водите в България"), br()),
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
                        nav_item(github))
                      ))# %>% shinyjs::hidden())
#-----------------------------------------------
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
  #----------------------------------------------
  output$foods <- renderDT(
    df_2025 %>% arrange(price) %>% 
      datatable(rownames = F, filter = "top",
                colnames = c("Дата" = "date",
                             "Населено място" = "location",
                             "Супермаркет" = "source", 
                             "Продуктова група" = "type",
                             "Продукт" = "product",
                             "Цена (лв)" = "price"),
                options = list(dom = 'frtip', pageLength = 100)) %>% 
      formatStyle("Супермаркет", backgroundColor = styleEqual(food_levels, food_colors)))
  
  output$pharms <- renderDT(
    pharms %>% arrange(price) %>% 
      datatable(rownames = F, filter = "top",
                colnames = c("Дата" = "date",
                             "Аптека" = "source", 
                             "Продуктова група" = "type",
                             "Продукт" = "product", 
                             "Цена (лв)" = "price"), 
                options = list(dom = 'frtip', pageLength = 100)) %>% 
      formatStyle("Аптека", backgroundColor = styleEqual(pharm_levels, pharm_colors)))
#-------------------------------------------------------------------------------------
  new_old_df <- reactive({
    
    df_2025 %>% 
      filter(date >= input$date_range_inf[1] & date <= input$date_range_inf[2]) %>%
      summarise(price_change = (last(price, na_rm = T) - first(price, na_rm = T)) / first(price, na_rm = T), 
                .by = c(location, source, type, unit, product)) %>% 
      filter(price_change != 0) %>%
      mutate(product = fct_reorder(product, price_change), col = price_change > 0)
    
  })
  
  inf_price_source <- reactive({
    filter(new_old_df(), source == input$inf_price_source)
  })
  
  observeEvent(inf_price_source(), {
    freezeReactiveValue(input, "inf_location")
    choices <- unique(inf_price_source()$location)
    updateSelectInput(inputId = "inf_location", choices = choices)
  })
  
  inf_location <- reactive({
    req(input$inf_location)
    filter(inf_price_source(), location == input$inf_location)
  })
  
  observeEvent(inf_location(), {
    freezeReactiveValue(input, "inf_type")
    choices <- unique(inf_location()$type)
    updateSelectInput(inputId = "inf_type", choices = choices)
  })
  
  inf_type <- reactive({
    req(input$inf_type)
    filter(inf_location(), type == input$inf_type)
  })
  
  observeEvent(inf_type(), {
    freezeReactiveValue(input, "inf_unit")
    choices <- unique(inf_type()$unit)
    updateSelectInput(inputId = "inf_unit", choices = choices)
  })
  
  inf_unit <- reactive({
    req(input$inf_unit)
    filter(inf_type(), unit == input$inf_unit)
  })
  
  output$inf_price_trend <- renderPlot({
    
    inf_unit() %>% 
      ggplot(aes(price_change, product, fill = col)) +
      geom_col(show.legend = F) +
      scale_fill_manual(values = colors_percent) +
      scale_x_continuous(expand = expansion(mult = c(0.01, 0.10))) +
      geom_text(aes(label = scales::percent(price_change, accuracy = 1)),
                position = position_dodge(width = 1), hjust = -0.1, size = 3.5) +
      labs(y = NULL, x = NULL,
           title = glue::glue("Промяна в цената (%) на продуктите от {input$date_range_inf[1]} до ",
                              "{input$date_range_inf[2]}.")) +
      theme(text = element_text(size = 14), axis.text.x = element_blank(), 
            axis.ticks.x = element_blank())
    
  }, height = function() input$height, width = 1550, res = 96)
#-------------------------------------------------------------
session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui, server)
