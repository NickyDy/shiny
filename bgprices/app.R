library(tidyverse)
library(shiny)
library(bslib)
library(DT)

foods <- read_rds("foods_week31.rds") %>% mutate(price = round(price, 2)) %>% arrange(price)
pharms <- read_rds("pharm_week31.rds") %>% mutate(price = round(price, 2)) %>% arrange(price)
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
    foods %>% 
      datatable(rownames = F, filter = "top",
                colnames = c("Седмица" = "date",
                             "Населено място" = "location",
                             "Супермаркет" = "source", 
                             "Продуктова група" = "type",
                             "Продукт" = "product",
                             "Цена (лв)" = "price"),
                options = list(dom = 'frtip', pageLength = 100)) %>% 
      formatStyle("Супермаркет", backgroundColor = styleEqual(food_levels, food_colors)))
  
  output$pharms <- renderDT(
    pharms %>% 
      datatable(rownames = F, filter = "top",
                colnames = c("Седмица" = "date",
                             "Аптека" = "source", 
                             "Продуктова група" = "type",
                             "Продукт" = "product", 
                             "Цена (лв)" = "price"), 
                options = list(dom = 'frtip', pageLength = 100)) %>% 
      formatStyle("Аптека", backgroundColor = styleEqual(pharm_levels, pharm_colors)))
#--------------------------------------------------
session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui, server)
