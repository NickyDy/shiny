library(tidyverse)
library(rvest)
library(arrow)
library(tidytext)
library(shiny)
library(bslib)

temp_nimh_new <- read_html("http://www.weather.bg/index.php?koiFail=tekushti&lng=0") %>%
  html_element("table") %>% html_table() %>% 
  select(station = Станция, date = Дата, 
         hour = Час, temp = `Температура[°C]`, 
         weather = Време, wind_speed = `Вятър-скорост[m/s]`, 
         wind_dir = `Вятър-посока`, pressure = `Налягане[hPa]`) %>% 
  mutate(date = dmy(date))
#----------------------------------------
mail <- tags$a(icon("envelope"), "Email", 
               href = "mailto:nickydyakov@gmail.com", 
               tagret = "_blank")
github <- tags$a(icon("github"), "Github", 
                 href = "https://github.com/NickyDy", 
                 tagret = "_blank")
#--------------------------------------------
ui <- page_fillable(h3("Времето в България!"),
                    fillable_mobile = TRUE,
                    theme = bslib::bs_theme(bootswatch = "darkly"),
                    navset_pill(
                      nav_panel(title = "Температура",
                                plotOutput("temp")),
                      nav_panel(title = "Вятър",
                                plotOutput("wind")),
                      nav_panel(tags$img(src = "shiny.png", width = 40),
                                "Други полезни приложения:",
                                tags$a(href = "https://nickydy.shinyapps.io/elections/", br(),
                                       "Избори в България!"), br(),
                                tags$a(href = "https://nickydy.shinyapps.io/climate/",
                                       "Климатът на България!"), br(),
                                tags$a(href = "https://nickydy.shinyapps.io/inlation/",
                                       "Inflation in EU!"), br(),
                                tags$a(href = "https://ndapps.shinyapps.io/bgprices/",
                                       "Сравнение на цените в България!"), br(),
                                tags$a(href = "https://ndapps.shinyapps.io/agri/",
                                       "Цени на селскостопанска продукция в ЕС!"), br(),
                                tags$a(href = "https://nickydy.shinyapps.io/eurostat/",
                                       "Евростат за България!"), br(),
                                tags$a(href = "https://nickydy.shinyapps.io/demography/",
                                       "Демография на България!"), br(),
                                tags$a(href = "https://ndapps.shinyapps.io/und_water/",
                                       "Чистота на водите в България!"), br()),
                      nav_panel(tags$img(src = "kofi.png", width = 40),
                                "Ако Ви харесва приложението,
               можете да ме подкрепите като направите дарение в евро към
               следната сметка:",
                                br(),
                                br(),
                                "Име: Nikolay Dyakov",
                                br(),
                                "IBAN: BE89 9670 3038 2685",
                                br(),
                                "BIC: TRWIBEB1XXX",
                                br(),
                                "Адрес: Rue de Trone 100, 3rd floor,",
                                br(),
                                "Brussels,",
                                br(),
                                "1050,",
                                br(),
                                "Belgium"),
                      nav_spacer(),
                      nav_menu(
                        title = "Links",
                        nav_item(mail),
                        nav_item(github)
    )
  )
)
#-------------------------------------------
server <- function(input, output, session) {
output$temp <- renderPlot({
  temp_nimh_new %>% 
    mutate(station = reorder_within(station, temp, weather)) %>% 
    ggplot(aes(temp, station, fill = temp)) +
    geom_col(show.legend = F) +
    geom_text(aes(label = temp), position = position_dodge(width = 1), hjust = -0.1, size = 5) +
    scale_x_continuous(expand = expansion(mult = c(.01, .2))) +
    scale_y_reordered() +
    theme(text = element_text(size = 16), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) +
    scale_fill_gradient(low = "white", high = "red") +
    labs(y = NULL, x = "Температура (\u00B0C)", 
         title = paste0("Дата на измерване: ", unique(temp_nimh_new$date), "; ",
                        "Час на измерване: ", unique(temp_nimh_new$hour), ":00"),
         caption = "Източник на данните: НИМХ") +
    facet_wrap(vars(weather), scales = "free_y", ncol = 4)
  }, height = 800, width = 1800, res = 96)
  #-----------------------------------------
output$wind <- renderPlot({
  temp_nimh_new %>% 
    mutate(station = reorder_within(station, wind_speed, weather)) %>% 
    ggplot(aes(wind_speed, station, fill = wind_speed)) +
    geom_col(show.legend = F) +
    geom_text(aes(label = wind_speed), position = position_dodge(width = 1), hjust = -0.1, size = 5) +
    scale_y_reordered() +
    scale_x_continuous(expand = expansion(mult = c(.01, .2))) +
    theme(text = element_text(size = 16), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) +
    scale_fill_gradient(low = "white", high = "blue") +
    labs(y = NULL, x = "Скорост на вятъра (m/s)", 
         title = paste0("Дата на измерване: ", unique(temp_nimh_new$date), "; ",
                        "Час на измерване: ", unique(temp_nimh_new$hour), ":00"),
         caption = "Източник на данните: НИМХ") +
    facet_wrap(vars(weather), scales = "free_y", ncol = 4)
  }, height = 800, width = 1800, res = 96)
 
  session$onSessionEnded(function() {
    stopApp()
  })
}
shinyApp(ui, server)