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
rain_nimh_new <- read_html("http://www.weather.bg/index.php?koiFail=tekushti&lng=0") %>%
  html_element("center") %>% html_table() %>% slice(-c(1, 168)) %>% 
  select(id = X1, station = X2, rain = X3, mean_rain = X4) %>% 
  mutate(rain = str_replace(rain, "n.a.", ""),
         rain = parse_number(rain), date = Sys.Date(), .after = station) %>% 
  filter(rain > 0)

bg <- read_html("http://weather.bg/index.php?koiFail=bg&lng=0") %>%
  html_element("table") %>% html_table() %>% pivot_longer(-Град) %>% 
  slice(-c(1:8)) %>% select(station = Град, date = name, weather = value)
pl <- read_html("http://weather.bg/index.php?koiFail=bg&lng=0") %>%
  html_element("#planini") %>% html_table() %>% pivot_longer(-Пункт) %>% 
  slice(-c(1:5)) %>% select(station = Пункт, date = name, weather = value)
eu <- read_html("http://weather.bg/index.php?koiFail=eubp&lng=0") %>%
  html_element("table") %>% html_table() %>% pivot_longer(-Град) %>% 
  slice(-c(1:6)) %>% select(station = Град, date = name, weather = value)
forcast <- bind_rows(bg, pl, eu)
temp <- forcast %>% filter(str_detect(weather, "^\\d")) %>% 
  separate_wider_delim(weather, delim = "/", 
                       names = c("Минимална температура", 
                                 "Максимална температура")) %>% 
  mutate(across(3:4, parse_number))
weather <- forcast %>% filter(str_detect(weather, "^[:alpha:]"))
forcast_df <- inner_join(temp, weather) %>% 
  pivot_longer(`Минимална температура`:`Максимална температура`) %>%
  mutate(name = fct_rev(name)) %>% 
  arrange(station) %>% 
  filter(!weather == "n.a.")
#----------------------------------------
mail <- tags$a(icon("envelope"), "Email", 
               href = "mailto:nickydyakov@gmail.com", 
               tagret = "_blank")
github <- tags$a(icon("github"), "Github", 
                 href = "https://github.com/NickyDy", 
                 tagret = "_blank")
#--------------------------------------------
ui <- page_fillable(h3("Времето в България!"),
                    theme = bslib::bs_theme(bootswatch = "darkly"),
                    navset_pill(
                      nav_panel(title = "Температура",
                                plotOutput("temp")),
                      nav_panel(title = "Вятър",
                                plotOutput("wind")),
                      nav_panel(title = "Валеж",
                                plotOutput("rain")),
                      nav_panel(title = "Прогноза",
                                selectInput("sett", "Населено място/Станция:",
                                            choices = unique(forcast_df$station)),
                                plotOutput("forcast")),
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
    geom_text(aes(label = temp), 
              position = position_dodge(width = 1), hjust = -0.1, size = 4) +
    scale_x_continuous(expand = expansion(mult = c(.01, .3))) +
    scale_y_reordered() +
    theme(text = element_text(size = 14), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) +
    scale_fill_gradient(low = "white", high = "red") +
    labs(y = NULL, x = "Градуси (\u00B0C)", 
         title = paste0("Дата и час на измерване: ", 
                        unique(temp_nimh_new$date), ", ", 
                        unique(temp_nimh_new$hour), ":00 ч."),
         caption = "Източник на данните: НИМХ") +
    facet_wrap(vars(weather), scales = "free_y", nrow = 1,
               labeller = labeller(weather = label_wrap_gen(20)))
  }, height = 800, width = 1800, res = 96)
  #-----------------------------------------
output$wind <- renderPlot({
  temp_nimh_new %>% 
    mutate(station = reorder_within(station, wind_speed, weather),
           wind_dir = fct_relevel(wind_dir, "N", "NE", "E", "SE", "S", 
                                            "SW", "W", "NW", "тихо")) %>% 
    ggplot(aes(wind_speed, station, fill = wind_speed)) +
    geom_col(show.legend = F) +
    geom_text(aes(label = wind_speed), 
              position = position_dodge(width = 1), hjust = -0.1, size = 4) +
    scale_y_reordered() +
    scale_x_continuous(expand = expansion(mult = c(.01, .5))) +
    theme(text = element_text(size = 14), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) +
    scale_fill_gradient(low = "white", high = "darkgreen") +
    labs(y = NULL, x = "Скорост на вятъра (m/s)", 
         title = paste0("Дата и час на измерване: ", 
                        unique(temp_nimh_new$date), ", ",
                        unique(temp_nimh_new$hour), ":00 ч."),
         caption = "Източник на данните: НИМХ") +
    facet_wrap(vars(wind_dir), scales = "free_y", ncol = 4)
  }, height = 800, width = 1800, res = 96)
#-----------------------------------------
output$rain <- renderPlot({
  rain_nimh_new %>% drop_na(rain) %>% 
    mutate(code = case_when(
      str_detect(id, "^1") ~ "1",
      str_detect(id, "^2") ~ "2",
      str_detect(id, "^3") ~ "3",
      str_detect(id, "^4") ~ "4",
      str_detect(id, "^5") ~ "5",
      str_detect(id, "^6") ~ "6",
      str_detect(id, "^7") ~ "7",
    )) %>% 
    mutate(station = reorder_within(station, rain, code)) %>% 
    ggplot(aes(rain, station, fill = rain)) +
    geom_col(show.legend = F) +
    geom_text(aes(label = rain), 
              position = position_dodge(width = 1), hjust = -0.1, size = 4) +
    scale_y_reordered() +
    scale_x_continuous(expand = expansion(mult = c(.01, .5))) +
    theme(text = element_text(size = 14), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) +
    scale_fill_gradient(low = "white", high = "blue") +
    labs(y = NULL, x = "Количество на валежа (mm)", 
         caption = "Източник на данните: НИМХ",
         title = paste0("Дата: ", unique(rain_nimh_new$date),
                        " (от 7:30 ч. на предния ден до 7:30 ч. на отбелязаната дата)")) +
    facet_wrap(vars(code), scales = "free_y", nrow = 1)
}, height = 800, width = 1800, res = 96)

output$forcast <- renderPlot({
  forcast_df %>% 
    filter(station == input$sett) %>% 
    ggplot(aes(value, station, fill = name)) +
    geom_col(position = "dodge") +
    facet_grid(weather ~ date, 
               labeller = labeller(weather = label_wrap_gen(15))) +
    theme(text = element_text(size = 16), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank(),
          legend.position = "top") +
    scale_x_continuous(expand = expansion(mult = c(.01, .3))) +
    scale_fill_manual(values = c("Минимална температура" = "blue", 
                                 "Максимална температура" = "red")) +
    geom_text(aes(label = paste0(value, " (\u00B0C)")), 
              position = position_dodge(width = 1), hjust = -0.1, size = 5) +
    labs(y = NULL, x = NULL, fill = "Легенда:", 
         caption = "Източник на данните: НИМХ") +
    guides(fill = guide_legend(reverse = TRUE))
}, height = 800, width = 1800, res = 96)
 
  session$onSessionEnded(function() {
    stopApp()
  })
}
shinyApp(ui, server)