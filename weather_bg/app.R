library(tidyverse)
library(rvest)
library(tidytext)
library(shiny)
library(bslib)

temp_nimh_new <- read_html("http://www.weather.bg/index.php?koiFail=tekushti&lng=0") |>
  html_element("table") |> html_table() |>
  select(station = Станция, date = Дата, 
         hour = Час, temp = `Температура[°C]`, 
         weather = Време, wind_speed = `Вятър-скорост[m/s]`, 
         wind_dir = `Вятър-посока`, pressure = `Налягане[hPa]`) |>
  mutate(wind_speed = str_remove(wind_speed, "n.a."), 
         date = dmy(date), 
         wind_speed = parse_number(wind_speed))

snow_nimh_new <- read_html("http://www.weather.bg/index.php?koiFail=tekushti&lng=0") |>
    html_element("td:nth-child(1) table") |> html_table() |> slice(-c(1:2))
colnames(snow_nimh_new) <- c('station_no','station', "station_type", 
                      "prec", "prec_type", "snow_cover")
snow_nimh_new <- snow_nimh_new |> 
  mutate(across(c(prec, snow_cover), parse_number)) |> 
  drop_na(snow_cover)
date_snow <- read_html("http://www.weather.bg/index.php?koiFail=tekushti&lng=0") |>
  html_element("#tSnow h2") |> html_text(trim = T) |> as.data.frame()

rain_nimh_new <- read_html("http://www.weather.bg/index.php?koiFail=tekushti&lng=0") |>
  html_element("center") |> html_table() |> slice(-c(1, 168)) |> 
  select(id = X1, station = X2, rain = X3, mean_rain = X4) |> 
  mutate(rain = str_replace(rain, "n.a.", ""),
         rain = parse_number(rain), date = Sys.Date(), .after = station) |> 
  filter(rain > 0)

bg <- read_html("http://weather.bg/index.php?koiFail=bg&lng=0") |>
  html_element("table") |> html_table() |> pivot_longer(-Град) |> 
  filter(!Град == "") |> mutate(region = "България", .before = Град) |> 
  select(region, station = Град, date = name, weather = value)
  
pl <- read_html("http://weather.bg/index.php?koiFail=bg&lng=0") |>
  html_element("#planini") |> html_table() |> pivot_longer(-Пункт) |> 
  filter(!Пункт == "") |> mutate(region = "Планини", .before = Пункт) |> 
  select(region, station = Пункт, date = name, weather = value)
  
eu <- read_html("http://weather.bg/index.php?koiFail=eubp&lng=0") |>
  html_element("table") |> html_table() |> pivot_longer(-Град) |> 
  filter(!Град == "") |> mutate(region = "Европа", .before = Град) |> 
  select(region, station = Град, date = name, weather = value)
  
forcast <- bind_rows(bg, pl, eu)
temp <- forcast |> filter(str_detect(weather, "^[:punct:]|\\d"), 
                           str_detect(weather, "/")) |>
  separate_wider_delim(weather, delim = "/", 
                       names = c("Min", 
                                 "Max")) |> 
  mutate(across(4:5, parse_number))
weather <- forcast |> filter(str_detect(weather, "^[:alpha:]"))
forcast_df <- inner_join(temp, weather, by = join_by(region, station, date)) |> 
  pivot_longer(Min:Max) |>
  mutate(name = fct_rev(name)) |> 
  arrange(station) |> 
  filter(!weather == "n.a.")

rivers <- read_html("http://meteo.bg/meteo7/bg/rekiTablitsa") |> 
  html_element("center") |> html_table()
colnames(rivers) <- c('station_no','river','station', "q_min", 
                      "q_mean", "q_max", "depth", "ottok", "change_depth")
rivers <- rivers |> 
  mutate(date = Sys.Date(), .before = everything(), 
         river = fct_recode(river, "Тунджа" = "Tунджа")) |> 
  filter(!station_no == "№ ХМС") |>
  mutate(across(5:10, ~ str_remove(., "n.a."))) |> 
  mutate(across(5:10, ~ str_replace_all(., " ", ""))) |> 
  mutate(across(5:10, ~ str_replace_all(., ",", "."))) |> 
  mutate(across(5:10, parse_number)) |> 
  mutate(across(5:10, ~ round(., 1))) |> 
  drop_na(river) |> arrange(river)

date_rivers <- read_html("http://meteo.bg/meteo7/bg/rekiTablitsa") |> 
  html_elements("h2") |> html_text(trim = T) |> as.data.frame()
#----------------------------------------------------------------
mail <- tags$a(icon("envelope"), "Email", 
               href = "mailto:nickydyakov@gmail.com", 
               tagret = "_blank")
github <- tags$a(icon("github"), "Github", 
                 href = "https://github.com/NickyDy", 
                 tagret = "_blank")
#--------------------------------------------
ui <- page_fillable(#h3("Времето в България!"),
                    theme = bslib::bs_theme(bootswatch = "darkly"),
                    navset_pill(
                      nav_panel(title = "Температура",
                                selectInput("rows", "Брой колонки:",
                                            choices = c(2, 1)),
                                plotOutput("temp")),
                      nav_panel(title = "Вятър",
                                plotOutput("wind")),
                      nav_panel(title = "Валеж",
                                plotOutput("rain")),
                      nav_panel(title = "Снежна покривка",
                                plotOutput("snow_cover")),
                      nav_panel(title = "Прогноза", layout_columns(
                                selectInput("region", "Регион:",
                                            choices = c("България", "Планини", "Европа")),
                                selectInput("sett", "Населено място/Станция:",
                                            choices = NULL), col_widths = c(2, 2)),
                                plotOutput("forcast")),
                      nav_panel(title = "Речен отток",
                                selectInput("river", "Река:",
                                            choices = unique(rivers$river)),
                                plotOutput("rivers")),
                      nav_panel(title = "Речно ниво",
                                selectInput("river_depth", "Река:",
                                            choices = unique(rivers$river)),
                                plotOutput("depths")),
                      nav_panel(tags$img(src = "shiny.png", width = 40),
                                "Други полезни приложения:",
                                tags$a(href = "https://nickydy.shinyapps.io/elections/", br(),
                                       "Избори в България!"), br(),
                                tags$a(href = "https://nickydy.shinyapps.io/climate/",
                                       "Климатът на България!"), br(),
                                tags$a(href = "https://nickydy.shinyapps.io/inlation/",
                                       "Inflation in EU!"), br(),
                                # tags$a(href = "https://ndapps.shinyapps.io/bgprices/",
                                #        "Сравнение на цените в България!"), br(),
                                tags$a(href = "https://ndapps.shinyapps.io/agri/",
                                       "Цени на селскостопанска продукция в ЕС!"), br(),
                                tags$a(href = "https://nickydy.shinyapps.io/eurostat/",
                                       "Евростат за България!"), br(),
                                tags$a(href = "https://nickydy.shinyapps.io/demography/",
                                       "Демография на България!"), br(),
                                tags$a(href = "https://ndapps.shinyapps.io/und_water/",
                                       "Чистота на водите в България!"), br()),
               #        nav_panel(tags$img(src = "kofi.png", width = 40),
               #                  "Ако Ви харесва приложението,
               # можете да ме подкрепите като направите дарение в евро към
               # следната сметка:",
               #                  br(),
               #                  br(),
               #                  "Име: Nikolay Dyakov",
               #                  br(),
               #                  "IBAN: BE89 9670 3038 2685",
               #                  br(),
               #                  "BIC: TRWIBEB1XXX",
               #                  br(),
               #                  "Адрес: Rue de Trone 100, 3rd floor,",
               #                  br(),
               #                  "Brussels,",
               #                  br(),
               #                  "1050,",
               #                  br(),
               #                  "Belgium"),
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
  temp_nimh_new |> 
    mutate(station = reorder_within(station, temp, weather)) |> 
    ggplot(aes(temp, station, fill = temp)) +
    geom_col(show.legend = F) +
    geom_text(aes(label = temp), 
              position = position_dodge(width = 1), hjust = -0.1, size = 3.5) +
    scale_x_continuous(expand = expansion(mult = c(.01, .3))) +
    scale_y_reordered() +
    theme(text = element_text(size = 12), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
    labs(y = NULL, x = "Градуси (\u00B0C)", 
         title = paste0("Дата и час на измерване: ", 
                        unique(temp_nimh_new$date), ", ", 
                        unique(temp_nimh_new$hour), ":00 ч."),
         caption = "Източник на данните: НИМХ") +
    facet_wrap(vars(weather), scales = "free_y", nrow = as.numeric(input$rows),
               labeller = labeller(weather = label_wrap_gen(20)))
  }, height = 800, width = 1800, res = 96)
  #-----------------------------------------
output$wind <- renderPlot({
  temp_nimh_new |> 
    mutate(station = reorder_within(station, wind_speed, weather),
           wind_dir = fct_relevel(wind_dir, "N", "NE", "E", "SE", "S", 
                                            "SW", "W", "NW", "тихо")) |> 
    ggplot(aes(wind_speed, station, fill = wind_speed)) +
    geom_col(show.legend = F) +
    geom_text(aes(label = wind_speed), 
              position = position_dodge(width = 1), hjust = -0.1, size = 3.5) +
    scale_y_reordered() +
    scale_x_continuous(expand = expansion(mult = c(.01, .5))) +
    theme(text = element_text(size = 12), 
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
  rain_nimh_new |> drop_na(rain) |> 
    mutate(code = case_when(
      str_detect(id, "^1") ~ "1",
      str_detect(id, "^2") ~ "2",
      str_detect(id, "^3") ~ "3",
      str_detect(id, "^4") ~ "4",
      str_detect(id, "^5") ~ "5",
      str_detect(id, "^6") ~ "6",
      str_detect(id, "^7") ~ "7",
    )) |> 
    mutate(station = reorder_within(station, rain, code)) |> 
    ggplot(aes(rain, station, fill = rain)) +
    geom_col(show.legend = F) +
    geom_text(aes(label = rain), 
              position = position_dodge(width = 1), hjust = -0.1, size = 3.5) +
    scale_y_reordered() +
    scale_x_continuous(expand = expansion(mult = c(.01, .5))) +
    theme(text = element_text(size = 12), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) +
    scale_fill_gradient(low = "white", high = "blue") +
    labs(y = NULL, x = "Количество на валежа (mm)", 
         caption = "Източник на данните: НИМХ",
         title = paste0("Дата: ", unique(rain_nimh_new$date),
                        " (от 7:30 ч. на предния ден до 7:30 ч. на отбелязаната дата)")) +
    facet_wrap(vars(code), scales = "free_y", nrow = 1)
}, height = 800, width = 1800, res = 96)
#---------------------------------------
output$snow_cover <- renderPlot({
  snow_nimh_new |> 
    mutate(station = reorder_within(station, snow_cover, station_type)) |> 
    ggplot(aes(snow_cover, station, fill = snow_cover)) +
    geom_col(show.legend = F) +
    geom_text(aes(label = snow_cover), 
              position = position_dodge(width = 1), hjust = -0.1, size = 3.5) +
    scale_y_reordered() +
    scale_x_continuous(expand = expansion(mult = c(.01, .1))) +
    theme(text = element_text(size = 12), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) +
    scale_fill_gradient(low = "white", high = "#0096FF") +
    labs(y = NULL, x = "Височина на снежната покривка (cm)", 
         title = paste(date_snow, "г."),
         caption = "Източник на данните: НИМХ") +
    facet_wrap(vars(station_type), scales = "free_y")
}, height = 800, width = 1800, res = 96)
#------------------------
forcast_data <- reactive({
  filter(forcast_df, region == input$region)
})
observeEvent(forcast_data(), {
  freezeReactiveValue(input, "sett")
  choices <- unique(forcast_data()$station)
  updateSelectInput(inputId = "sett", choices = choices)
})

output$forcast <- renderPlot({
  forcast_data() |> 
    mutate(date_weather = paste0(date, "\n(", weather, ")")) |> 
    filter(station == input$sett) |> 
    ggplot(aes(name, value, fill = value)) +
    geom_col(show.legend = F, position = "dodge") +
    facet_wrap(vars(date_weather), nrow = 1) +
    theme(text = element_text(size = 18),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.text = element_text(size = 12)) +
    scale_y_continuous(expand = expansion(mult = c(.01, .3))) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
    geom_text(aes(label = paste0(value, " (\u00B0C)")), 
              position = position_dodge(width = 1), vjust = -0.3, size = 5) +
    labs(y = NULL, x = NULL, caption = "Източник на данните: НИМХ")
}, height = 700, width = 1800, res = 96)

output$rivers <- renderPlot({
  rivers |> 
    filter(river == input$river) |> 
    ggplot(aes(station, ottok)) +
    geom_col(fill = "blue") +
    geom_text(aes(label = ottok), 
              position = position_dodge(width = 1), vjust = -1, size = 6, color = "blue") +
    geom_hline(aes(yintercept = q_min), linewidth = 0.7, lty = 2, color = "red") +
    geom_hline(aes(yintercept = q_mean), linewidth = 0.7, lty = 2, color = "darkgreen") +
    geom_label(aes(label = "минимален отток", x = 1.4, y = q_min), size = 4, color = "red", fontface = "bold") +
    geom_label(aes(label = "среден отток", x = 1.4, y = q_mean), size = 4, color = "darkgreen", fontface = "bold") +
    scale_y_continuous(expand = expansion(mult = c(.01, .1))) +
    theme(text = element_text(size = 16), axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(title = paste("Дата: ", date_rivers$.[3]), x = NULL, 
         y = expression(paste("Речен отток ", "(", m^3, "/s)")),
         caption = "Източник на данните: НИМХ") +
    facet_wrap(vars(station), scales = "free_x")
}, height = 700, width = 1800, res = 96)

output$depths <- renderPlot({
  rivers |> 
    filter(river == input$river_depth) |> 
    mutate(old_depth = depth - change_depth) |> 
    pivot_longer(c(depth, old_depth)) |> 
    ggplot(aes(name, value, fill = name)) +
    geom_col(show.legend = F) +
    geom_text(aes(label = value),
              position = position_dodge(width = 1), vjust = -0.5, size = 6) +
    scale_y_continuous(expand = expansion(mult = c(.01, .3))) +
    scale_x_discrete(labels = c("Нова дълбочина", "Стара дълбочина")) +
    scale_fill_manual(values = c("blue", "gray")) +
    theme(text = element_text(size = 16)) +
    labs(title = paste("Дата: ", date_rivers$.[3]), x = NULL, 
         y = "Дълбочина (cm)", caption = "Източник на данните: НИМХ") +
    facet_wrap(vars(station), scales = "free_x")
}, height = 800, width = 1800, res = 96)
 
  session$onSessionEnded(function() {
    stopApp()
  })
}
shinyApp(ui, server)