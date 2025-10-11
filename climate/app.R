library(shiny)
library(shinyWidgets)
library(tidyverse)
library(rvest)
library(bslib)

rain <- read_rds("rain.rds")
temp <- read_rds("temp.rds")

rain_new <- read_html("https://www.stringmeteo.com/synop/prec_month.php") %>%
  html_element("table") %>% 
  html_table() %>%
  filter(str_detect(X1, "^[:punct:]\\d{1,2}[:punct:]")) %>% 
  select(2:17, 19:34) %>%
  rename(station = X2) %>% 
  rename_with(~ as.character(c(1:31)), starts_with("X")) %>%
  mutate(
    station = str_remove_all(station, "\\("), 
    station = str_remove_all(station, "\\)"),
    status = case_when(
      station %in% c("Видин", "Ловеч", "Разград", "Варна", "вр. Мургаш",
                     "София", "вр. Мусала", "Пазарджик", "Сливен", "Бургас",
                     "Сандански", "Кърджали") ~ "official", 
      station %in% c("с. Гложене", "Варна-Акчелар", "Варна-Боровец", "Климентово",
                     "Обзор", "Дупница", "Орландовци", "Бояна", "Княжево", "Панагюрище",
                     "Ямбол", "Петрич", "Стралджа", "Шумен", "с.Рупите") ~ "unofficial",
      str_detect(station, "Ключ") ~ "unofficial",
      str_detect(station, "Рупите") ~ "unofficial",
      str_detect(station, "Илинденци") ~ "unofficial",
      str_detect(station, "Конгур") ~ "unofficial"), .after = station,
    year = 2025, month = 10,
    elev = case_when(
      station == "Видин" ~ 31, station == "Ловеч" ~ 220, str_detect(station, "Конгур") ~ 1284,
      station == "Разград" ~ 345, station == "Варна" ~ 41, station == "Варна-Акчелар" ~ 180,
      station == "Варна-Боровец" ~ 193, station == "Климентово" ~ 281, station == "Обзор" ~ 20,
      station == "вр. Мургаш" ~ 1687, station == "Дупница" ~ 551, station == "София" ~ 586,
      station == "Орландовци" ~ 527, station == "Бояна" ~ 744, station == "Княжево" ~ 700,
      station == "вр. Мусала" ~ 2925, station == "Панагюрище" ~ 519, station == "Пазарджик" ~ 213,
      station == "Сливен" ~ 257, station == "Ямбол" ~ 147, station == "Бургас" ~ 16,
      station == "Петрич" ~ 200, station == "Сандански" ~ 206, station == "Стралджа" ~ 139,
      station == "Кърджали" ~ 330, station == "Шумен" ~ 218, station == "с. Гложене" ~ 64)) %>%
  mutate(decade = case_when(
    year %in% c("2004", "2005", "2006", "2007", "2008", "2009") ~ "00s",
    year %in% c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019") ~ "10s",
    year %in% c("2020", "2021", "2022", "2023", "2024", "2025") ~ "20s")) %>%
  relocate(decade, .after = status) %>% relocate(elev, .after = status) %>% 
  pivot_longer(7:37, names_to = "day", values_to = "rain") %>%
  mutate(rain = str_remove(rain, "---")) %>% 
  mutate(across(c(2, 4:7), as.factor)) %>%
  mutate(rain = parse_number(rain))

temp_new <- read_html("https://www.stringmeteo.com/synop/temp_month.php") %>%
  html_element("table") %>% 
  html_table() %>%
  filter(str_detect(X1, "^[:punct:]\\d{1,2}[:punct:]")) %>% 
  select(2:17, 19:34) %>%
  rename(station = X2) %>% 
  rename_with(~ as.character(c(1:31)), starts_with("X")) %>%
  mutate(
    station = str_remove_all(station, "\\("), 
    station = str_remove_all(station, "\\)"),
    station = str_squish(station),
    status = case_when(
      station %in% c("Видин", "Ловеч", "Разград", "Варна", "вр. Мургаш", "София",
                     "вр. Мусала", "Пазарджик", "Сливен", "Бургас", "Сандански",
                     "Кърджали", "Димитровгр. С.") ~ "official", 
      station %in% c("Гложене", "Варна-Акчелар", "Варна-Боровец", "Климентово",
                     "Обзор", "Дупница", "Орландовци", "Бояна", "Княжево",
                     "Панагюрище", "Ямбол", "Петрич", "Турну Мъгуреле Р.",
                     "Кълъраш Р.", "Одрин Т.", "Рилци", "Добри дол") ~ "unofficial"), .after = station,
    year = 2025, month = 10,
    elev = case_when(
      station == "Видин" ~ 31, station == "Гложене" ~ 64, station == "Ловеч" ~ 220, station == "Разград" ~ 345,
      station == "Варна" ~ 41, station == "Варна-Акчелар" ~ 180, station == "Варна-Боровец" ~ 193,
      station == "Климентово" ~ 281, station == "Обзор" ~ 20, station == "вр. Мургаш" ~ 1687,
      station == "Дупница" ~ 551, station == "София" ~ 586, station == "Орландовци" ~ 527,
      station == "Бояна" ~ 744, station == "Княжево" ~ 700, station == "вр. Мусала" ~ 2925,
      station == "Панагюрище" ~ 519, station == "Пазарджик" ~ 213, station == "Сливен" ~ 257,
      station == "Ямбол" ~ 147, station == "Бургас" ~ 16, station == "Петрич" ~ 200,
      station == "Сандански" ~ 206, station == "Кърджали" ~ 330, station == "Добри дол" ~ 118,
      station == "Турну Мъгуреле Р." ~ 31, station == "Димитровгр. С." ~ 125,
      station == "Кълъраш Р." ~ 12, station == "Одрин Т." ~ 42, station == "Рилци" ~ 378)) %>%
  mutate(decade = case_when(
    year %in% c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009") ~ "00s",
    year %in% c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019") ~ "10s",
    year %in% c("2020", "2021", "2022", "2023", "2024", "2025") ~ "20s")) %>%
  relocate(decade, .after = status) %>% relocate(elev, .after = status) %>% 
  pivot_longer(7:37, names_to = "day", values_to = "temp") %>% 
  mutate(temp = str_remove(temp, "---")) %>% 
  mutate(across(c(2, 4:7), as.factor)) %>%
  mutate(temp = parse_number(temp))

rain <- bind_rows(rain, rain_new) %>% 
  mutate(month = factor(month, levels = c(1:12)))
temp <- bind_rows(temp, temp_new)

colors_temp <- c("1" = "red", "2" = "orange" , "3" = "green", "4" = "#0096FF", "5" = "blue")
labels_temp <- c("1" = "Много топло", "2" = "Топло" , "3" = "Умерено", "4" = "Хладно", "5" = "Много хладно")
colors_rain <- c("1" = "blue" , "2" = "#0096FF" , "3" = "green", "4" = "orange", "5" = "red")
labels_rain <- c("1" = "Много дъждовно", "2" = "Дъждовно", "3" = "Умерено", "4" = "Сухо", "5" = "Много сухо")
#------------------------------------------------------------------------------------------
mail <- tags$a(icon("envelope"), "Email", 
               href = "mailto:nickydyakov@gmail.com", 
               tagret = "_blank")
github <- tags$a(icon("github"), "Github", 
                 href = "https://github.com/NickyDy", 
                 tagret = "_blank")
#-----------------------------------------------
ui <- page_fillable(#h3("Климатът на България!"),
                    theme = bslib::bs_theme(bootswatch = "darkly"),
                    navset_pill(
                      nav_panel("Температура",
                                sliderInput("elev_temp", "Надморска височина:",
                                            min = min(temp$elev, na.rm = T), 
                                            max = max(temp$elev, na.rm = T),
                                            value = 1200, step = 100, sep = " "),
                                plotOutput("temp")),
                      nav_panel("Валеж",
                                sliderInput("elev_rain", "Надморска височина:",
                                            min = min(rain$elev, na.rm = T), 
                                            max = max(rain$elev, na.rm = T),
                                            value = 1200, step = 100, sep = " "),
                                plotOutput("rain")),
                      nav_panel("Температура (по дни)", layout_columns(
                                sliderInput("elev_temp_day", "Надморска височина:",
                                            min = min(temp$elev, na.rm = T), 
                                            max = max(temp$elev, na.rm = T),
                                            value = 1200, step = 100, sep = " "),
                                selectInput("month_temp_day", "Месец: ",
                                            choices = 1:12, selected = month(Sys.Date())),
                                col_widths = c(2, 1)),
                                plotOutput("temp_day")),
                      nav_panel("Валеж (по дни)", layout_columns(
                                sliderInput("elev_rain_day", "Надморска височина:",
                                            min = min(rain$elev, na.rm = T), 
                                            max = max(rain$elev, na.rm = T),
                                            value = 1200, step = 100, sep = " "),
                                selectInput("month_rain_day", "Месец: ",
                                            choices = 1:12, selected = month(Sys.Date())),
                                col_widths = c(2, 1)),
                                plotOutput("rain_day")),
                      nav_panel(tags$img(src = "shiny.png", width = 40),
                                "Други полезни приложения:",
                                tags$a(href = "https://nickydy.shinyapps.io/elections/", br(),
                                       "Избори в България!"), br(),
                                tags$a(href = "https://nickydy.shinyapps.io/demography/",
                                       "Демография на България!"), br(),
                                tags$a(href = "https://nickydy.shinyapps.io/inlation/",
                                       "Inflation in EU!"), br(),
                                # tags$a(href = "https://ndapps.shinyapps.io/bgprices/",
                                #        "Сравнение на цените в България!"), br(),
                                tags$a(href = "https://ndapps.shinyapps.io/agri/",
                                       "Цени на селскостопанска продукция в ЕС!"), br(),
                                tags$a(href = "https://nickydy.shinyapps.io/eurostat/",
                                       "Евростат за България!"), br(),
                                tags$a(href = "https://ndapps.shinyapps.io/und_water/",
                                       "Чистота на водите в България"), br()),
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
  
  t_year <- reactive({
    
  temp %>% 
      filter(month %in% c(1:12), elev <= input$elev_temp, status == "official") %>% 
      summarise(m = round(mean(temp, na.rm = T), 1), .by = c(year, month)) %>%
      summarise(mean_year = mean(m), .by = year) %>% 
      mutate(tot_mean = round(mean(mean_year), 1))
  })
  
 d_year <- reactive({
    
  rain %>%
      filter(month %in% c(1:12), elev <= input$elev_rain, status == "official") %>% 
      summarise(s = round(sum(rain, na.rm = T), 1), .by = c(station, year, month)) %>%
      summarise(sm = round(mean(s, na.rm = T), 1), .by = c(year, month)) %>%
      summarise(s_year = sum(sm), .by = year) %>% 
      mutate(tot_mean = round(mean(s_year), 0))
  })
  
 output$temp <- renderPlot({
    
    temp %>%
      filter(month %in% c(1:12), elev <= input$elev_temp, status == "official") %>% 
      summarise(m = round(mean(temp, na.rm = T), 1), .by = c(year, month)) %>%
      group_by(month) %>%
      mutate(mm = round(mean(m, na.rm = T), 1), 
             iqr = IQR(m), col = case_when(
               m < mm - iqr * 1.2 ~ "5",
               m > mm + iqr * 1.2 ~ "1",
               m < mm - iqr * 0.5 ~ "4",
               m > mm + iqr * 0.5 ~ "2",
               m <= mm + iqr * 0.5 ~ "3")) %>% 
      ungroup() %>%
      group_by(year) %>% 
      mutate(mean_year = mean(m, na.rm = T), 
             label_year = paste0(year, ": ", round(mean_year, 1), " (\u00B0C)"),
             total_mean = mean(mean_year, na.rm = T)) %>% 
      ungroup() %>% 
      ggplot(aes(month, m)) +
      geom_col(aes(fill = col), show.legend = T) +
      geom_text(aes(label = round(m, 1)), size = 4, vjust = -0.2) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
      scale_fill_manual(values = colors_temp, labels = labels_temp) +
      labs(x = "Месеци", y = "Средна денонощна температура (\u00B0C)", fill = "Легенда:",
           caption = "Източник на данните: www.stringmeteo.com",
           title = paste0("Средно за периода (", first(t_year()$year), "-", last(t_year()$year), 
                          "г.): ", t_year()$tot_mean, " (\u00B0C)")) +
      theme(text = element_text(size = 20), legend.position = "top",
            plot.title = element_text(color = "red", face = "bold"),
            legend.justification = c(1, 0)) +
      facet_wrap(vars(label_year), ncol = 5)
    
  }, height = 800, width = 1850)
  
  output$rain <- renderPlot({
    
    rain %>%
      filter(month %in% c(1:12), elev <= input$elev_rain, status == "official") %>%
      summarise(s = round(sum(rain, na.rm = T), 1), .by = c(station, year, month)) %>%
      summarise(sm = round(mean(s, na.rm = T), 1), .by = c(year, month)) %>%
      group_by(month) %>% 
      mutate(ss = round(mean(sm, na.rm = T), 1), 
             iqr = IQR(sm), col = case_when(
               sm < ss - iqr ~ "5",
               sm > ss + iqr ~ "1",
               sm < ss - iqr * 0.5  ~ "4",
               sm > ss + iqr * 0.5  ~ "2",
               sm <= ss + iqr * 0.5 ~ "3")) %>% 
      ungroup() %>%
      group_by(year) %>% 
      mutate(mean_year = sum(sm, na.rm = T), 
             mean_year = paste0(year, ": ", round(mean_year, 0), " (mm)")) %>% 
      ungroup() %>% 
      ggplot(aes(month, sm)) +
      geom_col(aes(fill = col), show.legend = T) +
      geom_text(aes(label = round(sm, 0)), size = 5, vjust = -0.2) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
      scale_fill_manual(values = colors_rain, labels = labels_rain) +
      labs(x = "Месеци", y = "Средно месечно количество на валежите (mm)", fill = "Легенда:",
           caption = "Източник на данните: www.stringmeteo.com",
           title = paste0("Средно за периода (", first(d_year()$year), "-", last(d_year()$year), 
                          "г.): ", d_year()$tot_mean, " (mm)")) +
      theme(text = element_text(size = 20), legend.position = "top",
            plot.title = element_text(color = "blue", face = "bold"),
            legend.justification = c(1, 0)) +
      facet_wrap(vars(mean_year), ncol = 5)
    
  }, height = 800, width = 1850)
  
  month_mean_temp <- reactive({
  temp %>% 
    filter(month %in% c(input$month_temp_day), elev <= input$elev_temp_day, status == "official") %>%
    summarise(m = round(mean(temp, na.rm = T), 1), .by = c(year)) %>%
    summarise(month_m = mean(m, na.rm = T))
})
  
  output$temp_day <- renderPlot({
    
  temp %>% 
      filter(month %in% c(input$month_temp_day), elev <= input$elev_temp_day, status == "official") %>%
      summarise(mean_temp = mean(temp, na.rm = T), .by = c(year, month, day)) %>%
      group_by(year) %>% 
      mutate(temp_year = mean(mean_temp, na.rm = T), 
             label_year = paste0(year, ": ", round(temp_year, 1), " (\u00B0C)")) %>% 
      ungroup() %>%
      mutate(col = case_when(
        mean_temp <= 0 ~ "1",
        mean_temp <= 5 ~ "2", 
        mean_temp >= 25 ~ "4",
        .default = "3")) %>%
      ggplot(aes(day, mean_temp)) +
      geom_col(aes(fill = col), show.legend = T) +
      theme(text = element_text(size = 20), legend.position = "top",
            plot.title = element_text(color = "red", face = "bold")) +
      scale_y_continuous(n.breaks = 3) +
      scale_x_discrete(breaks = c(1, 5, 10, 15, 20, 25, 30)) +
      scale_fill_manual(values = c("1" = "blue", "2" = "green", "3" = "orange", "4" = "red"),
                        labels = c("1" = "< 0 \u00B0C", "2" = "0-5 \u00B0C", "3" = "5-25 \u00B0C", "4" = "25-30 \u00B0C"),
                        name = "Легенда: ") +
      labs(x = "Ден", y = "Средна денонощна температура (\u00B0C)",
           title = paste0("Средно за месеца за целия период: ", round(month_mean_temp()$month_m, 1), " (\u00B0C)")) +
      facet_wrap(vars(label_year), ncol = 4)
    
  }, height = 800, width = 1850)
  
  month_mean_rain <- reactive({
  rain %>% 
      filter(month %in% c(input$month_rain_day), elev <= input$elev_rain_day, status == "official") %>%
    summarise(s = round(sum(rain, na.rm = T), 1), .by = c(station, year, month)) %>%
    summarise(s = mean(s, na.rm = T), .by = c(year)) %>%
    summarise(month_m = mean(s))
  })
  
  output$rain_day <- renderPlot({
  rain %>% 
      filter(month %in% c(input$month_rain_day), elev <= input$elev_rain_day, status == "official") %>%
    summarise(sum_rain = round(sum(rain, na.rm = T), 1), .by = c(station, year, month, day)) %>%
    summarise(mean_year = mean(sum_rain, na.rm = T), .by = c(year, month, day)) %>% 
    group_by(year) %>% 
    mutate(rain_year = sum(mean_year, na.rm = T), 
           label_year = paste0(year, ": ", round(rain_year, 0), " (mm)")) %>% 
    ungroup() %>% 
    mutate(col = case_when(
      mean_year <= 5 ~ "1",
      mean_year <= 15 ~ "2", 
      mean_year >= 30 ~ "4",
      .default = "3")) %>%
    ggplot(aes(day, mean_year)) +
    geom_col(aes(fill = col), show.legend = T) +
    theme(text = element_text(size = 20), legend.position = "top",
          plot.title = element_text(color = "blue", face = "bold")) +
    scale_y_continuous(n.breaks = 5) +
    scale_x_discrete(breaks = c(1, 5, 10, 15, 20, 25, 30)) +
    scale_fill_manual(values = c("4" = "blue", "3" = "#0096FF", "2" = "green", "1" = "orange"),
                      labels = c("1" = "< 5 (mm)", "2" = "5-15 (mm)", "3" = "15-30 (mm)", "4" = "> 30 (mm)"),
                      name = "Легенда: ") +
    labs(x = "Ден", y = "Средно количетсво на валежите (mm)", 
         title = paste0("Средно за месеца за целия период: ", round(month_mean_rain()$month_m, 0), " (mm)")) +
    facet_wrap(vars(label_year), ncol = 4)
  }, height = 800, width = 1850)

session$onSessionEnded(function() {
  stopApp()
})

}
shinyApp(ui, server)
