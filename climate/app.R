library(shiny)
library(tidyverse)
library(sf)
library(arrow)
library(bslib)

df <- read_parquet("daily.parquet") %>% 
  mutate(month = fct_recode(month, "януари" = "1", "февруари" = "2", "март" = "3",
                            "април" = "4", "май" = "5", "юни" = "6", "юли" = "7",
                            "август" = "8", "септември" = "9", "октомври" = "10",
                            "ноември" = "11", "декември" = "12")) %>% 
  rename(Минимална_температура = temp_min, Максимална_температура = temp_max, 
         Средна_температура = temp_mean, Валеж = prec_sum, Снеговалеж = snow_sum, 
         Скорост_на_вятъра = wind_max, Посока_на_вятъра = wind_dir)
obl_map <- st_read("obl_map.gpkg") %>% 
  mutate(oblast_bg = fct_recode(oblast_bg, "София" = "София – област"))

min_colors <- c("<-20 \u00B0C" = "purple", "-20:-10 \u00B0C" = "blue", ">-10 \u00B0C" = "lightblue")
max_colors <- c(">35 \u00B0C" = "red", "25-35 \u00B0C" = "orange", "<25 \u00B0C" = "green")
#------------------------------------------------------------------------------------------
mail <- tags$a(icon("envelope"), "Email", 
               href = "mailto:nickydyakov@gmail.com", 
               tagret = "_blank")
github <- tags$a(icon("github"), "Github", 
                 href = "https://github.com/NickyDy", 
                 tagret = "_blank")
#-----------------------------------------------
ui <- page_fillable(h3("Климатът на България!"),
                    theme = bslib::bs_theme(bootswatch = "darkly"),
                    navset_pill(
                      nav_panel("Средна температура",
                                layout_columns(
                                selectInput("location_mean", "Станция:", 
                                            choices = unique(df$location)),
                                selectInput("month", "Месец:", 
                                            choices = unique(df$month)), 
                                col_widths = c(2, 1)), 
                                plotOutput("mean")),
                      nav_panel("Минимална температура",
                                layout_columns(
                                selectInput("location_min", "Станция:", 
                                            choices = unique(df$location)),
                                selectInput("year_min", "Година:", 
                                            choices = unique(df$year),
                                            selected = "2023"),
                                selectInput("month_min", "Месец:", 
                                            choices = unique(df$month)), 
                                col_widths = c(2, 1, 1)),
                                plotOutput("min")),
                      nav_panel("Максимална температура",
                                layout_columns(
                                selectInput("location_max", "Станция:", 
                                            choices = unique(df$location)),
                                selectInput("year_max", "Година:", 
                                            choices = unique(df$year),
                                            selected = "2023"),
                                selectInput("month_max", "Месец:", 
                                            choices = unique(df$month)), 
                                col_widths = c(2, 1, 1)),
                                plotOutput("max")),
                      nav_panel("Валежи",
                                layout_columns(
                                selectInput("location_rain", "Станция:", 
                                            choices = unique(df$location)),
                                selectInput("month_rain", "Месец:", 
                                            choices = unique(df$month)), 
                                col_widths = c(2, 1)),
                                plotOutput("rain")),
                      nav_panel("Снежна покривка",
                                layout_columns(
                                selectInput("location_snow", "Станция:", 
                                            choices = unique(df$location)),
                                selectInput("month_snow", "Месец:", 
                                            choices = unique(df$month)), 
                                col_widths = c(2, 1)),
                                plotOutput("snow_depth")),
                      nav_panel("Средногодишни данни",
                                layout_columns(
                                selectInput("location_yearly", "Станция:", 
                                            choices = unique(df$location)), 
                                col_widths = c(2)),
                                plotOutput("yearly_temp", height = 280), 
                                plotOutput("yearly_prec", height = 280),
                                plotOutput("yearly_snow", height = 280)),
                      nav_panel("Топ 10 (min/max)",
                                layout_columns(
                                selectInput("location_top_10", "Станция:", 
                                            choices = unique(df$location)), 
                                col_widths = c(2)),
                                plotOutput("top_10_min", height = 400), 
                                plotOutput("top_10_max", height = 400)),
                      nav_panel("Карта",
                                layout_columns(
                                selectInput("map_year", "Година:", choices = unique(df$year), 
                                            selected = last(df$year)),
                                selectInput("map_month", "Месец:", choices = unique(df$month)),
                                selectInput("map_day", "Ден", choices = unique(df$day)),
                                varSelectInput("map_var", "Променлива:", df %>% select(6:11)), 
                                col_widths = c(1, 1, 1, 2)),
                                plotOutput("map")),
                      nav_panel(tags$img(src = "shiny.png", width = 40),
                                "Други полезни приложения:",
                                tags$a(href = "https://nickydy.shinyapps.io/elections/", br(),
                                       "Избори в България!"), br(),
                                tags$a(href = "https://nickydy.shinyapps.io/demography/",
                                       "Демография на България!"), br(),
                                tags$a(href = "https://nickydy.shinyapps.io/inlation/",
                                       "Inflation in EU!"), br(),
                                tags$a(href = "https://ndapps.shinyapps.io/bgprices/",
                                       "Сравнение на цените в България!"), br(),
                                tags$a(href = "https://ndapps.shinyapps.io/agri/",
                                       "Цени на селскостопанска продукция в ЕС!"), br(),
                                tags$a(href = "https://nickydy.shinyapps.io/eurostat/",
                                       "Евростат за България!"), br(),
                                tags$a(href = "https://ndapps.shinyapps.io/und_water/",
                                       "Чистота на водите в България"), br()),
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
  
  output$min <- renderPlot({
    df %>% 
      filter(location == input$location_min, year == input$year_min, month == input$month_min) %>% 
      mutate(extreme = case_when(Минимална_температура < -20 ~ "<-20 \u00B0C", 
                                 between(Минимална_температура, -20, -10) ~ "-20:-10 \u00B0C",
                                 TRUE ~ ">-10 \u00B0C"),
             extreme = fct_relevel(extreme, ">-10 \u00B0C", "-20:-10 \u00B0C", "<-20 \u00B0C")) %>%
      ggplot(aes(day, Минимална_температура, fill = extreme)) +
      geom_col(show.legend = T) +
      geom_text(aes(label = Минимална_температура), size = 5, vjust = -0.5) +
      scale_fill_manual(values = min_colors) +
      scale_y_continuous(expand = expansion(mult = c(.01, .05))) +
      scale_x_discrete(breaks = c(1:31)) +
      labs(x = "Дни от месеца", y = "Минимална дневна температура (\u00B0C)", fill = "Легенда:",
           caption = "Източник на данните: openmeteo",
           title = paste0("Минимални температури през месец ", input$month_min, " за станция ", 
                          input$location_min, ", ", input$year_min, " година")) +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme(text = element_text(size = 18))
  }, height = 750, width = 1850)
  
  output$mean <- renderPlot({
    df %>% 
      filter(location == input$location_mean, month == input$month) %>% 
      group_by(year) %>% 
      summarise(m = round(mean(Средна_температура, na.rm = T), 1)) %>%
      mutate(col = m < mean(m)) %>%
      ggplot(aes(year, m, fill = col)) +
      geom_col() +
      geom_hline(aes(yintercept = mean(m)), linewidth = 0.5, lty = 2, color = "black") +
      geom_text(aes(label = m), size = 4, vjust = -0.5) +
      scale_y_continuous(expand = expansion(mult = c(.01, .05))) +
      scale_fill_discrete(labels = c("Топла", "Студена")) +
      theme(text = element_text(size = 18), 
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      labs(x = "Години", y = "Средна месечна температура (ºС)", fill = "Легенда:",
           caption = "Източник на данните: openmeteo",
           title = paste0("Средни температури през месец ", input$month, " за станция ", input$location_mean))
  }, height = 750, width = 1850)
  
  output$max <- renderPlot({
    df %>% 
      filter(location == input$location_max, year == input$year_max, month == input$month_max) %>% 
      mutate(extreme = case_when(Максимална_температура > 35 ~ ">35 \u00B0C", 
                                 Максимална_температура >= 25 & Максимална_температура <= 35 ~ "25-35 \u00B0C",
                                 TRUE ~ "<25 \u00B0C"),
             extreme = fct_relevel(extreme, "<25 \u00B0C", "25-35 \u00B0C", ">35 \u00B0C")) %>% 
      ggplot(aes(day, Максимална_температура, fill = extreme)) +
      geom_col(show.legend = T) +
      geom_text(aes(label = round(Максимална_температура, 1)), vjust = -0.5, size = 5) +
      scale_fill_manual(values = max_colors) +
      scale_y_continuous(expand = expansion(mult = c(.01, .05))) +
      scale_x_discrete(breaks = c(1:31)) +
      labs(x = "Дни от месеца", y = "Максимална дневна температура (\u00B0C)", fill = "Легенда:",
           caption = "Източник на данните: openmeteo",
           title = paste0("Максимални температури през месец ", input$month_max, " за станция ", 
                          input$location_max, ", ", input$year_max, " година")) +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme(text = element_text(size = 18))
  }, height = 750, width = 1850)
  
  output$rain <- renderPlot({
    df %>% 
      filter(location == input$location_rain, month == input$month_rain) %>% 
      group_by(year) %>%
      mutate(sum = sum(Валеж, na.rm = T)) %>%
      group_by(year) %>% 
      summarise(p = round(mean(sum, na.rm = T), 0)) %>%
      mutate(col = p > mean(p)) %>%
      ggplot(aes(year, p, fill = col)) +
      geom_col() +
      geom_hline(aes(yintercept = mean(p)), linewidth = 0.5, lty = 2, color = "black") +
      geom_text(aes(label = p), size = 4, vjust = -0.5) +
      scale_y_continuous(expand = expansion(mult = c(.01, .05))) +
      scale_fill_discrete(labels = c("Суха", "Дъждовна")) +
      theme(text = element_text(size = 18), 
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      labs(x = "Години", y = "Месечно количество на валежите (mm)", fill = "Легенда:",
           caption = "Източник на данните: openmeteo",
           title = paste0("Количество на валежите през месец ", 
                          input$month_rain, " за станция ", input$location_rain)) +
      guides(fill = guide_legend(reverse = TRUE))
  }, height = 750, width = 1850)
  
  output$snow_depth <- renderPlot({
    df %>% 
      filter(location == input$location_snow, month == input$month_snow) %>% 
      group_by(year) %>% 
      summarise(s = round(sum(Снеговалеж, na.rm = T), 0)) %>% 
      mutate(col = s > mean(s)) %>% 
      ggplot(aes(year, s, fill = col)) +
      geom_col() +
      geom_hline(aes(yintercept = mean(s)), linewidth = 0.5, lty = 2, color = "black") +
      geom_text(aes(label = s), size = 4, vjust = -0.5) +
      scale_y_continuous(expand = expansion(mult = c(.01, .05))) +
      scale_fill_discrete(labels = c("Безснежна", "Снежна")) +
      theme(text = element_text(size = 18), 
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      labs(x = "Години", y = "Дебелина на снежната покривка (cm)", fill = "Легенда:",
           caption = "Източник на данните: openmeteo",
           title = paste0("Дебелина на снежната покривка през месец ", 
                          input$month_snow, " за станция ", input$location_snow)) +
      guides(fill = guide_legend(reverse = TRUE))
  }, height = 750, width = 1850)
  
  output$yearly_temp <- renderPlot({
    df %>% 
      filter(location == input$location_yearly) %>% 
      group_by(year) %>% 
      summarise(m = round(mean(Средна_температура, na.rm = T), 1)) %>%
      mutate(col = m < mean(m)) %>%
      ggplot(aes(year, m, fill = col)) +
      geom_col() +
      geom_hline(aes(yintercept = mean(m)), linewidth = 0.5, lty = 2, color = "black") +
      geom_text(aes(label = m), size = 3, vjust = -0.5) +
      scale_y_continuous(expand = expansion(mult = c(.01, .07))) +
      scale_fill_discrete(labels = c("Топла", "Студена")) +
      theme(text = element_text(size = 16),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      labs(x = NULL, y = "Средна годишна\nтемпература (ºС)", fill = "Легенда:",
           title = paste0("Средногодишна температура, валежи и снежна покривка за станция ", input$location_yearly))
  }, height = 280, width = 1850)
  
  output$yearly_prec <- renderPlot({
    df %>% 
      filter(location == input$location_yearly) %>% 
      group_by(year) %>%
      mutate(sum = sum(Валеж, na.rm = T)) %>%
      group_by(year) %>% 
      summarise(p = round(mean(sum, na.rm = T), 0)) %>%
      mutate(col = p > mean(p)) %>%
      ggplot(aes(year, p, fill = col)) +
      geom_col() +
      geom_hline(aes(yintercept = mean(p)), linewidth = 0.5, lty = 2, color = "black") +
      geom_text(aes(label = p), size = 3.5, vjust = -0.5) +
      scale_y_continuous(expand = expansion(mult = c(.01, .07))) +
      scale_fill_discrete(labels = c("Суха", "Дъждовна")) +
      theme(text = element_text(size = 16),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      labs(x = NULL, y = "Годишно количество\nна валежите (mm)", fill = "Легенда:") +
      guides(fill = guide_legend(reverse = TRUE))
  }, height = 280, width = 1850)
  
  output$yearly_snow <- renderPlot({
    df %>% 
      filter(location == input$location_yearly) %>% 
      group_by(year) %>% 
      summarise(s = round(sum(Снеговалеж, na.rm = T), 0)) %>% 
      mutate(col = s > mean(s)) %>% 
      ggplot(aes(year, s, fill = col)) +
      geom_col() +
      geom_hline(aes(yintercept = mean(s)), linewidth = 0.5, lty = 2, color = "black") +
      geom_text(aes(label = s), size = 3.5, vjust = -0.5) +
      scale_y_continuous(expand = expansion(mult = c(.01, .07))) +
      scale_fill_discrete(labels = c("Безснежна", "Снежна")) +
      theme(text = element_text(size = 16),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      labs(x = "Години", y = "Дебелина на\nснежната покривка (cm)", fill = "Легенда:",
           caption = "Източник на данните: openmeteo") +
      guides(fill = guide_legend(reverse = TRUE))
  }, height = 280, width = 1850)
  
  output$top_10_min <- renderPlot({
    df %>%
      filter(location == input$location_top_10) %>% 
      slice_min(order_by = Минимална_температура, n = 10) %>% 
      unite("united", year:day, remove = F) %>% 
      ggplot(aes(united, Минимална_температура, fill = month)) +
      geom_col() +
      scale_y_continuous(expand = expansion(mult = c(.07, .01))) +
      geom_text(aes(label = paste0(round(Минимална_температура, 1), " \u00B0C")), size = 5, vjust = -0.5) +
      theme(text = element_text(size = 18)) +
      labs(x = NULL, y = "Минимална температура (\u00B0C)", fill = "Месец:",
           title = paste0("Топ 10 минимални и максимални температури за станция ", input$location_top_10))
  }, height = 380, width = 1850)
  
  output$top_10_max <- renderPlot({
    df %>% 
      filter(location == input$location_top_10) %>% 
      slice_max(order_by = Максимална_температура, n = 10) %>% 
      unite("united", year:day, remove = F) %>% 
      ggplot(aes(united, Максимална_температура, fill = month)) +
      geom_col() +
      scale_y_continuous(expand = expansion(mult = c(.01, .07))) +
      geom_text(aes(label = paste0(round(Максимална_температура, 1), " \u00B0C")), size = 5, vjust = -0.5) +
      theme(text = element_text(size = 18)) +
      labs(x = "Дата", y = "Максимална температура (\u00B0C)", fill = "Месец:",
           caption = "Източник на данните: openmeteo")
  }, height = 380, width = 1850)

climate <- reactive(df %>% filter(year == input$map_year, month == input$map_month, day == input$map_day))

map_climate <- reactive(obl_map %>% inner_join(climate(), by = c("oblast_bg" = "location")))

output$map <- renderPlot({
  map_climate() %>%
    ggplot() +
    geom_sf(aes(fill = !!input$map_var), alpha = 0.4) +
    geom_sf_text(aes(label = paste0(!!input$map_var)), check_overlap = TRUE, size = 4) + 
    scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
    labs(x = NULL, y = NULL, caption = "Източник на данните: openmeteo",
         title = "Данните са от станция в областния град - температурите са измерени в \u00B0C,\nвалежите в mm, снеговалежа в cm, а скоростта на вятъра в km/h.") +
    theme(text = element_text(size = 18), legend.position = "none",
          axis.text = element_blank(), axis.ticks = element_blank())
}, height = 750, width = 1850)

session$onSessionEnded(function() {
  stopApp()
})

}
shinyApp(ui, server)
