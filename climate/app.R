library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(sf)

df <- read_rds("daily.rds") %>% 
  mutate(month = fct_recode(month, "януари" = "1", "февруари" = "2", "март" = "3",
                            "април" = "4", "май" = "5", "юни" = "6", "юли" = "7",
                            "август" = "8", "септември" = "9", "октомври" = "10",
                            "ноември" = "11", "декември" = "12")) %>% 
  rename(Минимална_температура = temp_min, Максимална_температура = temp_max, 
         Средна_температура = temp_mean, Валеж = prec_sum, Снеговалеж = snow_sum, 
         Скорост_на_вятъра = wind_max)
obl_map <- st_read("obl_map.gpkg") %>% 
  mutate(oblast_bg = fct_recode(oblast_bg, "София" = "София – област"))

min_colors <- c("<-20 \u00B0C" = "purple", "-20:-10 \u00B0C" = "blue", ">-10 \u00B0C" = "lightblue")
max_colors <- c(">35 \u00B0C" = "red", "25-35 \u00B0C" = "orange", "<25 \u00B0C" = "green")

ml <- tags$a(href = "mailto:nickydyakov@gmail.com", icon("envelope"))
gh <- tags$a(href = "https://github.com/NickyDy", icon("github"))

ui <- fluidPage(#theme = shinytheme("cyborg"),
                fluidRow(column(4, titlePanel("Климатът в България")), h5(ml, gh)),
                tabsetPanel(
                  tabPanel("Средна температура", 
                           fluidRow(column(2, selectInput("location_mean", "Станция", choices = unique(df$location))),
                                    column(2, selectInput("month", "Месец", choices = unique(df$month))), 
                           fluidRow(column(8, plotOutput("mean"))))),
                  tabPanel("Минимална температура", 
                           fluidRow(column(2, selectInput("location_min", "Станция", choices = unique(df$location))),
                                    column(2, selectInput("year_min", "Година", choices = unique(df$year))),
                                    column(2, selectInput("month_min", "Месец", choices = unique(df$month))),
                           fluidRow(column(8, plotOutput("min"))))), 
                  tabPanel("Максимална температура", 
                           fluidRow(column(2, selectInput("location_max", "Станция", choices = unique(df$location))),
                                    column(2, selectInput("year_max", "Година", choices = unique(df$year))),
                                    column(2, selectInput("month_max", "Месец", choices = unique(df$month))),
                           fluidRow(column(8, plotOutput("max"))))), 
                  tabPanel("Валежи", 
                           fluidRow(column(2, selectInput("location_rain", "Станция", choices = unique(df$location))),
                                    column(2, selectInput("month_rain", "Месец", choices = unique(df$month))),
                           fluidRow(column(8, plotOutput("rain"))))),
                  tabPanel("Снежна покривка", 
                           fluidRow(column(2, selectInput("location_snow", "Станция", choices = unique(df$location))),
                                    column(2, selectInput("month_snow", "Месец", choices = unique(df$month))),
                           fluidRow(column(8, plotOutput("snow_depth"))))), 
                  tabPanel("Средногодишни данни", 
                           fluidRow(column(2, selectInput("location_yearly", "Станция", choices = unique(df$location)))),
                           fluidRow(column(8, plotOutput("yearly_temp", height = 280), 
                                              plotOutput("yearly_prec", height = 280),
                                              plotOutput("yearly_snow", height = 280)))),
                  tabPanel("Топ 10 (min/max)", 
                           fluidRow(column(2, selectInput("location_top_10", "Станция", choices = unique(df$location)))),
                           fluidRow(column(8, plotOutput("top_10_min", height = 400), 
                                              plotOutput("top_10_max", height = 400)))),
                  tabPanel("Таблица", 
                           fluidRow(column(12, DTOutput("table")))),
                  tabPanel("Карта", 
                           fluidRow(column(2, selectInput("map_year", "Година", choices = unique(df$year), selected = last(df$year))),
                                    column(2, selectInput("map_month", "Месец", choices = unique(df$month))),
                                    column(2, selectInput("map_day", "Ден", choices = unique(df$day))),
                                    column(2, varSelectInput("map_var", "Променлива", df %>% select(6:11))),
                                    fluidRow(column(8, plotOutput("map")))))))

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
           title = paste0("Минимални температури за станция ", input$location_min, " през месец ",
                          input$month_min, ", ", input$year_min, " година")) +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme(text = element_text(size = 16))
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
      geom_text(aes(label = m), size = 3.5, vjust = -0.5) +
      scale_y_continuous(expand = expansion(mult = c(.01, .05))) +
      scale_fill_discrete(labels = c("Топла", "Студена")) +
      theme(text = element_text(size = 16), 
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      labs(x = "Години", y = "Средна месечна температура (ºС)", fill = "Легенда:",
           caption = "Източник на данните: openmeteo",
           title = paste0("Средни температури за станция ", input$location_mean, " през месец ",
                          input$month))
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
           title = paste0("Максимални температури за станция ", input$location_max, " през месец ",
                          input$month_max, ", ", input$year_max, " година")) +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme(text = element_text(size = 16))
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
      theme(text = element_text(size = 16), 
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      labs(x = "Години", y = "Месечно количество на валежите (mm)", fill = "Легенда:",
           caption = "Източник на данните: openmeteo",
           title = paste0("Количество на валежите за станция ", input$location_rain, " през месец ",
                          input$month_rain)) +
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
      theme(text = element_text(size = 16), 
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      labs(x = "Години", y = "Дебелина на снежната покривка (cm)", fill = "Легенда:",
           caption = "Източник на данните: openmeteo",
           title = paste0("Дебелина на снежната покривка за станция ", input$location_snow, " през месец ",
                          input$month_snow)) +
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
      theme(text = element_text(size = 16)) +
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
      theme(text = element_text(size = 16)) +
      labs(x = "Дата", y = "Максимална температура (\u00B0C)", fill = "Месец:",
           caption = "Източник на данните: openmeteo")
  }, height = 380, width = 1850)

output$table <- renderDT(
  df %>% 
    datatable(filter = 'top', 
              extensions = 'Buttons', 
              options = list(dom = 'Bfrtip', 
                             buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                             pageLength = 20)) %>% 
    formatStyle("Минимална_температура", color = styleInterval(c(0), c("#69cfd5", "#fd6d98"))) %>% 
    formatStyle("Средна_температура", color = styleInterval(c(0), c("#69cfd5", "#fd6d98"))) %>% 
    formatStyle("Максимална_температура", color = styleInterval(c(0), c("#69cfd5", "#fd6d98")))
)

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
    theme(text = element_text(size = 16), legend.position = "none",
          axis.text = element_blank(), axis.ticks = element_blank())
}, height = 750, width = 1850)

}
shinyApp(ui, server)
