library(tidyverse)
library(shiny)
library(bslib)
library(scales)
options(scipen = 100)

gdp <- read_rds("nama_10_gdp.rds") %>% 
  filter(!str_detect(geo, "^Euro")) %>% 
  arrange(TIME_PERIOD)
gdp_pc <- read_rds("nama_10_pc.rds") %>% 
  filter(!str_detect(geo, "^Euro")) %>% 
  arrange(TIME_PERIOD)
gini <- read_rds("ilc_di12.rds") %>% 
  filter(!str_detect(geo, "^Euro")) %>% 
  arrange(TIME_PERIOD)
inf <- read_rds("prc_hicp_mmor.rds") %>% 
  filter(!str_detect(geo, "^Euro")) %>% 
  arrange(TIME_PERIOD)
sal <- read_rds("nama_10_fte.rds") %>% 
  filter(!str_detect(geo, "^Euro"), unit == "Euro") %>% 
  arrange(TIME_PERIOD)
wage <- read_rds("earn_mw_cur.rds") %>% 
  arrange(TIME_PERIOD) %>% drop_na(values)
house <- read_rds("prc_hpi_a.rds") %>% 
  filter(!str_detect(geo, "^Euro")) %>% 
  arrange(TIME_PERIOD)
unemp <- read_rds("tps00203.rds") %>% 
  filter(!str_detect(geo, "^Euro")) %>% 
  arrange(TIME_PERIOD)
imp_exp <- read_rds("nama_10_exi.rds") %>% 
  filter(!str_detect(geo, "^Euro")) %>% 
  arrange(TIME_PERIOD)
labor <- read_rds("nama_10_lp_ulc.rds") %>% 
  filter(!str_detect(geo, "^Euro")) %>% 
  arrange(TIME_PERIOD)
debt <- read_rds("gov_10dd_edpt1.rds") %>% 
  filter(!str_detect(geo, "^Euro")) %>% 
  arrange(TIME_PERIOD)
ppp <- read_rds("prc_ppp_ind.rds") %>% 
  filter(!str_detect(geo, "^Euro")) %>% 
  filter(!str_detect(geo, "^Cand")) %>% 
  arrange(TIME_PERIOD)
gva <- read_rds("nama_10_a10.rds") %>% 
  filter(!str_detect(geo, "^Euro")) %>% 
  filter(!str_detect(geo, "^Cand")) %>% 
  arrange(TIME_PERIOD)
emp <- read_rds("nama_10_a10_e.rds") %>% 
  filter(!str_detect(geo, "^Euro")) %>% 
  arrange(TIME_PERIOD)
tec00009 <- read_rds("tec00009.rds") %>% 
  filter(!str_detect(geo, "^Euro")) %>% 
  arrange(TIME_PERIOD)
tec00010 <- read_rds("tec00010.rds") %>% 
  filter(!str_detect(geo, "^Euro")) %>% 
  arrange(TIME_PERIOD)
tec00011 <- read_rds("tec00011.rds") %>% 
  filter(!str_detect(geo, "^Euro")) %>% 
  arrange(TIME_PERIOD)
#-----------------------------------------
space_s <- function (x, accuracy = NULL, scale = 1, prefix = "", suffix = "", 
                     big.mark = " ", decimal.mark = ".", trim = TRUE, digits, 
                     ...)
{
  if (!missing(digits)) {
    lifecycle::deprecate_stop(when = "1.0.0", what = "comma(digits)", 
                              with = "comma(accuracy)")
  }
  number(x = x, accuracy = accuracy, scale = scale, prefix = prefix, 
         suffix = suffix, big.mark = big.mark, decimal.mark = decimal.mark, 
         trim = trim, ...)
}
#----------------------------------------
mail <- tags$a(icon("envelope"), "Email", 
               href = "mailto:nickydyakov@gmail.com", 
               tagret = "_blank")
github <- tags$a(icon("github"), "Github", 
                 href = "https://github.com/NickyDy", 
                 tagret = "_blank")
#------------------------------------------------
ui <- page_fillable(#h3("Евростат за България!"),
                    theme = bslib::bs_theme(bootswatch = "darkly"),
                    navset_pill_list(widths = c(2, 10),
                      nav_panel(title = "БВП",
                                layout_columns(
                                  selectInput("date_gdp", "Дата:",
                                              choices = unique(gdp$TIME_PERIOD),
                                              selected = last(gdp$TIME_PERIOD)),
                                  selectInput("na_item_gdp", "Показател:",
                                              choices = NULL),
                                  selectInput("unit_gdp", "Индекс, мерна единица:",
                                              choices = NULL),
                                  col_widths = c(2, 4, 6)),
                                plotOutput("gdp_plot")),
                      nav_panel(title = "БВП (in time)",
                                layout_columns(
                                  selectInput("country_gdp", "Държава:",
                                              choices = unique(gdp$geo),
                                              selected = "Bulgaria"),
                                  selectInput("na_item_gdp_time", "Показател:",
                                              choices = NULL),
                                  selectInput("unit_gdp_time", "Индекс, мерна единица:",
                                              choices = NULL),
                                  col_widths = c(2, 4, 6)),
                                plotOutput("gdp_plot_time")),
                      nav_panel(title = "БВП на глава",
                                layout_columns(
                                  selectInput("date_gdp_pc", "Дата:",
                                              choices = unique(gdp_pc$TIME_PERIOD),
                                              selected = last(gdp_pc$TIME_PERIOD)),
                                  selectInput("na_item_gdp_pc", "Показател:",
                                              choices = NULL),
                                  selectInput("unit_gdp_pc", "Индекс, мерна единица:",
                                              choices = NULL),
                                  col_widths = c(2, 4, 6)),
                                plotOutput("plot_gdp_pc")),
                      nav_panel(title = "БВП на глава (in time)",
                                layout_columns(
                                  selectInput("country_gdp_pc", "Държава:",
                                              choices = unique(gdp_pc$geo),
                                              selected = "Bulgaria"),
                                  selectInput("na_item_gdp_time_pc", "Показател:",
                                              choices = NULL),
                                  selectInput("unit_gdp_time_pc", "Индекс, мерна единица:",
                                              choices = NULL),
                                  col_widths = c(2, 4, 6)),
                                plotOutput("gdp_plot_time_pc")),
                      nav_panel(title = "Инфлация",
                                layout_columns(
                                  dateRangeInput("date_inf", "Дата:",
                                            start = first(inf$TIME_PERIOD),
                                            end = last(inf$TIME_PERIOD),
                                            min = first(inf$TIME_PERIOD),
                                            max = last(inf$TIME_PERIOD),
                                            language = "bg",),
                                  selectInput("coicop_inf", "Показател:",
                                            choices = unique(inf$coicop),
                                            selected = "All-items HICP"),
                                  selectInput("country_inf", "Държава",
                                              choices = unique(inf$geo),
                                              selected = "Bulgaria"),
                                  selectInput("coicop_inf_line", "Показател:",
                                              choices = unique(inf$coicop),
                                              selected = "All-items HICP"),
                                  col_widths = c(2, 4, 2, 4)),
                                layout_columns(
                                  plotOutput("inf_plot"),
                                  plotOutput("inf_line"),
                                  col_widths = c(6, 6))),
                      nav_panel(title = "Gini",
                                layout_columns(gap = "400px",
                                  selectInput("date_gini", "Дата:",
                                              choices = unique(gini$TIME_PERIOD),
                                              selected = last(gini$TIME_PERIOD)),
                                  selectInput("country_gini", "Държава:",
                                              choices = unique(gini$geo),
                                              selected = "Bulgaria"),
                                  col_widths = c(2, 2)), 
                                layout_columns(
                                  plotOutput("gini_plot"),
                                  plotOutput("gini_time"), 
                                  col_widths = c(6, 6))),
                      nav_panel("Минимална заплата",
                                layout_columns(
                                               selectInput("wage_date", "Дата:",
                                                           choices = unique(wage$TIME_PERIOD),
                                                           selected = last(wage$TIME_PERIOD)),
                                               selectInput("wage_currency", "Мерна единица:",
                                                           choices = unique(wage$currency)),
                                               selectInput("wage_country", "Държава:",
                                                           choices = unique(wage$geo),
                                                           selected = "Bulgaria"),
                                               selectInput("wage_currency_line", "Мерна единица:",
                                                           choices = unique(wage$currency)),
                                               col_widths = c(2, 4, 2, 4)),
                                layout_columns(
                                  plotOutput("wage_plot"),
                                  plotOutput("wage_time"),
                                  col_widths = c(6, 6))),
                      nav_panel("Средна заплата", 
                                layout_columns(gap = "400px",
                                  selectInput("sal_date", "Дата:",
                                              choices = unique(sal$TIME_PERIOD),
                                              selected = last(sal$TIME_PERIOD)),
                                  selectInput("sal_country", "Държава:",
                                              choices = unique(sal$geo),
                                              selected = "Bulgaria"),
                                  col_widths = c(2, 2)), 
                                layout_columns(
                                  plotOutput("sal_plot"),
                                  plotOutput("sal_time"), 
                                  col_widths = c(6, 6))),
                      nav_panel("Цени на имоти", layout_columns(
                        selectInput("house_date", "Дата:",
                                    choices = unique(house$TIME_PERIOD),
                                    selected = last(house$TIME_PERIOD)),
                        selectInput("purchase_house", "Тип имот:",
                                    choices = NULL),
                        selectInput("unit_house", "Индекс:",
                                    choices = NULL),
                        col_widths = c(2, 3, 3)),
                        plotOutput("house_plot")),
                      nav_panel("Безработица", 
                                layout_columns(
                                  selectInput("unemp_date", "Дата:",
                                              choices = unique(unemp$TIME_PERIOD),
                                              selected = last(unemp$TIME_PERIOD)),
                                  selectInput("unemp_unit", "Индекс:",
                                              choices = unique(unemp$unit)),
                                  selectInput("unemp_country", "Държава",
                                              choices = unique(unemp$geo),
                                              selected = "Bulgaria"),
                                  selectInput("unemp_unit_line", "Индекс:",
                                              choices = unique(unemp$unit)),
                                  col_widths = c(2, 4, 2, 4)),
                                layout_columns(
                                  plotOutput("unemp_plot"),
                                  plotOutput("plot_unemp_line"),
                                  col_widths = c(6, 6))),
                      nav_panel("Внос/Износ", layout_columns(
                        selectInput("date_imp_exp", "Дата:",
                                    choices = unique(imp_exp$TIME_PERIOD),
                                    selected = last(imp_exp$TIME_PERIOD)),
                        selectInput("na_item_imp_exp", "Показател:",
                                    choices = NULL),
                        selectInput("unit_imp_exp", "Индекс, мерна единица:",
                                    choices = NULL),
                        col_widths = c(2, 5, 4)),
                        plotOutput("imp_exp_plot")),
                      nav_panel("Производителност на труда", layout_columns(
                        selectInput("date_labor", "Дата:",
                                    choices = unique(labor$TIME_PERIOD),
                                    selected = last(labor$TIME_PERIOD)),
                        selectInput("na_item_labor", "Показател:",
                                    choices = NULL),
                        selectInput("unit_labor", "Индекс, мерна единица:",
                                    choices = NULL),
                        col_widths = c(2, 3, 6)),
                        plotOutput("labor_plot")),
                      nav_panel("Дтржавен дълг", layout_columns(
                        selectInput("date_debt", "Дата:",
                                    choices = unique(debt$TIME_PERIOD),
                                    selected = last(debt$TIME_PERIOD)),
                        selectInput("sector_debt", "Сектор:",
                                    choices = NULL),
                        selectInput("na_item_debt", "Показател:",
                                    choices = NULL),
                        selectInput("unit_debt", "Индекс, мерна единица:",
                                    choices = NULL),
                        col_widths = c(2, 2, 5, 3)),
                        plotOutput("debt_plot")),
                      nav_panel("Покупателна способност", layout_columns(
                        selectInput("date_ppp", "Дата:",
                                    choices = unique(ppp$TIME_PERIOD),
                                    selected = last(ppp$TIME_PERIOD)),
                        selectInput("na_item_ppp", "Показател:",
                                    choices = NULL),
                        selectInput("ppp_cat_ppp", "Категория:",
                                    choices = NULL),
                        col_widths = c(2, 4, 4)),
                        plotOutput("plot_ppp")),
                      nav_panel("БДС", layout_columns(
                        selectInput("date_gva", "Дата:",
                                    choices = unique(gva$TIME_PERIOD),
                                    selected = last(gva$TIME_PERIOD)),
                        selectInput("na_item_gva", "Показател:",
                                    choices = NULL),
                        selectInput("nace_r2_gva", "Категория:",
                                    choices = NULL),
                        selectInput("unit_gva", "Индекс, мерна единица:",
                                    choices = NULL),
                        col_widths = c(2, 2, 4, 4)),
                        plotOutput("plot_gva")),
                      nav_panel("Заети лица", layout_columns(
                        selectInput("date_emp", "Дата:",
                                    choices = unique(emp$TIME_PERIOD),
                                    selected = last(emp$TIME_PERIOD)),
                        selectInput("na_item_emp", "Показател:",
                                    choices = NULL),
                        selectInput("nace_r2_emp", "Категория:",
                                    choices = NULL),
                        selectInput("unit_emp", "Индекс, мерна единица:",
                                    choices = NULL),
                        col_widths = c(2, 3, 3, 3)),
                        plotOutput("plot_emp")),
                      nav_panel("Разходи на домакинствата", 
                        layout_columns(
                        selectInput("tec00009_date", "Дата:",
                                    choices = unique(tec00009$TIME_PERIOD),
                                    selected = last(tec00009$TIME_PERIOD)),
                        selectInput("tec00009_unit", "Индекс:",
                                    choices = unique(tec00009$unit)),
                        selectInput("tec00009_country", "Държава",
                                    choices = unique(tec00009$geo),
                                    selected = "Bulgaria"),
                        selectInput("tec00009_unit_line", "Индекс:",
                                    choices = unique(tec00009$unit)),
                        col_widths = c(2, 4, 2, 4)),
                        layout_columns(
                        plotOutput("plot_tec00009"),
                        plotOutput("plot_tec00009_line"),
                        col_widths = c(6, 6))),
                      nav_panel("Разходи на правителството", 
                        layout_columns(
                        selectInput("tec00010_date", "Дата:",
                                    choices = unique(tec00010$TIME_PERIOD),
                                    selected = last(tec00010$TIME_PERIOD)),
                        selectInput("tec00010_unit", "Индекс:",
                                    choices = unique(tec00010$unit)),
                        selectInput("tec00010_country", "Държава",
                                    choices = unique(tec00010$geo),
                                    selected = "Bulgaria"),
                        selectInput("tec00010_unit_line", "Индекс:",
                                    choices = unique(tec00010$unit)),
                        col_widths = c(2, 4, 2, 4)),
                        layout_columns(
                        plotOutput("plot_tec00010"),
                        plotOutput("plot_tec00010_line"),
                        col_widths = c(6, 6))),
                      nav_panel("Инвестиции", 
                        layout_columns(
                        selectInput("tec00011_date", "Дата:",
                                    choices = unique(tec00011$TIME_PERIOD),
                                    selected = last(tec00011$TIME_PERIOD)),
                        selectInput("tec00011_unit", "Индекс:",
                                    choices = unique(tec00011$unit)),
                        selectInput("tec00011_country", "Държава",
                                    choices = unique(tec00011$geo),
                                    selected = "Bulgaria"),
                        selectInput("tec00011_unit_line", "Индекс:",
                                    choices = unique(tec00011$unit)),
                        col_widths = c(2, 4, 2, 4)),
                        layout_columns(
                        plotOutput("plot_tec00011"),
                        plotOutput("plot_tec00011_line"),
                        col_widths = c(6, 6))),
                      nav_panel(tags$img(src = "shiny.png", width = 40),
                                "Други полезни приложения:",
                                tags$a(href = "https://nickydy.shinyapps.io/elections/", br(),
                                       "Избори в България!"), br(),
                                tags$a(href = "https://nickydy.shinyapps.io/climate/",
                                       "Климатът на България!"), br(),
                                tags$a(href = "https://nickydy.shinyapps.io/demography/",
                                       "Демография на България!"), br(),
                                tags$a(href = "https://nickydy.shinyapps.io/inlation/",
                                       "Inflation in EU!"), br(),
                                # tags$a(href = "https://ndapps.shinyapps.io/bgprices/",
                                #        "Сравнение на цените в България!"), br(),
                                tags$a(href = "https://ndapps.shinyapps.io/agri/",
                                       "Цени на селскостопанска продукция в ЕС!"), br(),
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
)
#-------------------------------------------
server <- function(input, output, session) {
  
  date_gdp <- reactive({
    filter(gdp, TIME_PERIOD == input$date_gdp)
  })
  
  observeEvent(date_gdp(), {
    freezeReactiveValue(input, "na_item_gdp")
    choices <- unique(date_gdp()$na_item)
    updateSelectInput(inputId = "na_item_gdp", choices = choices)
  })
  
  na_item_gdp <- reactive({
    req(input$date_gdp)
    filter(date_gdp(), na_item == input$na_item_gdp)
  })
  
  observeEvent(na_item_gdp(), {
    freezeReactiveValue(input, "unit_gdp")
    choices <- unique(na_item_gdp()$unit)
    updateSelectInput(inputId = "unit_gdp", choices = choices)
  })
  
  unit_gdp <- reactive({
    req(input$na_item_gdp)
    filter(na_item_gdp(), unit == input$unit_gdp)
  })
  
output$gdp_plot <- renderPlot({
  unit_gdp() %>% 
      filter(na_item %in% c(input$na_item_gdp),
             unit %in% c(input$unit_gdp)) %>% 
      mutate(geo = fct_reorder(geo, values),
             col = if_else(geo == "Bulgaria", "1", "0")) %>% 
      ggplot(aes(values, geo, fill = col)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_x_continuous(expand = expansion(mult = c(0.01, 0.3))) +
      geom_text(aes(label = space_s(values)),
                position = position_dodge(width = 1), hjust = -0.1, size = 4.5) +
      scale_fill_manual(values = c("gray50", "red")) +
      theme(text = element_text(size = 14), legend.position = "none") +
      labs(x = input$unit_gdp, y = NULL, 
           caption = "Източник на данните: Eurostat")
  }, height = 800, width = 1550, res = 96)

# GDP (in time)
country_gdp <- reactive({
  filter(gdp, geo == input$country_gdp)
})

observeEvent(country_gdp(), {
  freezeReactiveValue(input, "na_item_gdp_time")
  choices <- unique(country_gdp()$na_item)
  updateSelectInput(inputId = "na_item_gdp_time", choices = choices)
})

na_item_gdp_time <- reactive({
  req(input$country_gdp)
  filter(country_gdp(), na_item == input$na_item_gdp_time)
})

observeEvent(na_item_gdp_time(), {
  freezeReactiveValue(input, "unit_gdp_time")
  choices <- unique(na_item_gdp_time()$unit)
  updateSelectInput(inputId = "unit_gdp_time", choices = choices)
})

unit_gdp_time <- reactive({
  req(input$na_item_gdp_time)
  filter(na_item_gdp_time(), unit == input$unit_gdp_time)
})

output$gdp_plot_time <- renderPlot({
  
  unit_gdp_time() %>% 
    filter(na_item %in% c(input$na_item_gdp_time),
           unit %in% c(input$unit_gdp_time)) %>%
    ggplot(aes(TIME_PERIOD, values)) +
    geom_line() +
    geom_point() +
    theme(text = element_text(size = 12), plot.title.position = "plot") +
    labs(y = paste0(input$unit_gdp_time), x = NULL,
         caption = "Източник на данните: Eurostat")
  
}, height = 800, width = 1550, res = 96)
#------------------------------------------
date_gdp_pc <- reactive({
  filter(gdp_pc, TIME_PERIOD == input$date_gdp_pc)
})

observeEvent(date_gdp_pc(), {
  freezeReactiveValue(input, "na_item_gdp_pc")
  choices <- unique(date_gdp_pc()$na_item)
  updateSelectInput(inputId = "na_item_gdp_pc", choices = choices)
})

na_item_gdp_pc <- reactive({
  req(input$date_gdp_pc)
  filter(date_gdp_pc(), na_item == input$na_item_gdp_pc)
})

observeEvent(na_item_gdp_pc(), {
  freezeReactiveValue(input, "unit_gdp_pc")
  choices <- unique(na_item_gdp_pc()$unit)
  updateSelectInput(inputId = "unit_gdp_pc", choices = choices)
})

unit_gdp_pc <- reactive({
  req(input$na_item_gdp_pc)
  filter(na_item_gdp_pc(), unit == input$unit_gdp_pc)
})

output$plot_gdp_pc <- renderPlot({
  unit_gdp_pc() %>% 
    filter(na_item %in% c(input$na_item_gdp_pc),
           unit %in% c(input$unit_gdp_pc)) %>% 
    mutate(geo = fct_reorder(geo, values),
           col = if_else(geo == "Bulgaria", "1", "0")) %>% 
    ggplot(aes(values, geo, fill = col)) +
    geom_col(position = position_dodge2(preserve = "single")) +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.3))) +
    geom_text(aes(label = space_s(values)),
              position = position_dodge(width = 1), hjust = -0.1, size = 4.5) +
    scale_fill_manual(values = c("gray50", "red")) +
    theme(text = element_text(size = 14), legend.position = "none") +
    labs(x = input$unit_gdp_pc, y = NULL, 
         caption = "Източник на данните: Eurostat")
}, height = 800, width = 1550, res = 96)

# GDP per capita (in time)
country_gdp_pc <- reactive({
  filter(gdp_pc, geo == input$country_gdp_pc)
})

observeEvent(country_gdp_pc(), {
  freezeReactiveValue(input, "na_item_gdp_time_pc")
  choices <- unique(country_gdp_pc()$na_item)
  updateSelectInput(inputId = "na_item_gdp_time_pc", choices = choices)
})

na_item_gdp_time_pc <- reactive({
  req(input$country_gdp_pc)
  filter(country_gdp_pc(), na_item == input$na_item_gdp_time_pc)
})

observeEvent(na_item_gdp_time_pc(), {
  freezeReactiveValue(input, "unit_gdp_time_pc")
  choices <- unique(na_item_gdp_time_pc()$unit)
  updateSelectInput(inputId = "unit_gdp_time_pc", choices = choices)
})

unit_gdp_time_pc <- reactive({
  req(input$na_item_gdp_time_pc)
  filter(na_item_gdp_time_pc(), unit == input$unit_gdp_time_pc)
})

output$gdp_plot_time_pc <- renderPlot({
  
  unit_gdp_time_pc() %>% 
    filter(na_item %in% c(input$na_item_gdp_time_pc),
           unit %in% c(input$unit_gdp_time_pc)) %>%
    ggplot(aes(TIME_PERIOD, values)) +
    geom_line() +
    geom_point() +
    theme(text = element_text(size = 12), plot.title.position = "plot") +
    labs(y = paste0(input$unit_gdp_time_pc), x = NULL,
         caption = "Източник на данните: Eurostat")
  
}, height = 800, width = 1550, res = 96)
  #-----------------------------------------
inf_last <- reactive({
  inf %>%
  filter(TIME_PERIOD >= input$date_inf[1] & TIME_PERIOD <= input$date_inf[2],
    coicop %in% c(input$coicop_inf)) %>% 
  group_by(coicop, geo) %>% 
  summarise(sm = sum(values, na.rm = T)) %>%
  ungroup() %>% 
  mutate_if(is.numeric, round, 1) %>% 
    mutate(geo = fct_reorder(geo, sm),
           col = if_else(geo == "Bulgaria", "1", "0"))
})

  output$inf_plot <- renderPlot({

inf_last() %>% 
  ggplot(aes(sm, geo, fill = col)) +
  geom_col() +
  scale_fill_manual(values = c("gray50", "red")) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.3))) +
  geom_text(aes(label = sm),
      position = position_dodge(width = 1), hjust = -0.1, size = 4.5) +
  theme(text = element_text(size = 12), legend.position = "none", 
        plot.title.position = "plot") +
  labs(x = "Натрупана инфлация (%)", y = NULL, 
  caption = "Източник на данните: Eurostat")

  }, height = 700, width = 750, res = 96)
  
  inf_line_r <- reactive({
    inf %>% 
    filter(TIME_PERIOD >= input$date_inf[1] & TIME_PERIOD <= input$date_inf[2],
           geo %in% c(input$country_inf),
           coicop %in% c(input$coicop_inf_line)) %>% 
    group_by(coicop, geo) %>% 
    mutate(cs = cumsum(values)) %>% 
    ungroup()
  })
  
  output$inf_line <- renderPlot({
    
    inf_line_r() %>% 
      ggplot(aes(TIME_PERIOD, cs)) +
      geom_line() +
      geom_point() +
      theme(text = element_text(size = 12), plot.title.position = "plot") +
      labs(y = "Натрупана инфлация (%)", x = NULL, 
           caption = "Източник на данните: Eurostat")
    
  }, height = 700, width = 750, res = 96)
  #---------------------------------------
  output$gini_plot <- renderPlot({

    gini %>% 
      filter(TIME_PERIOD == input$date_gini, age == "Total") %>% 
      mutate(geo = fct_reorder(geo, values),
             col = if_else(geo == "Bulgaria", "1", "0")) %>% 
      ggplot(aes(values, geo, fill = col)) +
      geom_col() +
      scale_fill_manual(values = c("gray50", "red")) +
      scale_x_continuous(expand = expansion(mult = c(0.01, 0.1))) +
      geom_text(aes(label = values),
                position = position_dodge(width = 1), hjust = -0.1, size = 4.5) +
      theme(text = element_text(size = 14), legend.position = "none") +
      labs(x = "Коефициент Gini", y = NULL, 
           caption = "Източник на данните: Eurostat")
  }, height = 700, width = 750, res = 96)
  
  output$gini_time <- renderPlot({
    
    gini %>% 
      filter(geo %in% c(input$country_gini), age == "Total") %>% 
      ggplot(aes(TIME_PERIOD, values)) +
      geom_line() +
      geom_point() +
      scale_y_continuous(expand = expansion(mult = c(0.01, 0.1))) +
      geom_text(aes(label = values), check_overlap = T,
                position = position_dodge(width = 1), vjust = -0.5, size = 4) +
      theme(text = element_text(size = 14), legend.position = "none") +
      labs(y = "Коефициент Gini", x = NULL, 
           caption = "Източник на данните: Eurostat")
  }, height = 700, width = 750, res = 96)
  #--------------------------------------
  output$wage_plot <- renderPlot({
    
    wage %>% 
      filter(TIME_PERIOD == input$wage_date,
             currency %in% c(input$wage_currency)) %>% 
      mutate(geo = fct_reorder(geo, values),
             col = if_else(geo == "Bulgaria", "1", "0")) %>% 
      ggplot(aes(values, geo, fill = col)) +
      geom_col() +
      scale_fill_manual(values = c("gray50", "red")) +
      scale_x_continuous(expand = expansion(mult = c(0.01, 0.2))) +
      geom_text(aes(label = space_s(round(values), 1)),
                position = position_dodge(width = 1), hjust = -0.1, size = 4.5) +
      theme(text = element_text(size = 14), legend.position = "none") +
      labs(x = paste0("Минимална месечна заплата ", "(", input$wage_currency, ")"), y = NULL, 
           caption = "Източник на данните: Eurostat")
  }, height = 700, width = 750, res = 96)
  
  output$wage_time <- renderPlot({
    
    wage %>% 
      filter(geo %in% c(input$wage_country),
             currency %in% c(input$wage_currency_line)) %>% 
      ggplot(aes(TIME_PERIOD, values)) +
      geom_line() +
      geom_point() +
      scale_y_continuous(expand = expansion(mult = c(0.01, 0.1))) +
      geom_text(aes(label = space_s(round(values), 1)), check_overlap = T,
                position = position_dodge(width = 1), vjust = -0.5, size = 4) +
      theme(text = element_text(size = 14), legend.position = "none") +
      labs(y = paste0("Минимална месечна заплата ", "(", input$wage_currency_line, ")"), x = NULL, 
           caption = "Източник на данните: Eurostat")
  }, height = 700, width = 750, res = 96)
  #--------------------------------------
  output$sal_plot <- renderPlot({
    
    sal %>% 
      filter(TIME_PERIOD == input$sal_date) %>% 
      mutate(geo = fct_reorder(geo, values),
             col = if_else(geo == "Bulgaria", "1", "0")) %>% 
      ggplot(aes(values, geo, fill = col)) +
      geom_col() +
      scale_fill_manual(values = c("gray50", "red")) +
      scale_x_continuous(expand = expansion(mult = c(0.01, 0.2))) +
      geom_text(aes(label = space_s(values)),
                position = position_dodge(width = 1), hjust = -0.1, size = 4.5) +
      theme(text = element_text(size = 14), legend.position = "none") +
      labs(x = "Средна годишна заплата (\u20AC)", y = NULL, 
           caption = "Източник на данните: Eurostat")
  }, height = 700, width = 750, res = 96)
  
  output$sal_time <- renderPlot({
    
    sal %>% 
      filter(geo %in% c(input$sal_country)) %>% 
      ggplot(aes(TIME_PERIOD, values)) +
      geom_line() +
      geom_point() +
      scale_y_continuous(expand = expansion(mult = c(0.01, 0.1))) +
      geom_text(aes(label = space_s(values)), check_overlap = T,
                position = position_dodge(width = 1), vjust = -0.5, size = 4) +
      theme(text = element_text(size = 14), legend.position = "none") +
      labs(y = "Средна годишна заплата (\u20AC)", x = NULL, 
           caption = "Източник на данните: Eurostat")
  }, height = 700, width = 750, res = 96)
  #---------------------------------------
  house_date <- reactive({
    filter(house, TIME_PERIOD == input$house_date)
  })

  observeEvent(house_date(), {
    freezeReactiveValue(input, "purchase_house")
    choices <- unique(house_date()$purchase)
    updateSelectInput(inputId = "purchase_house", choices = choices)
  })

  purchase_house <- reactive({
    req(input$house_date)
    filter(house_date(), purchase == input$purchase_house)
  })
  
  observeEvent(purchase_house(), {
    freezeReactiveValue(input, "unit_house")
    choices <- unique(purchase_house()$unit)
    updateSelectInput(inputId = "unit_house", choices = choices)
  })
  
  unit_house <- reactive({
    req(input$purchase_house)
    filter(purchase_house(), unit == input$unit_house)
  })

  output$house_plot <- renderPlot({

    unit_house() %>% 
      filter(TIME_PERIOD == input$house_date,
             purchase %in% c(input$purchase_house),
             unit %in% c(input$unit_house)) %>% 
      mutate(geo = fct_reorder(geo, values),
             col = if_else(geo == "Bulgaria", "1", "0")) %>% 
      ggplot(aes(values, geo, fill = col)) +
      geom_col() +
      scale_fill_manual(values = c("gray50", "red")) +
      scale_x_continuous(expand = expansion(mult = c(0.01, 0.3))) +
      geom_text(aes(label = values),
                position = position_dodge(width = 1), hjust = -0.1, size = 4.5) +
      theme(text = element_text(size = 14), legend.position = "none") +
      labs(x = input$unit_house, y = NULL, 
           caption = "Източник на данните: Eurostat")

  }, height = 800, width = 1550, res = 96)
  #---------------------------------------
  
  output$unemp_plot <- renderPlot({

    unemp %>% 
      filter(TIME_PERIOD == input$unemp_date,
             unit %in% c(input$unemp_unit)) %>% 
      mutate(geo = fct_reorder(geo, values),
             col = if_else(geo == "Bulgaria", "1", "0")) %>% 
      ggplot(aes(values, geo, fill = col)) +
      geom_col() +
      scale_fill_manual(values = c("gray50", "red")) +
      scale_x_continuous(expand = expansion(mult = c(0.01, 0.3))) +
      geom_text(aes(label = values),
                position = position_dodge(width = 1), hjust = -0.1, size = 4.5) +
      theme(text = element_text(size = 14), legend.position = "none") +
      labs(x = input$unemp_unit, y = NULL, 
           caption = "Източник на данните: Eurostat")

  }, height = 700, width = 750, res = 96)
  
  output$plot_unemp_line <- renderPlot({
    
    unemp %>% 
      filter(geo %in% c(input$unemp_country),
             unit %in% c(input$unemp_unit_line)) %>% 
      ggplot(aes(TIME_PERIOD, values)) +
      geom_line() +
      geom_point() +
      scale_y_continuous(expand = expansion(mult = c(0.01, 0.1))) +
      geom_text(aes(label = values), check_overlap = T,
                position = position_dodge(width = 1), vjust = -0.5, size = 4) +
      theme(text = element_text(size = 14), legend.position = "none") +
      labs(y = input$unemp_unit_line, x = NULL, 
           caption = "Източник на данните: Eurostat")
  }, height = 700, width = 750, res = 96)
  #---------------------------------------
  date_imp_exp <- reactive({
    filter(imp_exp, TIME_PERIOD == input$date_imp_exp)
  })
  
  observeEvent(date_imp_exp(), {
    freezeReactiveValue(input, "na_item_imp_exp")
    choices <- unique(date_imp_exp()$na_item)
    updateSelectInput(inputId = "na_item_imp_exp", choices = choices)
  })
  
  na_item_imp_exp <- reactive({
    req(input$date_imp_exp)
    filter(date_imp_exp(), na_item == input$na_item_imp_exp)
  })
  
  observeEvent(na_item_imp_exp(), {
    freezeReactiveValue(input, "unit_imp_exp")
    choices <- unique(na_item_imp_exp()$unit)
    updateSelectInput(inputId = "unit_imp_exp", choices = choices)
  })
  
  unit_imp_exp <- reactive({
    req(input$na_item_imp_exp)
    filter(na_item_imp_exp(), unit == input$unit_imp_exp)
  })
  
  output$imp_exp_plot <- renderPlot({
    unit_imp_exp() %>% 
      filter(na_item %in% c(input$na_item_imp_exp),
             unit %in% c(input$unit_imp_exp)) %>% 
      mutate(geo = fct_reorder(geo, values),
             col = if_else(geo == "Bulgaria", "1", "0")) %>% 
      ggplot(aes(values, geo, fill = col)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_x_continuous(expand = expansion(mult = c(0.01, 0.3))) +
      geom_text(aes(label = space_s(values)),
                position = position_dodge(width = 1), hjust = -0.1, size = 4.5) +
      scale_fill_manual(values = c("gray50", "red")) +
      theme(text = element_text(size = 14), legend.position = "none") +
      labs(x = paste0(input$na_item_imp_exp, " [", input$unit_imp_exp, "]"), y = NULL, 
           caption = "Източник на данните: Eurostat")
  }, height = 800, width = 1550, res = 96)
  #---------------------------------------
  date_labor <- reactive({
    filter(labor, TIME_PERIOD == input$date_labor)
  })
  
  observeEvent(date_labor(), {
    freezeReactiveValue(input, "na_item_labor")
    choices <- unique(date_labor()$na_item)
    updateSelectInput(inputId = "na_item_labor", choices = choices)
  })
  
  na_item_labor <- reactive({
    req(input$date_labor)
    filter(date_labor(), na_item == input$na_item_labor)
  })
  
  observeEvent(na_item_labor(), {
    freezeReactiveValue(input, "unit_labor")
    choices <- unique(na_item_labor()$unit)
    updateSelectInput(inputId = "unit_labor", choices = choices)
  })
  
  unit_labor <- reactive({
    req(input$na_item_labor)
    filter(na_item_labor(), unit == input$unit_labor)
  })
  
  output$labor_plot <- renderPlot({
    
    unit_labor() %>% 
      filter(na_item %in% c(input$na_item_labor),
             unit %in% c(input$unit_labor)) %>% 
      mutate(geo = fct_reorder(geo, values),
             col = if_else(geo == "Bulgaria", "1", "0")) %>% 
      ggplot(aes(values, geo, fill = col)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_x_continuous(expand = expansion(mult = c(0.01, 0.3))) +
      geom_text(aes(label = space_s(values)),
                position = position_dodge(width = 1), hjust = -0.1, size = 4.5) +
      scale_fill_manual(values = c("gray50", "red")) +
      theme(text = element_text(size = 14), legend.position = "none") +
      labs(x = paste0(input$na_item_labor, " [", input$unit_labor, "]"), y = NULL, 
           caption = "Източник на данните: Eurostat")
    
  }, height = 800, width = 1550, res = 96)
  #---------------------------------------
  date_debt <- reactive({
    filter(debt, TIME_PERIOD == input$date_debt)
  })
  
  observeEvent(date_debt(), {
    freezeReactiveValue(input, "sector_debt")
    choices <- unique(date_debt()$sector)
    updateSelectInput(inputId = "sector_debt", choices = choices)
  })
  
  sector_debt <- reactive({
    req(input$date_debt)
    filter(date_debt(), sector == input$sector_debt)
  })
  
  observeEvent(sector_debt(), {
    freezeReactiveValue(input, "na_item_debt")
    choices <- unique(sector_debt()$na_item)
    updateSelectInput(inputId = "na_item_debt", choices = choices)
  })
  
  na_item_debt <- reactive({
    req(input$sector_debt)
    filter(sector_debt(), na_item == input$na_item_debt)
  })
  
  observeEvent(na_item_debt(), {
    freezeReactiveValue(input, "unit_debt")
    choices <- unique(na_item_debt()$unit)
    updateSelectInput(inputId = "unit_debt", choices = choices)
  })
  
  unit_debt <- reactive({
    req(input$na_item_debt)
    filter(na_item_debt(), unit == input$unit_debt)
  })
  
output$debt_plot <- renderPlot({
    
  unit_debt() %>% 
      filter(sector %in% c(input$sector_debt),
             na_item %in% c(input$na_item_debt),
             unit %in% c(input$unit_debt)) %>% 
      mutate(geo = fct_reorder(geo, values),
             col = if_else(geo == "Bulgaria", "1", "0")) %>% 
      ggplot(aes(values, geo, fill = col)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_x_continuous(expand = expansion(mult = c(0.01, 0.3))) +
      geom_text(aes(label = space_s(values)),
                position = position_dodge(width = 1), hjust = -0.1, size = 4.5) +
      scale_fill_manual(values = c("gray50", "red")) +
      theme(text = element_text(size = 14), legend.position = "none") +
      labs(x = paste0(input$na_item_debt, " [", input$unit_debt, "]"), y = NULL, 
           caption = "Източник на данните: Eurostat")
    
  }, height = 800, width = 1550, res = 96)
#---------------------------------------
date_ppp <- reactive({
  filter(ppp, TIME_PERIOD == input$date_ppp)
})

observeEvent(date_ppp(), {
  freezeReactiveValue(input, "na_item_ppp")
  choices <- unique(date_ppp()$na_item)
  updateSelectInput(inputId = "na_item_ppp", choices = choices)
})

na_item_ppp <- reactive({
  req(input$date_ppp)
  filter(date_ppp(), na_item == input$na_item_ppp)
})

observeEvent(na_item_ppp(), {
  freezeReactiveValue(input, "ppp_cat_ppp")
  choices <- unique(na_item_ppp()$ppp_cat)
  updateSelectInput(inputId = "ppp_cat_ppp", choices = choices)
})

ppp_cat_ppp <- reactive({
  req(input$na_item_ppp)
  filter(na_item_ppp(), ppp_cat == input$ppp_cat_ppp)
})

output$plot_ppp <- renderPlot({
  
  ppp_cat_ppp() %>% 
    filter(na_item %in% c(input$na_item_ppp),
           ppp_cat %in% c(input$ppp_cat_ppp)) %>% 
    mutate(geo = fct_reorder(geo, values),
           col = if_else(geo == "Bulgaria", "1", "0")) %>% 
    ggplot(aes(values, geo, fill = col)) +
    geom_col() +
    scale_fill_manual(values = c("gray50", "red")) +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.3))) +
    geom_text(aes(label = space_s(values)),
              position = position_dodge(width = 1), hjust = -0.1, size = 4.5) +
    theme(text = element_text(size = 14), legend.position = "none") +
    labs(x = input$na_item_ppp, y = NULL, 
         caption = "Източник на данните: Eurostat")
  
}, height = 800, width = 1550, res = 96)
  #---------------------------------------
date_gva <- reactive({
  filter(gva, TIME_PERIOD == input$date_gva)
})

observeEvent(date_gva(), {
  freezeReactiveValue(input, "na_item_gva")
  choices <- unique(date_gva()$na_item)
  updateSelectInput(inputId = "na_item_gva", choices = choices)
})

na_item_gva <- reactive({
  req(input$date_gva)
  filter(date_gva(), na_item == input$na_item_gva)
})

observeEvent(na_item_gva(), {
  freezeReactiveValue(input, "nace_r2_gva")
  choices <- unique(na_item_gva()$nace_r2)
  updateSelectInput(inputId = "nace_r2_gva", choices = choices)
})

nace_r2_gva <- reactive({
  req(input$na_item_gva)
  filter(na_item_gva(), nace_r2 == input$nace_r2_gva)
})

observeEvent(nace_r2_gva(), {
  freezeReactiveValue(input, "unit_gva")
  choices <- unique(nace_r2_gva()$unit)
  updateSelectInput(inputId = "unit_gva", choices = choices)
})

unit_gva <- reactive({
  req(input$nace_r2_gva)
  filter(nace_r2_gva(), unit == input$unit_gva)
})

output$plot_gva <- renderPlot({
  
  unit_gva() %>% 
    filter(na_item %in% c(input$na_item_gva),
           nace_r2 %in% c(input$nace_r2_gva),
           unit %in% c(input$unit_gva)) %>% 
    mutate(geo = fct_reorder(geo, values),
           col = if_else(geo == "Bulgaria", "1", "0")) %>% 
    ggplot(aes(values, geo, fill = col)) +
    geom_col() +
    scale_fill_manual(values = c("gray50", "red")) +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.3))) +
    geom_text(aes(label = space_s(values)),
              position = position_dodge(width = 1), hjust = -0.1, size = 4.5) +
    theme(text = element_text(size = 14), legend.position = "none") +
    labs(x = input$unit_gva, y = NULL, 
         caption = "Източник на данните: Eurostat")
  
}, height = 800, width = 1550, res = 96)
  #---------------------------------------
date_emp <- reactive({
  filter(emp, TIME_PERIOD == input$date_emp)
})

observeEvent(date_emp(), {
  freezeReactiveValue(input, "na_item_emp")
  choices <- unique(date_emp()$na_item)
  updateSelectInput(inputId = "na_item_emp", choices = choices)
})

na_item_emp <- reactive({
  req(input$date_emp)
  filter(date_emp(), na_item == input$na_item_emp)
})

observeEvent(na_item_emp(), {
  freezeReactiveValue(input, "nace_r2_emp")
  choices <- unique(na_item_emp()$nace_r2)
  updateSelectInput(inputId = "nace_r2_emp", choices = choices)
})

nace_r2_emp <- reactive({
  req(input$na_item_emp)
  filter(na_item_emp(), nace_r2 == input$nace_r2_emp)
})

observeEvent(nace_r2_emp(), {
  freezeReactiveValue(input, "unit_emp")
  choices <- unique(nace_r2_emp()$unit)
  updateSelectInput(inputId = "unit_emp", choices = choices)
})

unit_emp <- reactive({
  req(input$nace_r2_emp)
  filter(nace_r2_emp(), unit == input$unit_emp)
})

output$plot_emp <- renderPlot({
  
  unit_emp() %>% 
    filter(na_item %in% c(input$na_item_emp),
           nace_r2 %in% c(input$nace_r2_emp),
           unit %in% c(input$unit_emp)) %>% 
    mutate(geo = fct_reorder(geo, values),
           col = if_else(geo == "Bulgaria", "1", "0")) %>% 
    ggplot(aes(values, geo, fill = col)) +
    geom_col() +
    scale_fill_manual(values = c("gray50", "red")) +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.3))) +
    geom_text(aes(label = values),
              position = position_dodge(width = 1), hjust = -0.1, size = 4.5) +
    theme(text = element_text(size = 14), legend.position = "none") +
    labs(x = input$unit_emp, y = NULL, 
         caption = "Източник на данните: Eurostat")
  
}, height = 800, width = 1550, res = 96)
#---------------------------------------

output$plot_tec00009 <- renderPlot({
  
  tec00009 %>% 
    filter(TIME_PERIOD == input$tec00009_date,
           unit %in% c(input$tec00009_unit)) %>% 
    mutate(geo = fct_reorder(geo, values),
           col = if_else(geo == "Bulgaria", "1", "0")) %>% 
    ggplot(aes(values, geo, fill = col)) +
    geom_col() +
    scale_fill_manual(values = c("gray50", "red")) +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.3))) +
    geom_text(aes(label = space_s(values)),
              position = position_dodge(width = 1), hjust = -0.1, size = 4.5) +
    theme(text = element_text(size = 14), legend.position = "none") +
    labs(x = input$tec00009_unit, y = NULL, 
         caption = "Източник на данните: Eurostat")
  
}, height = 700, width = 750, res = 96)

output$plot_tec00009_line <- renderPlot({
  
  tec00009 %>% 
    filter(geo %in% c(input$tec00009_country),
           unit %in% c(input$tec00009_unit_line)) %>% 
    ggplot(aes(TIME_PERIOD, values)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.1))) +
    geom_text(aes(label = space_s(values)), check_overlap = T,
              position = position_dodge(width = 1), vjust = -0.5, size = 4) +
    theme(text = element_text(size = 14), legend.position = "none") +
    labs(y = input$tec00009_unit_line, x = NULL, 
         caption = "Източник на данните: Eurostat")
}, height = 700, width = 750, res = 96)

#---------------------------------------
 
output$plot_tec00010 <- renderPlot({
  
  tec00010 %>% 
    filter(TIME_PERIOD == input$tec00010_date,
           unit %in% c(input$tec00010_unit)) %>% 
    mutate(geo = fct_reorder(geo, values),
           col = if_else(geo == "Bulgaria", "1", "0")) %>% 
    ggplot(aes(values, geo, fill = col)) +
    geom_col() +
    scale_fill_manual(values = c("gray50", "red")) +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.3))) +
    geom_text(aes(label = space_s(values)),
              position = position_dodge(width = 1), hjust = -0.1, size = 4.5) +
    theme(text = element_text(size = 14), legend.position = "none") +
    labs(x = input$tec00010_unit, y = NULL, 
         caption = "Източник на данните: Eurostat")
  
}, height = 700, width = 750, res = 96)

output$plot_tec00010_line <- renderPlot({
  
  tec00010 %>% 
    filter(geo %in% c(input$tec00010_country),
           unit %in% c(input$tec00010_unit_line)) %>% 
    ggplot(aes(TIME_PERIOD, values)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.1))) +
    geom_text(aes(label = space_s(values)), check_overlap = T,
              position = position_dodge(width = 1), vjust = -0.5, size = 4) +
    theme(text = element_text(size = 14), legend.position = "none") +
    labs(y = input$tec00010_unit_line, x = NULL, 
         caption = "Източник на данните: Eurostat")
}, height = 700, width = 750, res = 96)
#---------------------------------------
output$plot_tec00011 <- renderPlot({
  
  tec00011 %>% 
    filter(TIME_PERIOD == input$tec00011_date,
           unit %in% c(input$tec00011_unit)) %>% 
    mutate(geo = fct_reorder(geo, values),
           col = if_else(geo == "Bulgaria", "1", "0")) %>% 
    ggplot(aes(values, geo, fill = col)) +
    geom_col() +
    scale_fill_manual(values = c("gray50", "red")) +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.3))) +
    geom_text(aes(label = space_s(values)),
              position = position_dodge(width = 1), hjust = -0.1, size = 4.5) +
    theme(text = element_text(size = 14), legend.position = "none") +
    labs(x = input$tec00011_unit, y = NULL, 
         caption = "Източник на данните: Eurostat")
  
}, height = 700, width = 750, res = 96)

output$plot_tec00011_line <- renderPlot({
  
  tec00011 %>% 
    filter(geo %in% c(input$tec00011_country),
           unit %in% c(input$tec00011_unit_line)) %>% 
    ggplot(aes(TIME_PERIOD, values)) +
    geom_line() +
    geom_point() +
    scale_y_continuous(expand = expansion(mult = c(0.01, 0.1))) +
    geom_text(aes(label = space_s(values)), check_overlap = T,
              position = position_dodge(width = 1), vjust = -0.5, size = 4) +
    theme(text = element_text(size = 14), legend.position = "none") +
    labs(y = input$tec00011_unit_line, x = NULL, 
         caption = "Източник на данните: Eurostat")
}, height = 700, width = 750, res = 96)
#---------------------------------------

  session$onSessionEnded(function() {
    stopApp()
  })
}
shinyApp(ui, server)