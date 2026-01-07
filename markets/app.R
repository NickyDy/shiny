library(tidyverse)
library(nanoparquet)
library(ggtext)
library(shiny)
library(bslib)
library(DT)

df_markets_2026 <- read_parquet("df_markets_2026.parquet")
df_markets_2025 <- read_parquet("df_markets.parquet")
df_markets <- bind_rows(df_markets_2025, df_markets_2026) %>%
  mutate(kategoria_c = fct_collapse(kategoria, "Бял хляб" = "1", "Кашкавал от краве мляко" = c("10", "11"),
                                    "Краве масло" = "12", "Извара" = c("13", "14"), "Цяло пиле" = "15",
                                    "Пилешко филе" = "16", "Пилешко бутче" = "17", "Свинска плешка" = "18",
                                    "Свински бут" = "19", "Хляб Добруджа" = "2", "Свинско филе" = "20",
                                    "Свински врат" = "21", "Свинско месо за готвене" = "22",
                                    "Кайма смес" = "25", "Кренвирши" = "26", "Колбаси" = "27",
                                    "Салами" = "28", "Замразена риба" = "29", "Ръжен хляб" = "3",
                                    "Охладена риба" = "30", "Яйца M" = "31", "Яйца L" = "32",
                                    "Боб" = "33", "Леща" = "34", "Ориз" = "35", "Макарони" = "36",
                                    "Спагети" = "37", "Захар" = "38", "Сол" = "39", "Типов хляб" = "4",
                                    "Брашно тип 500" = "40", "Брашно екстра" = "41", "Олио" = "42",
                                    "Маслиново масло" = "43", "Винен оцет" = "44", "Ябълков оцет" = "45",
                                    "Бял боб (консерва)" = "46", "Зелен грах (консерва)" = "47",
                                    "Домати (консерва)" = "48", "Лютеница" = "49", "Точени кори" = "5",
                                    "Лимони" = "50", "Портокали" = "51", "Банани" = "52", "Ябълки" = "53",
                                    "Домати" = "54", "Кромид лук" = "55", "Моркови" = "56", "Зеле" = "57",
                                    "Краставици" = "58", "Чесън" = "59", "Прясно мляко" = "6",
                                    "Гъби" = "60", "Картофи" = "61", "Маслини" = "62", "Каша" = "63",
                                    "Пюре" = "64", "Сухо мляко" = "65", "Бисквити" = "66",
                                    "Кроасани" = "67", "Банички" = "68", "Шоколади" = "69",
                                    "Кисело мляко" = "7", "Мляно кафе" = "70", "Кафе на зърна" = "71",
                                    "Чай" = "72", "Минерална вода" = "73", "Бира" = "74", "Бяло вино" = "75",
                                    "Червено вино" = "76", "Ракия" = "77", "Цигари" = "78", 
                                    "Препарат за съдове" = "79", "Краве сирене" = c("8", "9"), "Четки за зъби" = "80",
                                    "Паста за зъби" = "81", "Шампоани" = "82", "Сапуни" = "83", 
                                    "Мокри кърпички" = "84", "Тоалетна хартия" = "85"), .after = kategoria) %>% 
  mutate(kategoria = as.numeric(kategoria)) %>% 
  filter(!cena_na_drebno == 0) %>% 
  arrange(date, kategoria)

df_market <- read_parquet("df_market.parquet") %>% mutate(
  date = as.character(date),
  product = str_replace(product, "\\\n", " ")) %>% 
  mutate(product = fct_recode(product, 'Брашно тип "500" /пакет 1 кг/' = 'Брашно тип"500" /пакет 1 кг/'),
         product = fct_recode(product, "Брашно тип \"500\" /пакет 1 кг/" = "Брашно тип \"500\" /пакет 1 кг/ "),
         product = fct_recode(product, "Прясно мляко 3% кутия/бутилка 1 л" = "Прясно мляко 3%  кутия/бутилка 1 л"))

colors_percent <- c("TRUE" = "#00BFC4", "FALSE" = "#F8766D")
#----------------------------------------
mail <- tags$a(icon("envelope"), "Email", 
               href = "mailto:nickydyakov@gmail.com", 
               tagret = "_blank")
github <- tags$a(icon("github"), "Github", 
                 href = "https://github.com/NickyDy", 
                 tagret = "_blank")
#------------------------------------------------
ui <- page_fillable(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  navset_pill(
    nav_panel(
      title = "Супермаркети (таблица)",
      DTOutput("markets_table", width = 1850)),
    nav_panel(title = "Инфлация (продукти)",
              layout_columns(
                dateRangeInput("inf_date", "Дата:",
                  start = "2026-01-01",
                  end = last(df_markets$date),
                  min = first(df_markets$date),
                  max = last(df_markets$date),
                  separator = " до ",
                  weekstart = 1,
                  language = "bg"),
                sliderInput("height", "Височина на графиката:",
                min = 800, max = 3000, value = 800, step = 100),
              col_widths = c(2, 2)),
              layout_columns(
                plotOutput("inf_plot"),
                col_widths = c(12))),
    nav_panel(title = "Инфлация (по ключова дума)",
              layout_columns(
                selectInput("key_market", "Супермаркет:",
                            choices = c("Кауфланд", "Лидл", "Билла", "T Market", "Славекс", "Вилтон")),
                textInput("key_input", "Ключова дума/думи:", value = "", 
                          placeholder = "ключова дума"),
                col_widths = c(2)),
              layout_columns(
                plotOutput("key_plot"),
                col_widths = c(12))),
    nav_panel(title = "Инфлация (групи храни)",
              layout_columns(
                dateRangeInput("inf_markets_date", "Дата:",
                               start = "2026-01-01",
                               end = last(df_markets$date),
                               min = first(df_markets$date),
                               max = last(df_markets$date),
                               separator = " до ",
                               weekstart = 1,
                               language = "bg"),
                sliderInput("height_markets", "Височина на графиката:",
                            min = 800, max = 3000, value = 1000, step = 100),
                col_widths = c(2, 2)),
              layout_columns(
                plotOutput("inf_markets_plot"),
                col_widths = c(12))),
    nav_panel(title = "Групи храни (време)",
              layout_columns(
                selectInput("cat_select", "Категория:",
                            choices = unique(df_markets$kategoria_c)),
                col_widths = c(2)),
              layout_columns(
                plotOutput("time_plot"),
                col_widths = c(12))),
    nav_panel(
      title = "Борса (таблица)",
      DTOutput("market_foods", width = 1850)),
    nav_panel(
      title = "Инфлация (борса)", layout_columns(
        selectInput("market_date_first", "От дата:",
                    choices = unique(df_market$date),
                    selected = first(df_market$date)),
        selectInput("market_date_last", "До дата:",
                    choices = unique(df_market$date),
                    selected = last(df_market$date)),
        col_widths = c(1, 1)),
      plotOutput("market_inf_plot")),
    nav_panel(
      title = "Тренд (борса)", layout_columns(
        selectInput("market_unit_trend", "Грамаж:",
                    choices = unique(df_market$unit)),
        selectInput("market_product_trend", "Продукт:",
                    choices = NULL),
        col_widths = c(1, 3)),
      plotOutput("market_trend_plot")),
    nav_spacer(),
    nav_menu(
      title = "Links",
      nav_item(mail),
      nav_item(github)
    )
  )
)
#-------------------------------------
server <- function(input, output, session) {
  
  output$markets_table <- renderDT(
    
    df_markets %>% 
      select(date, market, kategoria_c, naimenovanie_na_produkta,
             cena_na_drebno, cena_v_promocia) %>% 
      arrange(date) %>%
      datatable(
        rownames = F, filter = "top",
        colnames = c(
          "Дата" = "date",
          "Супермаркет" = "market",
          "Категория" = "kategoria_c",
          "Продукт" = "naimenovanie_na_produkta",
          "Цена на дребно" = "cena_na_drebno",
          "Цена в промоция" = "cena_v_promocia"),
        options = list(dom = "frtip", pageLength = 15))
    
  )
  
  output$inf_plot <- renderPlot({
    
    df_markets %>%
      mutate(naimenovanie_na_produkta = str_remove(naimenovanie_na_produkta, "___.+$")) %>%
      filter(date %in% c(input$inf_date[1], input$inf_date[2]), !cena_na_drebno == 0) %>% 
      summarise(
        price_change = (last(cena_na_drebno) - first(
          cena_na_drebno)) / first(cena_na_drebno) * 100,
        .by = c(market, naimenovanie_na_produkta)) %>%
      filter(!between(price_change, -1, 1)) %>%
      mutate(naimenovanie_na_produkta = fct_reorder(
        naimenovanie_na_produkta, price_change), col = price_change > 0,
             market = fct_relevel(market, "Кауфланд", "Лидл", "Билла", "T Market", "Славекс", "Вилтон")) %>% 
      ggplot(aes(price_change, naimenovanie_na_produkta, fill = col)) +
      geom_col(show.legend = F) +
      scale_fill_manual(values = colors_percent) +
      scale_x_continuous(expand = expansion(mult = c(0.01, 0.3))) +
      geom_text(aes(label = paste0(round(price_change, 0), "%")),
                position = position_dodge(width = 1), hjust = -0.1, size = 3.5) +
      labs(y = NULL, x = NULL) +
      theme(text = element_text(size = 14), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
      facet_wrap(vars(market), scales = "free_y")
    
}, height = function() input$height, width = 1800, res = 96)
  
  debounced <- reactive({input$key_input}) %>% debounce(1000)
  
  online <- reactive({
    
    validate(need(input$key_input != "", "Моля, въведете ключова дума на кирилица!"))
    
    df_markets %>% 
      filter(market == input$key_market,
             str_detect(naimenovanie_na_produkta, regex(debounced(), ignore_case = T)))
  })
  
  output$key_plot <- renderPlot({
    
    online() %>%
      mutate(naimenovanie_na_produkta = str_remove(naimenovanie_na_produkta, "___.+$"),
             date = ymd(date)) %>% 
     mutate(naimenovanie_na_produkta = str_wrap(naimenovanie_na_produkta, 30)) %>%
      pivot_longer(7:8) %>% drop_na(value) %>%
      mutate(market = glue::glue(" <span style='color:blue'>**{market}**</span>")) %>% 
      unite(c(market, naimenovanie_na_produkta), col = "market_product", sep = ": ") %>%
      ggplot(aes(date, value, group = name, color = name)) +
      geom_point(size = 1) +
      geom_line(linewidth = 0.3, linetype = 2) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b") +
      scale_color_manual(values = c("black", "red"), 
                         labels = c("Цена на дребно", "Цена в промоция")) +
      theme(text = element_text(size = 14), legend.position = "top",
            strip.text = element_markdown()) +
      labs(y = "Цена (евро)", x = "Дата", color = "Легенда:") +
      facet_wrap(vars(market_product))
    
  }, height = 800, width = 1800, res = 96)
  
output$inf_markets_plot <- renderPlot({
  
df_markets %>%
    filter(date %in% c(input$inf_markets_date[1], input$inf_markets_date[2]), !cena_na_drebno == 0) %>%
    summarise(cena_na_drebno = mean(cena_na_drebno, na.rm = T), .by = c(market, kategoria_c, date)) %>%
    summarise(
      price_change = (last(cena_na_drebno) - first(cena_na_drebno)) / first(cena_na_drebno) * 100,
      .by = c(market, kategoria_c)) %>%
    filter(price_change != 0) %>%
    mutate(
      market = fct_relevel(market, "Кауфланд", "Лидл", "Билла", "T Market", "Славекс", "Вилтон"),
      kategoria_c = fct_rev(kategoria_c),
      col = price_change > 0) %>%
    ggplot(aes(price_change, kategoria_c, fill = col)) +
    geom_col(show.legend = F) +
    scale_fill_manual(values = colors_percent) +
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.4))) +
    geom_text(aes(label = paste0(round(price_change, 2), "%")),
              position = position_dodge(width = 1), hjust = -0.1, size = 4) +
    labs(y = NULL, x = NULL) +
    theme(text = element_text(size = 16),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    facet_wrap(vars(market), nrow = 1)

}, height = function() input$height_markets, width = 1800, res = 96)

output$time_plot <- renderPlot({
  
  df_markets %>%
    mutate(
      market = fct_relevel(market, "Кауфланд", "Лидл", "Билла", "T Market", "Славекс", "Вилтон"),
      date = ymd(date),
      cena_v_promocia = as.numeric(cena_v_promocia)) %>%
    filter(kategoria_c == input$cat_select) %>%
    summarise(cena_na_drebno = mean(cena_na_drebno, na.rm = T),
              cena_v_promocia = mean(cena_v_promocia, na.rm = T),
              .by = c(market, kategoria_c, date)) %>%
    # filter(cena_na_drebno != 0) %>%
    pivot_longer(4:5) %>% drop_na(value) %>%
    ggplot(aes(date, value, group = name, color = name)) +
    geom_point() +
    geom_line(linewidth = 0.3, linetype = 2) +
    scale_color_manual(values = c("black", "red"), 
                       labels = c("Цена на дребно", "Цена в промоция")) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    labs(y = "Средна цена (евро)", x = "Дата", color = "Легенда:") +
    theme(text = element_text(size = 14), legend.position = "top") +
    facet_wrap(vars(market), nrow = 1)
  
}, height = 800, width = 1800, res = 96)

output$market_foods <- renderDT(
  
  df_market %>% arrange(date) %>%
    datatable(
      rownames = F, filter = "top",
      colnames = c(
        "Дата" = "date",
        "Продукт" = "product",
        "Грамаж" = "unit",
        "Цена (лв)" = "price"),
      options = list(dom = "frtip", pageLength = 15))
  
)

df_market_plot <- reactive(
  df_market %>%
    filter(date %in% c(input$market_date_first, input$market_date_last)) %>%
    summarise(
      price_change = (last(price, na_rm = T) - first(price, na_rm = T)) / first(price, na_rm = T),
      .by = c(unit, product)
    ) %>%
    filter(price_change != 0) %>%
    mutate(product = fct_reorder(product, price_change))
)

inf_sum <- reactive(
  df_market %>%
    filter(date %in% c(input$market_date_first, input$market_date_last)) %>%
    summarise(
      price_change = (last(price, na_rm = T) - first(price, na_rm = T)) / first(price, na_rm = T),
      .by = c(unit, product)
    ) %>%
    filter(price_change != 0) %>%
    summarise(sum_inflation = sum(price_change)) %>% pull()
)

output$market_inf_plot <- renderPlot({
    df_market_plot() %>%
      ggplot(aes(price_change, product, fill = price_change > 0)) +
      geom_col(show.legend = F) +
      geom_text(aes(label = paste0(round(price_change * 100, 2), "%")), hjust = -0.03) +
      scale_x_continuous(expand = expansion(mult = c(0.01, 0.1))) +
      theme(
        text = element_text(size = 14), axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
      labs(
        x = NULL, y = NULL,
        title = paste0("Изчислена инфлация от ", input$market_date_first, " до ", input$market_date_last),
        subtitle = paste0("Обща инфлация: ", round(inf_sum(), 2), "%")
      )
  }, height = 800, width = 1850, res = 96)

market_unit_trend <- reactive({
  filter(df_market, unit == input$market_unit_trend)
})

observeEvent(market_unit_trend(), {
  freezeReactiveValue(input, "market_product_trend")
  choices <- unique(market_unit_trend()$product)
  updateSelectInput(inputId = "market_product_trend", choices = choices)
})

market_product_trend <- reactive({
  req(input$market_product_trend)
  filter(market_unit_trend(), product == input$market_product_trend)
})

output$market_trend_plot <- renderPlot({
    market_product_trend() %>%
      mutate(date = ymd(date), m = month(date)) %>%
      ggplot(aes(date, price)) +
      #geom_smooth(method = "loess", se = F) +
      geom_line(linetype = 2) +
      geom_point(size = 2) +
      scale_x_date(breaks = "1 month", date_labels = "%b-%Y") +
      theme(text = element_text(size = 16)) +
      labs(x = "Дата", y = "Цена (лв)")
  }, height = 800, width = 1850, res = 96)
  
session$onSessionEnded(function() {stopApp()})
}

shinyApp(ui, server)
