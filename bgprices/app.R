library(tidyverse)
library(nanoparquet)
library(shiny)
library(bslib)
library(DT)

df_2025 <- read_rds("df_2025.rds") %>%
  mutate(
    date = str_replace(date, "2025-02-06", "2025-02-08"),
    price = round(price, 2)
  ) %>%
  arrange(date)
kaufland <- read_rds("kaufland.rds") %>%
  mutate(price = round(price, 2)) %>%
  arrange(date) %>%
  drop_na(price, product)
df_market <- read_parquet("df_market.parquet") %>% mutate(
  date = as.character(date),
  product = str_replace(product, "\\\n", " ")
)
food_levels <- c(
  "Zasiti", "VMV", "Taraba", "T MARKET",
  "Superbag", "Shop24", "Gladen",
  "BulMag", "Морски дар", "Randi",
  "Rusebag", "Бакалийка"
)
food_colors <- c(
  "#984ea3", "#4daf4a", "#e41a1c", "#8dd3c7",
  "#ffed6f", "#fb8072", "darkgreen",
  "#fdb462", "#b3de69", "blue", "pink",
  "midnightblue"
)
colors_percent <- c("TRUE" = "#00BFC4", "FALSE" = "#F8766D")
#-----------------------------------------------------------
mail <- tags$a(icon("envelope"), "Email",
  href = "mailto:nickydyakov@gmail.com",
  tagret = "_blank"
)
github <- tags$a(icon("github"), "Github",
  href = "https://github.com/NickyDy",
  tagret = "_blank"
)
#---------------------------------------------------------
ui <- page_fillable(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  navset_pill(
    nav_panel(
      title = "Храни",
      DTOutput("foods", width = 1850)
    ),
    nav_panel(
      title = "Инфлация (хранителни продукти)", layout_columns(
        selectInput("date_first", "От дата:",
          choices = unique(df_2025$date),
          selected = first(df_2025$date)
        ),
        selectInput("date_last", "До дата:",
          choices = unique(df_2025$date),
          selected = last(df_2025$date)
        ),
        selectInput("inf_price_source", "Източник на цените:",
          choices = unique(df_2025$source)
        ),
        selectInput("inf_location", "Населено място:", choices = NULL),
        selectInput("inf_type", "Продуктова група:", choices = NULL),
        selectInput("inf_unit", "Грамаж:", choices = NULL),
        sliderInput("height", "Височина на графиката:",
          min = 800, max = 7000, value = 800, step = 100
        ),
        col_widths = c(1, 1, 2, 2, 2, 2, 2)
      ),
      plotOutput("inf_price_change")
    ),
    nav_panel(
      title = "Инфлация (продуктови групи)", layout_columns(
        selectInput("date_first_group", "От дата:",
          choices = unique(df_2025$date),
          selected = first(df_2025$date)
        ),
        selectInput("date_last_group", "До дата:",
          choices = unique(df_2025$date),
          selected = last(df_2025$date)
        ),
        selectInput("inf_price_source_group", "Източник на цените:",
          choices = unique(df_2025$source)
        ),
        col_widths = c(1, 1, 2)
      ),
      plotOutput("inf_price_group")
    ),
    nav_panel(
      title = "Средна инфлация", layout_columns(
        selectInput("date_first_total", "От дата:",
          choices = unique(df_2025$date),
          selected = first(df_2025$date)
        ),
        selectInput("date_last_total", "До дата:",
          choices = unique(df_2025$date),
          selected = last(df_2025$date)
        ),
        col_widths = c(1, 1)
      ),
      plotOutput("inf_price_total")
    ),
    nav_panel(
      title = "Ценови тренд", layout_columns(
        selectInput("location_trend", "Локация:",
          choices = unique(df_2025$location)
        ),
        selectInput("source_trend", "Източник:",
          choices = NULL
        ),
        selectInput("type_trend", "Продуктова група:",
          choices = NULL
        ),
        selectInput("unit_trend", "Грамаж:",
          choices = NULL
        ),
        selectInput("product_trend", "Продукт:",
          choices = NULL
        ),
        col_widths = c(2, 2, 2, 2, 4)
      ),
      plotOutput("plot_trend")
    ),
    nav_panel(
      title = "Kaufland (таблица)",
      DTOutput("kaufland_table", width = 1850)
    ),
    nav_panel(
      title = "Kaufland (инфлация)", layout_columns(
        selectInput("kaufland_first_date", "От дата:",
          choices = unique(kaufland$date),
          selected = first(kaufland$date)
        ),
        selectInput("kaufland_last_date", "До дата:",
          choices = unique(kaufland$date),
          selected = last(kaufland$date)
        ),
        sliderInput("kaufland_height", "Височина на графиката:",
          min = 800, max = 7000, value = 800, step = 100
        ),
        col_widths = c(1, 1, 2)
      ),
      plotOutput("kaufland_plot")
    ),
    nav_panel(
      title = "Kaufland (тренд)", layout_columns(
      selectInput("kaufland_trend_product", "Продукт:",
                  choices = unique(kaufland$product)),
      col_widths = c(4)
      ),
      plotOutput("kaufland_trend_plot")
    ),
    nav_panel(
      title = "Борса (таблица)",
      DTOutput("market_foods", width = 1850)
    ),
    nav_panel(
      title = "Инфлация (борса)", layout_columns(
        selectInput("market_date_first", "От дата:",
          choices = unique(df_market$date),
          selected = first(df_market$date)
        ),
        selectInput("market_date_last", "До дата:",
          choices = unique(df_market$date),
          selected = last(df_market$date)
        ),
        col_widths = c(1, 1)
      ),
      plotOutput("market_inf_plot")
    ),
    nav_panel(
      title = "Тренд (борса)", layout_columns(
        selectInput("market_unit_trend", "Грамаж:",
          choices = unique(df_market$unit)
        ),
        selectInput("market_product_trend", "Продукт:",
          choices = NULL
        ),
        col_widths = c(1, 3)
      ),
      plotOutput("market_trend_plot")
    ),
    nav_panel(
      tags$img(src = "shiny.png", width = 40),
      "Други полезни приложения:",
      tags$a(
        href = "https://nickydy.shinyapps.io/elections/", br(),
        "Избори в България!"
      ), br(),
      tags$a(
        href = "https://nickydy.shinyapps.io/climate/",
        "Климатът на България!"
      ), br(),
      tags$a(
        href = "https://nickydy.shinyapps.io/demography/",
        "Демография на България!"
      ), br(),
      tags$a(
        href = "https://nickydy.shinyapps.io/inflation/",
        "Inflation in EU"
      ), br(),
      tags$a(
        href = "https://ndapps.shinyapps.io/agri/",
        "Цени на селскостопанска продукция в ЕС!"
      ), br(),
      tags$a(
        href = "https://nickydy.shinyapps.io/eurostat/",
        "Евростат за България!"
      ), br(),
      tags$a(
        href = "https://ndapps.shinyapps.io/und_water/",
        "Чистота на водите в България"
      ), br()
    ),
    nav_spacer(),
    nav_menu(
      title = "Links",
      nav_item(mail),
      nav_item(github)
    )
  )
)
#-----------------------------------------------
server <- function(input, output, session) {
  output$foods <- renderDT(
    df_2025 %>% arrange(price) %>%
      datatable(
        rownames = F, filter = "top",
        colnames = c(
          "Дата" = "date",
          "Населено място" = "location",
          "Супермаркет" = "source",
          "Продуктова група" = "type",
          "Продукт" = "product",
          "Грамаж" = "unit",
          "Цена (лв)" = "price"
        ),
        options = list(dom = "frtip", pageLength = 15)
      ) %>%
      formatStyle("Супермаркет", backgroundColor = styleEqual(food_levels, food_colors))
  )

  output$kaufland_table <- renderDT(
    kaufland %>% arrange(price) %>%
      datatable(
        rownames = F, filter = "top",
        colnames = c(
          "Дата" = "date",
          "Населено място" = "location",
          "Супермаркет" = "source",
          "Продукт" = "product",
          "Грамаж" = "unit",
          "Цена (лв)" = "price"
        ),
        options = list(dom = "frtip", pageLength = 15)
      )
  )

  output$market_foods <- renderDT(
    df_market %>% arrange(date) %>%
      datatable(
        rownames = F, filter = "top",
        colnames = c(
          "Дата" = "date",
          "Продукт" = "product",
          "Грамаж" = "unit",
          "Цена (лв)" = "price"
        ),
        options = list(dom = "frtip", pageLength = 15)
      )
  )
  #-------------------------------------------------------------------------------------
  new_old_df <- reactive({
    df_2025 %>%
      filter(date %in% c(input$date_first, input$date_last)) %>%
      summarise(
        price_change = (last(price, na_rm = T) - first(price, na_rm = T)) / first(price, na_rm = T),
        .by = c(location, source, type, unit, product)
      ) %>%
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

  output$inf_price_change <- renderPlot(
    {
      inf_unit() %>%
        ggplot(aes(price_change, product, fill = col)) +
        geom_col(show.legend = F) +
        scale_fill_manual(values = colors_percent) +
        scale_x_continuous(expand = expansion(mult = c(0.01, 0.10))) +
        geom_text(aes(label = scales::percent(price_change, accuracy = 1)),
          position = position_dodge(width = 1), hjust = -0.1, size = 3.5
        ) +
        labs(
          y = NULL, x = NULL,
          title = glue::glue(
            "Промяна в цената на продуктите от {input$date_first} до ",
            "{input$date_last}"
          )
        ) +
        theme(
          text = element_text(size = 14), axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
    },
    height = function() input$height,
    width = 1550,
    res = 96
  )
  #-------------------------------------------------------------
  inf_group <- reactive({
    df_2025 %>%
      filter(date %in% c(input$date_first_group, input$date_last_group), source == input$inf_price_source_group) %>%
      summarise(
        price_change = (last(price, na_rm = T) - first(price, na_rm = T)) / first(price, na_rm = T),
        .by = c(location, source, type, unit, product)
      ) %>%
      summarise(price_change = mean(price_change, na.rm = T) * 100, .by = c(source, type)) %>%
      filter(price_change != 0) %>%
      mutate(type = fct_reorder(type, price_change), col = price_change > 0)
  })

  inf_group_total <- reactive({
    inf_group() %>%
      summarise(mean_group = round(mean(price_change), 2))
  })

  output$inf_price_group <- renderPlot(
    {
      inf_group() %>%
        ggplot(aes(price_change, type, fill = col)) +
        geom_col(show.legend = F) +
        scale_fill_manual(values = colors_percent) +
        scale_x_continuous(expand = expansion(mult = c(0.01, 0.10))) +
        geom_text(aes(label = paste0(round(price_change, 2), "%")),
          position = position_dodge(width = 1), hjust = -0.1, size = 3.5
        ) +
        labs(
          y = NULL, x = NULL,
          title = glue::glue(
            "Средна инфлация по продуктови групи в {input$inf_price_source_group} от {input$date_first_group} до ",
            "{input$date_last_group}"
          ),
          subtitle = glue::glue("Средна инфлация за супермаркета: {inf_group_total()$mean_group}%")
        ) +
        theme(
          text = element_text(size = 14), axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
    },
    height = 800,
    width = 1550,
    res = 96
  )
  #---------------------------------------
  inf_total <- reactive({
    df_2025 %>%
      filter(date %in% c(input$date_first_total, input$date_last_total)) %>%
      summarise(
        price_change = (last(price, na_rm = T) - first(price, na_rm = T)) / first(price, na_rm = T),
        .by = c(location, source, type, unit, product)
      ) %>%
      summarise(price_change = mean(price_change, na.rm = T) * 100, .by = c(type)) %>%
      filter(price_change != 0) %>%
      mutate(type = fct_reorder(type, price_change), col = price_change > 0)
  })

  inf_total_total <- reactive({
    inf_total() %>%
      summarise(mean_total = round(mean(price_change), 2))
  })

  output$inf_price_total <- renderPlot(
    {
      inf_total() %>%
        ggplot(aes(price_change, type, fill = col)) +
        geom_col(show.legend = F) +
        scale_fill_manual(values = colors_percent) +
        scale_x_continuous(expand = expansion(mult = c(0.01, 0.10))) +
        geom_text(aes(label = paste0(round(price_change, 2), "%")),
          position = position_dodge(width = 1), hjust = -0.1, size = 3.5
        ) +
        labs(
          y = NULL, x = NULL,
          title = glue::glue(
            "Средна инфлация по продуктови групи от {input$date_first_total} до ",
            "{input$date_last_total}"
          ),
          subtitle = glue::glue("Обща средна инфлация: {inf_total_total()$mean_total}%")
        ) +
        theme(
          text = element_text(size = 14), axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
    },
    height = 800,
    width = 1550,
    res = 96
  )
  #-----------------------------------------
  location_trend <- reactive({
    filter(df_2025, location == input$location_trend)
  })
  
  observeEvent(location_trend(),{
    freezeReactiveValue(input, "source_trend")
    choices <- unique(location_trend()$source)
    updateSelectInput(inputId = "source_trend", choices = choices)
  })
  
  source_trend <- reactive({
    req(input$source_trend)
    filter(location_trend(), source == input$source_trend)
  })
  
  observeEvent(source_trend(), {
    freezeReactiveValue(input, "type_trend")
    choices <- unique(source_trend()$type)
    updateSelectInput(inputId = "type_trend", choices = choices)
  })
  
  type_trend <- reactive({
    req(input$type_trend)
    filter(source_trend(), type == input$type_trend)
  })
  
  observeEvent(type_trend(), {
    freezeReactiveValue(input, "unit_trend")
    choices <- unique(type_trend()$unit)
    updateSelectInput(inputId = "unit_trend", choices = choices)
  })
  
  unit_trend <- reactive({
    req(input$unit_trend)
    filter(type_trend(), unit == input$unit_trend)
  })
  
  observeEvent(unit_trend(), {
    freezeReactiveValue(input, "product_trend")
    choices <- unique(unit_trend()$product)
    updateSelectInput(inputId = "product_trend", choices = choices)
  })
  
  product_trend <- reactive({
    req(input$product_trend)
    filter(unit_trend(), product == input$product_trend)
  })
  
output$plot_trend <- renderPlot({
      product_trend() %>%
        mutate(date = ymd(date)) %>%
        ggplot(aes(date, price)) +
        geom_point(show.legend = F, size = 2) +
        geom_smooth(linewidth = 1, se = F, method = "loess") +
        scale_x_date(breaks = "1 month", date_labels = "%b-%Y") +
        labs(y = "Цена (лв)", x = NULL, title = input$product_trend) +
        theme(text = element_text(size = 14))
    },
    height = 800,
    width = 1850,
    res = 96
  )
#-------------------------------------
  output$kaufland_plot <- renderPlot(
    {
      kaufland %>%
        filter(date %in% c(input$kaufland_first_date, input$kaufland_last_date)) %>%
        summarise(
          price_change = (last(price, na_rm = T) - first(price, na_rm = T)) / first(price, na_rm = T),
          .by = c(product)
        ) %>%
        filter(price_change != 0) %>%
        mutate(product = fct_reorder(product, price_change), col = price_change > 0) %>%
        ggplot(aes(price_change, product, fill = col)) +
        geom_col(show.legend = F) +
        scale_fill_manual(values = colors_percent) +
        scale_x_continuous(expand = expansion(mult = c(0.01, 0.10))) +
        geom_text(aes(label = scales::percent(price_change, accuracy = 1)),
          position = position_dodge(width = 1), hjust = -0.1, size = 3.5
        ) +
        labs(
          y = NULL, x = NULL,
          title = glue::glue(
            "Промяна в цената на продуктите от {input$kaufland_first_date} до ",
            "{input$kaufland_last_date}"
          )
        ) +
        theme(
          text = element_text(size = 14), axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )
    },
    height = function() input$kaufland_height,
    width = 1550,
    res = 96
  )

output$kaufland_trend_plot <- renderPlot({
  kaufland %>%
    mutate(date = ymd(date)) %>%
    filter(product == input$kaufland_trend_product) %>% 
    ggplot(aes(date, price)) +
    geom_point(show.legend = F, size = 2) +
    geom_smooth(linewidth = 1, se = F, method = "loess") +
    scale_x_date(breaks = "1 month", date_labels = "%b-%Y") +
    labs(y = "Цена (лв)", x = NULL, title = input$kaufland_trend_product) +
    theme(text = element_text(size = 14))
},
height = 800,
width = 1850,
res = 96
)
#----------------------------------------------------------------------
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

  inf_mean <- reactive(
    df_market %>%
      filter(date %in% c(input$market_date_first, input$market_date_last)) %>%
      summarise(
        price_change = (last(price, na_rm = T) - first(price, na_rm = T)) / first(price, na_rm = T),
        .by = c(unit, product)
      ) %>%
      filter(price_change != 0) %>%
      summarise(mean_inflation = mean(price_change)) %>% pull()
  )

  output$market_inf_plot <- renderPlot(
    {
      df_market_plot() %>%
        ggplot(aes(price_change, product, fill = price_change > 0)) +
        geom_col(show.legend = F) +
        geom_text(aes(label = paste0(round(price_change * 100, 2), "%")), hjust = -0.03) +
        scale_x_continuous(expand = expansion(mult = c(0.01, 0.1))) +
        theme(
          text = element_text(size = 14), axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        ) +
        labs(
          x = NULL, y = NULL,
          title = paste0("Изчислена инфлация от ", input$market_date_first, " до ", input$market_date_last),
          subtitle = paste0("Средна инфлация: ", round(inf_mean(), 3), "%")
        )
    },
    height = 800,
    width = 1850,
    res = 96
  )

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

  output$market_trend_plot <- renderPlot(
    {
      market_product_trend() %>%
        mutate(date = ymd(date), m = month(date)) %>%
        ggplot(aes(date, price, group = m)) +
        geom_smooth(linewidth = 1, se = F) +
        geom_point(size = 2) +
        scale_x_date(breaks = "1 month", date_labels = "%b-%Y") +
        theme(text = element_text(size = 16)) +
        labs(x = "Дата", y = "Цена (лв)")
    },
    height = 800,
    width = 1850,
    res = 96
  )

  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui, server)
