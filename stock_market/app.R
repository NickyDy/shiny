library(tidyverse)
library(nanoparquet)
library(shiny)
library(bslib)
library(DT)

df <- read_parquet("df.parquet") %>% mutate(date = as.character(date), 
                                            product = str_replace(product, "\\\n", " "))
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
  navset_pill(
    nav_panel(title = "Храни (таблица)",
              DTOutput("foods", width = 1850)),
    nav_panel(title = "Инфлация", layout_columns(
      selectInput("date_first", "От дата:",
                  choices = unique(df$date),
                  selected = first(df$date)),
      selectInput("date_last", "До дата:",
                  choices = unique(df$date),
                  selected = last(df$date)),
      col_widths = c(1, 1)),
      plotOutput("inf_plot")),
    nav_panel(title = "Ценови тренд", layout_columns(
      selectInput("unit_trend", "Грамаж:",
                  choices = unique(df$unit)),
      selectInput("product_trend", "Продукт:",
                  choices = NULL),
      col_widths = c(1, 3)),
      plotOutput("trend_plot")),
    nav_spacer(),
    nav_menu(
      title = "Links",
      nav_item(mail),
      nav_item(github))
  ))
#-----------------------------------------------
server <- function(input, output, session) {

output$foods <- renderDT(
    df %>% arrange(date) %>% 
      datatable(rownames = F, filter = "top", 
                colnames = c("Дата" = "date",
                             "Продукт" = "product",
                             "Грамаж" = "unit",
                             "Цена (лв)" = "price"),
                options = list(dom = 'frtip', pageLength = 15)))
  #-------------------------------------------------------------
df_plot <- reactive(
  df %>% 
  filter(date %in% c(input$date_first, input$date_last)) %>%
  summarise(price_change = (last(price, na_rm = T) - first(price, na_rm = T)) / first(price, na_rm = T), 
            .by = c(unit, product)) %>% 
  filter(price_change != 0) %>%
  mutate(product = fct_reorder(product, price_change))
)

inf_mean <- reactive(
  df %>% 
    filter(date %in% c(input$date_first, input$date_last)) %>%
    summarise(price_change = (last(price, na_rm = T) - first(price, na_rm = T)) / first(price, na_rm = T), 
              .by = c(unit, product)) %>% 
    filter(price_change != 0) %>%
    summarise(mean_inflation = mean(price_change)) %>% pull()
)

 output$inf_plot <- renderPlot({
    
   df_plot() %>% 
     ggplot(aes(price_change, product, fill = price_change > 0)) +
     geom_col(show.legend = F) +
     geom_text(aes(label = paste0(round(price_change * 100, 2), "%")), hjust = -0.03) +
     scale_x_continuous(expand = expansion(mult = c(0.01, 0.1))) +
     theme(text = element_text(size = 14), axis.text.x = element_blank(), 
           axis.ticks.x = element_blank()) +
     labs(x = NULL, y = NULL,
          title = paste0("Изчислена инфлация от ", input$date_first, " до ", input$date_last, "\n",
                         "Средна инфлация: ", round(inf_mean(), 2), "%"))
    
  }, height = 850, width = 1850, res = 96)
  #-----------------------------------------------------------
unit_trend <- reactive({
  filter(df, unit == input$unit_trend)
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

output$trend_plot <- renderPlot({
    
  product_trend() %>%
      mutate(date = ymd(date), group = month(date)) %>%
      ggplot(aes(date, price, group = group)) +
      geom_line(linetype = 2, linewidth = 0.3) +
      geom_point(size = 2) +
      theme(text = element_text(size = 16)) +
      labs(x = "Дата", y = "Цена (лв)")
    
  }, height = 850, width = 1850, res = 96)
  #----------------------------------------------------------------------
  session$onSessionEnded(function() {
    stopApp()
  })
}

shinyApp(ui, server)
