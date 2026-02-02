library(shiny)
library(tidyverse)
library(eurostat)
library(bslib)
library(nanoparquet)

inf <- read_parquet("prc_hicp_mmor.parquet") %>% 
  filter(!str_detect(geo, "^Euro")) %>% 
  arrange(TIME_PERIOD) %>% drop_na(values)

mail <- tags$a(icon("envelope"), "Email", href = "mailto:nickydyakov@gmail.com", tagret = "_blank")
github <- tags$a(icon("github"), "Github", href = "https://github.com/NickyDy", tagret = "_blank")

ui <- page_fillable(
  #title = h3("Inflation in EU!"),
  theme = bslib::bs_theme(bootswatch = "darkly"),
   navset_pill(
    nav_panel("Инфлация по държави", layout_columns(
              selectInput("item", "Показател:", 
                          choices = unique(inf$coicop),
                          selected = "All-items HICP"),
              dateRangeInput("date", "Дата:",
                             weekstart = 1, separator = "до",
                             start = first(inf$TIME_PERIOD), end = last(inf$TIME_PERIOD),
                             min = first(inf$TIME_PERIOD), max = last(inf$TIME_PERIOD)),
              col_widths = c(4, 2)),
              plotOutput("price")),
    nav_panel("Инфлация във времето", layout_columns(
              selectInput("item_time", "Показател:", 
                          choices = unique(inf$coicop),
                          selected = "All-items HICP"),
              selectInput("country", "Държава:", 
                          choices = unique(inf$geo), 
                          selected = "Bulgaria"),
              col_widths = c(2, 2)),
              plotOutput("accum")),
    nav_panel(tags$img(src = "shiny.png", width = 40),
              "Other useful apps:",
              tags$a(href = "https://nickydy.shinyapps.io/elections/", br(),
                     "Bulgarian elections!"), br(),
              tags$a(href = "https://nickydy.shinyapps.io/climate/",
                     "Bulgarian climate!"), br(),
              tags$a(href = "https://nickydy.shinyapps.io/demography/",
                     "Bulgarian demography!"), br(),
              # tags$a(href = "https://ndapps.shinyapps.io/bgprices/",
              #        "Comparing prices in Bulgaria!"), br(),
              tags$a(href = "https://ndapps.shinyapps.io/agri/",
                     "Prices of agricultural products in EU!"), br(),
              tags$a(href = "https://nickydy.shinyapps.io/eurostat/",
                     "Eurostat for Bulgaria!"), br(),
              tags$a(href = "https://ndapps.shinyapps.io/und_water/",
                     "Water purity in Bulgaria!"), br()),
    # nav_panel(tags$img(src = "kofi.png", width = 40),
    #           "If you like the app,
    #            you can support me by donating \u20AC to
    #            the following account:",
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

server <- function(input, output, session) {
  
  prices <- reactive({
	  inf %>%
		filter(coicop == input$item, 
		       TIME_PERIOD >= input$date[1] & TIME_PERIOD <= input$date[2]) %>% 
		group_by(coicop, geo) %>% 
		summarise(sm = sum(values, na.rm = T)) %>%
		ungroup() %>% 
		mutate_if(is.numeric, round, 1) %>% 
    mutate(col = if_else(geo == "Bulgaria", "0", "1"),
           geo = fct_reorder(geo, sm))
	  })
	
	output$price <- renderPlot({
	  
	  prices() %>%
	    ggplot(aes(sm, geo, fill = col)) +
	    geom_col(show.legend = F) +
	    geom_text(aes(label = paste0(sm, "%")), size = 5, hjust = -0.05) +
	    scale_x_continuous(expand = expansion(mult = c(0.005, 0.1))) +
	    labs(y = NULL, x = "Натрупана инфлация", caption = "Източник на данните: Eurostat",
	         title = paste0(input$item, ", от ", input$date[1], " до ", input$date[2])) +
	    theme(text = element_text(size = 20), axis.text.x = element_blank(),
	          axis.ticks.x = element_blank())
	  
	}, height = 750, width = 1800)
	
	output$accum <- renderPlot({
	  
	  time <- inf %>%
	    filter(coicop == input$item_time,
	           geo == input$country,
	           TIME_PERIOD >= "2000-01-01")
	  
	  pos <- time %>%
	    mutate(year = year(TIME_PERIOD), 
	           csum = round(cumsum(values), 1)) %>%
	    summarise(max = max(csum), .by = year)# %>% 
	    #filter(year != 2025)
	  
	  level <- max(pos$max) / 2
	  
	  tot_year <- time %>% 
	    mutate(year = year(TIME_PERIOD)) %>% 
	    summarise(s = round(sum(values), 1), .by = year)# %>% 
	    #filter(year != 2025)
	  
	  tot <- time %>% 
	    summarise(s = round(sum(values), 0))
	  
	  seg <- tibble(
	    a = c(first(time$TIME_PERIOD), last(time$TIME_PERIOD)),
	    b = c(level, level))
	  
	  date <- time %>%
	    mutate(month = str_detect(TIME_PERIOD, "^\\d{4}-01")) %>% 
	    filter(month == "TRUE") %>% pull(TIME_PERIOD)
	  
	  text_data <- time %>% 
	    filter(str_detect(TIME_PERIOD, "^\\d{4}-07-\\d{2}")) %>% 
	    select(TIME_PERIOD) %>%
	    mutate(pos_max = pos$max, tot_year_s = tot_year$s)
	  
	  euro_zone <- time %>% 
	    group_by(geo) %>% 
	    mutate(euro = case_when(
	      geo == "Greece" ~ as.Date("2001-01-01"),
	      geo == "Slovenia" ~ as.Date("2007-01-01"),
	      geo == "Cyprus" ~ as.Date("2008-01-01"),
	      geo == "Malta" ~ as.Date("2008-01-01"),
	      geo == "Slovakia" ~ as.Date("2009-01-01"),
	      geo == "Estonia" ~ as.Date("2011-01-01"),
	      geo == "Latvia" ~ as.Date("2014-01-01"),
	      geo == "Lithuania" ~ as.Date("2015-01-01"),
	      geo == "Croatia" ~ as.Date("2023-01-01"),
	      geo == "Bulgaria" ~ as.Date("2026-01-01")),
	      label = "Euro-\nzone", x = euro, y = 0) %>% 
	    distinct(euro, label, x, y)
	  
	  time %>%
	    mutate(col = values >= 0, csum = round(cumsum(values), 1)) %>%
	    ggplot(aes(TIME_PERIOD, csum)) +
	    geom_text(data = text_data, aes(label = paste0(tot_year_s, "%"), x = TIME_PERIOD, y = pos_max + 2), size = 5) +
	    annotate("text", x = time$TIME_PERIOD[7], y = level, 
	             label = paste0(tot$s, "%"), size = 6, color = "red", fontface = "bold") +
	    geom_segment(x = time$TIME_PERIOD[1], xend = time$TIME_PERIOD[2], y = level, yend = level, linewidth = 0.2) +
	    geom_segment(x = time$TIME_PERIOD[12], xend = last(time$TIME_PERIOD), y = level, yend = level, linewidth = 0.2) +
	    geom_point(aes(as.Date(a), b), data = seg, size = 2) +
	    geom_point(aes(color = col), show.legend = F, size = 1) +
	    geom_line(linewidth = 0.2) +
	    theme(text = element_text(size = 18)) +
	    scale_x_date(breaks = "1 year", date_labels = "%Y", expand = c(0.02, 0.02)) +
	    scale_y_continuous(n.breaks = 10) +
	    geom_vline(xintercept = date, color = "black", lty = 2) +
	    geom_vline(data = euro_zone, aes(xintercept = euro), color = "blue", lty = 1, linewidth = 1) +
	    geom_vline(xintercept = as.Date("2022-02-24"), color = "black", lty = 1, linewidth = 1) +
	    geom_label(aes(label = "UKR\nWAR", x = as.Date("2022-02-24"), y = 0), 
	               size = 5, color = "black", hjust = -0.1, fontface = "bold") +
	    geom_label(data = euro_zone, aes(label = label, x = x, y = y), 
	              size = 5, color = "blue", hjust = -0.1, fontface = "bold") +
	    scale_color_manual(values = c("red", "black")) +
	    labs(x = NULL, y = "Инфлация", caption = "Източник на данните: Eurostat",
	         title = paste0(input$item, ", ", input$country))
	}, height = 750, width = 1800)
	
	session$onSessionEnded(function() {
	  stopApp()
	})
	
}
shinyApp(ui, server)
	