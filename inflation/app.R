library(shiny)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(eurostat)
library(bslib)

eur <- st_read("eur.gpkg")
inf <- read_rds("prc_hicp_mmor.rds") %>% arrange(TIME_PERIOD) %>% drop_na(values)

mail <- tags$a(icon("envelope"), "Email", href = "mailto:nickydyakov@gmail.com", tagret = "_blank")
github <- tags$a(icon("github"), "Github", href = "https://github.com/NickyDy", tagret = "_blank")

ui <- page_sidebar(
  #title = h3("Inflation in EU!"),
  theme = bslib::bs_theme(bootswatch = "darkly"),
  sidebar = sidebar(width = 270, list(
    selectInput("item", "Item:", choices = unique(inf$coicop))),
    selectInput("country", "Country:", choices = unique(inf$geo), selected = "Bulgaria"),
    dateRangeInput("date", "Date:",
                   weekstart = 1, separator = "to",
                   start = first(inf$TIME_PERIOD), end = last(inf$TIME_PERIOD),
                   min = first(inf$TIME_PERIOD), max = last(inf$TIME_PERIOD))),
  navset_pill(
    nav_panel(title = "Inflation in space", 
              plotOutput("price", width = "100%")),
    nav_panel(title = "Inflation in time", 
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
		mutate(geo = fct_recode(geo, "Turkey" = "Türkiye")) %>% 
		filter(coicop == input$item, 
		       TIME_PERIOD >= input$date[1] & TIME_PERIOD <= input$date[2]) %>% 
		group_by(coicop, geo) %>% 
		summarise(sm = sum(values, na.rm = T)) %>%
		ungroup() %>% 
		mutate_if(is.numeric, round, 1)
	  })
	
	df <- reactive({
	  eur %>% 
		inner_join(prices(), by = c("name" = "geo"))
	  })
	
	output$price <- renderPlot({
		df() %>% 
			ggplot() +
			geom_sf(aes(fill = sm), alpha = 0.4) +
			coord_sf(xlim = c(-25, 40), ylim = c(34, 72)) + 
			geom_sf_text(aes(label = paste0(sm, "%")), check_overlap = TRUE, size = 4) + 
			scale_fill_gradient2(low = "blue", mid = "white", high = "red") +
			labs(x = NULL, y = NULL, fill = "%", 
					 subtitle = paste0("Cumulative inflation for ", "'", 
					                   input$item, "'", " from ", 
					                   input$date[1], " until ", input$date[2], "!"),
					 caption = "Data source: Eurostat") +
			theme(text = element_text(size = 18), legend.position = "none",
						axis.text = element_blank(),
						axis.ticks = element_blank())
	}, height = 800, width = 1600)
	
	output$accum <- renderPlot({
	  
	  time <- inf %>%
	    filter(coicop == input$item,
	           geo == input$country,
	           TIME_PERIOD >= "2000-01-01")
	  
	  pos <- time %>%
	    mutate(year = year(TIME_PERIOD), 
	           csum = round(cumsum(values), 1)) %>%
	    summarise(max = max(csum), .by = year) %>% 
	    filter(year != 2025)
	  
	  level <- max(pos$max) / 2
	  
	  tot_year <- time %>% 
	    mutate(year = year(TIME_PERIOD)) %>% 
	    summarise(s = round(sum(values), 1), .by = year) %>% 
	    filter(year != 2025)
	  
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
	      geo == "Croatia" ~ as.Date("2023-01-01"),),
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
	    geom_vline(data = euro_zone, aes(xintercept = euro), color = "red", lty = 1, linewidth = 1) +
	    geom_label(data = euro_zone, aes(label = label, x = x, y = y), 
	              size = 5, color = "red", hjust = 0 - 0.1, fontface = "bold") +
	    scale_color_manual(values = c("red", "black")) +
	    labs(x = NULL, y = "Inflation (%)", caption = "Data source: Eurostat",
	         title = input$item)
	}, height = 800, width = 1600)
	
	session$onSessionEnded(function() {
	  stopApp()
	})
	
}
shinyApp(ui, server)
	