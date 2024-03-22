library(shiny)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(eurostat)
library(arrow)
library(bslib)

eur <- st_read("eur.gpkg")
inf <- read_parquet("inflation.parquet") %>% arrange(TIME_PERIOD)

# eur <- ne_download(scale = 50, type = "sovereignty", returnclass = "sf") %>% 
#   janitor::clean_names() %>% 
#   select(name, geometry) %>% 
#   mutate(name = fct_recode(name, "Bosnia and Herzegovina" = "Bosnia and Herz."))

# prc_hicp_mmor <- get_eurostat("prc_hicp_mmor", type = "label", time_format = "date") %>%
#   mutate_if(is_character, as_factor)
# write_parquet(prc_hicp_mmor, "inflation.parquet")

choices <- inf %>% count(coicop, sort = T) %>% distinct(coicop)
time <- inf %>% count(TIME_PERIOD) %>% distinct(TIME_PERIOD)

mail <- tags$a(icon("envelope"), "Email", href = "mailto:nickydyakov@gmail.com", tagret = "_blank")
github <- tags$a(icon("github"), "Github", href = "https://github.com/NickyDy", tagret = "_blank")

ui <- page_sidebar(
  title = h3("Inflation in EU!"),
  sidebar = sidebar(width = 270, list(
    selectInput("item", "Item:", choices = choices)),
    dateRangeInput("date", "Date:",
                   weekstart = 1, separator = "to",
                   start = first(inf$TIME_PERIOD), end = last(inf$TIME_PERIOD),
                   min = first(inf$TIME_PERIOD), max = last(inf$TIME_PERIOD))),
  navset_pill(
    nav_panel(title = "Price map", 
              plotOutput("price", width = "100%")),
    nav_panel(title = "Time series", 
              plotOutput("series")),
    nav_panel(tags$img(src = "kofi.png", width = 40),
              "If you like the app,
               you can support me by donating \u20AC to
               the following account:",
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
			scale_fill_gradient(low = "white", high = "red") +
			labs(x = NULL, y = NULL, fill = "%", 
					 subtitle = paste0("Cumulative inflation for ", "'", 
					                   input$item, "'", " from ", 
					                   input$date[1], " until ", input$date[2], "!"),
					 caption = "Data source: Eurostat") +
			theme(text = element_text(size = 18), legend.position = "none",
						axis.text = element_blank(),
						axis.ticks = element_blank())
	}, height = 800, width = 1600)
	
	output$series <- renderPlot({
	  inf %>% 
	    mutate(geo = fct_recode(geo, "Turkey" = "Türkiye")) %>% 
	    filter(coicop == input$item, 
	           TIME_PERIOD >= input$date[1] & TIME_PERIOD <= input$date[2], 
	           !str_detect(geo, "Euro"), 
	           !str_detect(geo, "United States")) %>% 
	    mutate(geo = str_wrap(geo, 40)) %>% 
	    ggplot(aes(TIME_PERIOD, values, color = geo)) +
	    geom_path(show.legend = F) +
	    labs(x = NULL, y = "Inflation (%)") +
	    scale_x_date(date_labels = "%Y-%m") +
	    theme(text = element_text(size = 18), 
	          axis.text.x = element_text(angle = 45, hjust = 1)) +
	    facet_wrap(vars(geo), ncol = 6)
	}, height = 800, width = 1600)
	
	session$onSessionEnded(function() {
	  stopApp()
	})
	
}
shinyApp(ui, server)
	