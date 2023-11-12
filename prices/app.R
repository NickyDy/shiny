library(shiny)
library(shinythemes)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(eurostat)

eur <- ne_download(scale = 50, type = "sovereignty", returnclass = "sf") %>% 
  janitor::clean_names() %>% 
  select(name, geometry) %>% 
  mutate(name = fct_recode(name, "Czechia" = "Czech Rep.", "North Macedonia" = "Macedonia",
                         "Bosnia and Herzegovina" = "Bosnia and Herz."))

prc_hicp_mmor <- get_eurostat("prc_hicp_mmor", type = "label", time_format = "date") %>%
	mutate_if(is_character, as_factor)

choices <- prc_hicp_mmor %>% count(coicop, sort = T) %>% distinct(coicop)
time <- prc_hicp_mmor %>% count(time) %>% distinct(time)

ml <- tags$a(href = "mailto:nickydyakov@gmail.com", icon("envelope"))
gh <- tags$a(href = "https://github.com/NickyDy", icon("github"))

ui <- fluidPage(theme = shinytheme("cyborg"),
                titlePanel("Inflation in EU"),
								sidebarLayout(
									sidebarPanel(
										selectInput("item", "Item", choices = choices),
										selectInput("date", "Date", choices = time), 
										fluidRow(ml, gh),
										width = 4),
									mainPanel(
									  tabsetPanel(
										tabPanel("Price map", plotOutput("price", height = 750)),
										tabPanel("Time series", plotOutput("series", height = 750))))))

server <- function(input, output, session) {
	
	prices <- reactive(prc_hicp_mmor %>%
		mutate(geo = fct_recode(geo, "Turkey" = "Türkiye", 
														"Germany" = "Germany (until 1990 former territory of the FRG)",
														"United Kingdom" = "United Kingdom",
														"Kosovo" = "Kosovo (under United Nations Security Council Resolution 1244/99)")) %>% 
		filter(coicop == input$item, time >= input$date) %>% 
		group_by(coicop, geo) %>% 
		summarise(sm = sum(values, na.rm = T)) %>%
		mutate_if(is.numeric, round, 1))
	
	df <- reactive(eur %>% 
		inner_join(prices(), by = c("name" = "geo")))
	
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
					                   input$date, " until today!"),
					 caption = "Data source: Eurostat") +
			theme(text = element_text(size = 16), legend.position = "none",
						axis.text = element_blank(),
						axis.ticks = element_blank())
	}, height = 800, width = 1200)
	
	output$series <- renderPlot({
	  prc_hicp_mmor %>% 
	    mutate(geo = fct_recode(geo, "Turkey" = "Türkiye", 
	                            "Germany" = "Germany (until 1990 former territory of the FRG)",
	                            "United Kingdom" = "United Kingdom",
	                            "Kosovo" = "Kosovo (under United Nations Security Council Resolution 1244/99)")) %>% 
	    filter(coicop == input$item, time >= input$date, 
	           !str_detect(geo, "Euro"), 
	           !str_detect(geo, "United States")) %>% 
	    mutate(geo = str_wrap(geo, 40)) %>% 
	    ggplot(aes(time, values, color = geo)) +
	    geom_path(show.legend = F) +
	    labs(x = "Time", y = "Price change (%)") +
	    scale_x_date(date_labels = "%Y-%m") +
	    theme(text = element_text(size = 16), axis.text.x = element_text(angle = 45, hjust = 1)) +
	    facet_wrap(vars(geo), ncol = 6)
	}, height = 800, width = 1200)
}
shinyApp(ui, server)
	