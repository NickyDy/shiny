library(tidyverse)
library(shiny)

votes <- read_rds("votes.rds")

risk_sec <- votes %>% 
  group_by(oblast, obshtina, section) %>% 
  summarise(v = var(votes)) %>% 
  mutate(var = case_when(
    v <= 500 ~ "Нисък",
    v > 500 & v <= 1000 ~ "Среден",
    v > 1000 ~ "Висок"), .after = v)

colors <- c(
	"Продължаваме Промяната" = "yellow",
	"ГЕРБ-СДС" = "blue",
	"Движение за права и свободи – ДПС" = "purple",
	"БСП за БЪЛГАРИЯ" = "red",
	"ИМА ТАКЪВ НАРОД" = "#0096FF",
	"ДЕМОКРАТИЧНА БЪЛГАРИЯ" = "darkblue",
	"ПП-ДБ" = "darkblue",
	"ИЗПРАВИ СЕ! МУТРИ ВЪН!" = "green",
	"ВЪЗРАЖДАНЕ" = "black",
	"БЪЛГАРСКИ ВЪЗХОД" = "darkgreen",
	"НФСБ" = "black",
	"ГЕРБ" = "blue",
	"ОБЕДИНЕНИ ПАТРИОТИ – НФСБ, АТАКА и ВМРО" = "brown",
	"ВОЛЯ" = "pink",
	"Обединение ДОСТ" = "gray")

risk_colors <- c("Висок" = "red","Среден" = "orange","Нисък" = "darkgreen")

ml <- tags$a(href = "mailto:nickydyakov@gmail.com", icon("envelope"))
gh <- tags$a(href = "https://github.com/NickyDy", icon("github"))

ui <- fluidPage(theme = bslib::bs_theme(base_font = "Liberation Sans"),
	titlePanel("Избори в България"),
	sidebarLayout(
		sidebarPanel(
			selectInput("obl", "Област/Избирателен район:", choices = unique(votes$oblast)),
			selectInput("obsh", "Община:", choices = NULL),
			selectInput("sett", "Населено място:", choices = NULL),
			selectInput("sec", "Секция:", choices = NULL),
			sliderInput("prop_slider", "Филтър проценти:", min = 0, max = 0.5, value = 0.04, step = 0.01),
			fluidRow(ml, gh),
			width = 2),
		mainPanel(
			tabsetPanel(
				tabPanel("Графика (%)",
			    plotOutput("obsh_perc", height = 290),
			    plotOutput("sett_perc", height = 290),
			    plotOutput("sec_perc", height = 290)), 
			  tabPanel("Рискови секции", 
				  plotOutput("risk_sec", height = 800, width = 1500))))))

server <- function(input, output, session) {
  obl <- reactive({
    filter(votes, oblast == input$obl)
  })
  observeEvent(obl(), {
    freezeReactiveValue(input, "obsh")
    choices <- unique(obl()$obshtina)
    updateSelectInput(inputId = "obsh", choices = choices) 
  })
	obsh <- reactive({
	  req(input$obsh)
		filter(obl(), obshtina == input$obsh)
	})
	observeEvent(obsh(), {
		freezeReactiveValue(input, "sett")
		choices <- unique(obsh()$section)
		updateSelectInput(inputId = "sett", choices = choices) 
	})
	sett <- reactive({
		req(input$sett)
		filter(obsh(), section == input$sett)
	})
	observeEvent(sett(), {
		freezeReactiveValue(input, "sec")
		choices <- unique(sett()$code)
		updateSelectInput(inputId = "sec", choices = choices)
  })
#-------------------------------
output$obsh_perc <- renderPlot({
		obsh() %>% 
			filter(obshtina == input$obsh) %>% 
			mutate(vote_date = fct_relevel(vote_date, 
			                               "Април_2023",
																		 "Октомври_2022", 
																		 "Ноември_2021", 
																		 "Юли_2021", 
																		 "Април_2021", 
																		 "Март_2017")) %>%
			group_by(vote_date, party) %>%
			summarise(sum_votes = sum(votes)) %>%
			mutate(prop = sum_votes / sum(sum_votes)) %>%
			mutate(party = fct_reorder(party, sum_votes)) %>%
			filter(prop >= input$prop_slider) %>% 
			ggplot(aes(prop, party, fill = party)) +
			geom_col(position = "dodge", show.legend = F) +
			guides(fill = guide_legend(reverse = TRUE)) +
			scale_x_continuous(expand = expansion(mult = c(.05, .5))) +
			scale_y_discrete(labels = scales::label_wrap(50)) +
			scale_fill_manual(values = colors) +
			geom_text(aes(label = scales::percent(prop, accuracy = 0.01)),
								position = position_dodge(width = 1), hjust = -0.1, size = 3.5) +
			theme(text = element_text(size = 12), 
						axis.text.x = element_blank(), 
						axis.ticks.x = element_blank()) +
			labs(x = NULL, y = NULL, title = paste0("Община: ", input$obsh)) +
			facet_wrap(~ vote_date, ncol = 6, drop = F)
	}, height = 300, width = 1500, res = 96)
	
output$sett_perc <- renderPlot({
		sett() %>% 
			filter(section == input$sett) %>% 
			mutate(vote_date = fct_relevel(vote_date, 
			                               "Април_2023",
																		 "Октомври_2022", 
																		 "Ноември_2021", 
																		 "Юли_2021", 
																		 "Април_2021", 
																		 "Март_2017")) %>%
			group_by(vote_date, party) %>%
			summarise(sum_votes = sum(votes)) %>%
			mutate(prop = sum_votes / sum(sum_votes)) %>%
			mutate(party = fct_reorder(party, sum_votes)) %>%
			filter(prop >= input$prop_slider) %>% 
			ggplot(aes(prop, party, fill = party)) +
			geom_col(position = "dodge", show.legend = F) +
			guides(fill = guide_legend(reverse = TRUE)) +
			scale_x_continuous(expand = expansion(mult = c(.05, .5))) +
			scale_y_discrete(labels = scales::label_wrap(50)) +
			scale_fill_manual(values = colors) +
			geom_text(aes(label = scales::percent(prop, accuracy = 0.01)),
								position = position_dodge(width = 1), hjust = -0.1, size = 3.5) +
			theme(text = element_text(size = 12), 
						axis.text.x = element_blank(), 
						axis.ticks.x = element_blank()) +
			labs(x = NULL, y = NULL, title = paste0("Населено място: ", input$sett)) +
			facet_wrap(~ vote_date, ncol = 6, drop = F)
	}, height = 300, width = 1500, res = 96)
	
output$sec_perc <- renderPlot({
		sett() %>% 
			filter(code == input$sec) %>% 
			mutate(vote_date = fct_relevel(vote_date, 
			                               "Април_2023",
																		 "Октомври_2022", 
																		 "Ноември_2021", 
																		 "Юли_2021", 
																		 "Април_2021", 
																		 "Март_2017")) %>%
			group_by(vote_date, party) %>%
			summarise(sum_votes = sum(votes)) %>%
			mutate(prop = sum_votes / sum(sum_votes)) %>%
			mutate(party = fct_reorder(party, sum_votes)) %>%
			filter(prop >= input$prop_slider) %>% 
			ggplot(aes(prop, party, fill = party)) +
			geom_col(position = "dodge", show.legend = F) +
			guides(fill = guide_legend(reverse = TRUE)) +
			scale_x_continuous(expand = expansion(mult = c(.05, .5))) +
			scale_y_discrete(labels = scales::label_wrap(50)) +
			scale_fill_manual(values = colors) +
			geom_text(aes(label = scales::percent(prop, accuracy = 0.01)),
								position = position_dodge(width = 1), hjust = -0.1, size = 3.5) +
			theme(text = element_text(size = 12), 
						axis.text.x = element_blank(), 
						axis.ticks.x = element_blank()) +
			labs(x = NULL, y = NULL, title = paste0("Секция: ", input$sec),
					 caption = "Бележка: Оцветени са само партиите и коалициите влизали в Парламента, останалите са в сиво.\nИзточник на данните: ЦИК.") +
			facet_wrap(~ vote_date, ncol = 6, drop = F)
	}, height = 300, width = 1500, res = 96)
	
output$risk_sec <- renderPlot({
	  risk_sec %>% 
	    filter(oblast == input$obl, obshtina == input$obsh) %>% 
	    mutate(section = fct_reorder(section, abs(v)),
	           var = fct_relevel(var, "Висок", "Среден", "Нисък")) %>% 
	    ggplot(aes(v, section, fill = var)) +
	    geom_col() +
	    labs(x = "Дисперсия (вариация)", y = NULL, fill = "Риск:",
	         title = paste0("Рискови секции в община: ", input$obsh),
	         caption = "Източник на данните: ЦИК") +
	    theme(text = element_text(size = 12), legend.position = "right") +
	    scale_fill_manual(values = risk_colors)
	}, height = 850, width = 1500, res = 96)

session$onSessionEnded(function() {
  stopApp()
})
}
shinyApp(ui, server)
