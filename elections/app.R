library(tidyverse)
library(shiny)
library(DT)
library(arrow)
library(bslib)
library(tidytext)
library(sf)

votes <- read_parquet("votes.parquet")
mand <- read_parquet("mand.parquet") %>% 
  mutate(year = paste0("(", year, ")")) %>% 
  unite("ns_year", 1:2, sep = " ")
act <- read_parquet("election_activity.parquet")
address <- read_parquet("df_2024.parquet") %>% arrange(oblast, obshtina)
obsh_map <- read_sf("obsh_map.gpkg")

risk_sec <- votes %>% 
  group_by(oblast, obshtina, section, code) %>% 
  summarise(v = var(votes)) %>% 
  ungroup() %>% 
  mutate(v = round(v, 1), 
         var = case_when(
    v <= 500 ~ "Нисък",
    v > 500 & v <= 1000 ~ "Среден",
    v > 1000 ~ "Висок"), .after = v,
    var = factor(var, levels = c("Висок", "Среден", "Нисък")))

colors <- c(
  "ПП" = "yellow",
  "ГЕРБ-СДС" = "blue",
  "ДПС" = "purple",
  "БСП" = "red",
  "ИТН" = "#0096FF",
  "ДБ" = "darkblue",
  "ПП-ДБ" = "darkblue",
  "ИЗПРАВИ СЕ! МУТРИ ВЪН!" = "green",
  "ВЪЗРАЖДАНЕ" = "black",
  "БЪЛГАРСКИ ВЪЗХОД" = "darkgreen",
  "НФСБ" = "black",
  "ГЕРБ" = "blue",
  "ОП (НФСБ, АТАКА и ВМРО)" = "brown",
  "ВОЛЯ" = "pink",
  "Обединение ДОСТ" = "gray")

colors_mand <- c(
  "ПП" = "yellow",
  "НДСВ" = "yellow",
  "ГЕРБ-СДС" = "blue",
  "СДС" = "blue",
  "ОДС" = "blue",
  "ДПС" = "purple",
  "БСП" = "red",
  "КБ" = "red",
  "ИТН" = "#0096FF",
  "ДБ" = "darkblue",
  "ПП-ДБ" = "darkblue",
  "ИСМВ" = "green",
  "ВЪЗРАЖДАНЕ" = "black",
  "БВ" = "darkgreen",
  "АТАКА" = "black",
  "ГЕРБ" = "blue",
  "КП ОП" = "black",
  "ВОЛЯ" = "pink",
  "РЗС" = "pink",
  "БББ" = "pink",
  "Независими" = "gray",
  "БЗНС" = "orange",
  "ОС" = "black",
  "ДЛ" = "red",
  "НС" = "black",
  "ОПТ" = "brown",
  "СП" = "red",
  "ОНС" = "purple",
  "БЕ" = "red",
  "НО АТАКА" = "black",
  "ДСБ" = "darkblue",
  "БНС" = "red",
  "СК" = "darkblue",
  "КП БСП" = "red",
  "КП РБ" = "darkblue",
  "КП ПФ" = "black",
  "КП ББЦ" = "pink",
  "КП АБВ" = "red",
  "ИБГНИ" = "green")
#----------------------------------------
mail <- tags$a(icon("envelope"), "Email", 
               href = "mailto:nickydyakov@gmail.com", 
               tagret = "_blank")
github <- tags$a(icon("github"), "Github", 
                 href = "https://github.com/NickyDy", 
                 tagret = "_blank")
#----------------------------------
ui <- page_sidebar(
  title = h3("Избори в България!"), 
  theme = bslib::bs_theme(bootswatch = "darkly"),
  sidebar = sidebar(list(
    selectInput("obl", "Избирателен район:", 
                choices = unique(votes$oblast)),
    selectInput("obsh", "Община:", choices = NULL),
    selectInput("sett", "Населено място:", choices = NULL),
    selectInput("sec", "Секция:", choices = NULL),
    sliderInput("prop_slider", "Филтър проценти:", 
                min = 0, max = 0.5, value = 0.04, step = 0.01),
    sliderInput("height_slider", "Височина на графиката (адресна регистрация):", 
                min = 800, max = 4000, value = 800, step = 100))),
  navset_pill(
    nav_panel(title = "Изборни резултати", 
              plotOutput("obsh_perc", height = 290), 
              plotOutput("sett_perc", height = 290),
              plotOutput("sec_perc", height = 290)),
    nav_panel(title = "Общо за страната", 
              plotOutput("country")),
    nav_panel(title = "Карта на общините", layout_columns(
              selectInput("vote_date_map", "Изборна дата:",
                          choices = unique(votes$vote_date),
                          selected = last(unique(votes$vote_date))),
              selectInput("party_map", "Партия:",
                          choices = NULL),
              col_widths = c(2, 4)),
              plotOutput("plot_map")),
    nav_panel(title = "Рискови секции", 
              DTOutput("dt_table", width = 1600)),
    nav_panel(title = "Адресна регистрация", layout_columns(
              selectInput("oblast_add", "Област:",
                          choices = unique(address$oblast)),
              selectInput("obshtina_add", "Община:",
                          choices = NULL),
              selectInput("month_first", "Месец за сравнение:",
                          choices = c("януари", "февруари", "март", "април"),
                          selected = "април"),
              selectInput("month_last", "Месец за сравнение:",
                          choices = c("януари", "февруари", "март", "април"),
                          selected = "януари"),
                          col_widths = c(2, 2, 2)),
              plotOutput("add_plot")),
    nav_panel(title = "Избирателна активност", 
              plotOutput("elec_act")),
    nav_panel(title = "Депутатски мандати", 
              plotOutput("mand_plot")),
    nav_panel(tags$img(src = "shiny.png", width = 40),
              "Други полезни приложения:",
              tags$a(href = "https://nickydy.shinyapps.io/demography/", br(),
                     "Демография на България!"), br(),
              tags$a(href = "https://nickydy.shinyapps.io/climate/",
                     "Климатът на България!"), br(),
              tags$a(href = "https://nickydy.shinyapps.io/inlation/",
                     "Inflation in EU!"), br(),
              tags$a(href = "https://ndapps.shinyapps.io/bgprices/",
                     "Сравнение на цените в България!"), br(),
              tags$a(href = "https://ndapps.shinyapps.io/agri/",
                     "Цени на селскостопанска продукция в ЕС!"), br(),
              tags$a(href = "https://nickydy.shinyapps.io/eurostat/",
                     "Евростат за България!"), br(),
              tags$a(href = "https://ndapps.shinyapps.io/und_water/",
                     "Чистота на водите в България"), br()),
    nav_panel(tags$img(src = "kofi.png", width = 40),
              "Ако Ви харесва приложението,
               можете да ме подкрепите като направите дарение в евро към
               следната сметка:",
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
	sec <- reactive({
	  req(input$sec)
	  filter(sett(), code == input$sec)
	})
#-------------------------------
output$obsh_perc <- renderPlot({
		obsh() %>%
			filter(obshtina %in% c(input$obsh)) %>%
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
	}, height = 290, width = 1600, res = 96)

output$sett_perc <- renderPlot({
		sett() %>%
			filter(section %in% c(input$sett)) %>%
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
	}, height = 290, width = 1600, res = 96)

output$sec_perc <- renderPlot({
		sec() %>%
			filter(code %in% c(input$sec)) %>%
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
	}, height = 290, width = 1600, res = 96)

output$country <- renderPlot({
  
  votes %>%
    mutate(vote_date = fct_relevel(vote_date, 
                                   "Април_2023",
                                   "Октомври_2022", 
                                   "Ноември_2021", 
                                   "Юли_2021", 
                                   "Април_2021", 
                                   "Март_2017")) %>%
    group_by(vote_date, party) %>%
    summarise(sum_votes = sum(votes, na.rm = T)) %>%
    mutate(prop = sum_votes / sum(sum_votes)) %>%
    mutate(party = fct_reorder(party, sum_votes)) %>%
    filter(prop >= input$prop_slider) %>%
    ggplot(aes(prop, party, fill = party)) +
    geom_col(position = "dodge", show.legend = F) +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_x_continuous(expand = expansion(mult = c(.05, .55))) +
    scale_y_discrete(labels = scales::label_wrap(50)) +
    scale_fill_manual(values = colors) +
    geom_text(aes(label = scales::percent(prop, accuracy = 0.01)),
              position = position_dodge(width = 1), hjust = -0.1, size = 4) +
    theme(text = element_text(size = 16), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) +
    labs(x = NULL, y = NULL,
         caption = "Бележка: Оцветени са само партиите и коалициите влизали в Парламента, останалите са в сиво.\nИзточник на данните: ЦИК.") +
    facet_wrap(~ vote_date, ncol = 6)
  
}, height = 800, width = 1600, res = 96)
#---------------------------------------
vote_date_map <- reactive({
  filter(votes, vote_date == input$vote_date_map)
})
observeEvent(vote_date_map(), {
  freezeReactiveValue(input, "party_map")
  choices <- unique(vote_date_map()$party)
  updateSelectInput(inputId = "party_map", choices = choices)
})
party_map <- reactive({
  req(input$party_map)
  filter(vote_date_map(), party == input$party_map)
})

df <- reactive({
  votes %>%
  group_by(vote_date, obshtina, party) %>%
  summarise(sum_party = sum(votes)) %>%
  group_by(vote_date, obshtina) %>%
  mutate(sum_obshtina = sum(sum_party), 
         perc = sum_party / sum_obshtina * 100) %>%
  filter(party %in% c(input$party_map))
})

map <- reactive({
  obsh_map %>%
  left_join(df(), by = c("obshtina_bg" = "obshtina")) %>%
  mutate_if(is.numeric, round, 1) %>%
  filter(vote_date %in% c(input$vote_date_map)) %>%
  mutate(party = fct_reorder(party, perc))
})

output$plot_map <- renderPlot({
map() %>%
  ggplot() +
  geom_sf(aes(fill = perc), alpha = .4) +
  geom_sf_text(aes(label = perc),
               check_overlap = TRUE, size = 3) +
  geom_sf_text(aes(label = obshtina_bg),
               check_overlap = TRUE, size = 2, vjust = -1.5) +
  theme(text = element_text(size = 16), legend.position = "right",
        axis.text = element_blank(),
        axis.ticks = element_blank(), plot.title.position = "plot") +
  labs(x = NULL, y = NULL, fill = "(%)",
       title = paste0("Вот за ", input$party_map,
       " през ", input$vote_date_map, "!")) +
  scale_fill_gradient(low = "white", high = "red")
}, height = 800, width = 1600, res = 96)
#-------------------------------------------------
output$elec_act <- renderPlot({

  act %>%
    mutate(election = fct_reorder(election, activity)) %>%
    ggplot(aes(activity, election, fill = round)) +
    geom_col(position = "dodge") +
    geom_text(aes(label = paste0(activity, "%")), position = position_dodge(width = 1),
              hjust = -0.1, size = 4) +
    scale_x_continuous(expand = expansion(mult = c(.05, .5))) +
    labs(x = "Избирателна активност", y = NULL, fill = "Тур:",
         caption = "Източник на данните: Wikipedia") +
    theme(text = element_text(size = 16)) +
    guides(fill = guide_legend(reverse = TRUE)) +
    facet_wrap(vars(type_election))

}, height = 800, width = 1600, res = 96)
#---------------------------------------
output$mand_plot <- renderPlot({
  
  mand %>% 
    mutate(col = party, 
           party = reorder_within(party, mandates, ns_year)) %>%
    ggplot(aes(mandates, party, fill = col)) +
    geom_col(show.legend = F) +
    geom_text(aes(label = mandates), position = position_dodge(width = 1),
              hjust = -0.1, size = 4) +
    scale_fill_manual(values = colors_mand) +
    scale_y_reordered() +
    scale_x_continuous(expand = expansion(mult = c(.01, .2))) +
    labs(y = NULL, x = "Брой мандати",
         caption = "Източник на данните: Wikipedia") +
    theme(text = element_text(size = 14), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) +
    facet_wrap(vars(ns_year), scales = "free_y")
  
}, height = 800, width = 1600, res = 96)
#---------------------------------------
oblast_add <- reactive({
  filter(address, oblast %in% c(input$oblast_add))
})

observeEvent(oblast_add(), {
  freezeReactiveValue(input, "obshtina_add")
  choices <- unique(oblast_add()$obshtina)
  updateSelectInput(inputId = "obshtina_add", choices = choices)
})

obshtina_add <- reactive({
  req(input$oblast_add)
  filter(oblast_add(), obshtina == input$obshtina_add)
})
#------------------------------
output$add_plot <- renderPlot({
  
  obshtina_add() %>% 
    filter(obshtina == input$obshtina_add) %>% 
    mutate(diff = .data[[input$month_first]] - .data[[input$month_last]], 
           sett = fct_reorder(sett, diff), 
           col = diff > 0) %>% 
    filter(diff != 0) %>% 
    ggplot(aes(diff, sett, fill = col)) +
    geom_col(show.legend = F) +
    geom_text(aes(label = diff), 
              position = position_dodge(width = 1), hjust = -0.05, size = 4) +
    scale_x_continuous(expand = expansion(mult = c(.01, .15))) +
    scale_fill_manual(values = c("TRUE" = "#00BFC4", "FALSE" = "#F8766D")) +
    theme(text = element_text(size = 16)) +
    labs(x = "Промяна в броя жители", y = NULL, 
         subtitle = paste0("Разлика в броя жители в съответното населено място между месеците ", 
                        input$month_first, " и ", input$month_last, " (2024 г.)"),
         caption = "Източник на данните: МРРБ") +
    facet_wrap(vars(address))
  
}, height = function() input$height_slider, width = 1600, res = 96)

dt_rend <- reactive({
  risk_sec %>%
    filter(oblast %in% c(input$obl), 
           obshtina %in% c(input$obsh)) %>%
    arrange(-v) %>%
    select(-v)
})

output$dt_table <- renderDT(
 dt_rend() %>% 
    datatable(rownames = F,
      colnames = c("Избирателен район" = "oblast",
                   "Община" = "obshtina",
                   "Населено място" = "section",
                   "Секция" = "code",
                   "Риск" = "var"), 
      options = list(dom = 'Brtip', pageLength = 100)) %>% 
      formatStyle("Риск", backgroundColor = styleEqual(
        c("Висок", "Среден", "Нисък"), 
        c("red", "orange", "darkgreen"))))

session$onSessionEnded(function() {
  stopApp()
})
}
shinyApp(ui, server)
