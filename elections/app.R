library(tidyverse)
library(shiny)
library(DT)
library(bslib)
library(scales)
library(tidytext)

votes <- read_rds("votes_new.rds") %>% 
  mutate(vote_date = fct_relevel(vote_date,
                                 "Октомври_2024",
                                 "Юни_2024",
                                 "Април_2023",
  															 "Октомври_2022",
  															 "Ноември_2021",
  															 "Юли_2021",
  															 "Април_2021",
  															 "Март_2017"))
mand <- read_rds("mand.rds")
act <- read_rds("activity.rds")
#address <- read_parquet("df_2024.parquet") %>% arrange(oblast, obshtina)

risk_sec <- votes %>% 
  filter(!oblast == "Извън страната") %>%
  summarise(v = var(votes, na.rm = T), .by = c(oblast, obshtina, section, code)) %>%
  mutate(v = round(v, 1), 
         var = case_when(
    v <= 500 ~ "Нисък",
    v > 500 & v <= 1500 ~ "Среден",
    v > 1500 ~ "Висок"), .after = v,
    var = factor(var, levels = c("Висок", "Среден", "Нисък"))) %>% 
  drop_na(var)

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
  "ВЕЛИЧИЕ" = "darkgreen",
  "ДПС-НH" = "purple",
  "АПС" = "purple",
  "МЕЧ" = "maroon",
  "БСП-ОЛ" = "red")

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
  "ИБГНИ" = "green",
  "ВЕЛИЧИЕ" = "darkgreen",
  "ДПС-НH" = "purple",
  "АПС" = "purple",
  "МЕЧ" = "maroon",
  "БСП-ОЛ" = "red")

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
#----------------------------------
ui <- page_sidebar(
  #title = h3("Избори в България!"), 
  theme = bslib::bs_theme(bootswatch = "darkly"),
  sidebar = sidebar(list(
    selectInput("obl", "Избирателен район:", 
                choices = unique(votes$oblast)),
    selectInput("obsh", "Община:", choices = NULL),
    selectInput("sett", "Населено място:", choices = NULL),
    selectInput("sec", "Секция:", choices = NULL),
    sliderInput("prop_slider", "Филтър проценти:", 
                min = 0, max = 0.5, value = 0.04, step = 0.01),
    sliderInput("votes_slider", "Филтър брой гласове:", 
                min = 0, max = 1000000, value = 50000, step = 10000),
    sliderInput("height_slider", "Височина на графиката:", 
                min = 800, max = 4000, value = 800, step = 100))),
  navset_pill(
    nav_panel(title = "Изборни резултати",
              textOutput("text"),
              tags$head(tags$style("#text{color: red;
                                 font-size: 20px;
                                 font-style: bold;
                                 }")), br(),
              plotOutput("obsh_perc", height = 350), 
              plotOutput("sett_perc", height = 350),
              plotOutput("sec_perc", height = 350)),
    nav_panel(title = "Общо за страната (%)",
              textOutput("text1"),
              tags$head(tags$style("#text1{color: red;
                                 font-size: 20px;
                                 font-style: bold;
                                 }")), br(),
              plotOutput("country")),
    nav_panel(title = "Общо за страната (брой гласове)",
              textOutput("text2"),
              tags$head(tags$style("#text2{color: red;
                                 font-size: 20px;
                                 font-style: bold;
                                 }")), br(),
              plotOutput("votes_country")),
    nav_panel(title = "Загуба/Печалба на гласове", 
              textOutput("text3"),
              tags$head(tags$style("#text3{color: red;
                                 font-size: 20px;
                                 font-style: bold;
                                 }")), br(),
              layout_columns(
              selectInput("first_date", "Последни избори:",
                          choices = "Октомври_2024"),
              selectInput("second_date", "Сравни със:",
                          choices = c("Юни_2024",
                                      "Април_2023", 
                                      "Октомври_2022", 
                                      "Ноември_2021", 
                                      "Юли_2021", 
                                      "Април_2021", 
                                      "Март_2017"),
                          selected = "Юни_2024"),
              col_widths = c(2, 2)),
              plotOutput("lost_gained_votes")),
    nav_panel(title = "Рискови секции",
              textOutput("text4"),
              tags$head(tags$style("#text4{color: red;
                                 font-size: 20px;
                                 font-style: bold;
                                 }")), br(),
              DTOutput("dt_table", width = 1600)),
    # nav_panel(title = "Адресна регистрация", layout_columns(
    #           selectInput("oblast_add", "Област:",
    #                       choices = unique(address$oblast)),
    #           selectInput("obshtina_add", "Община:",
    #                       choices = NULL),
    #           selectInput("month_first", "Месец за сравнение:",
    #                       choices = c("януари", "февруари", "март", "април", "май", "юни"),
    #                       selected = "юни"),
    #           selectInput("month_last", "Месец за сравнение:",
    #                       choices = c("януари", "февруари", "март", "април", "май"),
    #                       selected = "януари"),
    #                       col_widths = c(2, 2, 2)),
    #           plotOutput("add_plot")),
    nav_panel(title = "Избирателна активност", 
              textOutput("text5"),
              tags$head(tags$style("#text5{color: red;
                                 font-size: 20px;
                                 font-style: bold;
                                 }")), br(),
              plotOutput("elec_act")),
    nav_panel(title = "Депутатски мандати",
              textOutput("text6"),
              tags$head(tags$style("#text6{color: red;
                                 font-size: 20px;
                                 font-style: bold;
                                 }")), br(),
              plotOutput("mand_plot")),
    nav_panel(tags$img(src = "shiny.png", width = 40),
              "Други полезни приложения:",
              tags$a(href = "https://nickydy.shinyapps.io/demography/", br(),
                     "Демография на България!"), br(),
              tags$a(href = "https://nickydy.shinyapps.io/climate/",
                     "Климатът на България!"), br(),
              tags$a(href = "https://nickydy.shinyapps.io/inlation/",
                     "Inflation in EU!"), br(),
              # tags$a(href = "https://ndapps.shinyapps.io/bgprices/",
              #        "Сравнение на цените в България!"), br(),
              tags$a(href = "https://ndapps.shinyapps.io/agri/",
                     "Цени на селскостопанска продукция в ЕС!"), br(),
              tags$a(href = "https://nickydy.shinyapps.io/eurostat/",
                     "Евростат за България!"), br(),
              tags$a(href = "https://ndapps.shinyapps.io/und_water/",
                     "Чистота на водите в България!"), br()),
    # nav_panel(tags$img(src = "kofi.png", width = 40),
    #           "Ако Ви харесва приложението,
    #            можете да ме подкрепите като направите дарение в евро към
    #            следната сметка:",
    #            br(),
    #            br(),
    #            "Име: Nikolay Dyakov",
    #            br(),
    #            "IBAN: BE89 9670 3038 2685",
    #            br(),
    #            "BIC: TRWIBEB1XXX",
    #            br(),
    #            "Адрес: Rue de Trone 100, 3rd floor,",
    #            br(),
    #            "Brussels,",
    #            br(),
    #            "1050,",
    #            br(),
    #            "Belgium"),
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
output$text <- renderText({ "Панелът работи със следните филтри: Избирателен район, Община, Населено място, Секция, Филтър проценти." })

output$obsh_perc <- renderPlot({
		obsh() %>%
			filter(obshtina %in% c(input$obsh)) %>%
			group_by(vote_date, party) %>%
			summarise(sum_votes = sum(votes)) %>%
			mutate(prop = sum_votes / sum(sum_votes)) %>%
			mutate(party = fct_reorder(party, sum_votes)) %>%
			filter(prop >= input$prop_slider) %>%
			ggplot(aes(prop, party, fill = party)) +
			geom_col(position = "dodge", show.legend = F) +
			guides(fill = guide_legend(reverse = TRUE)) +
			scale_x_continuous(expand = expansion(mult = c(.05, .6))) +
			scale_y_discrete(labels = scales::label_wrap(50)) +
			scale_fill_manual(values = colors) +
			geom_text(aes(label = scales::percent(prop, accuracy = 0.01)),
								position = position_dodge(width = 1), hjust = -0.05, size = 3.5) +
			theme(text = element_text(size = 12),
						axis.text.x = element_blank(),
						axis.ticks.x = element_blank()) +
			labs(x = NULL, y = NULL, title = paste0("Община: ", input$obsh)) +
			facet_wrap(~ vote_date, nrow = 1, drop = F)
	}, height = 350, width = 1600, res = 96)

output$sett_perc <- renderPlot({
		sett() %>%
			filter(section %in% c(input$sett)) %>%
			group_by(vote_date, party) %>%
			summarise(sum_votes = sum(votes)) %>%
			mutate(prop = sum_votes / sum(sum_votes)) %>%
			mutate(party = fct_reorder(party, sum_votes)) %>%
			filter(prop >= input$prop_slider) %>%
			ggplot(aes(prop, party, fill = party)) +
			geom_col(position = "dodge", show.legend = F) +
			guides(fill = guide_legend(reverse = TRUE)) +
			scale_x_continuous(expand = expansion(mult = c(.05, .6))) +
			scale_y_discrete(labels = scales::label_wrap(50)) +
			scale_fill_manual(values = colors) +
			geom_text(aes(label = scales::percent(prop, accuracy = 0.01)),
								position = position_dodge(width = 1), hjust = -0.05, size = 3.5) +
			theme(text = element_text(size = 12),
						axis.text.x = element_blank(),
						axis.ticks.x = element_blank()) +
			labs(x = NULL, y = NULL, title = paste0("Населено място: ", input$sett)) +
			facet_wrap(~ vote_date, nrow = 1, drop = F)
	}, height = 350, width = 1600, res = 96)

output$sec_perc <- renderPlot({
		sec() %>%
			filter(code %in% c(input$sec)) %>%
			group_by(vote_date, party) %>%
			summarise(sum_votes = sum(votes)) %>%
			mutate(prop = sum_votes / sum(sum_votes)) %>%
			mutate(party = fct_reorder(party, sum_votes)) %>%
			filter(prop >= input$prop_slider) %>%
			ggplot(aes(prop, party, fill = party)) +
			geom_col(position = "dodge", show.legend = F) +
			guides(fill = guide_legend(reverse = TRUE)) +
			scale_x_continuous(expand = expansion(mult = c(.05, .6))) +
			scale_y_discrete(labels = scales::label_wrap(50)) +
			scale_fill_manual(values = colors) +
			geom_text(aes(label = scales::percent(prop, accuracy = 0.01)),
								position = position_dodge(width = 1), hjust = -0.05, size = 3.5) +
			theme(text = element_text(size = 12),
						axis.text.x = element_blank(),
						axis.ticks.x = element_blank()) +
			labs(x = NULL, y = NULL, title = paste0("Секция: ", input$sec),
					 caption = "Източник на данните: ЦИК.") +
			facet_wrap(~ vote_date, nrow = 1, drop = F)
	}, height = 350, width = 1600, res = 96)

output$country <- renderPlot({
  output$text1 <- renderText({ "Панелът работи със следните филтри: Филтър проценти, Височина на графиката." })
  
  votes %>%
    group_by(vote_date, party) %>%
    summarise(sum_votes = sum(votes, na.rm = T)) %>%
    mutate(prop = sum_votes / sum(sum_votes)) %>%
    mutate(party = fct_reorder(party, sum_votes)) %>%
    filter(prop >= input$prop_slider) %>%
    ggplot(aes(prop, party, fill = party)) +
    geom_col(position = "dodge", show.legend = F) +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_x_continuous(expand = expansion(mult = c(.05, .65))) +
    scale_y_discrete(labels = scales::label_wrap(50)) +
    scale_fill_manual(values = colors) +
    geom_text(aes(label = scales::percent(prop, accuracy = 0.01)),
              position = position_dodge(width = 1), hjust = -0.1, size = 4) +
    theme(text = element_text(size = 16), 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) +
    labs(x = NULL, y = NULL,
         caption = "Източник на данните: ЦИК.") +
    facet_wrap(~ vote_date, nrow = 1)
  
}, height = function() input$height_slider, width = 1600, res = 96)
#---------------------------------------
output$votes_country <- renderPlot({
  output$text2 <- renderText({ "Панелът работи със следните филтри: Филтър брой гласове, Височина на графиката." })
  
  votes %>%
  group_by(vote_date, party) %>%
  summarise(sum_votes = sum(votes)) %>%
  filter(sum_votes >= input$votes_slider) %>%
  mutate(party = fct_reorder(party, sum_votes)) %>% 
  ggplot(aes(sum_votes, party, fill = party)) +
  geom_col(position = "dodge", show.legend = F) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_y_discrete(labels = scales::label_wrap(50)) +
  scale_x_continuous(expand = expansion(mult = c(.05, .9))) +
  scale_fill_manual(values = colors) +
  geom_text(aes(label = space_s(sum_votes)), 
            position = position_dodge(width = 1), hjust = -0.05, size = 4) +
  theme(text = element_text(size = 16), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  labs(y = NULL, x = "Брой гласове", title = NULL,
       caption = "Източник на данните: ЦИК.") +
  facet_wrap(~ vote_date, nrow = 1)
}, height = function() input$height_slider, width = 1600, res = 96)
#---------------------------------------
output$lost_gained_votes <- renderPlot({
  output$text3 <- renderText({ "Панелът не работи с филтрите от страничния бар." })
  
  votes %>%
    group_by(vote_date, party) %>%
    summarise(sum_votes = sum(votes)) %>%
    pivot_wider(names_from = vote_date, values_from = sum_votes) %>% 
    mutate(diff = .data[[input$first_date]] - .data[[input$second_date]], 
           party = fct_reorder(party, diff, .na_rm = T),
           col = diff > 0, col = as.factor(col),
           col = fct_recode(col, "Загуба на гласове" = "FALSE",
                            "Печалба на гласове" = "TRUE")) %>% 
    drop_na(diff) %>% 
    ggplot(aes(diff, party, fill = col)) +
    geom_col() +
    geom_text(aes(label = space_s(diff)), 
              position = position_dodge(width = 1), hjust = -0.05, size = 16, size.unit = "pt") +
    scale_fill_manual(values = c("red", "blue")) +
    scale_x_continuous(expand = expansion(mult = c(.01, .1))) +
    theme(text = element_text(size = 16), legend.position = "top", 
          axis.text.x = element_blank(), 
          axis.ticks.x = element_blank()) +
    labs(y = NULL, x = "Брой гласове", fill = "Легенда:")
  
}, height = 700, width = 1600, res = 96)
#---------------------------------------
output$elec_act <- renderPlot({
  output$text5 <- renderText({ "Панелът не работи с филтрите от страничния бар." })

  act %>%
    mutate(election = fct_rev(election)) %>%
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
  output$text6 <- renderText({ "Панелът не работи с филтрите от страничния бар." })
  
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
# oblast_add <- reactive({
#   filter(address, oblast %in% c(input$oblast_add))
# })
# 
# observeEvent(oblast_add(), {
#   freezeReactiveValue(input, "obshtina_add")
#   choices <- unique(oblast_add()$obshtina)
#   updateSelectInput(inputId = "obshtina_add", choices = choices)
# })
# 
# obshtina_add <- reactive({
#   req(input$oblast_add)
#   filter(oblast_add(), obshtina == input$obshtina_add)
# })
#------------------------------
# output$add_plot <- renderPlot({
#   
#   obshtina_add() %>% 
#     filter(obshtina == input$obshtina_add) %>% 
#     mutate(diff = .data[[input$month_first]] - .data[[input$month_last]], 
#            sett = fct_reorder(sett, diff), 
#            col = diff > 0) %>% 
#     filter(diff != 0) %>% 
#     ggplot(aes(diff, sett, fill = col)) +
#     geom_col(show.legend = F) +
#     geom_text(aes(label = diff), 
#               position = position_dodge(width = 1), hjust = -0.05, size = 4) +
#     scale_x_continuous(expand = expansion(mult = c(.01, .15))) +
#     scale_fill_manual(values = c("TRUE" = "#00BFC4", "FALSE" = "#F8766D")) +
#     theme(text = element_text(size = 16)) +
#     labs(x = "Промяна в броя жители", y = NULL, 
#          subtitle = paste0("Разлика в броя жители в съответното населено място между месеците ", 
#                         input$month_first, " и ", input$month_last, " (2024 г.)"),
#          caption = "Източник на данните: МРРБ") +
#     facet_wrap(vars(address))
#   
# }, height = function() input$height_slider, width = 1600, res = 96)

dt_rend <- reactive({
  risk_sec %>%
    filter(oblast %in% c(input$obl), 
           obshtina %in% c(input$obsh)) %>%
    arrange(-v) %>%
    select(-v)
})

output$text4 <- renderText({ "Панелът работи със следните филтри: Избирателен район, Община." })
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
