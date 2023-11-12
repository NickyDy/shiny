library(shiny)
library(tidyverse)

dec_2022 <- read_csv("dec_2022.csv")
oct_2023 <- read_csv("oct_2023.csv") %>% mutate(id = row_number(), .before = oblast)

df <- left_join(dec_2022, oct_2023, by = c("id", "oblast", "obshtina", "sett"))

add_2019 <- read_csv("add_2019.csv") %>% 
  mutate(type = fct_recode(type, "Нстоящ адрес" = "Настоящ", "Постоянен адрес" = "Постоянен"))

location_sex <- read_csv("location_sex.csv", col_types = "fffffd") %>% 
  mutate(oblast = str_to_upper(oblast),
         oblast = fct_recode(oblast, "СОФИЙСКА" = "СОФИЯ – ОБЛАСТ"),
         obshtina = str_to_upper(obshtina),
         obshtina = fct_recode(obshtina, "ДОБРИЧ-ГРАД" = "ДОБРИЧ"))

year_oblast_age <- read_csv("year_location_age.csv", col_types = "fffffd") %>% 
  mutate(oblast = str_to_upper(oblast),
         oblast = fct_recode(oblast, "СОФИЙСКА" = "СОФИЯ", "СОФИЯ" = "СОФИЯ СТОЛИЦА"))

sentenced <- read_csv("sentenced.csv", col_types = "fdfd")

ethno <- read_csv("ethno_2021.csv") %>% 
  pivot_longer(3:9, names_to = "ethno", values_to = "value") %>% 
  mutate(oblast = str_to_upper(oblast), obshtina = str_to_upper(obshtina),
         oblast = fct_recode(oblast, "СОФИЙСКА" = "СОФИЯ – ОБЛАСТ"),
         obshtina = fct_recode(obshtina, "ДОБРИЧ-ГРАД" = "ДОБРИЧ"))
religion <- read_delim("religion_2021.csv") %>% 
  pivot_longer(3:10, names_to = "religion", values_to = "value") %>% 
  mutate(oblast = str_to_upper(oblast), obshtina = str_to_upper(obshtina),
         oblast = fct_recode(oblast, "СОФИЙСКА" = "СОФИЯ – ОБЛАСТ"),
         obshtina = fct_recode(obshtina, "ДОБРИЧ-ГРАД" = "ДОБРИЧ"))
edu <- read_csv("edu_2021.csv") %>% 
  pivot_longer(3:6, names_to = "education", values_to = "value") %>% 
  mutate(oblast = str_to_upper(oblast), obshtina = str_to_upper(obshtina),
         oblast = fct_recode(oblast, "СОФИЙСКА" = "СОФИЯ – ОБЛАСТ"),
         obshtina = fct_recode(obshtina, "ДОБРИЧ-ГРАД" = "ДОБРИЧ"))
lit <- read_csv("lit_2021.csv") %>% 
  pivot_longer(3:4, names_to = "litteracy", values_to = "value") %>% 
  mutate(oblast = str_to_upper(oblast), obshtina = str_to_upper(obshtina),
         oblast = fct_recode(oblast, "СОФИЙСКА" = "СОФИЯ – ОБЛАСТ"),
         obshtina = fct_recode(obshtina, "ДОБРИЧ-ГРАД" = "ДОБРИЧ"))
work <- read_csv("work_2021.csv") %>% 
  mutate(oblast = str_to_upper(oblast), obshtina = str_to_upper(obshtina),
         oblast = fct_recode(oblast, "СОФИЙСКА" = "СОФИЯ – ОБЛАСТ"),
         obshtina = fct_recode(obshtina, "ДОБРИЧ-ГРАД" = "ДОБРИЧ"))

colors <- c("Мъже" = "#F8766D", "Жени" = "#00BFC4", "Градове" = "#F8766D", "Села" = "#00BFC4")

colors_crim <- c("осъждане" = "red", "условно осъждане" = "orange", 
                 "освобождаване от наказание" = "green", 
                 "оправдаване" = "blue", "прекратяване" = "brown")

ml <- tags$a(href = "mailto:nickydyakov@gmail.com", icon("envelope"))
gh <- tags$a(href = "https://github.com/NickyDy", icon("github"))

fct_sort <-  function(.f, .fun = sort) {
  f = forcats:::check_factor(.f)
  fct_relevel(f, .fun(levels(f)))
}

ui <- fluidPage(theme = bslib::bs_theme(base_font = "Liberation Sans"),
                titlePanel("Промяна на адресната регистрация в България"),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("oblast", "Област", choices = unique(df$oblast)),
                    selectInput("obshtina", "Община", choices = NULL), 
                    fluidRow(ml, gh),
                    width = 2),
                  mainPanel(width = 10,
                            tabsetPanel(
                              tabPanel("2023 г.", plotOutput("diff")), 
                              tabPanel("2019 г.", plotOutput("jan_oct_2019")),
                              tabPanel("Брой жители", plotOutput("people")), 
                              tabPanel("Възраст", selectInput("age_year", "Година", 
                                choices = unique(year_oblast_age$year), selected = 2021), 
                                plotOutput("age")),
                              tabPanel("Престъпност", selectInput("sentence", "Година", 
                                choices = unique(sentenced$year), multiple = TRUE, selected = 2021), 
                                plotOutput("crim")), 
                              tabPanel("Етнос", plotOutput("ethno")),
                              tabPanel("Религия", plotOutput("religion")), 
                              tabPanel("Образование", plotOutput("education")),
                              tabPanel("Грамотност", plotOutput("litteracy")),
                              tabPanel("Заетост", plotOutput("work"), height = 850, width = 1500)))))

server <- function(input, output, session) {
  obl <- reactive({
    filter(df, oblast == input$oblast)
  })
  observeEvent(obl(), {
    freezeReactiveValue(input, "obshtina")
    choices <- unique(obl()$obshtina)
    updateSelectInput(inputId = "obshtina", choices = choices) 
  })
  
  loc_sex <- reactive({
    filter(location_sex, oblast == input$oblast)
  })
  
  age_year <- reactive({
    filter(year_oblast_age, oblast == input$oblast)
  })
  
  address_2019 <- reactive({
    filter(add_2019, oblast == input$oblast)
  })
  
  ethno_2021 <- reactive({
    filter(ethno, oblast == input$oblast)
  })
  
  religion_2021 <- reactive({
    filter(religion, oblast == input$oblast)
  })
  
  education_2021 <- reactive({
    filter(edu, oblast == input$oblast)
  })
  
  litteracy_2021 <- reactive({
    filter(lit, oblast == input$oblast)
  })
  
  work_2021 <- reactive({
    filter(work, oblast == input$oblast)
  })
  
  output$diff <- renderPlot({
    obl() %>% 
      mutate(Постоянен_адрес = pa_oct_2023 - pa_dec_2022, 
             Настоящ_адрес = na_oct_2023 - na_dec_2022) %>% 
      pivot_longer(Постоянен_адрес:Настоящ_адрес, names_to = "Гражданство", values_to = "Разлика") %>% 
      mutate(col = Разлика > 0) %>% 
      filter(oblast == input$oblast, obshtina == input$obshtina) %>% 
      ggplot(aes(Разлика, sett, fill = col)) +
      geom_col(show.legend = F) +
      geom_text(aes(label = Разлика), 
                position = position_dodge(width = 1), 
                hjust = -0.1, size = 3.5) +
      scale_x_continuous(expand = expansion(mult = c(.03, .25))) +
      theme(text = element_text(size = 12)) +
      labs(y = NULL, x = "Брой жители", 
           subtitle = paste0("Разлика в броя жители между декември, 2022 г. и октомври, 2023 г. в Област: ", 
                             input$oblast, ", Община: ", input$obshtina),
           caption = "Източник на данните: МРРБ") +
      facet_wrap(vars(Гражданство))
  }, height = 850, width = 1500, res = 96)
  
  output$jan_oct_2019 <- renderPlot({
    address_2019() %>% 
      pivot_wider(names_from = month, values_from = value) %>% 
      mutate(diff = oct - jan) %>% 
      mutate(col = diff > 0) %>% 
      filter(oblast == input$oblast, obshtina == input$obshtina) %>% 
      ggplot(aes(diff, sett, fill = col)) +
      geom_col(show.legend = F) +
      geom_text(aes(label = diff), 
                position = position_dodge(width = 1), 
                hjust = -0.1, size = 3.5) +
      scale_x_continuous(expand = expansion(mult = c(.03, .25))) +
      theme(text = element_text(size = 12), legend.position = "right",
            axis.text.x = element_blank(), 
            axis.ticks.x = element_blank()) +
      labs(y = NULL, x = "Брой жители", 
           title = paste0("Разлика в броя жители между януари и октомври, 2019 г. в Област: ", 
                          input$oblast, ", Община: ", input$obshtina),
           caption = "Източник на данните: МРРБ") +
      facet_wrap(vars(type))
  }, height = 850, width = 1500, res = 96)
  
  output$people <- renderPlot({
    loc_sex() %>%
      filter(oblast == input$oblast, obshtina == input$obshtina, year == "2021") %>% 
      ggplot(aes(pop, sex, fill = sex)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = colors) +
      scale_x_continuous(expand = expansion(mult = c(.05, .55))) +
      geom_text(aes(label = pop), position = position_dodge(width = 1), hjust = -0.1, size = 3.5) +
      theme(text = element_text(size = 12), legend.position = "none", 
            axis.text.x = element_blank(), 
            axis.ticks.x = element_blank()) +
      labs(y = NULL, x = NULL, fill = "Пол:",
           subtitle = paste0("Брой жители по населени места към 2021 г. в Област: ", 
                             input$oblast, ", Община: ", input$obshtina),
           caption = "Източник на данните: Инфостат") +
      facet_wrap(vars(location), scales = "free_x")
  }, height = 850, width = 1500, res = 96)
  
  output$age <- renderPlot({
    age_year() %>% 
      filter(oblast == input$oblast, year %in% input$age_year) %>%
      group_by(year, age, settlement, sex) %>%
      summarise(s = sum(pop, na.rm = T)) %>%
      ungroup() %>%
      ggplot(aes(s, age, fill = sex)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = colors) +
      scale_x_continuous(expand = expansion(mult = c(0.02, 0.15))) +
      geom_text(aes(label = s), position = position_dodge(width = 1), hjust = -0.1, size = 3.5) +
      theme(text = element_text(size = 12), legend.position = "right") +
      labs(y = "Възраст", x = "Популация", 
           title = paste0("Възрастова и полова структура на населението в област: ", 
                          input$oblast, " (", input$age_year, " г.)"),
           fill = "Пол:", caption = "Източник на данните: Инфостат") +
      guides(fill = guide_legend(reverse = TRUE)) +
      facet_wrap(~settlement)
  }, height = 750, width = 1500, res = 96)
  
  output$crim <- renderPlot({
    sentenced %>% 
      mutate(sentence = fct_relevel(sentence, "прекратяване", 
                                    "оправдаване", 
                                    "освобождаване от наказание",
                                    "условно осъждане",
                                    "осъждане"),
             oblast = str_wrap(oblast, 20)) %>% 
      filter(year %in% input$sentence) %>% 
      group_by(oblast, sentence) %>% 
      summarise(s = sum(value)) %>% 
      ggplot(aes(s, sentence, fill = sentence)) +
      geom_col() +
      geom_text(aes(label = s), position = position_dodge(width = 1), hjust = -0.1, size = 3.5) +
      scale_x_continuous(expand = expansion(mult = c(0, 0.2)), n.breaks = 4) +
      scale_fill_manual(values = colors_crim) +
      theme(text = element_text(size = 12), legend.position = "none") +
      labs(x = "Брой дела", y = NULL, caption = "Източник на данните: Инфостат") +
      facet_wrap(vars(fct_sort(oblast)))
  }, height = 750, width = 1500, res = 96)
  
  output$ethno <- renderPlot({
    ethno_2021() %>% 
      filter(oblast == input$oblast) %>% 
      group_by(obshtina) %>% 
      mutate(sum = sum(value, na.rm = T), prop = value / sum) %>% 
      mutate(ethno = fct_relevel(ethno, "Непоказана", "Не желая да отговоря", "Не мога да определя", 
                                 "Друга", "Ромска", "Турска", "Българска")) %>% 
      ggplot(aes(prop, ethno, fill = ethno)) +
      geom_col(show.legend = F) +
      geom_text(aes(label = scales::percent(round(prop, digits = 3))), 
                position = position_dodge(width = 1), hjust = -0.1, size = 3.5) +
      theme(text = element_text(size = 12), axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      scale_x_continuous(expand = expansion(mult = c(.05, .25))) +
      labs(x = NULL, y = NULL, 
           title = paste0("Етническата принадлежност на населението на област: ", 
                          input$oblast, " (2021 г.)"), 
           caption = "Източник на данните: Инфостат") +
      facet_wrap(~ obshtina)
  }, height = 850, width = 1500, res = 96)
  
  output$religion <- renderPlot({
    religion_2021() %>% 
      filter(oblast == input$oblast) %>% 
      group_by(obshtina) %>% 
      mutate(sum = sum(value, na.rm = T), prop = value / sum) %>% 
      mutate(religion = fct_relevel(religion, "Непоказано", "Не желая да отговоря", "Не мога да определя", 
                                    "Нямам", "Друго", "Юдейско", "Мюсюлманско", "Християнско")) %>% 
      ggplot(aes(prop, religion, fill = religion)) +
      geom_col(show.legend = F) +
      geom_text(aes(label = scales::percent(round(prop, digits = 3))), 
                position = position_dodge(width = 1), hjust = -0.1, size = 3.5) +
      theme(text = element_text(size = 12), axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      scale_x_continuous(expand = expansion(mult = c(.05, .3))) +
      labs(x = NULL, y = NULL, 
           title = paste0("Религиозна принадлежност на населението в област: ", 
                          input$oblast, " (2021 г.)"), 
           caption = "Източник на данните: Инфостат") +
      facet_wrap(~ obshtina)
  }, height = 850, width = 1500, res = 96)
  
  output$education <- renderPlot({
    education_2021() %>% 
      filter(oblast == input$oblast) %>% 
      group_by(obshtina) %>% 
      mutate(sum = sum(value), prop = value / sum) %>% 
      mutate(education = fct_relevel(education, "Начално и по-ниско образование", "Основно", 
                                     "Средно образование", "Висше образование")) %>% 
      ggplot(aes(prop, education, fill = education)) +
      geom_col(show.legend = F) +
      geom_text(aes(label = scales::percent(round(prop, digits = 3))), 
                position = position_dodge(width = 1), hjust = -0.1, size = 3.5) +
      theme(text = element_text(size = 12), axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      scale_x_continuous(expand = expansion(mult = c(.05, .3))) +
      labs(x = NULL, y = NULL, 
           title = paste0("Образование на населението в област: ", input$oblast, " (2021 г.)"), 
           caption = "Източник на данните: Инфостат") +
      facet_wrap(~ obshtina)
  }, height = 850, width = 1500, res = 96)
  
  output$litteracy <- renderPlot({
    litteracy_2021() %>% 
      filter(oblast == input$oblast) %>% 
      group_by(obshtina) %>% 
      mutate(sum = sum(value), prop = value / sum) %>% 
      mutate(litteracy = fct_rev(litteracy)) %>% 
      ggplot(aes(prop, litteracy, fill = litteracy)) +
      geom_col(show.legend = F) +
      geom_text(aes(label = scales::percent(round(prop, digits = 3))), 
                position = position_dodge(width = 1), hjust = -0.1, size = 3.5) +
      theme(text = element_text(size = 12), axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      scale_x_continuous(expand = expansion(mult = c(.05, .3))) +
      labs(x = NULL, y = NULL, 
           title = paste0("Грамотност на населението в област: ", input$oblast, " (2021 г.)"), 
           caption = "Източник на данните: Инфостат") +
      facet_wrap(~ obshtina)
  }, height = 850, width = 1500, res = 96)
  
  output$work <- renderPlot({
    work_2021() %>% 
      filter(oblast == input$oblast, !name == "Работна сила") %>% 
      group_by(oblast, obshtina) %>% 
      mutate(prop = round(value/sum(value), 3)) %>% 
      mutate(name = fct_relevel(name, "Лица извън работната сила", "Безработни", "Заети",
                                "Работна сила")) %>% 
      ggplot(aes(prop, name, fill = sex)) +
      geom_col(show.legend = T, position = "dodge") +
      geom_text(aes(label = scales::percent(prop)), 
                position = position_dodge(width = 1), hjust = -0.1, size = 3.5) +
      theme(text = element_text(size = 12), axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      scale_x_continuous(expand = expansion(mult = c(.05, .3))) +
      labs(x = NULL, y = NULL, fill = "Пол:",
           title = paste0("Заетост на населението в област: ", input$oblast), 
           caption = "Източник на данните: Инфостат") +
      scale_fill_manual(values = colors) +
      guides(fill = guide_legend(reverse = TRUE)) +
      facet_wrap(~ obshtina)
  }, height = 850, width = 1500, res = 96)
  
session$onSessionEnded(function() {
    stopApp()
  })
}
shinyApp(ui, server)
