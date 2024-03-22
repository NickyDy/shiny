library(tidyverse)
library(shiny)
library(arrow)
library(scales)
library(bslib)

loc_sex <- read_parquet("loc_sex.parquet") %>% arrange(oblast) %>% drop_na()
obl_age_sex <- read_parquet("obl_age_sex.parquet")
labor_sett_sex <- read_parquet("labor_sett_sex.parquet") %>% select(-sett) %>% arrange(obsh)
birth_rate <- read_parquet("birth_rate.parquet") %>% arrange(oblast)
mortality <- read_parquet("mortality.parquet") %>% arrange(oblast)
brakove <- read_parquet("brakove.parquet") %>% arrange(obshtina)
razvodi <- read_parquet("razvodi.parquet") %>% arrange(obshtina)
int_migration <- read_parquet("int_migration.parquet") %>% filter(pop != 0) %>% arrange(obshtina)
ext_migration <- read_parquet("ext_migration.parquet")
school <- read_parquet("school.parquet")
university <- read_parquet("university.parquet") %>% arrange(oblast)
health <- read_parquet("health.parquet") %>% filter(pop != 0) %>% arrange(oblast)
kinder_gardens <- read_parquet("kinder_gardens.parquet") %>% arrange(obshtina)
poverty <- read_parquet("poverty.parquet")
#-----------------------------------------
colors_sex <- c("Мъже" = "#F8766D", "Жени" = "#00BFC4")
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
#------------------------------------------------
ui <- page_fillable(h3("Демография на България!"),
  navset_pill(
    nav_panel(title = "Население",
              layout_columns(
              selectInput("loc_sex_oblast", "Област:",
                          choices = unique(loc_sex$oblast)),
              selectInput("loc_sex_obshtina", "Община:",
                          choices = NULL),
              selectInput("loc_sex_location", "Населено място:",
                          choices = NULL), col_widths = c(2, 2, 2)),
              plotOutput("loc_sex_plot")),
    nav_panel(title = "Възраст",
              layout_columns(
                selectInput("obl_age_sex_oblast", "Област:",
                            choices = unique(obl_age_sex$oblast)),
                selectInput("obl_age_sex_year", "Населено място:",
                            choices = unique(obl_age_sex$year),
                            selected = "2022"), 
                            col_widths = c(2, 1)),
              plotOutput("obl_age_sex_plot")),
    nav_panel("Раждаемост",
              layout_columns(
                selectInput("birth_rate_oblast", "Област:",
                            choices = unique(birth_rate$oblast)),
                selectInput("birth_rate_coef", "Коефициент:",
                            choices = NULL),
                            col_widths = c(2, 4)),
             plotOutput("birth_rate_plot")),
    nav_panel("Смъртност", layout_columns(
                selectInput("mortality_oblast", "Област:",
                            choices = unique(mortality$oblast)),
                selectInput("mortality_coef", "Коефициент:",
                            choices = NULL),
                            col_widths = c(2, 2)),
             plotOutput("mortality_plot")),
    nav_panel("Трудоспособност", layout_columns(
               selectInput("labor_sett_sex_obsh", "Община:",
                            choices = unique(labor_sett_sex$obsh)), 
               col_widths = c(1)),
             plotOutput("labor_sett_sex_plot")),
    nav_panel("Бракове", layout_columns(
               selectInput("brakove_obsh", "Община:",
                            choices = unique(brakove$obshtina)),
               col_widths = c(1)),
             plotOutput("brakove_plot")),
    nav_panel("Разводи", layout_columns(
               selectInput("razvodi_obsh", "Община:",
                            choices = unique(razvodi$obshtina)),
               col_widths = c(1)),
             plotOutput("razvodi_plot")),
    nav_panel("Вътрешна миграция", layout_columns(
                selectInput("int_migration_obshtina", "Община:",
                            choices = unique(int_migration$obshtina)),
                selectInput("int_migration_sett", "Тип населено място:",
                            choices = NULL),
                            col_widths = c(1, 2)),
             plotOutput("int_migration_plot")),
    nav_panel("Външна миграция", layout_columns(
               selectInput("ext_migration_age", "Възраст:",
                            choices = unique(ext_migration$age)),
               col_widths = c(1)),
             plotOutput("ext_migration_plot")),
    nav_panel("Средно образование",
               selectInput("school_obsh", "Община:",
                            choices = unique(school$obshtina)),
             plotOutput("school_plot")),
    nav_panel("Висше образование",
               selectInput("university_obl", "Област:",
                            choices = unique(university$oblast)),
             plotOutput("university_plot")),
    nav_panel("Здравеопазване", layout_columns(
               selectInput("health_oblast", "Област:",
                            choices = unique(health$oblast)),
               selectInput("health_zab", "Причина за смъртта:",
                            choices = NULL),
               col_widths = c(2, 8)),
             plotOutput("health_plot")),
    nav_panel("Детски градини", layout_columns(
               selectInput("kinder_gardens_obsh", "Община:",
                            choices = unique(kinder_gardens$obshtina)),
               col_widths = c(1)),
             plotOutput("kinder_gardens_plot")),
    nav_panel("Работещи бедни",
             plotOutput("poverty_plot")),
    nav_panel(tags$img(src = "kofi.png", width = 40),
              "Ако Ви харесва приложението,
               можете да направите дарение в евро към
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
#-------------------------------------------
server <- function(input, output, session) {
  
  loc_sex_oblast <- reactive({
    filter(loc_sex, oblast %in% c(input$loc_sex_oblast))
  })
  
  observeEvent(loc_sex_oblast(), {
    freezeReactiveValue(input, "loc_sex_obshtina")
    choices <- unique(loc_sex_oblast()$obshtina)
    updateSelectInput(inputId = "loc_sex_obshtina", choices = choices)
  })
  
  loc_sex_obshtina <- reactive({
    req(input$loc_sex_oblast)
    filter(loc_sex_oblast(), obshtina == input$loc_sex_obshtina)
  })
  
  observeEvent(loc_sex_obshtina(), {
    freezeReactiveValue(input, "loc_sex_location")
    choices <- unique(loc_sex_obshtina()$location)
    updateSelectInput(inputId = "loc_sex_location", choices = choices)
  })
  
  loc_sex_location <- reactive({
    req(input$loc_sex_obshtina)
    filter(loc_sex_obshtina(), location == input$loc_sex_location)
  })
  
  output$loc_sex_plot <- renderPlot({
    loc_sex_location() %>% 
      filter(obshtina %in% c(input$loc_sex_obshtina),
             location %in% c(input$loc_sex_location)) %>% 
      ggplot(aes(year, pop, fill = sex)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = colors_sex) +
      scale_y_continuous(expand = expansion(mult = c(0.01, 0.3))) +
      geom_text(aes(label = space_s(pop)), 
                position = position_dodge(width = 1), hjust = -0.1, size = 4.5, angle = 90) +
      theme(text = element_text(size = 16), legend.position = "right") +
      labs(x = NULL, y = "Брой жители", fill = "Пол:",
           caption = "Източник на данните: Infostat")
  }, height = 800, width = 1800, res = 96)
  #-----------------------------------------
  obl_age_sex_oblast <- reactive({
    filter(obl_age_sex, oblast %in% c(input$obl_age_sex_oblast))
  })

  output$obl_age_sex_plot <- renderPlot({

    obl_age_sex_oblast() %>%
      filter(year %in% c(input$obl_age_sex_year)) %>%
      ggplot(aes(pop, sex, fill = sex)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = colors_sex) +
      scale_x_continuous(expand = expansion(mult = c(.01, .15))) +
      geom_text(aes(label = pop), position = position_dodge(width = 1), hjust = -0.1, size = 4) +
      theme(text = element_text(size = 14), legend.position = "none") +
      labs(y = NULL, x = "Брой жители", title = "", fill = "Пол:",
           caption = "Източник на данните: Infostat") +
      facet_grid(age ~ settlement)

  }, height = 1400, width = 1800, res = 96)
#---------------------------------------
  labor_sett_sex_obsh <- reactive({
    filter(labor_sett_sex, obsh %in% c(input$labor_sett_sex_obsh))
  })

  output$labor_sett_sex_plot <- renderPlot({

    labor_sett_sex_obsh() %>%
      ggplot(aes(year, pop, fill = sex)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = colors_sex) +
      scale_y_continuous(expand = expansion(mult = c(.01, .45))) +
      geom_text(aes(label = pop), position = position_dodge(width = 1),
                hjust = -0.1, size = 4, angle = 90) +
      theme(text = element_text(size = 16), legend.position = "right") +
      labs(x = NULL, y = "Брой хора", title = "", fill = "Пол:",
           caption = "Източник на данните: Infostat") +
      facet_wrap(vars(labor), ncol = 1)

  }, height = 800, width = 1800, res = 96)
#---------------------------------------
  birth_rate_oblast <- reactive({
    filter(birth_rate, oblast %in% c(input$birth_rate_oblast))
  })

  observeEvent(birth_rate_oblast(), {
    freezeReactiveValue(input, "birth_rate_coef")
    choices <- unique(birth_rate_oblast()$coef)
    updateSelectInput(inputId = "birth_rate_coef", choices = choices)
  })

  birth_rate_coef <- reactive({
    req(input$birth_rate_oblast)
    filter(birth_rate_oblast(), coef == input$birth_rate_coef)
  })

  output$birth_rate_plot <- renderPlot({

    birth_rate_coef() %>%
      filter(coef %in% c(input$birth_rate_coef)) %>%
      ggplot(aes(year, pop, fill = sett)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("В градовете" = "#00BFC4", "В селата" = "#F8766D")) +
      scale_y_continuous(expand = expansion(mult = c(.01, .15))) +
      geom_text(aes(label = pop), position = position_dodge(width = 1), vjust = -0.2, size = 4) +
      theme(text = element_text(size = 16), legend.position = "none") +
      labs(y = input$birth_rate_coef, x = NULL, fill = "Легенда:",
           caption = "Източник на данните: Infostat") +
      facet_wrap(vars(sett), ncol = 1)

  }, height = 800, width = 1800, res = 96)
#---------------------------------------
  mortality_oblast <- reactive({
    filter(mortality, oblast %in% c(input$mortality_oblast))
  })

  observeEvent(mortality_oblast(), {
    freezeReactiveValue(input, "mortality_coef")
    choices <- unique(mortality_oblast()$coef)
    updateSelectInput(inputId = "mortality_coef", choices = choices)
  })

  mortality_coef <- reactive({
    req(input$mortality_oblast)
    filter(mortality_oblast(), coef == input$mortality_coef)
  })

  output$mortality_plot <- renderPlot({

    mortality_coef() %>%
      filter(coef %in% c(input$mortality_coef)) %>%
      ggplot(aes(year, pop, fill = sex)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = c("Жени" = "#00BFC4", "Мъже" = "#F8766D")) +
      scale_y_continuous(expand = expansion(mult = c(.01, .15))) +
      geom_text(aes(label = pop), position = position_dodge(width = 1), vjust = -0.2, size = 3.5) +
      theme(text = element_text(size = 16), legend.position = "right") +
      labs(y = paste0(input$mortality_coef, " (%)"), x = NULL, fill = "Легенда:",
           caption = "Източник на данните: Infostat") +
      facet_wrap(vars(sett), ncol = 1)

  }, height = 800, width = 1800, res = 96)
#---------------------------------------
  output$brakove_plot <- renderPlot({

    brakove %>%
      filter(obshtina %in% c(input$brakove_obsh)) %>%
      ggplot(aes(year, pop, fill = sett)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = c("В градовете" = "#00BFC4", "В селата" = "#F8766D")) +
      scale_y_continuous(expand = expansion(mult = c(.01, .15))) +
      geom_text(aes(label = pop), position = position_dodge(width = 1), vjust = -0.1, size = 4) +
      theme(text = element_text(size = 16), legend.position = "none") +
      labs(x = NULL, y = "Брой бракове", fill = "Пол:",
           caption = "Източник на данните: Infostat") +
      facet_wrap(vars(sett), ncol = 1)

  }, height = 800, width = 1800, res = 96)
#---------------------------------------
  output$razvodi_plot <- renderPlot({

    razvodi %>%
      filter(obshtina %in% c(input$razvodi_obsh)) %>%
      ggplot(aes(year, pop, fill = sett)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = c("В градовете" = "#00BFC4", "В селата" = "#F8766D")) +
      scale_y_continuous(expand = expansion(mult = c(.01, .15))) +
      geom_text(aes(label = pop), position = position_dodge(width = 1), vjust = -0.1, size = 4) +
      theme(text = element_text(size = 16), legend.position = "none") +
      labs(x = NULL, y = "Брой разводи", fill = "Пол:",
           caption = "Източник на данните: Infostat") +
      facet_wrap(vars(sett), ncol = 1)

  }, height = 800, width = 1800, res = 96)
  #---------------------------------------
  int_migration_obshtina <- reactive({
    filter(int_migration, obshtina %in% c(input$int_migration_obshtina))
  })

  observeEvent(int_migration_obshtina(), {
    freezeReactiveValue(input, "int_migration_sett")
    choices <- unique(int_migration_obshtina()$sett)
    updateSelectInput(inputId = "int_migration_sett", choices = choices)
  })

  int_migration_sett <- reactive({
    req(input$int_migration_obshtina)
    filter(int_migration_obshtina(), sett == input$int_migration_sett)
  })

  output$int_migration_plot <- renderPlot({

    int_migration_sett() %>%
      filter(sett %in% c(input$int_migration_sett)) %>%
      ggplot(aes(year, pop, fill = sex)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = c("Жени" = "#00BFC4", "Мъже" = "#F8766D")) +
      scale_y_continuous(expand = expansion(mult = c(.01, .4))) +
      geom_text(aes(label = pop), position = position_dodge(width = 1),
                hjust = -0.1, size = 4, angle = 90) +
      theme(text = element_text(size = 16), legend.position = "right") +
      labs(y = "Брой хора", x = NULL, fill = "Пол:",
           caption = "Източник на данните: Infostat") +
      facet_wrap(vars(migrated), ncol = 1)

  }, height = 800, width = 1800, res = 96)
  #---------------------------------------
  output$ext_migration_plot <- renderPlot({

    ext_migration %>%
      filter(age %in% c(input$ext_migration_age)) %>%
      ggplot(aes(year, pop, fill = sex)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = c("Жени" = "#00BFC4", "Мъже" = "#F8766D")) +
      scale_y_continuous(expand = expansion(mult = c(.01, .55))) +
      geom_text(aes(label = pop), position = position_dodge(width = 1),
                hjust = -0.1, size = 4, angle = 90) +
      theme(text = element_text(size = 16), legend.position = "right") +
      labs(y = "Брой хора", x = NULL, fill = "Пол:",
           caption = "Източник на данните: Infostat") +
      facet_wrap(vars(migrated), ncol = 1)

  }, height = 800, width = 1800, res = 96)
  #---------------------------------------
  output$school_plot <- renderPlot({

    school %>%
      filter(obshtina %in% c(input$school_obsh)) %>%
      ggplot(aes(year, pop, fill = education)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = c("Средно образование" = "#00BFC4", "Основно образование" = "#F8766D")) +
      scale_y_continuous(expand = expansion(mult = c(.01, .2))) +
      geom_text(aes(label = pop), position = position_dodge(width = 1), vjust = -0.1, size = 4) +
      theme(text = element_text(size = 16), legend.position = "none") +
      labs(y = "Брой дипломирани ученици", x = NULL, fill = "Пол:",
           caption = "Източник на данните: Infostat") +
      facet_wrap(vars(education), ncol = 1) +
      guides(fill = guide_legend(reverse = TRUE))

  }, height = 800, width = 1800, res = 96)
  #---------------------------------------

  output$university_plot <- renderPlot({

    university %>%
      filter(oblast %in% c(input$university_obl)) %>%
      ggplot(aes(year, pop, fill = sex)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = c("Жени" = "#00BFC4", "Мъже" = "#F8766D")) +
      scale_y_continuous(expand = expansion(mult = c(.01, .2))) +
      geom_text(aes(label = pop), position = position_dodge(width = 1), vjust = -0.1, size = 4) +
      theme(text = element_text(size = 16), legend.position = "right") +
      labs(y = "Брой дипломирани студенти", x = NULL, fill = "Пол:",
           caption = "Източник на данните: Infostat") +
      facet_wrap(vars(grade), ncol = 1) +
      guides(fill = guide_legend(reverse = TRUE))

  }, height = 800, width = 1800, res = 96)
  #---------------------------------------
  health_oblast <- reactive({
    filter(health, oblast %in% c(input$health_oblast))
  })

  observeEvent(health_oblast(), {
    freezeReactiveValue(input, "health_zab")
    choices <- unique(health_oblast()$zabolqvane)
    updateSelectInput(inputId = "health_zab", choices = choices)
  })

  health_zab <- reactive({
    req(input$health_oblast)
    filter(health_oblast(), zabolqvane == input$health_zab)
  })

  output$health_plot <- renderPlot({

    health_zab() %>%
      filter(zabolqvane %in% c(input$health_zab)) %>%
      ggplot(aes(year, pop, fill = sex)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = c("Жени" = "#00BFC4", "Мъже" = "#F8766D")) +
      scale_y_continuous(expand = expansion(mult = c(.01, .2))) +
      geom_text(aes(label = round(pop, 1)), 
                position = position_dodge(width = 1), vjust = -0.1, size = 4) +
      theme(text = element_text(size = 16), legend.position = "right") +
      labs(y = "Брой починали", x = NULL, fill = "Пол:",
           caption = "Източник на данните: Infostat") +
      guides(fill = guide_legend(reverse = TRUE))

  }, height = 800, width = 1800, res = 96)
  #---------------------------------------

  output$kinder_gardens_plot <- renderPlot({

    kinder_gardens %>%
      filter(obshtina %in% c(input$kinder_gardens_obsh)) %>%
      ggplot(aes(year, pop, fill = sex)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = c("Момичета" = "#00BFC4", "Момчета" = "#F8766D")) +
      scale_y_continuous(expand = expansion(mult = c(.01, .2))) +
      geom_text(aes(label = space_s(pop)),
                position = position_dodge(width = 1), hjust = -0.1, size = 4, angle = 90) +
      theme(text = element_text(size = 16), legend.position = "right",
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      labs(y = "Брой деца", x = NULL, fill = "Пол:",
           caption = "Източник на данните: Infostat") +
      guides(fill = guide_legend(reverse = TRUE))

  }, height = 800, width = 1800, res = 96)
  #---------------------------------------

  output$poverty_plot <- renderPlot({

    poverty %>%
      ggplot(aes(year, pop, fill = sex)) +
      geom_col(position = position_dodge2(preserve = "single")) +
      scale_fill_manual(values = c("Жени" = "#00BFC4", "Мъже" = "#F8766D")) +
      scale_y_continuous(expand = expansion(mult = c(.01, .2))) +
      geom_text(aes(label = pop),
                position = position_dodge(width = 1), vjust = -0.1, size = 4) +
      theme(text = element_text(size = 16), legend.position = "right") +
      labs(y = "Процент от работещите", x = NULL, fill = "Пол:",
           caption = "Източник на данните: Infostat") +
      facet_wrap(vars(age), ncol = 1) +
      guides(fill = guide_legend(reverse = TRUE))

  }, height = 800, width = 1800, res = 96)
#---------------------------------------
  
session$onSessionEnded(function() {
    stopApp()
  })
}
shinyApp(ui, server)

