library(tidyverse)
library(rvest)
library(arrow)
#--------------------------------------------------------
base_url <- "https://www.afya-pharmacy.bg/farmatsiya-i-zdrave/page-%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".productName") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".currPrice") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Лекарства",
           source = "Afya", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
           price = price / 100,
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:223
lekarstva <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://www.afya-pharmacy.bg/homeopatiya/page-%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".productName") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".currPrice") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Хомеопатия",
           source = "Afya", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
           price = price / 100,
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:19
homeopatiq <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://www.afya-pharmacy.bg/bilki-i-chayove/page-%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".productName") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".currPrice") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Билки и чайове",
           source = "Afya", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
           price = price / 100,
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:14
bilki <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://www.afya-pharmacy.bg/kozmetika/page-%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".productName") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".currPrice") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Козметика",
           source = "Afya", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
           price = price / 100,
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:202
kozmetika <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://www.afya-pharmacy.bg/u-doma-i-navan/page-%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".productName") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".currPrice") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "У дома и навън",
           source = "Afya", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
           price = price / 100,
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:18
home_outside <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://www.afya-pharmacy.bg/mama-i-bebe/page-%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".productName") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".currPrice") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Мама и бебе",
           source = "Afya", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
           price = price / 100,
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:56
mom_baby <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://www.afya-pharmacy.bg/tya-i-toy/page-%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".productName") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".currPrice") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Тя и Той",
           source = "Afya", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
           price = price / 100,
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:17
she_him <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
afya <- bind_rows(lekarstva, kozmetika, mom_baby, bilki,
                  homeopatiq, home_outside, she_him)
glimpse(lekarstva)
write_parquet(afya, "shiny/scraping_med/afya.parquet")
