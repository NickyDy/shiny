library(tidyverse)
library(rvest)
library(arrow)
#--------------------------------------------------------
base_url <- "https://salvia.bg/bg/230-lekarstva?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".js-product-miniature") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".product-price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Лекарства",
           source = "Салвия", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:90
lekarstva <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://salvia.bg/bg/3-kozmetika?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".js-product-miniature") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".product-price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Козметика",
           source = "Салвия", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:133
kozmetika <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://salvia.bg/bg/4-khomeopatiya?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".js-product-miniature") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".product-price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Хомеопатия",
           source = "Салвия", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:31
homeopatiq <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://salvia.bg/bg/5-khranitelni-dobavki?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".js-product-miniature") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".product-price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Хранителни добавки",
           source = "Салвия", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:108
dobavki <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://salvia.bg/bg/9-mama-i-bebe?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".js-product-miniature") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".product-price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Мама и бебе",
           source = "Салвия", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:4
mom_baby <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://salvia.bg/bg/7-khigiena-i-zashhita?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".js-product-miniature") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".product-price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Хигиена и защита",
           source = "Салвия", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:63
higiena <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://salvia.bg/bg/8-aparati-i-termometri?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".js-product-miniature") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".product-price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Апарати и термометри",
           source = "Салвия", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:8
aparati <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
salvia <- bind_rows(lekarstva, dobavki, kozmetika, aparati,
                    mom_baby, higiena, homeopatiq)
glimpse(salvia)
write_parquet(salvia, "shiny/scraping_med/salvia.parquet")
