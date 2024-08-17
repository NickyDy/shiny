library(tidyverse)
library(rvest)
library(arrow)
#--------------------------------------------------------
base_url <- "https://www.aptekapromahon.com/farmakeio-c-186.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".grid-style") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-name") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".discount-price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Лекарства",
           source = "Аптека Промахон", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:81
lekarstva <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://www.aptekapromahon.com/bitamines-c-5083.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".grid-style") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-name") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".discount-price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Хранителни добавки",
           source = "Аптека Промахон", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:92
dobavki <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://www.aptekapromahon.com/ygieinh-c-40.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".grid-style") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-name") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".discount-price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Хигиена",
           source = "Аптека Промахон", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:205
higiena <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://www.aptekapromahon.com/omorfia-c-39.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".grid-style") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-name") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".discount-price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Красота",
           source = "Аптека Промахон", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:175
krasota <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://www.aptekapromahon.com/adynatisma-c-4961.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".grid-style") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-name") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".discount-price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Отслабване",
           source = "Аптека Промахон", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:13
otslabvane <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://www.aptekapromahon.com/prosfores-c-3340.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".grid-style") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-name") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".discount-price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Оферти",
           source = "Аптека Промахон", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:274
oferti <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://www.aptekapromahon.com/epoxiaka-c-3471.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".grid-style") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-name") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".discount-price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Сезонни продукти",
           source = "Аптека Промахон", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:59
sezonni <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------
promahon <- bind_rows(lekarstva, krasota, oferti, otslabvane,
                      dobavki, higiena, sezonni)
glimpse(promahon)
write_parquet(promahon, "shiny/scraping_med/promahon.parquet")
