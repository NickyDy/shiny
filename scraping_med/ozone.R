library(tidyverse)
library(rvest)
library(arrow)
#--------------------------------------------------------
base_url <- "https://www.ozone.bg/zdrave-i-krasota/apteka/?p=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-box") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Лекарства",
           source = "Ozone", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:48
lekarstva <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://www.ozone.bg/zdrave-i-krasota/kozmetika/?p=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-box") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Козметика",
           source = "Ozone", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:167
kozmetika <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://www.ozone.bg/zdrave-i-krasota/hranitelni-dobavki/?p=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-box") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Хранителни добавки",
           source = "Ozone", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:83
dobavki <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://www.ozone.bg/zdrave-i-krasota/sportni-dobavki/?p=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-box") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Спортни добавки",
           source = "Ozone", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:56
sport <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://www.ozone.bg/zdrave-i-krasota/zdravoslovni-hrani/?p=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-box") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Здравословни храни",
           source = "Ozone", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:18
hrani <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
ozone <- bind_rows(lekarstva, dobavki, kozmetika, sport, 
                   hrani)
glimpse(ozone)
write_parquet(ozone, "shiny/scraping_med/ozone.parquet")
