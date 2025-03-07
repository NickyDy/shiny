library(tidyverse)
library(rvest)
library(arrow)
#--------------------------------------------------------
base_url <- "https://epharm.bg/lekarstva.html?p=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-item-info") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-item-link") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Лекарства",
           source = "Epharm", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:49
lekarstva <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://epharm.bg/hranitelni-dobavki.html?p=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-item-info") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-item-link") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Хранителни добавки",
           source = "Epharm", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:228
dobavki <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://epharm.bg/kozmetika.html?p=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-item-info") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-item-link") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Козметика",
           source = "Epharm", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:94
kozmetika <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://epharm.bg/bebeshki-produkti.html?p=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-item-info") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-item-link") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Мама и бебе",
           source = "Epharm", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:26
mom_baby <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://epharm.bg/higiena.html?p=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-item-info") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-item-link") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Хигиена",
           source = "Epharm", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:37
hygene <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://epharm.bg/medicinska-tehnika.html?p=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-item-info") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-item-link") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Медицинска техника",
           source = "Epharm", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:32
tehnika <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------
epharm <- bind_rows(lekarstva, dobavki, kozmetika, mom_baby,
                    hygene, tehnika)
glimpse(epharm)
write_parquet(epharm, "shiny/scraping_med/epharm.parquet")
