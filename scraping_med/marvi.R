library(tidyverse)
library(rvest)
library(arrow)
#--------------------------------------------------------
base_url <- "https://www.marvi.bg/kategorii/lechenie-zdrave/1595?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".uk-card-body") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".uk-card-title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".product-price-new") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Лекарства",
           source = "Marvi", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:59
lekarstva <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://www.marvi.bg/kategorii/hranitelni-dobavki/198?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".uk-card-body") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".uk-card-title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".product-price-new") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Хранителни добавки",
           source = "Marvi", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:220
dobavki <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://www.marvi.bg/kategorii/mama-bebe/1596?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".uk-card-body") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".uk-card-title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".product-price-new") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Мама и бебе",
           source = "Marvi", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:62
mom_baby <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://www.marvi.bg/kategorii/kozmetika-krasota/1600?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".uk-card-body") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".uk-card-title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".product-price-new") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Козметика",
           source = "Marvi", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:52
kozmetika <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://www.marvi.bg/kategorii/bilkova-apteka/195?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".uk-card-body") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".uk-card-title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".product-price-new") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Билки",
           source = "Marvi", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:68
bilki <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://www.marvi.bg/kategorii/aparati-i-uredi/342?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".uk-card-body") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".uk-card-title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".product-price-new") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Апарати и уреди",
           source = "Marvi", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:26
aparati <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://www.marvi.bg/kategorii/homeopatiq/229?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".uk-card-body") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".uk-card-title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".product-price-new") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Хомеопатия",
           source = "Marvi", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:6
homeopatiq <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://www.marvi.bg/kategorii/higiena-dom/421?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".uk-card-body") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".uk-card-title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".product-price-new") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Хигиена",
           source = "Marvi", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:22
higiena <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------
marvi <- bind_rows(lekarstva, kozmetika, mom_baby, bilki,
                  homeopatiq, dobavki, higiena, aparati) %>% 
  drop_na(price)
glimpse(marvi)
write_parquet(marvi, "shiny/scraping_med/marvi.parquet")
