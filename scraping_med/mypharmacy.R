library(tidyverse)
library(rvest)
library(arrow)
#--------------------------------------------------------
base_url <- "https://mypharmacy.bg/lekarstva-i-zdrave/page-%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gridlistitem") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ty-simple-list__price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Лекарства",
           source = "Mypharmacy", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:71
lekarstva <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://mypharmacy.bg/hranitelni-dobavki/page-%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gridlistitem") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ty-simple-list__price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Хранителни добавки",
           source = "Mypharmacy", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:96
dobavki <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://mypharmacy.bg/kozmetika/page-%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gridlistitem") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ty-simple-list__price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Козметика",
           source = "Mypharmacy", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:62
kozmetika <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://mypharmacy.bg/mama-i-bebe/page-%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gridlistitem") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ty-simple-list__price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Мама и бебе",
           source = "Mypharmacy", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:15
mom_baby <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://mypharmacy.bg/homeopatiya-bg/page-%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gridlistitem") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ty-simple-list__price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Хомеопатия",
           source = "Mypharmacy", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:22
homeopatiq <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://mypharmacy.bg/lichna-grizha-bg/page-%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gridlistitem") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ty-simple-list__price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Лична грижа",
           source = "Mypharmacy", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:34
selfcare <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://mypharmacy.bg/dieta-bg/page-%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gridlistitem") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ty-simple-list__price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Диета",
           source = "Mypharmacy", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:7
dieta <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://mypharmacy.bg/aparati/page-%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gridlistitem") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".product-title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ty-simple-list__price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Апарати",
           source = "Mypharmacy", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:7
aparati <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
mypharmacy <- bind_rows(lekarstva, dobavki, kozmetika, mom_baby,
                        homeopatiq, selfcare, dieta, aparati)
glimpse(mypharmacy)
write_parquet(mypharmacy, "shiny/scraping_med/mypharmacy.parquet")