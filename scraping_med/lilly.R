library(tidyverse)
library(rvest)
library(arrow)
#--------------------------------------------------------
base_url <- "https://aptekililly.bg/category/lekarstva-i-zdrave?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".one-fifth") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag") %>% 
        html_text2(), 
      price = .x %>% 
        html_element("._product-price-inner") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Лекарства",
           source = "Аптеки Лили", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:73
lekarstva <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://aptekililly.bg/category/hranitelni-dobavki?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".one-fifth") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag") %>% 
        html_text2(), 
      price = .x %>% 
        html_element("._product-price-inner") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Хранителни добавки",
           source = "Аптеки Лили", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:57
dobavki <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://aptekililly.bg/category/kozmetika-i-lichna-higiena?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".one-fifth") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag") %>% 
        html_text2(), 
      price = .x %>% 
        html_element("._product-price-inner") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Козметика и лична хигиена",
           source = "Аптеки Лили", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:86
kozmetika <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://aptekililly.bg/category/mama-i-bebe?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".one-fifth") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag") %>% 
        html_text2(), 
      price = .x %>% 
        html_element("._product-price-inner") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Мама и бебе",
           source = "Аптеки Лили", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:17
mom_baby <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://aptekililly.bg/category/homeopatiya?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".one-fifth") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag") %>% 
        html_text2(), 
      price = .x %>% 
        html_element("._product-price-inner") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Хомеопатия",
           source = "Аптеки Лили", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:19
homeopatiq <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://aptekililly.bg/category/ustroystva-i-konsumativi?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".one-fifth") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag") %>% 
        html_text2(), 
      price = .x %>% 
        html_element("._product-price-inner") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Устройства и консумативи",
           source = "Аптеки Лили", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:4
aparati <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://aptekililly.bg/category/lichni-predpazni-sredstva?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".one-fifth") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag") %>% 
        html_text2(), 
      price = .x %>% 
        html_element("._product-price-inner") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Лични предпазни средства",
           source = "Аптеки Лили", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:5
predpazni <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://aptekililly.bg/category/sanitariya?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".one-fifth") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag") %>% 
        html_text2(), 
      price = .x %>% 
        html_element("._product-price-inner") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Санитарни средства",
           source = "Аптеки Лили", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:8
sanitarni <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
lilly <- bind_rows(lekarstva, dobavki, kozmetika, aparati,
                   mom_baby, homeopatiq, predpazni, sanitarni)
glimpse(lilly)
write_parquet(lilly, "shiny/scraping_med/lilly.parquet")
