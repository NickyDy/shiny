library(tidyverse)
library(rvest)
library(arrow)
#--------------------------------------------------------
base_url <- "https://366.bg/products/zdravni-problemi?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-link") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".name") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".prices") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Лекарства",
           source = "366", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:278
lekarstva <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://366.bg/products/grija-za-zdraveto?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-link") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".name") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".prices") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Лекарства",
           source = "366", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:258
lekarstva1 <- map_dfr(pages_to_scrape, scrape_prices)
 #--------------------------------------------------------
base_url <- "https://366.bg/products/bilkova-apteka?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-link") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".name") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".prices") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Билкова аптека",
           source = "366", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:76
bilki <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://366.bg/products/kozmetika?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-link") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".name") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".prices") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Козметика и красота",
           source = "366", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:131
cosmetics <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://366.bg/products/krasota?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-link") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".name") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".prices") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Козметика и красота",
           source = "366", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:33
cosmetics1 <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://366.bg/products/lichna-grija?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-link") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".name") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".prices") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Козметика и красота",
           source = "366", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:34
cosmetics2 <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://366.bg/products/higiena?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-link") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".name") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".prices") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Хигиена",
           source = "366", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:141
hygene <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://366.bg/products/mama-i-bebe?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-link") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".name") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".prices") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Мама и бебе",
           source = "366", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:85
mom_baby <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
bg366 <- bind_rows(lekarstva, lekarstva1, cosmetics, cosmetics1,
                   cosmetics2, bilki, hygene, mom_baby)

glimpse(bg366)
write_parquet(bg366, "shiny/scraping_med/bg366.parquet")
