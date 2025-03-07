library(tidyverse)
library(rvest)
#--------------------------------------------------------
base_url <- "https://sopharmacy.bg/bg/category/ZR00000000?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".products-item") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".products-item__title a") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price--s") %>% 
        html_text2(),
      )) %>% 
    mutate(date = Sys.Date(),
           type = "Лекарства",
           source = "Sopharmacy", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
           ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:149
lekarstva <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://sopharmacy.bg/bg/category/ZO00000000?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".products-item") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".products-item__title a") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price--s") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Лекарства и терапии",
           source = "Sopharmacy", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:144
terapii <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://sopharmacy.bg/bg/category/ZL00000000?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".products-item") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".products-item__title a") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price--s") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Здравословен живот",
           source = "Sopharmacy", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:126
health <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://sopharmacy.bg/bg/category/ZF00000000?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".products-item") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".products-item__title a") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price--s") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Козметика и красота",
           source = "Sopharmacy", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:115
cosmetics <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://sopharmacy.bg/bg/category/ZH00000000?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".products-item") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".products-item__title a") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price--s") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Хигиена",
           source = "Sopharmacy", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:126
hygene <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://sopharmacy.bg/bg/category/ZM00000000?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".products-item") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".products-item__title a") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price--s") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Мама и бебе",
           source = "Sopharmacy", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:73
mom_baby <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://sopharmacy.bg/bg/category/bio-natural?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".products-item") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".products-item__title a") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price--s") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Био и натурални",
           source = "Sopharmacy", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:41
bio_nat <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------
sopharmacy <- bind_rows(lekarstva, terapii, health, cosmetics, hygene,
                        mom_baby, bio_nat)
glimpse(sopharmacy)
write_rds(sopharmacy, "shiny/scraping_med/sopharmacy.rds")
