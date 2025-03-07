library(tidyverse)
library(rvest)
#--------------------------------------------------------
base_url <- "https://aptekaonline.bg/produkt-kategoriya/здраве/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-type-simple") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".woocommerce-loop-product__link") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Лекарства",
           source = "Аптека Витоша", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:226
lekarstva <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://aptekaonline.bg/produkt-kategoriya/козметика/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-type-simple") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".woocommerce-loop-product__link") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Козметика",
           source = "Аптека Витоша", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:11
kozmetika <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://aptekaonline.bg/produkt-kategoriya/мама-и-бебе/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-type-simple") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".woocommerce-loop-product__link") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Мама и бебе",
           source = "Аптека Витоша", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:43
mom_baby <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://aptekaonline.bg/produkt-kategoriya/хомеопатия/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-type-simple") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".woocommerce-loop-product__link") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Хомеопатия",
           source = "Аптека Витоша", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:3
homeopatiq <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://aptekaonline.bg/produkt-kategoriya/био-продукти/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-type-simple") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".woocommerce-loop-product__link") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Био продукти",
           source = "Аптека Витоша", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:10
bio <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://aptekaonline.bg/produkt-kategoriya/апарати-и-медицински-консумативи/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-type-simple") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".woocommerce-loop-product__link") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Апарати и консумативи",
           source = "Аптека Витоша", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:4
aparati <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------
vitosha <- bind_rows(lekarstva, kozmetika, mom_baby, bio,
                      homeopatiq, aparati)
glimpse(vitosha)
write_rds(vitosha, "shiny/scraping_med/vitosha.rds")
