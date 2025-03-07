library(tidyverse)
library(rvest)
library(arrow)
#--------------------------------------------------------
base_url <- "https://gpharm.bg/produkt-kategoriya/lekarstva/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-type-simple") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".woocommerce-loop-product__title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Лекарства",
           source = "Gpharm", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:60
lekarstva <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://gpharm.bg/produkt-kategoriya/hranytelny-dobavky/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-type-simple") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".woocommerce-loop-product__title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Хранителни добавки",
           source = "Gpharm", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:150
dobavki <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://gpharm.bg/produkt-kategoriya/kozmetyka/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-type-simple") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".woocommerce-loop-product__title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Козметика",
           source = "Gpharm", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:175
kozmetika <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://gpharm.bg/produkt-kategoriya/mama-y-bebe/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-type-simple") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".woocommerce-loop-product__title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Мама и бебе",
           source = "Gpharm", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:6
mom_baby <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://gpharm.bg/produkt-kategoriya/aparaty-y-medyczynsky-yzdelyya/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-type-simple") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".woocommerce-loop-product__title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Мидицински апарати и изделия",
           source = "Gpharm", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:25
aparati <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://gpharm.bg/produkt-kategoriya/hygyena-y-zasthyta/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-type-simple") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".woocommerce-loop-product__title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Хигиена и защита",
           source = "Gpharm", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:30
higiena <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://gpharm.bg/produkt-kategoriya/homeopatyya/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-type-simple") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".woocommerce-loop-product__title") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Хомеопатия",
           source = "Gpharm", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:28
homeopatiq <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
gpharm <- bind_rows(lekarstva, dobavki, kozmetika, aparati,
                    mom_baby, higiena, homeopatiq)

glimpse(gpharm)
write_parquet(gpharm, "shiny/scraping_med/gpharm.parquet")
