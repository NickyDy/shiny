library(tidyverse)
library(rvest)

#---------------------------------------------------------------------
base_url <- "https://zasiti.bg/category/плодове-и-зеленчуци-2/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".wc-loop-product-wrapper") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".wc-loop-product-title") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("del") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      # unit = .x %>% 
      #   html_element(".product-weight") %>% 
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна",
           type = "Плодове и зеленчуци",
           source = "Zasiti", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:3
fruit <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://zasiti.bg/category/алкохолни-напитки/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".wc-loop-product-wrapper") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".wc-loop-product-title") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("del") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      # unit = .x %>% 
      #   html_element(".product-weight") %>% 
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна",
           type = "Напитки",
           source = "Zasiti", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:28
alc <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://zasiti.bg/category/безалкохолни-напитки/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".wc-loop-product-wrapper") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".wc-loop-product-title") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("del") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      # unit = .x %>% 
      #   html_element(".product-weight") %>% 
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна",
           type = "Напитки",
           source = "Zasiti", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:24
bez <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://zasiti.bg/category/био-продукти/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".wc-loop-product-wrapper") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".wc-loop-product-title") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("del") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      # unit = .x %>% 
      #   html_element(".product-weight") %>% 
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна",
           type = "Специални храни",
           source = "Zasiti", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:7
veg <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://zasiti.bg/category/замразени-продукти/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".wc-loop-product-wrapper") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".wc-loop-product-title") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("del") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      # unit = .x %>% 
      #   html_element(".product-weight") %>% 
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна",
           type = "Замразени храни",
           source = "Zasiti", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:9 #10
frozen <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://zasiti.bg/category/захарни-изделия/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".wc-loop-product-wrapper") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".wc-loop-product-title") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("del") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      # unit = .x %>% 
      #   html_element(".product-weight") %>% 
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна",
           type = "Сладки и солени храни",
           source = "Zasiti", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:50
kandy <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://zasiti.bg/category/месни-продукти/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".wc-loop-product-wrapper") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".wc-loop-product-title") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("del") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      # unit = .x %>% 
      #   html_element(".product-weight") %>% 
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна",
           type = "Месо и месни продукти",
           source = "Zasiti", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:9
meat <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://zasiti.bg/category/млечни-продукти/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".wc-loop-product-wrapper") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".wc-loop-product-title") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("del") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      # unit = .x %>% 
      #   html_element(".product-weight") %>% 
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна",
           type = "Мляко и млечни продукти",
           source = "Zasiti", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:19
milk <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://zasiti.bg/category/пакетирани-изделия/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".wc-loop-product-wrapper") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".wc-loop-product-title") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("del") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      # unit = .x %>% 
      #   html_element(".product-weight") %>% 
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна",
           type = "Бакалия",
           source = "Zasiti", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:48 #49
bak <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://zasiti.bg/category/хлебни-изделия/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".wc-loop-product-wrapper") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".wc-loop-product-title") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("del") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      # unit = .x %>% 
      #   html_element(".product-weight") %>% 
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна",
           type = "Брашно и хляб",
           source = "Zasiti", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:5
hlqb <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://zasiti.bg/category/топли-напитки/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".wc-loop-product-wrapper") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".wc-loop-product-title") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("del") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      # unit = .x %>% 
      #   html_element(".product-weight") %>% 
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна",
           type = "Кафе и чай",
           source = "Zasiti", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:9
coffee <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------
zasiti <- bind_rows(fruit, alc, bez, veg, frozen, kandy, meat, milk, bak, hlqb, coffee)

zasiti <- zasiti %>% 
  mutate(unit = NA, discount = NA, price_kg = NA) %>% 
  select(date, location, source, type, product,
         unit, price, price_old, discount, price_kg) %>% 
  mutate(across(!c(date, price, price_old), as.character)) %>% 
  mutate(across(price:price_old, as.numeric))

glimpse(zasiti)
write_csv(zasiti, "shiny/scraping/zasiti.csv")
