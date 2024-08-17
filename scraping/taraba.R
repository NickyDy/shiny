library(tidyverse)
library(rvest)

#-----------------------------------------------------------
base_url <- "https://taraba.bg/plodove-i-zelencuti?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".col-xs-12") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".text-center a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
      # price_old = .x %>%
      #   html_element(".mb-n2") %>%
      #   html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>%
        html_element(".count-disc-pr") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Русе",
           type = "Плодове и зеленчуци",
           source = "Taraba", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:2
pz <- map_dfr(pages_to_scrape, scrape_prices)
#-----------------------------------------------------------
base_url <- "https://taraba.bg/meso-i-riba?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".col-xs-12") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".text-center a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
      # price_old = .x %>%
      #   html_element(".mb-n2") %>%
      #   html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>%
        html_element(".count-disc-pr") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Русе",
           type = "Месо и месни продукти",
           source = "Taraba", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:2
meat <- map_dfr(pages_to_scrape, scrape_prices)
#-----------------------------------------------------------
base_url <- "https://taraba.bg/млечни-яйца-майонеза?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".col-xs-12") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".text-center a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
      # price_old = .x %>%
      #   html_element(".mb-n2") %>%
      #   html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>%
        html_element(".count-disc-pr") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Русе",
           type = "Мляко и млечни продукти",
           source = "Taraba", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:8
milk <- map_dfr(pages_to_scrape, scrape_prices)
#-----------------------------------------------------------
base_url <- "https://taraba.bg/zakarni-izdeliy-4-127?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".col-xs-12") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".text-center a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
      # price_old = .x %>%
      #   html_element(".mb-n2") %>%
      #   html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>%
        html_element(".count-disc-pr") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Русе",
           type = "Сладки и солени храни",
           source = "Taraba", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:10
kandy <- map_dfr(pages_to_scrape, scrape_prices)
#-----------------------------------------------------------
base_url <- "https://taraba.bg/kolbasi-i-delikatesi?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".col-xs-12") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".text-center a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
      # price_old = .x %>%
      #   html_element(".mb-n2") %>%
      #   html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>%
        html_element(".count-disc-pr") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Русе",
           type = "Колбаси",
           source = "Taraba", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:9
kolbasi <- map_dfr(pages_to_scrape, scrape_prices)
#-----------------------------------------------------------
base_url <- "https://taraba.bg/konservirani-hrani?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".col-xs-12") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".text-center a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
      # price_old = .x %>%
      #   html_element(".mb-n2") %>%
      #   html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>%
        html_element(".count-disc-pr") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Русе",
           type = "Консерви",
           source = "Taraba", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:3
konservi <- map_dfr(pages_to_scrape, scrape_prices)
#-----------------------------------------------------------
base_url <- "https://taraba.bg/paketirani-krani?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".col-xs-12") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".text-center a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
      # price_old = .x %>%
      #   html_element(".mb-n2") %>%
      #   html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>%
        html_element(".count-disc-pr") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Русе",
           type = "Бакалия",
           source = "Taraba", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:17
bak <- map_dfr(pages_to_scrape, scrape_prices)
#-----------------------------------------------------------
base_url <- "https://taraba.bg/klyb-i-testeni?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".col-xs-12") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".text-center a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
      # price_old = .x %>%
      #   html_element(".mb-n2") %>%
      #   html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>%
        html_element(".count-disc-pr") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Русе",
           type = "Брашно и хляб",
           source = "Taraba", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:3
hlqb <- map_dfr(pages_to_scrape, scrape_prices)
#-----------------------------------------------------------
base_url <- "https://taraba.bg/napitki?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".col-xs-12") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".text-center a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
      # price_old = .x %>%
      #   html_element(".mb-n2") %>%
      #   html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>%
        html_element(".count-disc-pr") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Русе",
           type = "Напитки",
           source = "Taraba", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:10
drinks <- map_dfr(pages_to_scrape, scrape_prices)
#-----------------------------------------------------------
base_url <- "https://taraba.bg/zamrazeni-hrani?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".col-xs-12") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".text-center a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price") %>% 
        html_text2(),
      # price_old = .x %>%
      #   html_element(".mb-n2") %>%
      #   html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>%
        html_element(".count-disc-pr") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Русе",
           type = "Замразени храни",
           source = "Taraba", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:3
frozen <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------
taraba <- bind_rows(pz, meat, milk, kandy, kolbasi, konservi, bak, hlqb, drinks, frozen)

taraba <- taraba %>% 
  mutate(price_old = NA, discount = NA, price_kg = NA) %>% 
  select(date, location, source, type, product,
         unit, price, price_old, discount, price_kg) %>% 
  mutate(across(!c(date, price, price_old), as.character)) %>% 
  mutate(across(price:price_old, as.numeric))

glimpse(taraba)
write_csv(taraba, "shiny/scraping/taraba.csv")