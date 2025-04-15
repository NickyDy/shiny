library(tidyverse)
library(rvest)

#--------------------------------------------------------
base_url <- "https://trista.bg/zelenchuci-95.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".tb_style_1") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".caption a") %>% 
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
      # unit = .x %>%
      #   html_element(".categorygrid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна",
           type = "Плодове и зеленчуци",
           source = "Trista", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:2
pz <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------
base_url <- "https://trista.bg/alkoholni-napitki-35.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".tb_style_1") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".caption a") %>% 
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
      # unit = .x %>%
      #   html_element(".categorygrid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна",
           type = "Напитки",
           source = "Trista", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:11
alc <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------
base_url <- "https://trista.bg/bezalkoholni-napitki-5.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".tb_style_1") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".caption a") %>% 
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
      # unit = .x %>%
      #   html_element(".categorygrid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна",
           type = "Напитки",
           source = "Trista", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:8
bez <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------
base_url <- "https://trista.bg/index.php?route=product/category&path=147&page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".tb_style_1") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".caption a") %>% 
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
      # unit = .x %>%
      #   html_element(".categorygrid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна",
           type = "Кафе и чай",
           source = "Trista", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:4
coffee <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------
base_url <- "https://trista.bg/bakaliya-23.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".tb_style_1") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".caption a") %>% 
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
      # unit = .x %>%
      #   html_element(".categorygrid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна",
           type = "Бакалия",
           source = "Trista", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:16
bakaliq <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------
base_url <- "https://trista.bg/index.php?route=product/category&path=162&page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".tb_style_1") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".caption a") %>% 
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
      # unit = .x %>%
      #   html_element(".categorygrid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна",
           type = "Мляко и млечни продукти",
           source = "Trista", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:7
milk <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------
base_url <- "https://trista.bg/index.php?route=product/category&path=173&page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".tb_style_1") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".caption a") %>% 
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
      # unit = .x %>%
      #   html_element(".categorygrid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна",
           type = "Месо и месни продукти",
           source = "Trista", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:5 #6
meat <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------
base_url <- "https://trista.bg/hlebni-izdeliya-30.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".tb_style_1") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".caption a") %>% 
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
      # unit = .x %>%
      #   html_element(".categorygrid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна",
           type = "Брашно и хляб",
           source = "Trista", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:2
hlqb <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------
base_url <- "https://trista.bg/zaharni-izdeliya-7.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".tb_style_1") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".caption a") %>% 
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
      # unit = .x %>%
      #   html_element(".categorygrid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна",
           type = "Сладки и солени храни",
           source = "Trista", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:19
kandy <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------
base_url <- "https://trista.bg/soleni-izdeliya-51.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".tb_style_1") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".caption a") %>% 
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
      # unit = .x %>%
      #   html_element(".categorygrid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна",
           type = "Сладки и солени храни",
           source = "Trista", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:8
salty <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------
base_url <- "https://trista.bg/zdrave-fitnes-115.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".tb_style_1") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".caption a") %>% 
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
      # unit = .x %>%
      #   html_element(".categorygrid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна",
           type = "Специални храни",
           source = "Trista", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:3
vegan <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------
base_url <- "https://trista.bg/index.php?route=product/category&path=189&page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".tb_style_1") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".caption a") %>% 
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
      # unit = .x %>%
      #   html_element(".categorygrid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна",
           type = "Замразени храни",
           source = "Trista", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:3
frozen <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------
trista <- bind_rows(pz, alc, bez, coffee, bakaliq, milk, 
                    meat, hlqb, kandy, salty, frozen, vegan)

trista <- trista %>% 
  mutate(unit = NA, price_old = NA, discount = NA, price_kg = NA) %>% 
  select(date, location, source, type, product,
         unit, price, price_old, discount, price_kg) %>% 
  mutate(across(!c(date, price, price_old), as.character)) %>% 
  mutate(across(price:price_old, as.numeric))

glimpse(trista)
write_csv(trista, "shiny/scraping/trista.csv")
