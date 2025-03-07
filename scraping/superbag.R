library(tidyverse)
library(rvest)

#-------------------------------------------------------------------------------
base_url <- "https://www.superbagplovdiv.bg/plodove-i-zelenchutzi-2.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gs-item-data") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("h6 a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".gs-new-price") %>% 
        html_text2(),
      price_old = .x %>%
        html_element(".gs-old-price") %>%
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
           location = "Пловдив",
           type = "Плодове и зеленчуци",
           source = "Superbag", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:3
fruits <- map_dfr(pages_to_scrape, scrape_prices)
#-------------------------------------------------------------------------------
base_url <- "https://www.superbagplovdiv.bg/hliab-i-testeni-izdeliia.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gs-item-data") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("h6 a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".gs-new-price") %>% 
        html_text2(),
      price_old = .x %>%
        html_element(".gs-old-price") %>%
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
           location = "Пловдив",
           type = "Брашно и хляб",
           source = "Superbag", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:5
hlqb <- map_dfr(pages_to_scrape, scrape_prices)
#-------------------------------------------------------------------------------
base_url <- "https://www.superbagplovdiv.bg/mlechni-produkti.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gs-item-data") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("h6 a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".gs-new-price") %>% 
        html_text2(),
      price_old = .x %>%
        html_element(".gs-old-price") %>%
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
           location = "Пловдив",
           type = "Мляко и млечни продукти",
           source = "Superbag", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:18
milk <- map_dfr(pages_to_scrape, scrape_prices)
#-------------------------------------------------------------------------------
base_url <- "https://www.superbagplovdiv.bg/mesni-produkti.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gs-item-data") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("h6 a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".gs-new-price") %>% 
        html_text2(),
      price_old = .x %>%
        html_element(".gs-old-price") %>%
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
           location = "Пловдив",
           type = "Месо и месни продукти",
           source = "Superbag", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:3
meat <- map_dfr(pages_to_scrape, scrape_prices)
#-------------------------------------------------------------------------------
base_url <- "https://www.superbagplovdiv.bg/riba-1.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gs-item-data") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("h6 a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".gs-new-price") %>% 
        html_text2(),
      price_old = .x %>%
        html_element(".gs-old-price") %>%
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
           location = "Пловдив",
           type = "Риба",
           source = "Superbag", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:2
fish <- map_dfr(pages_to_scrape, scrape_prices)
#-------------------------------------------------------------------------------
base_url <- "https://www.superbagplovdiv.bg/kolbasi-i-delikatesi.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gs-item-data") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("h6 a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".gs-new-price") %>% 
        html_text2(),
      price_old = .x %>%
        html_element(".gs-old-price") %>%
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
           location = "Пловдив",
           type = "Колбаси",
           source = "Superbag", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:20
kolbasi <- map_dfr(pages_to_scrape, scrape_prices)
#-------------------------------------------------------------------------------
base_url <- "https://www.superbagplovdiv.bg/zamrazeni-hrani-1.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gs-item-data") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("h6 a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".gs-new-price") %>% 
        html_text2(),
      price_old = .x %>%
        html_element(".gs-old-price") %>%
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
           location = "Пловдив",
           type = "Замразени храни",
           source = "Superbag", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:10
frozen <- map_dfr(pages_to_scrape, scrape_prices)
#-------------------------------------------------------------------------------
base_url <- "https://www.superbagplovdiv.bg/paketirani-hrani.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gs-item-data") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("h6 a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".gs-new-price") %>% 
        html_text2(),
      price_old = .x %>%
        html_element(".gs-old-price") %>%
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
           location = "Пловдив",
           type = "Бакалия",
           source = "Superbag", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:73 #75
bak <- map_dfr(pages_to_scrape, scrape_prices)
#-------------------------------------------------------------------------------
base_url <- "https://www.superbagplovdiv.bg/sladki-i-soleni.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gs-item-data") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("h6 a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".gs-new-price") %>% 
        html_text2(),
      price_old = .x %>%
        html_element(".gs-old-price") %>%
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
           location = "Пловдив",
           type = "Сладки и солени храни",
           source = "Superbag", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:64
kandy <- map_dfr(pages_to_scrape, scrape_prices)
#-------------------------------------------------------------------------------
base_url <- "https://www.superbagplovdiv.bg/kafe-chai-i-kakao-1.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gs-item-data") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("h6 a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".gs-new-price") %>% 
        html_text2(),
      price_old = .x %>%
        html_element(".gs-old-price") %>%
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
           location = "Пловдив",
           type = "Кафе и чай",
           source = "Superbag", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:18
coffee <- map_dfr(pages_to_scrape, scrape_prices)
#-------------------------------------------------------------------------------
base_url <- "https://www.superbagplovdiv.bg/bezalkoholni-napitki-1.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gs-item-data") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("h6 a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".gs-new-price") %>% 
        html_text2(),
      price_old = .x %>%
        html_element(".gs-old-price") %>%
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
           location = "Пловдив",
           type = "Напитки",
           source = "Superbag", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:25
drinks <- map_dfr(pages_to_scrape, scrape_prices)
#-------------------------------------------------------------------------------
base_url <- "https://www.superbagplovdiv.bg/alkoholni-napitki.html?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gs-item-data") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("h6 a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".gs-new-price") %>% 
        html_text2(),
      price_old = .x %>%
        html_element(".gs-old-price") %>%
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
           location = "Пловдив",
           type = "Напитки",
           source = "Superbag", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:56
alc <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------
superbag <- bind_rows(fruits, hlqb, milk, meat, fish, kolbasi, 
                      frozen, bak, kandy, coffee, drinks, alc)

superbag <- superbag %>% 
  mutate(unit = NA, discount = NA, price_kg = NA) %>% 
  select(date, location, source, type, product,
         unit, price, price_old, discount, price_kg) %>% 
  mutate(across(!c(date, price, price_old), as.character)) %>% 
  mutate(across(price:price_old, as.numeric))

glimpse(superbag)
write_csv(superbag, "shiny/scraping/superbag.csv")
