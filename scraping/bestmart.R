library(tidyverse)
library(rvest)

#--------------------------------------------
base_url <- "https://bestmart.bg/produkt-kategoriya/plodove-i-zelenchutzi/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-type-simple") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".woocommerce-loop-product__title") %>% 
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
      #   html_element(".uom") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Плодове и зеленчуци",
           source = "Bestmart", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:2
fruit <- map_dfr(pages_to_scrape, scrape_prices)
#-----------------------------------------------
base_url <- "https://bestmart.bg/produkt-kategoriya/meso-i-riba/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-type-simple") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".woocommerce-loop-product__title") %>% 
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
      #   html_element(".uom") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Месо и месни продукти",
           source = "Bestmart", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:4
meat <- map_dfr(pages_to_scrape, scrape_prices)
#-----------------------------------------------
base_url <- "https://bestmart.bg/produkt-kategoriya/mlechni-produkti/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-type-simple") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".woocommerce-loop-product__title") %>% 
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
      #   html_element(".uom") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Мляко и млечни продукти",
           source = "Bestmart", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:7
milk <- map_dfr(pages_to_scrape, scrape_prices)
#-----------------------------------------------
base_url <- "https://bestmart.bg/produkt-kategoriya/paketirani-hrani/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-type-simple") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".woocommerce-loop-product__title") %>% 
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
      #   html_element(".uom") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Бакалия",
           source = "Bestmart", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:13
bak <- map_dfr(pages_to_scrape, scrape_prices)
#-----------------------------------------------
base_url <- "https://bestmart.bg/produkt-kategoriya/hlyab-i-testeni/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-type-simple") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".woocommerce-loop-product__title") %>% 
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
      #   html_element(".uom") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Брашно и хляб",
           source = "Bestmart", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:1
hlqb <- map_dfr(pages_to_scrape, scrape_prices)
#-----------------------------------------------
base_url <- "https://bestmart.bg/produkt-kategoriya/zamrazeni/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-type-simple") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".woocommerce-loop-product__title") %>% 
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
      #   html_element(".uom") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Замразени храни",
           source = "Bestmart", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:2
frozen <- map_dfr(pages_to_scrape, scrape_prices)
#-----------------------------------------------
base_url <- "https://bestmart.bg/produkt-kategoriya/zaharni-izdeliya/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-type-simple") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".woocommerce-loop-product__title") %>% 
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
      #   html_element(".uom") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Сладки и солени храни",
           source = "Bestmart", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:7
kandy <- map_dfr(pages_to_scrape, scrape_prices)
#-----------------------------------------------
base_url <- "https://bestmart.bg/produkt-kategoriya/alkohol/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-type-simple") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".woocommerce-loop-product__title") %>% 
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
      #   html_element(".uom") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Напитки",
           source = "Bestmart", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:2
alc <- map_dfr(pages_to_scrape, scrape_prices)
#-----------------------------------------------
base_url <- "https://bestmart.bg/produkt-kategoriya/bezalkoholni/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-type-simple") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".woocommerce-loop-product__title") %>% 
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
      #   html_element(".uom") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Напитки",
           source = "Bestmart", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:3
bez <- map_dfr(pages_to_scrape, scrape_prices)
#-----------------------------------------------
base_url <- "https://bestmart.bg/produkt-kategoriya/kafe/page/%d/"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-type-simple") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".woocommerce-loop-product__title") %>% 
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
      #   html_element(".uom") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Кафе и чай",
           source = "Bestmart", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:2
coffee <- map_dfr(pages_to_scrape, scrape_prices)
#------------------------------------------------
bestmart <- bind_rows(alc, bak, bez, coffee, frozen, fruit, hlqb, 
                      kandy, meat, milk)

bestmart <- bestmart %>% 
  mutate(unit = NA, price_old = NA, discount = NA, price_kg = NA) %>% 
  select(date, location, source, type, product,
         unit, price, price_old, discount, price_kg) %>% 
  mutate(across(!c(date, price, price_old), as.character)) %>% 
  mutate(across(price:price_old, as.numeric)) %>% drop_na(price)

glimpse(bestmart)
write_csv(bestmart, "shiny/scraping/bestmart.csv")