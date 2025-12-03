library(tidyverse)
library(rvest)
library(httr2)

req <- request("https://tmarketonline.bg/category/plodove-zelenchuci-i-yadki?page=1")
req |> req_headers() %>% 
  req_perform()
#---------------------------------------------------------------------------------
base_url <- "https://tmarketonline.bg/category/plodove-zelenchuci-i-yadki?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements("._product") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element("._product-price-inner span") %>% 
        html_text2(),
      price_old = .x %>% 
        html_element("._product-price-old") %>% 
        html_text2(),
      # discount = .x %>% 
      #   html_element(".a-pricetag__discount") %>% 
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>% 
        html_element("._button_unit") %>% 
        html_text2())) %>% 
    mutate(date = Sys.Date(),
           #location = "Благоевград",
           type = "Плодове и зеленчуци",
           source = "T MARKET", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:4
pz <- map_dfr(pages_to_scrape, scrape_prices)
#------------------------------------------------------
base_url <- "https://tmarketonline.bg/category/hlebni-i-testeni-izdeliya?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements("._product") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element("._product-price-inner span") %>% 
        html_text2(),
      price_old = .x %>% 
        html_element("._product-price-old") %>% 
        html_text2(),
      # discount = .x %>% 
      #   html_element(".a-pricetag__discount") %>% 
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>% 
        html_element("._button_unit") %>% 
        html_text2())) %>% 
    mutate(date = Sys.Date(),
           #location = "Благоевград",
           type = "Брашно и хляб",
           source = "T MARKET", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:3
hlqb <- map_dfr(pages_to_scrape, scrape_prices)
#------------------------------------------------------
base_url <- "https://tmarketonline.bg/category/mlechni-produkti-i-yayca?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements("._product") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element("._product-price-inner span") %>% 
        html_text2(),
      price_old = .x %>% 
        html_element("._product-price-old") %>% 
        html_text2(),
      # discount = .x %>% 
      #   html_element(".a-pricetag__discount") %>% 
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>% 
        html_element("._button_unit") %>% 
        html_text2())) %>% 
    mutate(date = Sys.Date(),
           #location = "Благоевград",
           type = "Мляко и млечни продукти",
           source = "T MARKET", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:10
milk <- map_dfr(pages_to_scrape, scrape_prices)
#------------------------------------------------------
base_url <- "https://tmarketonline.bg/category/meso?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements("._product") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element("._product-price-inner span") %>% 
        html_text2(),
      price_old = .x %>% 
        html_element("._product-price-old") %>% 
        html_text2(),
      # discount = .x %>% 
      #   html_element(".a-pricetag__discount") %>% 
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>% 
        html_element("._button_unit") %>% 
        html_text2())) %>% 
    mutate(date = Sys.Date(),
           #location = "Благоевград",
           type = "Месо и месни продукти",
           source = "T MARKET", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:3
meat <- map_dfr(pages_to_scrape, scrape_prices)
#------------------------------------------------------
base_url <- "https://tmarketonline.bg/category/paketirani-hrani?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements("._product") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element("._product-price-inner span") %>% 
        html_text2(),
      price_old = .x %>% 
        html_element("._product-price-old") %>% 
        html_text2(),
      # discount = .x %>% 
      #   html_element(".a-pricetag__discount") %>% 
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>% 
        html_element("._button_unit") %>% 
        html_text2())) %>% 
    mutate(date = Sys.Date(),
           #location = "Благоевград",
           type = "Бакалия",
           source = "T MARKET", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:19
bak <- map_dfr(pages_to_scrape, scrape_prices)
#------------------------------------------------------
base_url <- "https://tmarketonline.bg/category/kolbasi-i-shunki?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements("._product") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element("._product-price-inner span") %>% 
        html_text2(),
      price_old = .x %>% 
        html_element("._product-price-old") %>% 
        html_text2(),
      # discount = .x %>% 
      #   html_element(".a-pricetag__discount") %>% 
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>% 
        html_element("._button_unit") %>% 
        html_text2())) %>% 
    mutate(date = Sys.Date(),
           #location = "Благоевград",
           type = "Колбаси",
           source = "T MARKET", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:5
kolbasi <- map_dfr(pages_to_scrape, scrape_prices)
#------------------------------------------------------
base_url <- "https://tmarketonline.bg/category/zamrazeni-hrani?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements("._product") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element("._product-price-inner span") %>% 
        html_text2(),
      price_old = .x %>% 
        html_element("._product-price-old") %>% 
        html_text2(),
      # discount = .x %>% 
      #   html_element(".a-pricetag__discount") %>% 
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>% 
        html_element("._button_unit") %>% 
        html_text2())) %>% 
    mutate(date = Sys.Date(),
           #location = "Благоевград",
           type = "Замразени храни",
           source = "T MARKET", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:5
frozen <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------
tmarket <- bind_rows(pz, hlqb, milk, meat, bak, kolbasi, frozen)

tmarket <- tmarket %>% 
  mutate(location = "Цялата страна") %>% 
  mutate(discount = NA, price_kg = NA, price_old = str_replace(price_old, ",", "."),
         price_old = parse_number(price_old)) %>% 
  select(date, location, source, type, product,
         unit, price, price_old, discount, price_kg) %>% 
  mutate(across(!c(date, price, price_old), as.character)) %>% 
  mutate(across(price:price_old, as.numeric))

glimpse(tmarket)
write_csv(tmarket, "shiny/scraping/tmarket.csv")
