library(tidyverse)
library(rvest)

#---------------------------------------------------------------------
base_url <- "https://www.bakaliika.com/produkti-1/zamrazeni/page-%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-layout") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".name a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price-normal") %>% 
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
           location = "София",
           type = "Замразени храни",
           source = "Бакалийка", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:2
frozen <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://www.bakaliika.com/produkti-1/zaharni-izdeliya/page-%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-layout") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".name a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price-normal") %>% 
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
           location = "София",
           type = "Сладки и солени храни",
           source = "Бакалийка", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:18
kandy <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://www.bakaliika.com/produkti-1/kolbasi-i-delikatesi/page-%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-layout") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".name a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price-normal") %>% 
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
           location = "София",
           type = "Колбаси",
           source = "Бакалийка", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:3
kolbasi <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://www.bakaliika.com/produkti-1/konservi-i-burkani/page-%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-layout") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".name a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price-normal") %>% 
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
           location = "София",
           type = "Консерви",
           source = "Бакалийка", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:6
konservi <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://www.bakaliika.com/produkti-1/mlyako-i-mlechni-produkti/page-%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-layout") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".name a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price-normal") %>% 
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
           location = "София",
           type = "Мляко и млечни продукти",
           source = "Бакалийка", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:5
milk <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://www.bakaliika.com/produkti-1/napitki/page-%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-layout") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".name a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price-normal") %>% 
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
           location = "София",
           type = "Напитки",
           source = "Бакалийка", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:17
drinks <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://www.bakaliika.com/produkti-1/ohl-meso-i-riba/page-%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-layout") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".name a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price-normal") %>% 
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
           location = "София",
           type = "Месо и месни продукти",
           source = "Бакалийка", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:1
meat <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://www.bakaliika.com/produkti-1/paketirani-stoki/page-%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-layout") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".name a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price-normal") %>% 
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
           location = "София",
           type = "Бакалия",
           source = "Бакалийка", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:9
bak <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://www.bakaliika.com/produkti-1/hlya/page-%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-layout") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".name a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price-normal") %>% 
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
           location = "София",
           type = "Брашно и хляб",
           source = "Бакалийка", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:1
hlqb <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://www.bakaliika.com/produkti-1/yajca/page-%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".product-layout") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".name a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price-normal") %>% 
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
           location = "София",
           type = "Месо и месни продукти",
           source = "Бакалийка", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:1
eggs <- map_dfr(pages_to_scrape, scrape_prices)
#-----------------------------------------------
bakaliika <- bind_rows(bak, drinks, eggs, frozen, hlqb, kandy, kolbasi, konservi, meat, milk)

bakaliika <- bakaliika %>% 
  mutate(unit = NA, price_old = NA, discount = NA, price_kg = NA) %>% 
  select(date, location, source, type, product,
         unit, price, price_old, discount, price_kg) %>% 
  mutate(across(!c(date, price, price_old), as.character)) %>% 
  mutate(across(price:price_old, as.numeric))

glimpse(bakaliika)
write_csv(bakaliika, "shiny/scraping/bakaliika.csv")
