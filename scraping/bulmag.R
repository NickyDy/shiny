library(tidyverse)
library(rvest)

#--------------------------------------------
base_url <- "https://bulmag.org/category/plodove-i-zelenchuci?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".col") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".mb-2") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".product-price, .text-brand-red") %>% 
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
        html_element(".fw-normal") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна, Шумен, Търговище",
           type = "Плодове и зеленчуци",
           source = "BulMag", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:1
fruit <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------
base_url <- "https://bulmag.org/category/mesni-produkti?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".col") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".mb-2") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".product-price, .text-brand-red") %>% 
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
        html_element(".fw-normal") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна, Шумен, Търговище",
           type = "Месо и месни продукти",
           source = "BulMag", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:1
meat <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------
base_url <- "https://bulmag.org/category/mlechni-produkti?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".col") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".mb-2") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".product-price, .text-brand-red") %>% 
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
        html_element(".fw-normal") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна, Шумен, Търговище",
           type = "Мляко и млечни продукти",
           source = "BulMag", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:1
milk <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------
base_url <- "https://bulmag.org/category/zamrazeni-hrani?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".col") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".mb-2") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".product-price, .text-brand-red") %>% 
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
        html_element(".fw-normal") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна, Шумен, Търговище",
           type = "Замразени храни",
           source = "BulMag", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:1
frozen <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------
base_url <- "https://bulmag.org/category/paketirani-hrani?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".col") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".mb-2") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".product-price, .text-brand-red") %>% 
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
        html_element(".fw-normal") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна, Шумен, Търговище",
           type = "Бакалия",
           source = "BulMag", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:1
bak <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------
base_url <- "https://bulmag.org/category/sladki-i-soleni?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".col") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".mb-2") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".product-price, .text-brand-red") %>% 
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
        html_element(".fw-normal") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна, Шумен, Търговище",
           type = "Сладки и солени храни",
           source = "BulMag", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:1
slad_sol <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------
base_url <- "https://bulmag.org/category/napitki?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".col") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".mb-2") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".product-price, .text-brand-red") %>% 
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
        html_element(".fw-normal") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна, Шумен, Търговище",
           type = "Напитки",
           source = "BulMag", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:1
drinks <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------
base_url <- "https://bulmag.org/category/alkoholni-napitki?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".col") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".mb-2") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".product-price, .text-brand-red") %>% 
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
        html_element(".fw-normal") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна, Шумен, Търговище",
           type = "Напитки",
           source = "BulMag", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:1
alc <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------
base_url <- "https://bulmag.org/category/specialni-hrani?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".col") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".mb-2") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".product-price, .text-brand-red") %>% 
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
        html_element(".fw-normal") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "Варна, Шумен, Търговище",
           type = "Специални храни",
           source = "BulMag", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:1
spec <- map_dfr(pages_to_scrape, scrape_prices)
#-----------------------------------------------
bulmag <- bind_rows(fruit, meat, milk, frozen, bak, slad_sol, drinks, alc, spec)

bulmag <- bulmag %>% 
  mutate(price_old = NA, discount = NA, price_kg = NA) %>% 
  select(date, location, source, type, product,
         unit, price, price_old, discount, price_kg) %>% 
  mutate(across(!c(date, price, price_old), as.character)) %>% 
  mutate(across(price:price_old, as.numeric))

glimpse(bulmag)
write_csv(bulmag, "shiny/scraping/bulmag.csv")