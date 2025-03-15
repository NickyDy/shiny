library(tidyverse)
library(rvest)

#---------------------------------------------------------------------
base_url <- "https://vmv.bg/plodove-i-zelenchutsi-97d170e1/c?&page=%d#"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".iRKOWr") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".iCHBIP") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".bpNfCq") %>% 
        html_text2(),
      # price_promo = .x %>% 
      #   html_element(".vEeyx") %>% 
      #   html_text2(),
      # price_old = .x %>%
      #   html_element(".mb-n2") %>%
      #   html_text2(),
      # discount = .x %>%
      #   html_element(".gsGiYR") %>%
      #   html_text2(),
      price_kg = .x %>%
        html_element(".johwgq") %>%
        html_text2(),
      # unit = .x %>%
      #   html_element(".vid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Плодове и зеленчуци",
           source = "VMV", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% 
    distinct()
  return(pc)
}
pages_to_scrape <- 1:2
pz <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://vmv.bg/meso-97d170e1/c"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".iRKOWr") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".iCHBIP") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".bpNfCq") %>% 
        html_text2(),
      # price_promo = .x %>% 
      #   html_element(".vEeyx") %>% 
      #   html_text2(),
      # price_old = .x %>%
      #   html_element(".mb-n2") %>%
      #   html_text2(),
      # discount = .x %>%
      #   html_element(".gsGiYR") %>%
      #   html_text2(),
      price_kg = .x %>%
        html_element(".johwgq") %>%
        html_text2(),
      # unit = .x %>%
      #   html_element(".vid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Месо и месни продукти",
           source = "VMV", .before = product) %>% 
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
base_url <- "https://vmv.bg/riba-97d170e1/c"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".iRKOWr") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".iCHBIP") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".bpNfCq") %>% 
        html_text2(),
      # price_promo = .x %>% 
      #   html_element(".vEeyx") %>% 
      #   html_text2(),
      # price_old = .x %>%
      #   html_element(".mb-n2") %>%
      #   html_text2(),
      # discount = .x %>%
      #   html_element(".gsGiYR") %>%
      #   html_text2(),
      price_kg = .x %>%
        html_element(".johwgq") %>%
        html_text2(),
      # unit = .x %>%
      #   html_element(".vid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Риба",
           source = "VMV", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:1
fish <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://vmv.bg/mlechni-produkti-i-yaytsa-97d170e1/c?&page=%d#"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".iRKOWr") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".iCHBIP") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".bpNfCq") %>% 
        html_text2(),
      # price_promo = .x %>% 
      #   html_element(".vEeyx") %>% 
      #   html_text2(),
      # price_old = .x %>%
      #   html_element(".mb-n2") %>%
      #   html_text2(),
      # discount = .x %>%
      #   html_element(".gsGiYR") %>%
      #   html_text2(),
      price_kg = .x %>%
        html_element(".johwgq") %>%
        html_text2(),
      # unit = .x %>%
      #   html_element(".vid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Мляко и млечни продукти",
           source = "VMV", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:6
milk <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://vmv.bg/kolbasi-i-delikatesi-97d170e1/c?&page=%d#"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".iRKOWr") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".iCHBIP") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".bpNfCq") %>% 
        html_text2(),
      # price_promo = .x %>% 
      #   html_element(".vEeyx") %>% 
      #   html_text2(),
      # price_old = .x %>%
      #   html_element(".mb-n2") %>%
      #   html_text2(),
      # discount = .x %>%
      #   html_element(".gsGiYR") %>%
      #   html_text2(),
      price_kg = .x %>%
        html_element(".johwgq") %>%
        html_text2(),
      # unit = .x %>%
      #   html_element(".vid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Колбаси",
           source = "VMV", .before = product) %>% 
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
base_url <- "https://vmv.bg/hlyab-i-testeni-izdelia-97d170e1/c?&page=%d#"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".iRKOWr") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".iCHBIP") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".bpNfCq") %>% 
        html_text2(),
      # price_promo = .x %>% 
      #   html_element(".vEeyx") %>% 
      #   html_text2(),
      # price_old = .x %>%
      #   html_element(".mb-n2") %>%
      #   html_text2(),
      # discount = .x %>%
      #   html_element(".gsGiYR") %>%
      #   html_text2(),
      price_kg = .x %>%
        html_element(".johwgq") %>%
        html_text2(),
      # unit = .x %>%
      #   html_element(".vid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Брашно и хляб",
           source = "VMV", .before = product) %>% 
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
base_url <- "https://vmv.bg/bio-i-dietichni-hrani-97d170e1/c?&page=%d#"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".iRKOWr") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".iCHBIP") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".bpNfCq") %>% 
        html_text2(),
      # price_promo = .x %>% 
      #   html_element(".vEeyx") %>% 
      #   html_text2(),
      # price_old = .x %>%
      #   html_element(".mb-n2") %>%
      #   html_text2(),
      # discount = .x %>%
      #   html_element(".gsGiYR") %>%
      #   html_text2(),
      price_kg = .x %>%
        html_element(".johwgq") %>%
        html_text2(),
      # unit = .x %>%
      #   html_element(".vid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Специални храни",
           source = "VMV", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:8
bio <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://vmv.bg/paketirani-hrani-97d170e1/c?&page=%d#"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".iRKOWr") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".iCHBIP") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".bpNfCq") %>% 
        html_text2(),
      # price_promo = .x %>% 
      #   html_element(".vEeyx") %>% 
      #   html_text2(),
      # price_old = .x %>%
      #   html_element(".mb-n2") %>%
      #   html_text2(),
      # discount = .x %>%
      #   html_element(".gsGiYR") %>%
      #   html_text2(),
      price_kg = .x %>%
        html_element(".johwgq") %>%
        html_text2(),
      # unit = .x %>%
      #   html_element(".vid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Бакалия",
           source = "VMV", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:14
bakaliq <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://vmv.bg/sladkarski-i-zaharni-izdeliq-97d170e1/c?&page=%d#"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".iRKOWr") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".iCHBIP") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".bpNfCq") %>% 
        html_text2(),
      # price_promo = .x %>% 
      #   html_element(".vEeyx") %>% 
      #   html_text2(),
      # price_old = .x %>%
      #   html_element(".mb-n2") %>%
      #   html_text2(),
      # discount = .x %>%
      #   html_element(".gsGiYR") %>%
      #   html_text2(),
      price_kg = .x %>%
        html_element(".johwgq") %>%
        html_text2(),
      # unit = .x %>%
      #   html_element(".vid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Сладки и солени храни",
           source = "VMV", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:10
kandy <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://vmv.bg/chips-i-zurneni-zakuski-97d170e1/c?&page=%d#"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".iRKOWr") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".iCHBIP") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".bpNfCq") %>% 
        html_text2(),
      # price_promo = .x %>% 
      #   html_element(".vEeyx") %>% 
      #   html_text2(),
      # price_old = .x %>%
      #   html_element(".mb-n2") %>%
      #   html_text2(),
      # discount = .x %>%
      #   html_element(".gsGiYR") %>%
      #   html_text2(),
      price_kg = .x %>%
        html_element(".johwgq") %>%
        html_text2(),
      # unit = .x %>%
      #   html_element(".vid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Брашно и хляб",
           source = "VMV", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:4
chips <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://vmv.bg/konservi-97d170e1/c?&page=%d#"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".iRKOWr") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".iCHBIP") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".bpNfCq") %>% 
        html_text2(),
      # price_promo = .x %>% 
      #   html_element(".vEeyx") %>% 
      #   html_text2(),
      # price_old = .x %>%
      #   html_element(".mb-n2") %>%
      #   html_text2(),
      # discount = .x %>%
      #   html_element(".gsGiYR") %>%
      #   html_text2(),
      price_kg = .x %>%
        html_element(".johwgq") %>%
        html_text2(),
      # unit = .x %>%
      #   html_element(".vid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Консерви",
           source = "VMV", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:8 #9
konservi <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://vmv.bg/napitki-97d170e1/c?&page=%d#"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".iRKOWr") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".iCHBIP") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".bpNfCq") %>% 
        html_text2(),
      # price_promo = .x %>% 
      #   html_element(".vEeyx") %>% 
      #   html_text2(),
      # price_old = .x %>%
      #   html_element(".mb-n2") %>%
      #   html_text2(),
      # discount = .x %>%
      #   html_element(".gsGiYR") %>%
      #   html_text2(),
      price_kg = .x %>%
        html_element(".johwgq") %>%
        html_text2(),
      # unit = .x %>%
      #   html_element(".vid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Напитки",
           source = "VMV", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:19
drinks <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://vmv.bg/zamrazeni-hrani-97d170e1/c?&page=%d#"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".iRKOWr") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".iCHBIP") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".bpNfCq") %>% 
        html_text2(),
      # price_promo = .x %>% 
      #   html_element(".vEeyx") %>% 
      #   html_text2(),
      # price_old = .x %>%
      #   html_element(".mb-n2") %>%
      #   html_text2(),
      # discount = .x %>%
      #   html_element(".gsGiYR") %>%
      #   html_text2(),
      price_kg = .x %>%
        html_element(".johwgq") %>%
        html_text2(),
      # unit = .x %>%
      #   html_element(".vid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Замразени храни",
           source = "VMV", .before = product) %>% 
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
vmv <- bind_rows(pz, meat, fish, milk, kolbasi, hlqb, bio, 
                 bakaliq, kandy, chips, konservi, drinks, frozen)

vmv <- vmv %>% 
  mutate(unit = NA, price_old = NA, discount = NA, 
         price_kg = parse_number(price_kg)) %>% 
  select(date, location, source, type, product,
         unit, price, price_old, discount, price_kg) %>% 
  mutate(across(!c(date, price, price_old), as.character)) %>% 
  mutate(across(price:price_old, as.numeric))

glimpse(vmv)
write_csv(vmv, "shiny/scraping/vmv.csv")
