library(tidyverse)
library(rvest)

#---------------------------------------------------------------------
base_url <- "https://vmv.bg/categories/plodove-i-zelenchutsi-97d170e1?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".ProductCard_product-card__content___gikB") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._title--h3_1ldrp_26") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ProductCard_product-card__price__regular-price__dhOL9") %>% 
        html_text2(),
      unit = .x %>%
        html_element(".ProductCard_product-price__per-unit__unit__sSEY7") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Плодове и зеленчуци",
           source = "VMV", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
           ) %>% 
    distinct()
  return(pc)
}
pages_to_scrape <- 1:5
pz <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://vmv.bg/categories/meso-97d170e1?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".ProductCard_product-card__content___gikB") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._title--h3_1ldrp_26") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ProductCard_product-card__price__regular-price__dhOL9") %>% 
        html_text2(),
      unit = .x %>%
        html_element(".ProductCard_product-price__per-unit__unit__sSEY7") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Месо и месни продукти",
           source = "VMV", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
           ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:1
meat <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://vmv.bg/categories/riba-97d170e1"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".ProductCard_product-card__content___gikB") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._title--h3_1ldrp_26") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ProductCard_product-card__price__regular-price__dhOL9") %>% 
        html_text2(),
      unit = .x %>%
        html_element(".ProductCard_product-price__per-unit__unit__sSEY7") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Риба",
           source = "VMV", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
           ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:1
fish <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://vmv.bg/categories/mlechni-produkti-i-yaytsa-97d170e1?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".ProductCard_product-card__content___gikB") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._title--h3_1ldrp_26") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ProductCard_product-card__price__regular-price__dhOL9") %>% 
        html_text2(),
      unit = .x %>%
        html_element(".ProductCard_product-price__per-unit__unit__sSEY7") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Мляко и млечни продукти",
           source = "VMV", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
           ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:12
milk <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://vmv.bg/categories/kolbasi-i-delikatesi-97d170e1?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".ProductCard_product-card__content___gikB") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._title--h3_1ldrp_26") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ProductCard_product-card__price__regular-price__dhOL9") %>% 
        html_text2(),
      unit = .x %>%
        html_element(".ProductCard_product-price__per-unit__unit__sSEY7") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Колбаси",
           source = "VMV", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
           ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:6
kolbasi <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://vmv.bg/categories/hlyab-i-testeni-izdelia-97d170e1?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".ProductCard_product-card__content___gikB") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._title--h3_1ldrp_26") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ProductCard_product-card__price__regular-price__dhOL9") %>% 
        html_text2(),
      unit = .x %>%
        html_element(".ProductCard_product-price__per-unit__unit__sSEY7") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Брашно и хляб",
           source = "VMV", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
           ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:3
hlqb <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://vmv.bg/categories/bio-i-dietichni-hrani-97d170e1?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".ProductCard_product-card__content___gikB") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._title--h3_1ldrp_26") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ProductCard_product-card__price__regular-price__dhOL9") %>% 
        html_text2(),
      unit = .x %>%
        html_element(".ProductCard_product-price__per-unit__unit__sSEY7") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Специални храни",
           source = "VMV", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
           ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:14
bio <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://vmv.bg/categories/konservi-97d170e1?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".ProductCard_product-card__content___gikB") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._title--h3_1ldrp_26") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ProductCard_product-card__price__regular-price__dhOL9") %>% 
        html_text2(),
      unit = .x %>%
        html_element(".ProductCard_product-price__per-unit__unit__sSEY7") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Консерви",
           source = "VMV", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
           ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:12
konservi <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://vmv.bg/categories/napitki-97d170e1?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".ProductCard_product-card__content___gikB") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._title--h3_1ldrp_26") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ProductCard_product-card__price__regular-price__dhOL9") %>% 
        html_text2(),
      unit = .x %>%
        html_element(".ProductCard_product-price__per-unit__unit__sSEY7") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Напитки",
           source = "VMV", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
           ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:26
drinks <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://vmv.bg/categories/zamrazeni-hrani-97d170e1?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".ProductCard_product-card__content___gikB") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._title--h3_1ldrp_26") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ProductCard_product-card__price__regular-price__dhOL9") %>% 
        html_text2(),
      unit = .x %>%
        html_element(".ProductCard_product-price__per-unit__unit__sSEY7") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Замразени храни",
           source = "VMV", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
           ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:3
frozen <- map_dfr(pages_to_scrape, scrape_prices)
#-----------------------------------------------
base_url <- "https://vmv.bg/categories/paketirani-hrani-97d170e1?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".ProductCard_product-card__content___gikB") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._title--h3_1ldrp_26") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ProductCard_product-card__price__regular-price__dhOL9") %>% 
        html_text2(),
      unit = .x %>%
        html_element(".ProductCard_product-price__per-unit__unit__sSEY7") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Бакалия",
           source = "VMV", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:38
bakaliq <- map_dfr(pages_to_scrape, scrape_prices)
#------------------------------------------------
vmv <- bind_rows(pz, meat, fish, milk, kolbasi, hlqb, bio, 
                 bakaliq, konservi, frozen)

vmv <- vmv %>% 
  select(date, location, source, type, product, unit, price) %>% 
  mutate(across(!c(date, price), as.character)) %>% 
  mutate(price = as.numeric(price))

glimpse(vmv)
write_csv(vmv, "shiny/scraping/vmv.csv")
