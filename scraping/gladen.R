library(tidyverse)
library(rvest)
library(httr2)

req <- request("https://shop.gladen.bg/category/fruits-and-vegetables?page=1")
req |> req_headers() %>% 
  req_perform()
#---------------------------------------------------------------------------
base_url <- "https://shop.gladen.bg/category/fruits-and-vegetables?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements("._product-inner") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price, ._product-price-compare") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("._product-price-old") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>%
        html_element("._button_unit") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Плодове и зеленчуци",
           source = "Gladen", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
    return(pc)
}
pages_to_scrape <- 1:12
pz <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------------
base_url <- "https://shop.gladen.bg/category/mlechni-produkti-i-yayca?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements("._product-inner") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price, ._product-price-compare") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("._product-price-old") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>%
        html_element("._button_unit") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Мляко и млечни продукти",
           source = "Gladen", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:5
milk <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------------
base_url <- "https://shop.gladen.bg/category/meso-i-mesni-produkti?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements("._product-inner") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price, ._product-price-compare") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("._product-price-old") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>%
        html_element("._button_unit") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Месо и месни продукти",
           source = "Gladen", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:9
meat <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------------
base_url <- "https://shop.gladen.bg/category/kolbasi-i-delikatesi?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements("._product-inner") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price, ._product-price-compare") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("._product-price-old") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>%
        html_element("._button_unit") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Колбаси",
           source = "Gladen", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:15
kolbasi <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------------
base_url <- "https://shop.gladen.bg/category/brashno-zarneni-hrani-i-variva?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements("._product-inner") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price, ._product-price-compare") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("._product-price-old") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>%
        html_element("._button_unit") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Бакалия",
           source = "Gladen", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:6 #7
bakaliq <- map_dfr(pages_to_scrape, scrape_prices)
#-------------------------------------------------------------
base_url <- "https://shop.gladen.bg/category/konservi?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements("._product-inner") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price, ._product-price-compare") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("._product-price-old") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>%
        html_element("._button_unit") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Консерви",
           source = "Gladen", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:19
konservi <- map_dfr(pages_to_scrape, scrape_prices)
#----------------------------------------------------------------------------
base_url <- "https://shop.gladen.bg/category/hlyab-i-testeni-produkti?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements("._product-inner") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price, ._product-price-compare") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("._product-price-old") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>%
        html_element("._button_unit") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Брашно и хляб",
           source = "Gladen", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:2
hlqb <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------------
base_url <- "https://shop.gladen.bg/category/lyutenica-sosove-pesto-gorchica-i-mayoneza?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements("._product-inner") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price, ._product-price-compare") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("._product-price-old") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>%
        html_element("._button_unit") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Консерви",
           source = "Gladen", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:17
lutenica <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------------
base_url <- "https://shop.gladen.bg/category/sladki-soleni-i-yadki?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements("._product-inner") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price, ._product-price-compare") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("._product-price-old") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>%
        html_element("._button_unit") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Сладки и солени храни",
           source = "Gladen", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:50 # 97
kandy <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------------
base_url <- "https://shop.gladen.bg/category/olio-zehtin-ocet-i-maznini?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements("._product-inner") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price, ._product-price-compare") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("._product-price-old") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>%
        html_element("._button_unit") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Бакалия",
           source = "Gladen", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:5
olio <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------------
base_url <- "https://shop.gladen.bg/category/podpravki-sol-zahar-smesi?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements("._product-inner") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price, ._product-price-compare") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("._product-price-old") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>%
        html_element("._button_unit") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Бакалия",
           source = "Gladen", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:22
podpravki <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------------
base_url <- "https://shop.gladen.bg/category/zarneni-zakuski-i-myusli?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements("._product-inner") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price, ._product-price-compare") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("._product-price-old") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>%
        html_element("._button_unit") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Брашно и хляб",
           source = "Gladen", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:4
zurneni <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------------
base_url <- "https://shop.gladen.bg/category/med-sladka-techen-shokolad-i-tahani?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements("._product-inner") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price, ._product-price-compare") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("._product-price-old") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>%
        html_element("._button_unit") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Сладки и солени храни",
           source = "Gladen", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:5
med <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------------
base_url <- "https://shop.gladen.bg/category/zamrazeni-i-ohladeni-hrani?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements("._product-inner") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price, ._product-price-compare") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("._product-price-old") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>%
        html_element("._button_unit") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Замразени храни",
           source = "Gladen", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:8
frozen <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------------
base_url <- "https://shop.gladen.bg/category/vegan-i-specialni-hrani?page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements("._product-inner") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element("._product-name-tag") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".price, ._product-price-compare") %>% 
        html_text2(),
      price_old = .x %>%
        html_element("._product-price-old") %>%
        html_text2(),
      # discount = .x %>%
      #   html_element(".label-discount") %>%
      #   html_text2(),
      # price_kg = .x %>% 
      #   html_element(".m-offer-tile__basic-price") %>% 
      #   html_text2(),
      unit = .x %>%
        html_element("._button_unit") %>%
        html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Специални храни",
           source = "Gladen", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           #price_old = str_replace(price_old, ",", "."),
           price = parse_number(price),
           #price_old = parse_number(price_old)
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:12
vegan <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------
gladen <- bind_rows(pz, milk, meat, kolbasi, bakaliq, konservi, hlqb, 
                    lutenica, kandy, olio, podpravki, zurneni, med, frozen, vegan)

gladen <- gladen %>% 
  mutate(discount = NA, price_kg = NA, 
         price_old = str_replace(price_old, ",", "."),
         price_old = parse_number(price_old)) %>% 
  select(date, location, source, type, product,
         unit, price, price_old, discount, price_kg) %>% 
  mutate(across(!c(date, price, price_old), as.character)) %>% 
  mutate(across(price:price_old, as.numeric))

glimpse(gladen)
write_csv(gladen, "shiny/scraping/gladen.csv")
