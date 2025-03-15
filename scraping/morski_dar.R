library(tidyverse)
library(rvest)

#---------------------------------------------------------------------
base_url <- "https://morskidar.bg/products.php?category=pryasna-riba&page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".col-sm-6") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".shop-three-products-name a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".shop-three-products-price") %>% 
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
      #   html_element(".vid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Риба",
           source = "Морски дар", .before = product) %>% 
    separate_wider_delim(price, delim = " - ", names = c("unit", "price")) %>% 
    mutate(price = parse_number(price), unit = str_remove(unit, "\\.")) %>% 
  distinct()
  return(pc)
}
pages_to_scrape <- 1:4
prqsna <- map_dfr(pages_to_scrape, scrape_prices)
#---------------------------------------------------------------------
base_url <- "https://morskidar.bg/products.php?category=zamrazena-riba&page=%d"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".col-sm-6") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".shop-three-products-name a") %>% 
        html_text2(), 
      # subproduct = .x %>% 
      #   html_element(".m-offer-tile__title") %>% 
      #   html_text2(), 
      price = .x %>% 
        html_element(".shop-three-products-price") %>% 
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
      #   html_element(".vid") %>%
      #   html_text2()
    )) %>% 
    mutate(date = Sys.Date(),
           location = "София",
           type = "Риба",
           source = "Морски дар", .before = product) %>% 
    separate_wider_delim(price, delim = " - ", names = c("unit", "price")) %>% 
    mutate(price = parse_number(price), unit = str_remove(unit, "\\.")) %>% 
    distinct()
  return(pc)
}
pages_to_scrape <- 1:3
frozen <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------
morski_dar <- bind_rows(prqsna, frozen)

morski_dar <- morski_dar %>% 
  mutate(price_old = NA, discount = NA, price_kg = NA) %>% 
  select(date, location, source, type, product,
         unit, price, price_old, discount, price_kg) %>% 
  mutate(across(!c(date, price, price_old), as.character)) %>% 
  mutate(across(price:price_old, as.numeric)) %>% drop_na(price)

glimpse(morski_dar)
write_csv(morski_dar, "shiny/scraping/morski_dar.csv")
