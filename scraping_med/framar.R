library(tidyverse)
library(rvest)
library(arrow)
#--------------------------------------------------------
lekarstva <- read_html("https://apteka.framar.bg/%D0%BA%D0%B0%D1%82%D0%B5%D0%B3%D0%BE%D1%80%D0%B8%D0%B8/%D0%BA%D0%BE%D0%B7%D0%BC%D0%B5%D1%82%D0%B8%D0%BA%D0%B0?filters=v1&vars=4000,1,0,1") %>% 
  html_elements(".column") %>%
  map_dfr(~ tibble(
    product = .x %>% 
      html_element(".ttl") %>% 
      html_text2(), 
    price = .x %>% 
      html_element(".num") %>% 
      html_text2(),
  )) %>% 
  mutate(date = Sys.Date(),
         type = "Лекарства",
         source = "Фрамар", .before = product) %>% 
  mutate(price = str_replace(price, ",", "."), 
         price = parse_number(price)) %>% distinct()
#--------------------------------------------------------
cosmetics <- read_html("https://apteka.framar.bg/%D0%BA%D0%B0%D1%82%D0%B5%D0%B3%D0%BE%D1%80%D0%B8%D0%B8/%D0%BA%D0%BE%D0%B7%D0%BC%D0%B5%D1%82%D0%B8%D0%BA%D0%B0?filters=v1&vars=14800,1,0,1") %>% 
  html_elements(".column") %>%
  map_dfr(~ tibble(
    product = .x %>% 
      html_element(".ttl") %>% 
      html_text2(), 
    price = .x %>% 
      html_element(".num") %>% 
      html_text2(),
  )) %>% 
  mutate(date = Sys.Date(),
         type = "Козметика",
         source = "Фрамар", .before = product) %>% 
  mutate(price = str_replace(price, ",", "."), 
         price = parse_number(price)) %>% distinct()
#--------------------------------------------------------
natural <- read_html("https://apteka.framar.bg/%D0%BA%D0%B0%D1%82%D0%B5%D0%B3%D0%BE%D1%80%D0%B8%D0%B8/%D0%BD%D0%B0%D1%82%D1%83%D1%80%D0%B0%D0%BB%D0%BD%D0%B8-%D0%BF%D1%80%D0%BE%D0%B4%D1%83%D0%BA%D1%82%D0%B8?filters=v1&vars=1890,1,0,1") %>% 
  html_elements(".column") %>%
  map_dfr(~ tibble(
    product = .x %>% 
      html_element(".ttl") %>% 
      html_text2(), 
    price = .x %>% 
      html_element(".num") %>% 
      html_text2(),
  )) %>% 
  mutate(date = Sys.Date(),
         type = "Натурални продукти",
         source = "Фрамар", .before = product) %>% 
  mutate(price = str_replace(price, ",", "."), 
         price = parse_number(price)) %>% distinct()
#--------------------------------------------------------
hrani <- read_html("https://apteka.framar.bg/%D0%BA%D0%B0%D1%82%D0%B5%D0%B3%D0%BE%D1%80%D0%B8%D0%B8/%D1%85%D1%80%D0%B0%D0%BD%D0%B8?filters=v1&vars=1450,1,0,1") %>% 
  html_elements(".column") %>%
  map_dfr(~ tibble(
    product = .x %>% 
      html_element(".ttl") %>% 
      html_text2(), 
    price = .x %>% 
      html_element(".num") %>% 
      html_text2(),
  )) %>% 
  mutate(date = Sys.Date(),
         type = "Храни",
         source = "Фрамар", .before = product) %>% 
  mutate(price = str_replace(price, ",", "."), 
         price = parse_number(price)) %>% distinct()
#--------------------------------------------------------
mom_baby <- read_html("https://apteka.framar.bg/%D0%BA%D0%B0%D1%82%D0%B5%D0%B3%D0%BE%D1%80%D0%B8%D0%B8/%D0%B1%D0%B5%D0%B1%D0%B5%D1%88%D0%BA%D0%B8-%D0%B8-%D0%B4%D0%B5%D1%82%D1%81%D0%BA%D0%B8-%D1%81%D1%82%D0%BE%D0%BA%D0%B8-%D0%B8-%D0%BF%D1%80%D0%BE%D0%B4%D1%83%D0%BA%D1%82%D0%B8?filters=v1&vars=2500,1,0,1") %>% 
  html_elements(".column") %>%
  map_dfr(~ tibble(
    product = .x %>% 
      html_element(".ttl") %>% 
      html_text2(), 
    price = .x %>% 
      html_element(".num") %>% 
      html_text2(),
  )) %>% 
  mutate(date = Sys.Date(),
         type = "Мама и бебе",
         source = "Фрамар", .before = product) %>% 
  mutate(price = str_replace(price, ",", "."), 
         price = parse_number(price)) %>% distinct()
#--------------------------------------------------------
framar <- bind_rows(lekarstva, cosmetics, hrani, natural, mom_baby) %>% distinct()

glimpse(framar)
write_parquet(framar, "shiny/scraping_med/framar.parquet")
