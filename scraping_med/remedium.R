library(tidyverse)
library(rvest)
library(arrow)
#--------------------------------------------------------
base_url <- "https://remedium.bg/zdravni-problemi-5505/c?&page=%d#"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gRUunT") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".ekVkoV") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ikeAqT") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Лекарства",
           source = "Remedium", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:50
lekarstva <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://remedium.bg/hranitelni-dobavki-1652/c?&page=%d#"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gRUunT") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".ekVkoV") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ikeAqT") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Хранителни добавки",
           source = "Remedium", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:200
dobavki <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://remedium.bg/dietichni-hrani-1685/c?&page=%d#"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gRUunT") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".ekVkoV") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ikeAqT") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Храни",
           source = "Remedium", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:47
hrani <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://remedium.bg/bilki-1664/c?&page=%d#"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gRUunT") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".ekVkoV") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ikeAqT") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Билки",
           source = "Remedium", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:46
bilki <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://remedium.bg/kozmetika-za-litse-31602/c?&page=%d#"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gRUunT") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".ekVkoV") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ikeAqT") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Козметика",
           source = "Remedium", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:130
kozm1 <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://remedium.bg/kozmetika-za-kosa-31752/c?&page=%d#"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gRUunT") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".ekVkoV") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ikeAqT") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Козметика",
           source = "Remedium", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:133
kozm2 <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://remedium.bg/bebeshka-kozmetika-31544/c?&page=%d#"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gRUunT") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".ekVkoV") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ikeAqT") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Мама и бебе",
           source = "Remedium", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:23
mom_baby <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
base_url <- "https://remedium.bg/hrani-za-bebeta-1486/c?&page=%d#"
scrape_prices <- function(page) {
  url <- sprintf(base_url, page)
  page_content <- read_html(url)
  pc <- page_content %>% 
    html_elements(".gRUunT") %>%
    map_dfr(~ tibble(
      product = .x %>% 
        html_element(".ekVkoV") %>% 
        html_text2(), 
      price = .x %>% 
        html_element(".ikeAqT") %>% 
        html_text2(),
    )) %>% 
    mutate(date = Sys.Date(),
           type = "Мама и бебе",
           source = "Remedium", .before = product) %>% 
    mutate(price = str_replace(price, ",", "."), 
           price = parse_number(price),
    ) %>% distinct()
  return(pc)
}
pages_to_scrape <- 1:43
mom_baby1 <- map_dfr(pages_to_scrape, scrape_prices)
#--------------------------------------------------------
remedium <- bind_rows(lekarstva, dobavki, bilki, hrani, kozm1, 
                      kozm2, mom_baby, mom_baby1)

glimpse(remedium)
write_parquet(remedium, "shiny/scraping_med/remedium.parquet")
