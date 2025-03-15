library(tidyverse)
library(rvest)

#----------------------------------------------------------------------
bak <- read_html("https://shop24.bg/5-bakalya?id_category=5&n=300") %>%
  html_elements(".col-md-2") %>%
  map_dfr(~ tibble(
    product = .x %>% 
      html_element(".product-name") %>% 
      html_text2(), 
    # subproduct = .x %>% 
    #   html_element(".m-offer-tile__title") %>% 
    #   html_text2(), 
    price = .x %>% 
      html_element(".product-price") %>% 
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
         location = "Цялата страна",
         type = "Бакалия",
         source = "Shop24", .before = product) %>% 
  mutate(price = str_replace(price, ",", "."), 
         #price_old = str_replace(price_old, ",", "."),
         price = parse_number(price),
         #price_old = parse_number(price_old)
  ) %>% distinct()

milk <- read_html("https://shop24.bg/8-mlechni-produkti-i-yaitsa?id_category=5&n=264") %>%
  html_elements(".col-md-2") %>%
  map_dfr(~ tibble(
    product = .x %>% 
      html_element(".product-name") %>% 
      html_text2(), 
    # subproduct = .x %>% 
    #   html_element(".m-offer-tile__title") %>% 
    #   html_text2(), 
    price = .x %>% 
      html_element(".product-price") %>% 
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
         location = "Цялата страна",
         type = "Мляко и млечни продукти",
         source = "Shop24", .before = product) %>% 
  mutate(price = str_replace(price, ",", "."), 
         #price_old = str_replace(price_old, ",", "."),
         price = parse_number(price),
         #price_old = parse_number(price_old)
  ) %>% distinct()

meat <- read_html("https://shop24.bg/91-meso-i-mesni-produkti?id_category=5&n=43") %>%
  html_elements(".col-md-2") %>%
  map_dfr(~ tibble(
    product = .x %>% 
      html_element(".product-name") %>% 
      html_text2(), 
    # subproduct = .x %>% 
    #   html_element(".m-offer-tile__title") %>% 
    #   html_text2(), 
    price = .x %>% 
      html_element(".product-price") %>% 
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
         location = "Цялата страна",
         type = "Месо и месни продукти",
         source = "Shop24", .before = product) %>% 
  mutate(price = str_replace(price, ",", "."), 
         #price_old = str_replace(price_old, ",", "."),
         price = parse_number(price),
         #price_old = parse_number(price_old)
  ) %>% distinct()

fruit <- read_html("https://shop24.bg/92-plodove-i-zelenchtsi?id_category=5&n=183") %>%
  html_elements(".col-md-2") %>%
  map_dfr(~ tibble(
    product = .x %>% 
      html_element(".product-name") %>% 
      html_text2(), 
    # subproduct = .x %>% 
    #   html_element(".m-offer-tile__title") %>% 
    #   html_text2(), 
    price = .x %>% 
      html_element(".product-price") %>% 
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
         location = "Цялата страна",
         type = "Плодове и зеленчуци",
         source = "Shop24", .before = product) %>% 
  mutate(price = str_replace(price, ",", "."), 
         #price_old = str_replace(price_old, ",", "."),
         price = parse_number(price),
         #price_old = parse_number(price_old)
  ) %>% distinct()

drinks <- read_html("https://shop24.bg/17-napitki?id_category=5&n=261") %>%
  html_elements(".col-md-2") %>%
  map_dfr(~ tibble(
    product = .x %>% 
      html_element(".product-name") %>% 
      html_text2(), 
    # subproduct = .x %>% 
    #   html_element(".m-offer-tile__title") %>% 
    #   html_text2(), 
    price = .x %>% 
      html_element(".product-price") %>% 
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
         location = "Цялата страна",
         type = "Напитки",
         source = "Shop24", .before = product) %>% 
  mutate(price = str_replace(price, ",", "."), 
         #price_old = str_replace(price_old, ",", "."),
         price = parse_number(price),
         #price_old = parse_number(price_old)
  ) %>% distinct()

kon1 <- read_html("https://shop24.bg/31-konservi-zelenchukovi?id_category=5&n=102") %>%
  html_elements(".col-md-2") %>%
  map_dfr(~ tibble(
    product = .x %>% 
      html_element(".product-name") %>% 
      html_text2(), 
    # subproduct = .x %>% 
    #   html_element(".m-offer-tile__title") %>% 
    #   html_text2(), 
    price = .x %>% 
      html_element(".product-price") %>% 
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
         location = "Цялата страна",
         type = "Консерви",
         source = "Shop24", .before = product) %>% 
  mutate(price = str_replace(price, ",", "."), 
         #price_old = str_replace(price_old, ",", "."),
         price = parse_number(price),
         #price_old = parse_number(price_old)
  ) %>% distinct()

kon2 <- read_html("https://shop24.bg/1087-plodovi-konservi?id_category=5&n=28") %>%
  html_elements(".col-md-2") %>%
  map_dfr(~ tibble(
    product = .x %>% 
      html_element(".product-name") %>% 
      html_text2(), 
    # subproduct = .x %>% 
    #   html_element(".m-offer-tile__title") %>% 
    #   html_text2(), 
    price = .x %>% 
      html_element(".product-price") %>% 
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
         location = "Цялата страна",
         type = "Консерви",
         source = "Shop24", .before = product) %>% 
  mutate(price = str_replace(price, ",", "."), 
         #price_old = str_replace(price_old, ",", "."),
         price = parse_number(price),
         #price_old = parse_number(price_old)
  ) %>% distinct()

kon3 <- read_html("https://shop24.bg/260-konservi-s-meso?id_category=5&n=17") %>%
  html_elements(".col-md-2") %>%
  map_dfr(~ tibble(
    product = .x %>% 
      html_element(".product-name") %>% 
      html_text2(), 
    # subproduct = .x %>% 
    #   html_element(".m-offer-tile__title") %>% 
    #   html_text2(), 
    price = .x %>% 
      html_element(".product-price") %>% 
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
         location = "Цялата страна",
         type = "Консерви",
         source = "Shop24", .before = product) %>% 
  mutate(price = str_replace(price, ",", "."), 
         #price_old = str_replace(price_old, ",", "."),
         price = parse_number(price),
         #price_old = parse_number(price_old)
  ) %>% distinct()

kon4 <- read_html("https://shop24.bg/77-konservi-ribni?id_category=5&n=15") %>%
  html_elements(".col-md-2") %>%
  map_dfr(~ tibble(
    product = .x %>% 
      html_element(".product-name") %>% 
      html_text2(), 
    # subproduct = .x %>% 
    #   html_element(".m-offer-tile__title") %>% 
    #   html_text2(), 
    price = .x %>% 
      html_element(".product-price") %>% 
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
         location = "Цялата страна",
         type = "Консерви",
         source = "Shop24", .before = product) %>% 
  mutate(price = str_replace(price, ",", "."), 
         #price_old = str_replace(price_old, ",", "."),
         price = parse_number(price),
         #price_old = parse_number(price_old)
  ) %>% distinct()

shop24 <- bind_rows(bak, milk, meat, fruit, drinks, kon1, kon2, kon3, kon4)

shop24 <- shop24 %>% 
  mutate(unit = NA, price_old = NA, discount = NA, price_kg = NA) %>% 
  select(date, location, source, type, product,
         unit, price, price_old, discount, price_kg) %>% 
  mutate(across(!c(date, price, price_old), as.character)) %>% 
  mutate(across(price:price_old, as.numeric))

glimpse(shop24)
write_csv(shop24, "shiny/scraping/shop24.csv")
