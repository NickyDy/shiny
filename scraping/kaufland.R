library(tidyverse)
library(rvest)

#------------------------------------------------------
meat <- read_html("https://www.kaufland.bg/aktualni-predlozheniya/ot-ponedelnik/obsht-pregled.category=01_%D0%9C%D0%B5%D1%81%D0%BE__%D0%BF%D1%82%D0%B8%D1%87%D0%B5_%D0%BC%D0%B5%D1%81%D0%BE__%D0%BA%D0%BE%D0%BB%D0%B1%D0%B0%D1%81%D0%B8.html")
fish <- read_html("https://www.kaufland.bg/aktualni-predlozheniya/ot-ponedelnik/obsht-pregled.category=01a_%D0%9F%D1%80%D1%8F%D1%81%D0%BD%D0%B0_%D1%80%D0%B8%D0%B1%D0%B0.html")
fruits_veg <- read_html("https://www.kaufland.bg/aktualni-predlozheniya/ot-ponedelnik/obsht-pregled.category=02_%D0%9F%D0%BB%D0%BE%D0%B4%D0%BE%D0%B2%D0%B5_%D0%B8_%D0%B7%D0%B5%D0%BB%D0%B5%D0%BD%D1%87%D1%83%D1%86%D0%B8.html")
milk <- read_html("https://www.kaufland.bg/aktualni-predlozheniya/ot-ponedelnik/obsht-pregled.category=03_%D0%9C%D0%BB%D0%B5%D1%87%D0%BD%D0%B8_%D0%BF%D1%80%D0%BE%D0%B4%D1%83%D0%BA%D1%82%D0%B8.html")
frozen <- read_html("https://www.kaufland.bg/aktualni-predlozheniya/ot-ponedelnik/obsht-pregled.category=04_%D0%97%D0%B0%D0%BC%D1%80%D0%B0%D0%B7%D0%B5%D0%BD%D0%B8_%D0%BF%D1%80%D0%BE%D0%B4%D1%83%D0%BA%D1%82%D0%B8.html")
konservi <- read_html("https://www.kaufland.bg/aktualni-predlozheniya/ot-ponedelnik/obsht-pregled.category=05_%D0%9A%D0%BE%D0%BD%D1%81%D0%B5%D1%80%D0%B2%D0%B8__%D0%B4%D0%B5%D0%BB%D0%B8%D0%BA%D0%B0%D1%82%D0%B5%D1%81%D0%B8.html")
bakaliq <- read_html("https://www.kaufland.bg/aktualni-predlozheniya/ot-ponedelnik/obsht-pregled.category=06_%D0%9E%D1%81%D0%BD%D0%BE%D0%B2%D0%BD%D0%B8_%D1%85%D1%80%D0%B0%D0%BD%D0%B8.html")
coffee <- read_html("https://www.kaufland.bg/aktualni-predlozheniya/ot-ponedelnik/obsht-pregled.category=07_%D0%9A%D0%B0%D1%84%D0%B5__%D1%87%D0%B0%D0%B9__%D0%B7%D0%B0%D1%85%D0%B0%D1%80%D0%BD%D0%B8_%D0%B8%D0%B7%D0%B4%D0%B5%D0%BB%D0%B8%D1%8F__%D1%81%D0%BD%D0%B0%D0%BA%D1%81.html")
coffee1 <- read_html("https://www.kaufland.bg/aktualni-predlozheniya/ot-ponedelnik/obsht-pregled.category=07_%D0%9A%D0%B0%D1%84%D0%B5__%D1%87%D0%B0%D0%B9__%D0%B7%D0%B0%D1%85%D0%B0%D1%80%D0%BD%D0%B8_%D0%B8%D0%B7%D0%B4%D0%B5%D0%BB%D0%B8%D1%8F__%D1%81%D0%BD%D0%B0%D0%BA%D1%81.pageIndex=1.html")
drinks <- read_html("https://www.kaufland.bg/aktualni-predlozheniya/ot-ponedelnik/obsht-pregled.category=08_%D0%90%D0%BB%D0%BA%D0%BE%D1%85%D0%BE%D0%BB%D0%BD%D0%B8_%D0%B8_%D0%B1%D0%B5%D0%B7%D0%B0%D0%BB%D0%BA%D0%BE%D1%85%D0%BE%D0%BB%D0%BD%D0%B8_%D0%BD%D0%B0%D0%BF%D0%B8%D1%82%D0%BA%D0%B8.html")
razni1 <- read_html("https://www.kaufland.bg/aktualni-predlozheniya/ot-ponedelnik/obsht-pregled.category=20231115_%D0%94%D0%BE%D1%81%D1%82%D1%8A%D0%BF%D0%BD%D0%BE_%D0%B7%D0%B0_%D0%B2%D0%B0%D1%81.html")
razni2 <- read_html("https://www.kaufland.bg/aktualni-predlozheniya/ot-ponedelnik/obsht-pregled.category=20231127_%D0%9D%D0%B8%D0%BA%D1%83%D0%BB%D0%B4%D0%B5%D0%BD_.html")
razni3 <- read_html("https://www.kaufland.bg/aktualni-predlozheniya/ot-ponedelnik/obsht-pregled.category=20231127_%D0%9D%D0%B8%D0%BA%D1%83%D0%BB%D0%B4%D0%B5%D0%BD_.pageIndex=1.html")
razni4 <- read_html("https://www.kaufland.bg/aktualni-predlozheniya/ot-ponedelnik/obsht-pregled.category=20231127_%D0%9D%D0%B8%D0%BA%D1%83%D0%BB%D0%B4%D0%B5%D0%BD_.pageIndex=2.html")

meat_df <- meat %>% 
  html_elements(".m-offer-tile--mobile") %>%
  map_dfr(~ tibble(
    product = .x %>% 
      html_element(".m-offer-tile__subtitle") %>% 
      html_text2(), 
    subproduct = .x %>% 
      html_element(".m-offer-tile__title") %>% 
      html_text2(), 
    price = .x %>% 
      html_element(".a-pricetag__price") %>% 
      html_text2(),
    price_old = .x %>% 
      html_element(".a-pricetag__line-through") %>% 
      html_text2(),
    discount = .x %>% 
      html_element(".a-pricetag__discount") %>% 
      html_text2(),
    price_kg = .x %>% 
      html_element(".m-offer-tile__basic-price") %>% 
      html_text2(),
    unit = .x %>% 
      html_element(".m-offer-tile__quantity") %>% 
      html_text2())) %>% 
  mutate(date = Sys.Date(),
         location = "Цялата страна",
         type = "Месо и месни продукти",
         source = "Kaufland", .before = product) %>% 
  mutate(price = str_replace(price, ",", "."), price_old = str_replace(price_old, ",", ".")) %>% 
  mutate(price = parse_number(price), price_old = parse_number(price_old)) %>% 
  distinct()

fish_df <- fish %>% 
  html_elements(".m-offer-tile--mobile") %>%
  map_dfr(~ tibble(
    product = .x %>% 
      html_element(".m-offer-tile__subtitle") %>% 
      html_text2(), 
    subproduct = .x %>% 
      html_element(".m-offer-tile__title") %>% 
      html_text2(), 
    price = .x %>% 
      html_element(".a-pricetag__price") %>% 
      html_text2(),
    price_old = .x %>% 
      html_element(".a-pricetag__line-through") %>% 
      html_text2(),
    discount = .x %>% 
      html_element(".a-pricetag__discount") %>% 
      html_text2(),
    price_kg = .x %>% 
      html_element(".m-offer-tile__basic-price") %>% 
      html_text2(),
    unit = .x %>% 
      html_element(".m-offer-tile__quantity") %>% 
      html_text2())) %>% 
  mutate(date = Sys.Date(),
         location = "Цялата страна",
         type = "Риба",
         source = "Kaufland", .before = product) %>% 
  mutate(price = str_replace(price, ",", "."), price_old = str_replace(price_old, ",", ".")) %>% 
  mutate(price = parse_number(price), price_old = parse_number(price_old)) %>% 
  distinct()

fruits_veg_df <- fruits_veg %>% 
  html_elements(".m-offer-tile--mobile") %>%
  map_dfr(~ tibble(
    product = .x %>% 
      html_element(".m-offer-tile__subtitle") %>% 
      html_text2(), 
    subproduct = .x %>% 
      html_element(".m-offer-tile__title") %>% 
      html_text2(), 
    price = .x %>% 
      html_element(".a-pricetag__price") %>% 
      html_text2(),
    price_old = .x %>% 
      html_element(".a-pricetag__line-through") %>% 
      html_text2(),
    discount = .x %>% 
      html_element(".a-pricetag__discount") %>% 
      html_text2(),
    price_kg = .x %>% 
      html_element(".m-offer-tile__basic-price") %>% 
      html_text2(),
    unit = .x %>% 
      html_element(".m-offer-tile__quantity") %>% 
      html_text2())) %>% 
  mutate(date = Sys.Date(),
         location = "Цялата страна",
         type = "Плодове и зеленчуци",
         source = "Kaufland", .before = product) %>% 
  mutate(price = str_replace(price, ",", "."), price_old = str_replace(price_old, ",", ".")) %>% 
  mutate(price = parse_number(price), price_old = parse_number(price_old)) %>% 
  distinct()

milk_df <- milk %>% 
  html_elements(".m-offer-tile--mobile") %>%
  map_dfr(~ tibble(
    product = .x %>% 
      html_element(".m-offer-tile__subtitle") %>% 
      html_text2(), 
    subproduct = .x %>% 
      html_element(".m-offer-tile__title") %>% 
      html_text2(), 
    price = .x %>% 
      html_element(".a-pricetag__price") %>% 
      html_text2(),
    price_old = .x %>% 
      html_element(".a-pricetag__line-through") %>% 
      html_text2(),
    discount = .x %>% 
      html_element(".a-pricetag__discount") %>% 
      html_text2(),
    price_kg = .x %>% 
      html_element(".m-offer-tile__basic-price") %>% 
      html_text2(),
    unit = .x %>% 
      html_element(".m-offer-tile__quantity") %>% 
      html_text2())) %>% 
  mutate(date = Sys.Date(),
         location = "Цялата страна",
         type = "Мляко и млечни продукти",
         source = "Kaufland", .before = product) %>% 
  mutate(price = str_replace(price, ",", "."), price_old = str_replace(price_old, ",", ".")) %>% 
  mutate(price = parse_number(price), price_old = parse_number(price_old)) %>% 
  distinct()

frozen_df <- frozen %>% 
  html_elements(".m-offer-tile--mobile") %>%
  map_dfr(~ tibble(
    product = .x %>% 
      html_element(".m-offer-tile__subtitle") %>% 
      html_text2(), 
    subproduct = .x %>% 
      html_element(".m-offer-tile__title") %>% 
      html_text2(), 
    price = .x %>% 
      html_element(".a-pricetag__price") %>% 
      html_text2(),
    price_old = .x %>% 
      html_element(".a-pricetag__line-through") %>% 
      html_text2(),
    discount = .x %>% 
      html_element(".a-pricetag__discount") %>% 
      html_text2(),
    price_kg = .x %>% 
      html_element(".m-offer-tile__basic-price") %>% 
      html_text2(),
    unit = .x %>% 
      html_element(".m-offer-tile__quantity") %>% 
      html_text2())) %>% 
  mutate(date = Sys.Date(),
         location = "Цялата страна",
         type = "Замразени храни",
         source = "Kaufland", .before = product) %>% 
  mutate(price = str_replace(price, ",", "."), price_old = str_replace(price_old, ",", ".")) %>% 
  mutate(price = parse_number(price), price_old = parse_number(price_old)) %>% 
  distinct()

konservi_df <- konservi %>% 
  html_elements(".m-offer-tile--mobile") %>%
  map_dfr(~ tibble(
    product = .x %>% 
      html_element(".m-offer-tile__subtitle") %>% 
      html_text2(), 
    subproduct = .x %>% 
      html_element(".m-offer-tile__title") %>% 
      html_text2(), 
    price = .x %>% 
      html_element(".a-pricetag__price") %>% 
      html_text2(),
    price_old = .x %>% 
      html_element(".a-pricetag__line-through") %>% 
      html_text2(),
    discount = .x %>% 
      html_element(".a-pricetag__discount") %>% 
      html_text2(),
    price_kg = .x %>% 
      html_element(".m-offer-tile__basic-price") %>% 
      html_text2(),
    unit = .x %>% 
      html_element(".m-offer-tile__quantity") %>% 
      html_text2())) %>% 
  mutate(date = Sys.Date(),
         location = "Цялата страна",
         type = "Консерви",
         source = "Kaufland", .before = product) %>% 
  mutate(price = str_replace(price, ",", "."), price_old = str_replace(price_old, ",", ".")) %>% 
  mutate(price = parse_number(price), price_old = parse_number(price_old)) %>% 
  distinct()

bakaliq_df <- bakaliq %>% 
  html_elements(".m-offer-tile--mobile") %>%
  map_dfr(~ tibble(
    product = .x %>% 
      html_element(".m-offer-tile__subtitle") %>% 
      html_text2(), 
    subproduct = .x %>% 
      html_element(".m-offer-tile__title") %>% 
      html_text2(), 
    price = .x %>% 
      html_element(".a-pricetag__price") %>% 
      html_text2(),
    price_old = .x %>% 
      html_element(".a-pricetag__line-through") %>% 
      html_text2(),
    discount = .x %>% 
      html_element(".a-pricetag__discount") %>% 
      html_text2(),
    price_kg = .x %>% 
      html_element(".m-offer-tile__basic-price") %>% 
      html_text2(),
    unit = .x %>% 
      html_element(".m-offer-tile__quantity") %>% 
      html_text2())) %>% 
  mutate(date = Sys.Date(),
         location = "Цялата страна",
         type = "Бакалия",
         source = "Kaufland", .before = product) %>% 
  mutate(price = str_replace(price, ",", "."), price_old = str_replace(price_old, ",", ".")) %>% 
  mutate(price = parse_number(price), price_old = parse_number(price_old)) %>% 
  distinct()

coffee_df <- coffee %>% 
  html_elements(".m-offer-tile--mobile") %>%
  map_dfr(~ tibble(
    product = .x %>% 
      html_element(".m-offer-tile__subtitle") %>% 
      html_text2(), 
    subproduct = .x %>% 
      html_element(".m-offer-tile__title") %>% 
      html_text2(), 
    price = .x %>% 
      html_element(".a-pricetag__price") %>% 
      html_text2(),
    price_old = .x %>% 
      html_element(".a-pricetag__line-through") %>% 
      html_text2(),
    discount = .x %>% 
      html_element(".a-pricetag__discount") %>% 
      html_text2(),
    price_kg = .x %>% 
      html_element(".m-offer-tile__basic-price") %>% 
      html_text2(),
    unit = .x %>% 
      html_element(".m-offer-tile__quantity") %>% 
      html_text2())) %>% 
  mutate(date = Sys.Date(),
         location = "Цялата страна",
         type = "Кафе и чай",
         source = "Kaufland", .before = product) %>% 
  mutate(price = str_replace(price, ",", "."), price_old = str_replace(price_old, ",", ".")) %>% 
  mutate(price = parse_number(price), price_old = parse_number(price_old)) %>% 
  distinct()

coffee1_df <- coffee1 %>% 
  html_elements(".m-offer-tile--mobile") %>%
  map_dfr(~ tibble(
    product = .x %>% 
      html_element(".m-offer-tile__subtitle") %>% 
      html_text2(), 
    subproduct = .x %>% 
      html_element(".m-offer-tile__title") %>% 
      html_text2(), 
    price = .x %>% 
      html_element(".a-pricetag__price") %>% 
      html_text2(),
    price_old = .x %>% 
      html_element(".a-pricetag__line-through") %>% 
      html_text2(),
    discount = .x %>% 
      html_element(".a-pricetag__discount") %>% 
      html_text2(),
    price_kg = .x %>% 
      html_element(".m-offer-tile__basic-price") %>% 
      html_text2(),
    unit = .x %>% 
      html_element(".m-offer-tile__quantity") %>% 
      html_text2())) %>% 
  mutate(date = Sys.Date(),
         location = "Цялата страна",
         type = "Кафе и чай",
         source = "Kaufland", .before = product) %>% 
  mutate(price = str_replace(price, ",", "."), price_old = str_replace(price_old, ",", ".")) %>% 
  mutate(price = parse_number(price), price_old = parse_number(price_old)) %>% 
  distinct()

drinks_df <- drinks %>% 
  html_elements(".m-offer-tile--mobile") %>%
  map_dfr(~ tibble(
    product = .x %>% 
      html_element(".m-offer-tile__subtitle") %>% 
      html_text2(), 
    subproduct = .x %>% 
      html_element(".m-offer-tile__title") %>% 
      html_text2(), 
    price = .x %>% 
      html_element(".a-pricetag__price") %>% 
      html_text2(),
    price_old = .x %>% 
      html_element(".a-pricetag__line-through") %>% 
      html_text2(),
    discount = .x %>% 
      html_element(".a-pricetag__discount") %>% 
      html_text2(),
    price_kg = .x %>% 
      html_element(".m-offer-tile__basic-price") %>% 
      html_text2(),
    unit = .x %>% 
      html_element(".m-offer-tile__quantity") %>% 
      html_text2())) %>% 
  mutate(date = Sys.Date(),
         location = "Цялата страна",
         type = "Напитки",
         source = "Kaufland", .before = product) %>% 
  mutate(price = str_replace(price, ",", "."), price_old = str_replace(price_old, ",", ".")) %>% 
  mutate(price = parse_number(price), price_old = parse_number(price_old)) %>% 
  distinct()

# razni1 <- razni1 %>% 
#   html_elements(".m-offer-tile--mobile") %>%
#   map_dfr(~ tibble(
#     product = .x %>% 
#       html_element(".m-offer-tile__subtitle") %>% 
#       html_text2(), 
#     subproduct = .x %>% 
#       html_element(".m-offer-tile__title") %>% 
#       html_text2(), 
#     price = .x %>% 
#       html_element(".a-pricetag__price") %>% 
#       html_text2(),
#     price_old = .x %>% 
#       html_element(".a-pricetag__line-through") %>% 
#       html_text2(),
#     discount = .x %>% 
#       html_element(".a-pricetag__discount") %>% 
#       html_text2(),
#     price_kg = .x %>% 
#       html_element(".m-offer-tile__basic-price") %>% 
#       html_text2(),
#     unit = .x %>% 
#       html_element(".m-offer-tile__quantity") %>% 
#       html_text2())) %>% 
#   mutate(date = Sys.Date(),
#          location = "Цялата страна",
#          type = "Разни",
#          source = "Kaufland", .before = product) %>% 
#   mutate(price = str_replace(price, ",", "."), price_old = str_replace(price_old, ",", ".")) %>% 
#   mutate(price = parse_number(price), price_old = parse_number(price_old)) %>% 
#   distinct()
# 
# razni2 <- razni2 %>% 
#   html_elements(".m-offer-tile--mobile") %>%
#   map_dfr(~ tibble(
#     product = .x %>% 
#       html_element(".m-offer-tile__subtitle") %>% 
#       html_text2(), 
#     subproduct = .x %>% 
#       html_element(".m-offer-tile__title") %>% 
#       html_text2(), 
#     price = .x %>% 
#       html_element(".a-pricetag__price") %>% 
#       html_text2(),
#     price_old = .x %>% 
#       html_element(".a-pricetag__line-through") %>% 
#       html_text2(),
#     discount = .x %>% 
#       html_element(".a-pricetag__discount") %>% 
#       html_text2(),
#     price_kg = .x %>% 
#       html_element(".m-offer-tile__basic-price") %>% 
#       html_text2(),
#     unit = .x %>% 
#       html_element(".m-offer-tile__quantity") %>% 
#       html_text2())) %>% 
#   mutate(date = Sys.Date(),
#          location = "Цялата страна",
#          type = "Разни",
#          source = "Kaufland", .before = product) %>% 
#   mutate(price = str_replace(price, ",", "."), price_old = str_replace(price_old, ",", ".")) %>% 
#   mutate(price = parse_number(price), price_old = parse_number(price_old)) %>% 
#   distinct()
# 
# razni3 <- razni3 %>% 
#   html_elements(".m-offer-tile--mobile") %>%
#   map_dfr(~ tibble(
#     product = .x %>% 
#       html_element(".m-offer-tile__subtitle") %>% 
#       html_text2(), 
#     subproduct = .x %>% 
#       html_element(".m-offer-tile__title") %>% 
#       html_text2(), 
#     price = .x %>% 
#       html_element(".a-pricetag__price") %>% 
#       html_text2(),
#     price_old = .x %>% 
#       html_element(".a-pricetag__line-through") %>% 
#       html_text2(),
#     discount = .x %>% 
#       html_element(".a-pricetag__discount") %>% 
#       html_text2(),
#     price_kg = .x %>% 
#       html_element(".m-offer-tile__basic-price") %>% 
#       html_text2(),
#     unit = .x %>% 
#       html_element(".m-offer-tile__quantity") %>% 
#       html_text2())) %>% 
#   mutate(date = Sys.Date(),
#          location = "Цялата страна",
#          type = "Разни",
#          source = "Kaufland", .before = product) %>% 
#   mutate(price = str_replace(price, ",", "."), price_old = str_replace(price_old, ",", ".")) %>% 
#   mutate(price = parse_number(price), price_old = parse_number(price_old)) %>% 
#   distinct()
# 
# razni4 <- razni4 %>% 
#   html_elements(".m-offer-tile--mobile") %>%
#   map_dfr(~ tibble(
#     product = .x %>% 
#       html_element(".m-offer-tile__subtitle") %>% 
#       html_text2(), 
#     subproduct = .x %>% 
#       html_element(".m-offer-tile__title") %>% 
#       html_text2(), 
#     price = .x %>% 
#       html_element(".a-pricetag__price") %>% 
#       html_text2(),
#     price_old = .x %>% 
#       html_element(".a-pricetag__line-through") %>% 
#       html_text2(),
#     discount = .x %>% 
#       html_element(".a-pricetag__discount") %>% 
#       html_text2(),
#     price_kg = .x %>% 
#       html_element(".m-offer-tile__basic-price") %>% 
#       html_text2(),
#     unit = .x %>% 
#       html_element(".m-offer-tile__quantity") %>% 
#       html_text2())) %>% 
#   mutate(date = Sys.Date(),
#          location = "Цялата страна",
#          type = "Месо и месни продукти",
#          source = "Kaufland", .before = product) %>% 
#   mutate(price = str_replace(price, ",", "."), price_old = str_replace(price_old, ",", ".")) %>% 
#   mutate(price = parse_number(price), price_old = parse_number(price_old)) %>% 
#   distinct()
#--------------------------------------------------------------------------------------
kaufland <- bind_rows(bakaliq_df, coffee_df, drinks_df, fish_df, frozen_df, fruits_veg_df, 
                      konservi_df, meat_df, milk_df)

kaufland <- kaufland %>% 
  select(date, location, source, type, product, 
         subproduct, unit, price, price_old, discount, price_kg) %>% 
  mutate(across(!c(date, price, price_old), as.character)) %>% 
  mutate(across(price:price_old, as.numeric)) %>% 
  unite("product", product:subproduct, sep = ", ", remove = T) %>%
  mutate(product = str_remove(product, ", NA$"))

glimpse(kaufland)
write_csv(kaufland, "shiny/scraping/kaufland.csv")
