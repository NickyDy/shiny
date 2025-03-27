library(tidyverse)
library(rvest)

#------------------------------------------------------
kauf <- read_html("https://www.kaufland.bg/aktualni-predlozheniya/oferti.html?kloffer-category=02_%D0%9F%D0%BB%D0%BE%D0%B4%D0%BE%D0%B2%D0%B5_%D0%B8_%D0%B7%D0%B5%D0%BB%D0%B5%D0%BD%D1%87%D1%83%D1%86%D0%B8")

kauf_df <- kauf %>% 
  html_elements(".k-product-tile") %>%
  map_dfr(~ tibble(
    type = .x %>% 
      html_element(".k-product-section__headline") %>% 
      html_text2(),
    product = .x %>% 
      html_element(".k-product-tile__title") %>% 
      html_text2(),
    subproduct = .x %>% 
      html_element(".k-product-tile__subtitle") %>% 
      html_text2(), 
    unit = .x %>% 
      html_element(".k-product-tile__unit-price") %>% 
      html_text2(),
    price = .x %>% 
      html_element(".k-price-tag__price") %>% 
      html_text2())) %>% 
  mutate(date = Sys.Date(),
         location = "Цялата страна",
         source = "Kaufland", .before = product) %>% 
  mutate(price = str_replace(price, ",", "."), 
         price = parse_number(price)) %>% 
  unite(., "product", product:subproduct, sep = "\n") %>% 
  distinct()
#--------------------------------------------------------
kauf_df <- kauf_df %>% select(date, location, source, type, product, unit, price)
glimpse(kauf_df)

kaufland <- read_rds("shiny/bgprices/kaufland.rds")

kaufland <- bind_rows(kauf_df, kaufland)

write_rds(kauf_df, "shiny/bgprices/kaufland.rds")
write_rds(kaufland, "shiny/bgprices/kaufland.rds")
