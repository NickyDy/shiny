library(tidyverse)
library(arrow)

sopharmacy <- read_parquet("shiny/scraping_med/sopharmacy.parquet")
bg366 <- read_parquet("shiny/scraping_med/bg366.parquet")
framar <- read_parquet("shiny/scraping_med/framar.parquet")
remedium <- read_parquet("shiny/scraping_med/remedium.parquet")
gpharm <- read_parquet("shiny/scraping_med/gpharm.parquet")
salvia <- read_parquet("shiny/scraping_med/salvia.parquet")
ozone <- read_parquet("shiny/scraping_med/ozone.parquet")
lilly <- read_parquet("shiny/scraping_med/lilly.parquet")
epharm <- read_parquet("shiny/scraping_med/epharm.parquet")
mypharmacy <- read_parquet("shiny/scraping_med/mypharmacy.parquet")
afya <- read_parquet("shiny/scraping_med/afya.parquet")
marvi <- read_parquet("shiny/scraping_med/marvi.parquet")
promahon <- read_parquet("shiny/scraping_med/promahon.parquet")
vitosha <- read_parquet("shiny/scraping_med/vitosha.parquet")

files <- list.files(pattern = ".parquet")
pharm <- map_dfr(files, read_parquet)

pharm <- bind_rows(sopharmacy, bg366, framar, remedium, gpharm,
                   salvia, ozone, lilly, epharm, mypharmacy, afya,
                   marvi, promahon, vitosha)  %>% 
  filter(price > 0) %>%
  mutate(date = format(date, "%V"), 
         price = round(price, 2), 
         across(c(2:3), as.factor)) %>% 
  select(date, source, type, product, price) %>% 
  drop_na(price) %>% 
  distinct() %>% 
  arrange(price)

write_parquet(pharm, "shiny/bgprices/pharm_week31.parquet")

pharm <- read_parquet("shiny/bgprices/pharm_week14.parquet")

glimpse(pharm)
pharm %>% count(type) %>% view
pharm %>% map_dfr(~ sum(is.na(.)))
