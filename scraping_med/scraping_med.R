library(tidyverse)

sopharmacy <- read_rds("shiny/scraping_med/sopharmacy.rds")
bg366 <- read_rds("shiny/scraping_med/bg366.rds")
framar <- read_rds("shiny/scraping_med/framar.rds")
remedium <- read_rds("shiny/scraping_med/remedium.rds")
gpharm <- read_rds("shiny/scraping_med/gpharm.rds")
salvia <- read_rds("shiny/scraping_med/salvia.rds")
ozone <- read_rds("shiny/scraping_med/ozone.rds")
lilly <- read_rds("shiny/scraping_med/lilly.rds")
epharm <- read_rds("shiny/scraping_med/epharm.rds")
mypharmacy <- read_rds("shiny/scraping_med/mypharmacy.rds")
afya <- read_rds("shiny/scraping_med/afya.rds")
marvi <- read_rds("shiny/scraping_med/marvi.rds")
promahon <- read_rds("shiny/scraping_med/promahon.rds")
vitosha <- read_rds("shiny/scraping_med/vitosha.rds")

files <- list.files(pattern = ".rds")
pharm <- map_dfr(files, read_rds)

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

write_rds(pharm, "shiny/bgprices/pharm_week31.rds")

pharm <- read_rds("shiny/bgprices/pharm_week31.rds")

glimpse(pharm)
pharm %>% count(type) %>% view
pharm %>% map_dfr(~ sum(is.na(.)))
