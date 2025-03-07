library(tidyverse)
library(jsonlite)

beef_carc <- read_parquet("shiny/agri/beef_carc.parquet")
beef_live <- read_parquet("shiny/agri/beef_live.parquet")
piglets <- read_parquet("shiny/agri/piglets.parquet")
pigmeat_carc <- read_parquet("shiny/agri/pigmeat_carc.parquet")
pigmeat_cuts <- read_parquet("shiny/agri/pigmeat_cuts.parquet")
eggs <- read_parquet("shiny/agri/eggs.parquet")
poultry <- read_parquet("shiny/agri/poultry.parquet")
sheep_goat <- read_parquet("shiny/agri/sheep_goat.parquet")
raw_milk <- read_parquet("shiny/agri/raw_milk.parquet")
dairy <- read_parquet("shiny/agri/dairy.parquet")
fruit_veg <- read_parquet("shiny/agri/fruit_veg.parquet")
cereals <- read_parquet("shiny/agri/cereals.parquet")
oilseeds <- read_parquet("shiny/agri/oilseeds.parquet")
olive_oil <- read_parquet("shiny/agri/olive_oil.parquet")
wine <- read_parquet("shiny/agri/wine.parquet")

wine %>% map_dfr(~ sum(is.na(.)))

beef_carc_new <- fromJSON("https://www.ec.europa.eu/agrifood/api/beef/prices?&years=2025") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date), 
         price = parse_number(price)) %>% 
  select(date = begin_date, state = member_state_name, category, product = product_code, price)

beef_live_new <- fromJSON("https://www.ec.europa.eu/agrifood/api/liveAnimal/prices?years=2025") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = parse_number(price)) %>% 
  select(date = begin_date, state = member_state_name, category, 
         unit, price_eur = price)

piglets_new <- fromJSON("https://www.ec.europa.eu/agrifood/api/pigmeat/prices?beginDate=01/01/2024&endDate=31/12/2025") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = str_remove(price, "€"), 
         price = str_replace(price, ",", "."),
         price = as.numeric(price)) %>% 
  filter(pig_class == "Piglet") %>%
  select(date = begin_date, state = member_state_name,
         price_100kg_eur = price)

pigmeat_carc_new <- fromJSON("https://www.ec.europa.eu/agrifood/api/pigmeat/prices?beginDate=01/01/2024&endDate=31/12/2025") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = str_remove(price, "€"), 
         price = str_replace(price, ",", "."),
         price = as.numeric(price)) %>% 
  filter(!pig_class == "Piglet") %>%
  select(date = begin_date, state = member_state_name, product = pig_class,
         price_100kg_eur = price)

pigmeat_cuts_new <- fromJSON("https://www.ec.europa.eu/agrifood/api/pigmeat/cuts/prices?beginDate=01/01/2024&endDate=31/12/2025") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = str_remove(price, "€"), 
         price = str_replace(price, ",", "."),
         price = as.numeric(price)) %>% 
  select(date = begin_date, state = member_state_name, category, price_type,
         price_100kg_eur = price)

eggs_new <- fromJSON("https://www.ec.europa.eu/agrifood/api/poultry/egg/prices?beginDate=01/01/2024&endDate=31/12/2025") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = parse_number(price)) %>% 
  select(date = begin_date, state = member_state_name, farming_method,
         price_100kg_eur = price)

poultry_new <- fromJSON("https://www.ec.europa.eu/agrifood/api/poultry/prices?beginDate=01/01/2024&endDate=31/12/2025") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = str_remove(price, "€"), 
         price = str_replace(price, ",", "."),
         price = as.numeric(price)) %>% 
  select(date = begin_date, state = member_state_name, product = product_name,
         price_100kg_eur = price)

sheep_goat_new <- fromJSON("https://www.ec.europa.eu/agrifood/api/sheepAndGoat/prices?beginDate=01/01/2024&endDate=31/12/2025") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date), 
         price = parse_number(price)) %>% 
  select(date = begin_date, state = member_state_name, category,
         price_100kg_eur = price)

raw_milk_new <- fromJSON("https://www.ec.europa.eu/agrifood/api/rawMilk/prices?beginDate=01/01/2024&endDate=31/12/2025") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = parse_number(price)) %>% 
  select(date = begin_date, state = member_state_name, product,
         price_100kg_eur = price)

dairy_new <- fromJSON("https://www.ec.europa.eu/agrifood/api/dairy/prices?years=2025") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = parse_number(price)) %>% 
  select(date = begin_date, state = member_state_name, product,
         price_100kg_eur = price)

fruit_veg_new <- fromJSON("https://www.ec.europa.eu/agrifood/api/fruitAndVegetable/prices?beginDate=01/01/2024&endDate=31/12/2025") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = parse_number(price)) %>% 
  select(date = begin_date, state = member_state_name, product, variety, 
         product_description = description, price_100_kg_eur = price)

cereals_new <- fromJSON("https://www.ec.europa.eu/agrifood/api/cereal/prices?beginDate=01/01/2024&endDate=31/12/2024") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = str_remove(price, "€"), 
         price = str_replace(price, ",", "."),
         price = as.numeric(price)) %>% 
  select(date = begin_date, state = member_state_name, market_name,
         stage_name, product = product_name, price_tonne_eur = price)

oilseeds_new <- fromJSON("https://www.ec.europa.eu/agrifood/api/oilseeds/prices?beginDate=01/01/2024&endDate=31/12/2025") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = parse_number(price)) %>% 
  select(date = begin_date, state = member_state_name, market_stage, market,
         product_type, product, price_eur = price)

olive_oil_new <- fromJSON("https://www.ec.europa.eu/agrifood/api/oliveOil/prices?beginDate=01/01/2024&endDate=31/12/2025") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = parse_number(price)) %>% 
  select(date = begin_date, state = member_state_name, market,
         product, price_100kg_eur = price)

wine_new <- fromJSON("https://www.ec.europa.eu/agrifood/api/wine/prices?beginDate=01/01/2024&endDate=31/12/2025") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = str_remove(price, "€"), 
         price = str_replace(price, ",", "."),
         price = as.numeric(price)) %>% 
  select(date = begin_date, state = member_state_name, 
         wine_description = description, eur_price_per_hl = price)
#---------------------------------------------------------
beef_carc <- bind_rows(beef_carc, beef_carc_new) %>% 
  mutate(product = str_replace_all(product, " ", "")) %>% distinct()
beef_live <- bind_rows(beef_live, beef_live_new) %>% distinct()
piglets <- bind_rows(piglets, piglets_new) %>% distinct()
pigmeat_carc <- bind_rows(pigmeat_carc, pigmeat_carc_new) %>% distinct()
pigmeat_cuts <- bind_rows(pigmeat_cuts, pigmeat_cuts_new) %>% distinct() %>% drop_na()
eggs <- bind_rows(eggs, eggs_new) %>% distinct()
poultry <- bind_rows(poultry, poultry_new) %>% distinct() %>% drop_na()
sheep_goat <- bind_rows(sheep_goat, sheep_goat_new) %>% distinct()
raw_milk <- bind_rows(raw_milk, raw_milk_new) %>% distinct()
dairy <- bind_rows(dairy, dairy_new) %>% distinct()
fruit_veg <- bind_rows(fruit_veg, fruit_veg_new) %>% distinct()
cereals <- bind_rows(cereals, cereals_new) %>% distinct() %>% drop_na()
oilseeds <- bind_rows(oilseeds, oilseeds_new) %>% distinct()
olive_oil <- bind_rows(olive_oil, olive_oil_new) %>% distinct()
wine <- bind_rows(wine, wine_new) %>% distinct()
#---------------------------------------------------------
write_rds(beef_carc, "shiny/agri/beef_carc.rds")
write_rds(beef_live, "shiny/agri/beef_live.rds")
write_rds(piglets, "shiny/agri/piglets.rds")
write_rds(pigmeat_carc, "shiny/agri/pigmeat_carc.rds")
write_rds(pigmeat_cuts, "shiny/agri/pigmeat_cuts.rds")
write_rds(eggs, "shiny/agri/eggs.rds")
write_rds(poultry, "shiny/agri/poultry.rds")
write_rds(sheep_goat, "shiny/agri/sheep_goat.rds")
write_rds(raw_milk, "shiny/agri/raw_milk.rds")
write_rds(dairy, "shiny/agri/dairy.rds")
write_rds(fruit_veg, "shiny/agri/fruit_veg.rds")
write_rds(cereals, "shiny/agri/cereals.rds")
write_rds(oilseeds, "shiny/agri/oilseeds.rds")
write_rds(olive_oil, "shiny/agri/olive_oil.rds")
write_rds(wine, "shiny/agri/wine.rds")

eggs %>% 
  filter(date > "2023-01-01") %>% 
  group_by(state, farming_method) %>% 
  summarise(m = mean(price_kg_bgn)) %>% 
  mutate(farming_method = fct_reorder(farming_method, m)) %>% 
  ggplot(aes(m, farming_method, fill = farming_method)) +
  geom_col(show.legend = F) +
  geom_text(aes(label = round(m, 1)), 
            position = position_dodge(width = 1), hjust = -0.1, size = 4.5) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.2))) +
  labs(y = NULL, x = "Цена (лв/кг)") +
  theme(text = element_text(size = 16)) +
  facet_wrap(vars(state))

pigmeat_carc %>% 
  filter(date > "2023-12-31", product == "E") %>% 
  mutate(col = price_kg_bgn <= mean(b_max$bul_mean)) %>%
  ggplot(aes(date, price_kg_bgn, color = col, group = state)) +
  geom_line(linewidth = 1) +
  labs(x = NULL, y = "Цена (лв/кг)") +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  facet_wrap(vars(state))
