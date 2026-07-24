library(tidyverse)
library(jsonlite)
library(nanoparquet)

glimpse(cereals)

beef_carc <- fromJSON("https://www.ec.europa.eu/agrifood/api/beef/prices?&beginDate=01/01/2020&endDate=31/12/2026") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date), 
         price = parse_number(price)) %>% 
  select(date = begin_date, state = member_state_name, category, product = product_code, price)

beef_live <- fromJSON("https://www.ec.europa.eu/agrifood/api/liveAnimal/prices?beginDate=01/01/2020&endDate=31/12/2026") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = parse_number(price)) %>% 
  select(date = begin_date, state = member_state_name, category, 
         unit, price_eur = price)

piglets <- fromJSON("https://www.ec.europa.eu/agrifood/api/pigmeat/prices?beginDate=01/01/2020&endDate=31/12/2026") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = str_remove(price, "€"), 
         price = str_replace(price, ",", "."),
         price = as.numeric(price)) %>% 
  filter(pig_class == "Piglet") %>%
  select(date = begin_date, state = member_state_name,
         price_100kg_eur = price)

pigmeat_carc <- fromJSON("https://www.ec.europa.eu/agrifood/api/pigmeat/prices?beginDate=01/01/2020&endDate=31/12/2026") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = str_remove(price, "€"), 
         price = str_replace(price, ",", "."),
         price = as.numeric(price)) %>% 
  filter(!pig_class == "Piglet") %>%
  select(date = begin_date, state = member_state_name, product = pig_class,
         price_100kg_eur = price)

pigmeat_cuts <- fromJSON("https://www.ec.europa.eu/agrifood/api/pigmeat/cuts/prices?beginDate=01/01/2020&endDate=31/12/2026") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = str_remove(price, "€"), 
         price = str_replace(price, ",", "."),
         price = as.numeric(price)) %>% 
  select(date = begin_date, state = member_state_name, category, price_type,
         price_100kg_eur = price)

eggs <- fromJSON("https://www.ec.europa.eu/agrifood/api/poultry/egg/prices?beginDate=01/01/2020&endDate=31/12/2026") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = parse_number(price)) %>% 
  select(date = begin_date, state = member_state_name, farming_method,
         price_100kg_eur = price)

poultry <- fromJSON("https://www.ec.europa.eu/agrifood/api/poultry/prices?beginDate=01/01/2020&endDate=31/12/2026") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = str_remove(price, "€"), 
         price = str_replace(price, ",", "."),
         price = as.numeric(price)) %>% 
  select(date = begin_date, state = member_state_name, product = product_name,
         price_100kg_eur = price)

sheep_goat <- fromJSON("https://www.ec.europa.eu/agrifood/api/sheepAndGoat/prices?beginDate=01/01/2020&endDate=31/12/2026") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date), 
         price = parse_number(price)) %>% 
  select(date = begin_date, state = member_state_name, category,
         price_100kg_eur = price)

raw_milk <- fromJSON("https://www.ec.europa.eu/agrifood/api/rawMilk/prices?beginDate=01/01/2020&endDate=31/12/2026") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = parse_number(price)) %>% 
  select(date = begin_date, state = member_state_name, product,
         price_100kg_eur = price)

dairy <- fromJSON("https://www.ec.europa.eu/agrifood/api/dairy/prices?beginDate=01/01/2020&endDate=31/12/2026") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = parse_number(price)) %>% 
  select(date = begin_date, state = member_state_name, product,
         price_100kg_eur = price)

fruit_veg <- fromJSON("https://www.ec.europa.eu/agrifood/api/fruitAndVegetable/pricesSupplyChain?beginDate=01/01/2020&endDate=31/12/2026") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = parse_number(price)) %>% 
  select(date = begin_date, state = member_state_name, product = product_stage, 
         variety, product_description = market, price_100_kg_eur = price)

cereals <- fromJSON("https://www.ec.europa.eu/agrifood/api/cereal/prices?beginDate=01/01/2020&endDate=31/12/2026") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = str_remove(price, "€"), 
         price = str_replace(price, ",", "."),
         price = as.numeric(price)) %>% 
  select(date = begin_date, state = member_state_name, market_name,
         stage_name, product = product_name, price_tonne_eur = price)

oilseeds <- fromJSON("https://www.ec.europa.eu/agrifood/api/oilseeds/prices?beginDate=01/01/2020&endDate=31/12/2026") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = parse_number(price)) %>% 
  select(date = begin_date, state = member_state_name, market_stage, market,
         product_type, product, price_eur = price)

olive_oil <- fromJSON("https://www.ec.europa.eu/agrifood/api/oliveOil/prices?beginDate=01/01/2020&endDate=31/12/2026") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = parse_number(price)) %>% 
  select(date = begin_date, state = member_state_name, market,
         product, price_100kg_eur = price)

wine <- fromJSON("https://www.ec.europa.eu/agrifood/api/wine/prices?beginDate=01/01/2020&endDate=31/12/2026") %>% 
  janitor::clean_names() %>% 
  mutate(begin_date = dmy(begin_date),
         price = str_remove(price, "€"), 
         price = str_replace(price, ",", "."),
         price = as.numeric(price)) %>% 
  select(date = begin_date, state = member_state_name, 
         wine_description = description, eur_price_per_hl = price)
#-----------------------------------------------------------------
write_parquet(beef_carc, "shiny/agri/beef_carc.parquet")
write_parquet(beef_live, "shiny/agri/beef_live.parquet")
write_parquet(piglets, "shiny/agri/piglets.parquet")
write_parquet(pigmeat_carc, "shiny/agri/pigmeat_carc.parquet")
write_parquet(pigmeat_cuts, "shiny/agri/pigmeat_cuts.parquet")
write_parquet(eggs, "shiny/agri/eggs.parquet")
write_parquet(poultry, "shiny/agri/poultry.parquet")
write_parquet(sheep_goat, "shiny/agri/sheep_goat.parquet")
write_parquet(raw_milk, "shiny/agri/raw_milk.parquet")
write_parquet(dairy, "shiny/agri/dairy.parquet")
write_parquet(fruit_veg, "shiny/agri/fruit_veg.parquet")
write_parquet(cereals, "shiny/agri/cereals.parquet")
write_parquet(oilseeds, "shiny/agri/oilseeds.parquet")
write_parquet(olive_oil, "shiny/agri/olive_oil.parquet")
write_parquet(wine, "shiny/agri/wine.parquet")
