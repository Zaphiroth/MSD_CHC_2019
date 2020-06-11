# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  MSD CHC 2019
# Purpose:      Price
# programmer:   Zhe Liu
# Date:         2020-06-11
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Origin Price ----
price.origin <- msd.imp %>% 
  filter(units > 0) %>% 
  group_by(packid, year, quarter, province, city) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price = sales / units) %>% 
  select(-sales, -units)


##---- Mean price by year ----
price.year <- msd.imp %>% 
  filter(units > 0) %>% 
  group_by(packid, year, province, city) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_year = sales / units) %>% 
  select(-sales, -units)


##---- Mean price by city ----
price.city <- msd.imp %>% 
  filter(units > 0) %>% 
  group_by(packid, province, city) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_city = sales / units) %>% 
  select(-sales, -units)


##---- Mean price by province ----
price.province <- msd.imp %>% 
  filter(units > 0) %>% 
  group_by(packid, province) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_prov = sales / units) %>% 
  select(-sales, -units)


##---- Mean price by pack ID ----
price.pack <- msd.imp %>% 
  filter(units > 0) %>% 
  group_by(packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(price_pack = sales / units) %>% 
  select(-sales, -units)


##---- Add new price ----
msd.price <- universe.proj %>% 
  left_join(price.origin, by = c("province", "city", "year", "quarter", "packid")) %>% 
  left_join(price.year, by = c("province", "city", "year", "packid")) %>% 
  left_join(price.city, by = c("province", "city", "packid")) %>% 
  left_join(price.province, by = c("province", "packid")) %>% 
  left_join(price.pack, by = c("packid")) %>% 
  mutate(price = ifelse(is.na(price), price_year, price),
         price = ifelse(is.na(price), price_city, price),
         price = ifelse(is.na(price), price_prov, price),
         price = ifelse(is.na(price), price_pack, price)) %>% 
  mutate(units = sales / price) %>% 
  filter(price > 0, units > 0) %>% 
  select(year, quarter, province, city, market, atc3, 
         molecule_desc, packid, price, units, sales)

write_feather(msd.price, "03_Outputs/04_MSD_CHC_Price.feather")


