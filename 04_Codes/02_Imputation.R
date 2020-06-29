# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  MSD CHC 2019
# Purpose:      Imputation
# programmer:   Zhe Liu
# Date:         2020-06-10
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Imputing inside existing provinces ----
msd.imp.raw <- msd.raw %>% 
  filter(!(province %in% c("福建")))

# quarterly date continuity
date.continuity <- msd.imp.raw %>% 
  distinct(province, city, pchc, market, year, date) %>% 
  count(province, city, pchc, market, year) %>% 
  setDT() %>% 
  dcast(province + city + pchc + market ~ year, value.var = "n", fill = 0) %>% 
  mutate(cnt_min = pmin(`2018`, `2019`),
         cnt_max = pmax(`2018`, `2019`))

# city molecule yearly growth
city.growth <- date.continuity %>% 
  filter(cnt_min >= 2) %>% 
  inner_join(msd.imp.raw, by = c("province", "city", "pchc", "market")) %>% 
  group_by(province, city, year, market, atc3, molecule_desc) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  setDT() %>% 
  melt(id.vars = c("province", "city", "year", "market", "atc3", "molecule_desc"), 
       measure.vars = c("units", "sales"), variable.name = "cate", value.name = "value") %>% 
  unite("type", cate, year) %>% 
  setDT() %>% 
  dcast(province + city + market + atc3 + molecule_desc ~ type, value.var = "value", fill = 0) %>% 
  mutate(sales_growth = sales_2019 / sales_2018,
         units_growth = units_2019 / units_2018,
         sales_growth = if_else(is.na(sales_growth) | sales_growth < 0.1 | sales_growth > 10, 1, sales_growth),
         units_growth = if_else(is.na(units_growth) | units_growth < 0.1 | units_growth > 10, 1, units_growth)) %>% 
  select("province", "city", "market", "atc3", "molecule_desc", "sales_growth", "units_growth")

# imputing
imputing.data <- date.continuity %>% 
  filter(cnt_max >= 2) %>% 
  select(province, city, pchc, market) %>% 
  left_join(msd.imp.raw, by = c("province", "city", "pchc", "market")) %>% 
  mutate(date = stri_sub(date, 5, 6)) %>% 
  setDT() %>% 
  melt(id.vars = c("province", "city", "pchc", "year", "date", 
                   "market", "atc3", "molecule_desc", "packid"),
       measure.vars = c("units", "sales"), variable.name = "cate", value.name = "value") %>% 
  unite("type", cate, year) %>% 
  dcast(province + city + pchc + market + atc3 + molecule_desc + packid + date ~ type,
        value.var = "value", fill = -1) %>%
  left_join(city.growth, by = c("province", "city", "market", "atc3", "molecule_desc")) %>% 
  mutate(sales_growth = if_else(is.na(sales_growth), 1, sales_growth),
         units_growth = if_else(is.na(units_growth), 1, units_growth),
         flag_2018 = if_else(`sales_2018` == -1, 1, 0),
         flag_2019 = if_else(`sales_2019` == -1, 1, 0),
         sales_2018 = if_else(flag_2018 == 1, `sales_2019` / sales_growth, `sales_2018`),
         sales_2019 = if_else(flag_2019 == 1, `sales_2018` * sales_growth, `sales_2019`),
         units_2018 = if_else(flag_2018 == 1, `units_2019` / units_growth, `units_2018`),
         units_2019 = if_else(flag_2019 == 1, `units_2018` * units_growth, `units_2019`)) %>% 
  setDT() %>% 
  melt(id.vars = c("province", "city", "pchc", "market", "atc3", 
                   "molecule_desc", "packid", "date"), 
       measure.vars = c("flag_2018", "flag_2019", "sales_2018", 
                        "sales_2019", "units_2018", "units_2019"), 
       variable.name = "cate", value.name = "value") %>% 
  separate(cate, c("type", "year"), sep = "_") %>% 
  dcast(province + city + pchc + year + date + market + atc3 + molecule_desc + packid ~ type, 
        value.var = "value") %>% 
  rename("units_imp" = "units",
         "sales_imp" = "sales") %>% 
  mutate(quarter = ifelse(date %in% c("01", "02", "03"), "Q1", 
                          ifelse(date %in% c("04", "05", "06"), "Q2", 
                                 ifelse(date %in% c("07", "08", "09"), "Q3", 
                                        ifelse(date %in% c("10", "11", "12"), "Q4", 
                                               NA_character_)))),
         date = stri_paste(year, date),
         quarter = stri_paste(year, quarter)) %>% 
  select(year, date, quarter, province, city, pchc, market, atc3, molecule_desc, 
         packid, units_imp, sales_imp, flag)

# joint
imputed.data <- msd.imp.raw %>% 
  full_join(imputing.data, by = c("year", "date", "quarter", "province", "city", "pchc", 
                                  "market", "atc3", "molecule_desc", "packid")) %>% 
  mutate(units = if_else(is.na(units), units_imp, units),
         sales = if_else(is.na(sales), sales_imp, sales),
         flag1 = if_else(is.na(flag), 0, flag)) %>% 
  select(year, date, quarter, province, city, pchc, market, atc3, molecule_desc, 
         packid, units, sales, flag1)

msd.in.imp <- msd.raw %>% 
  mutate(flag1 = 0) %>% 
  filter(province %in% c("福建")) %>% 
  bind_rows(imputed.data)


##---- Imputing outside existing provinces ----
# model data
model.data <- servier.raw %>% 
  group_by(year, date, quarter, province, city, pchc, market, atc3, molecule_desc, packid) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

# sample data
sample.data <- model.data %>% 
  filter(province %in% c("北京", "福建", "江苏", "安徽", "浙江")) %>% 
  mutate(flag = if_else(province %in% c("福建"), 0, 1))

# summarising pack ID by PCHC
sample.pchc <- sample.data %>% 
  group_by(province, city, pchc, date, flag) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

# model
model.set <- setDT(sample.pchc) %>% 
  dcast(province + city + pchc + flag ~ date, value.var = "sales", fill = 0)

train.set <- model.set[flag == 1, ]
test.set <- model.set[flag == 0, ]

train.set.tmp <- train.set[, -c("province", "city", "pchc")]
test.set.tmp <- test.set[, -c("province", "city", "pchc")]

knn.model <- kknn(flag ~ ., train = train.set.tmp, test = test.set.tmp, k = 3, scale = TRUE)

# weightage extraction
model.indice <- as.data.frame(knn.model$C) %>% 
  lapply(function(x) {
    train.set$pchc[x]
  }) %>% 
  as.data.frame(col.names = c("pchc_1", "pchc_2", "pchc_3")) %>% 
  bind_cols(test.set[, c("province", "city", "pchc")]) %>% 
  setDT() %>% 
  melt(id.vars = c("province", "city", "pchc"), variable.name = "knn_level", value.name = "knn_pchc")

model.weight <- as.data.frame(knn.model$D) %>% 
  lapply(function(x) {
    1 / (x+1)
  }) %>% 
  as.data.frame(col.names = c("pchc_1", "pchc_2", "pchc_3")) %>% 
  mutate(weight_sum = pchc_1 + pchc_2 + pchc_3,
         pchc_1 = pchc_1 / weight_sum,
         pchc_2 = pchc_2 / weight_sum,
         pchc_3 = pchc_3 / weight_sum) %>% 
  bind_cols(test.set[, c("province", "city", "pchc")]) %>% 
  select(-weight_sum) %>% 
  setDT() %>% 
  melt(id.vars = c("province", "city", "pchc"), variable.name = "knn_level", value.name = "knn_weight")

# ah, bj, js - 1910~1912
# ah.bj.js <- total.raw %>% 
#   filter(province == "安徽") %>% 
#   bind_rows(total.in.imp) %>% 
#   filter(province %in% c("安徽", "北京", "江苏"), date %in% c("201910","201911","201912"))

# sample growth
sample.sales <- sample.data %>% 
  filter(flag == 1, year %in% c("2018","2019")) %>% 
  # mutate(year = stri_sub(date, 1, 4)) %>% 
  # bind_rows(ah.bj.js) %>% 
  group_by(knn_pchc = pchc, molecule_desc, year) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

sample.growth <- model.indice %>% 
  left_join(model.weight, by = c("province", "city", "pchc", "knn_level")) %>% 
  inner_join(sample.sales, by = c("knn_pchc")) %>% 
  group_by(pchc, molecule_desc, year) %>% 
  summarise(sales = sum(sales * knn_weight, na.rm = TRUE)) %>% 
  ungroup() %>% 
  setDT() %>% 
  dcast(pchc + molecule_desc ~ year, value.var = "sales", fill = 0) %>% 
  mutate(growth = `2019` / `2018`,
         growth = if_else(is.na(growth), 1, growth),
         growth = if_else(growth > 3, 3, growth),
         growth = if_else(growth > quantile(growth, 0.9),
                          mean(growth[growth >= quantile(growth, 0.25) & growth <= quantile(growth, 0.75)]),
                          growth)) %>% 
  select("pchc", "molecule_desc", "growth")

# joint
msd.out.imp <- msd.in.imp %>% 
  filter(year %in% c("2018"),
         province %in% c("福建")) %>% 
  left_join(sample.growth, by = c("pchc", "molecule_desc")) %>% 
  mutate(units = units * growth,
         sales = sales * growth,
         date = gsub("2018", "2019", date),
         quarter = gsub("2018", "2019", quarter),
         year = "2019",
         flag2 = 1) %>% 
  select(-growth)


##---- Bind ----
msd.imp <- msd.in.imp %>% 
  mutate(flag2 = 0) %>% 
  bind_rows(msd.out.imp)

write_feather(msd.imp, "03_Outputs/02_MSD_CHC_Imp.feather")








