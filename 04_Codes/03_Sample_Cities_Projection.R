# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  MSD CHC 2019
# Purpose:      Sample Cities Projection
# programmer:   Zhe Liu
# Date:         2020-06-11
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin data ----
# PCHC
pchc.universe <- read.xlsx("02_Inputs/Universe_PCHCCode_20200507.xlsx", sheet = "PCHC")

hospital.universe <- pchc.universe %>% 
  group_by(pchc = PCHC_Code) %>% 
  summarise(province = first(na.omit(`省`)),
            city = first(na.omit(`地级市`)),
            district = first(na.omit(`区[县/县级市】`)),
            pop = first(na.omit(`人口`)),
            est = first(na.omit(`其中：西药药品收入（千元）`))) %>% 
  ungroup() %>% 
  filter(!is.na(est), !is.na(pop))

# Shanghai district
sh.seg.city <- servier.sh.raw %>% 
  distinct(pchc = PCHC_Code, city = `城市`, district = `区县`) %>% 
  bind_rows(hospital.universe) %>% 
  filter(city == "上海") %>% 
  group_by(pchc, city) %>% 
  summarise(district = first(na.omit(district))) %>% 
  ungroup() %>% 
  unite("seg_city", city, district, sep = "", na.rm = TRUE)

# segment
proj.segment <- read.xlsx("02_Inputs/seg_45cities.xlsx") %>% 
  mutate(seg_city = if_else(city == "上海", paste0(city, district), city)) %>% 
  select(seg_city, segment = seg_up)

# sampel PCHC
sample.pchc.list <- unique(msd.imp$pchc)

# universe PCHC
universe.pchc <- bind_rows(msd.imp, hospital.universe) %>% 
  group_by(pchc) %>% 
  summarise(province = first(province),
            city = first(city)) %>% 
  ungroup() %>% 
  select(province, city, pchc)

# sample range
sample.pick <- read_xlsx("02_Inputs/历史数据样本范围_13Cities.xlsx", sheet = 2)


##---- Projection ----
# quarter sales
msd.quarter <- msd.imp %>% 
  filter(year %in% c("2019")) %>% 
  group_by(year, quarter, province, city, pchc, market, atc3, molecule_desc, packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

# universe set
universe.set <- msd.quarter %>% 
  distinct(city, market, atc3, molecule_desc, packid) %>% 
  inner_join(universe.pchc, by = "city") %>% 
  left_join(sh.seg.city, by = "pchc") %>% 
  mutate(seg_city = if_else(is.na(seg_city), city, seg_city)) %>% 
  inner_join(proj.segment, by = "seg_city") %>% 
  merge(distinct(msd.quarter, year, quarter)) %>% 
  left_join(msd.quarter, by = c("year", "quarter", "province", "city", "pchc", 
                                "market", "atc3", "molecule_desc", "packid")) %>% 
  inner_join(hospital.universe[, c("pchc", "est")], by = "pchc") %>% 
  mutate(sample_label = if_else(pchc %in% sample.pick, 1, 0),
         sample_label = if_else(city == "上海", 1, sample_label))

# projection parameter
proj.parm <- data.table(universe.set[universe.set$sample_label == 1, ])[, {
  ux <- mean(est, na.rm = TRUE)
  uy <- mean(sales, na.rm = TRUE)
  slope <- uy / ux
  intercept <- 0
  predict_sales = est * slope
  spearman_cor <- cor(sales, predict_sales, method = "spearman")
  list(slope = slope, intercept = intercept, spearman_cor = spearman_cor)
}, by = list(quarter, segment, market, packid)]

# projection result
sample.proj <- universe.set %>% 
  left_join(proj.parm, by = c("quarter", "segment", "market", "packid")) %>% 
  mutate(predict_sales = est * slope,
         predict_sales = if_else(predict_sales < 0, 0, predict_sales),
         final_sales = if_else(is.na(sales), predict_sales, sales),
         flag = 0) %>% 
  filter(final_sales > 0) %>% 
  group_by(year, quarter, province, city, market, atc3, molecule_desc, packid, flag) %>% 
  summarise(sales = sum(final_sales, na.rm = TRUE)) %>% 
  ungroup()

write_feather(sample.proj, "03_Outputs/03_MSD_CHC_Sample_Cities_Projection.feather")







