# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  MSD CHC 2019
# Purpose:      Non-sample Cities Projection
# programmer:   Zhe Liu
# Date:         2020-06-11
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin data ----
# target city result
servier.chc.raw <- read.xlsx("02_Inputs/CHC_MAX_16Q419Q4_0317.xlsx")

msd.target.city <- servier.chc %>% 
  filter(Channel == "CHC",
         stri_sub(Date, 1, 4) == "2019", 
         # City %in% target.city, 
         MKT == "OAD", 
         Molecule_Desc %in% market.def$Molecule) %>% 
  mutate(Pack_ID = stri_pad_left(Pack_ID, 7, 0),
         Pack_ID = if_else(Prod_Desc == "JANUVIA" & 
                             City %in% c("南京", "苏州"), 
                           "4268602", 
                           Pack_ID),
         Pck_Desc = if_else(Prod_Desc == "JANUVIA" & 
                              City %in% c("南京", "苏州"), 
                            "TAB FLM CTD 100MG    7", 
                            Pck_Desc)) %>% 
  select(Pack_ID, Channel, Province, City, Date, ATC3, MKT, Molecule_Desc, 
         Prod_Desc, Pck_Desc, Corp_Desc, Sales, Units, DosageUnits)

# city tier
city.tier <- read.xlsx("02_Inputs/pchc_city_tier.xlsx") %>% 
  group_by(city) %>% 
  mutate(tier = ifelse(is.na(city_tier), first(na.omit(city_tier)), city_tier)) %>% 
  ungroup() %>% 
  mutate(tier = ifelse(is.na(tier), 5, tier)) %>% 
  distinct(city, tier)

# universe PCHC
universe.city <- pchc.universe %>% 
  group_by(pchc = PCHC_Code) %>% 
  summarise(province = first(na.omit(`省`)),
            city = first(na.omit(`地级市`)),
            district = first(na.omit(`区[县/县级市】`)),
            pop = first(na.omit(`人口`)),
            est = first(na.omit(`其中：西药药品收入（千元）`))) %>% 
  ungroup() %>% 
  filter(!is.na(est), !is.na(pop), !(city %in% unique(msd.target.city$City))) %>% 
  left_join(city.tier, by = "city") %>% 
  mutate(tier = ifelse(is.na(tier), 1, tier)) %>% 
  group_by(province, city, tier) %>% 
  summarise(pop = sum(pop, na.rm = TRUE),
            est = sum(est, na.rm = TRUE)) %>% 
  ungroup()

# universe district
proj.market <- msd.imp %>% 
  filter(!(city %in% unique(msd.target.city$City)),
         year == "2019") %>% 
  left_join(city.tier, by = "city") %>% 
  mutate(tier = ifelse(is.na(tier), 1, tier)) %>% 
  group_by(year, quarter, province, city, tier, market, atc3, molecule_desc, packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  inner_join(universe.city, by = c("province", "city", "tier"))


#---- Projection ----
proj.region.list <- vector("list", length = nrow(universe.city))

pb <- txtProgressBar(min = 1, max = nrow(universe.city), initial = 1) 

for (i in 1:nrow(universe.city)) {
  setTxtProgressBar(pb, i)
  
  proj.region.list[[i]] <- universe.city[i, ] %>% 
    left_join(proj.market, by = c("tier")) %>% 
    mutate(est_gap = abs(est.x - est.y)) %>% 
    filter(est_gap <= min(est_gap)) %>% 
    mutate(slope = ifelse(is.infinite(est.x / est.y) | is.na(est.x / est.y) | is.nan(est.x / est.y), 
                          1, 
                          est.x / est.y)) %>% 
    gather(quarter, sales, -setdiff(1:ncol(.), starts_with("20"))) %>% 
    select(quarter, province = province.x, city = city.x, tier, market, atc3, 
           molecule_desc, packid, sales, est.x, est.y, slope)
}

universe.proj <- proj.region.list %>% 
  bind_rows() %>% 
  mutate(slope = ifelse(slope > quantile(slope, 0.9, na.rm = TRUE), 
                        quantile(slope, 0.9, na.rm = TRUE), 
                        slope),
         final_sales = sales * slope,
         year = stri_sub(quarter, 1, 4)) %>% 
  filter(final_sales > 0, !(city %in% unique(proj.market$city))) %>% 
  mutate(flag = 1) %>% 
  select(year, quarter, province, city, market, atc3, molecule_desc, 
         packid, sales = final_sales, flag) %>% 
  bind_rows(sample.proj)

write_feather(universe.proj, "03_Outputs/04_MSD_CHC_Universe_Projection.feather")










