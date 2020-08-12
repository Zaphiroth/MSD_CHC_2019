# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  MSD CHC 2019
# Purpose:      Summary
# programmer:   Zhe Liu
# Date:         2020-06-11
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin data ----
# pack info
pack.ref <- fread("02_Inputs/cn_prod_ref_201912_1.txt", 
                  stringsAsFactors = FALSE, sep = "|") %>% 
  distinct() %>% 
  mutate(Pack_Id = stri_pad_left(Pack_Id, 7, 0))

# corp info
corp.ref <- fread("02_Inputs/cn_corp_ref_201912_1.txt", 
                  stringsAsFactors = FALSE, sep = "|") %>% 
  distinct()

# pack & corp
corp.pack <- pack.ref %>% 
  distinct(Pack_Id, Prd_desc, Pck_Desc, Corp_ID, PckSize_Desc) %>% 
  left_join(corp.ref, by = "Corp_ID") %>% 
  select(packid = Pack_Id, Pck_Desc, Corp_Desc, PckSize_Desc)

# product desc
prod.desc <- read.csv("02_Inputs/pfc与ims数据对应.csv") %>% 
  mutate(Pack_Id = stri_pad_left(Pack_Id, 7, 0)) %>% 
  select(packid = Pack_Id, Prd_desc = `商品名`)


##---- Result ----
# history delivery
msd.history <- read.xlsx("02_Inputs/CHC Audit Delivery to MSD 1115.xlsx", 
                         sheet = "Detail_Data") %>% 
  filter(stri_sub(Date, 1, 4) %in% c("2017", "2018")) %>% 
  tibble() %>% 
  mutate(Pack_ID = stri_pad_left(Pack_ID, 7, 0))

# final result
msd.2019 <- msd.price %>% 
  filter(!(city %in% unique(msd.target.city$City))) %>% 
  left_join(corp.pack, by = "packid") %>% 
  left_join(prod.desc, by = "packid") %>% 
  mutate(Channel = "CHC",
         dosageunits = PckSize_Desc * units) %>% 
  group_by(Pack_ID = packid, Channel, Province = province, City = city, 
           Date = quarter, ATC3 = atc3, MKT = market, Molecule_Desc = molecule_desc, 
           Prod_Desc = Prd_desc, Pck_Desc, Corp_Desc) %>% 
  summarise(Sales = sum(sales, na.rm = TRUE),
            Units = sum(units, na.rm = TRUE),
            DosageUnits = sum(dosageunits, na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(msd.target.city) %>% 
  mutate(Pack_ID = if_else(Pack_ID == '4777502', '5890602', Pack_ID), 
         Corp_Desc = if_else(Pack_ID == '4777502', 'ASTRAZENECA GROUP', Corp_Desc), 
         Pack_ID = if_else(Prod_Desc == 'GLUCOPHAGE', 
                           stri_paste('64895', stri_sub(Pack_ID, 6, 7)), 
                           Pack_ID), 
         Corp_Desc = if_else(Prod_Desc == 'GLUCOPHAGE', 'MERCK GROUP', Corp_Desc)) %>% 
  group_by(Pack_ID, Channel, Province, City, Date, ATC3, MKT, Molecule_Desc, 
           Prod_Desc, Pck_Desc, Corp_Desc) %>% 
  summarise(Sales = sum(Sales, na.rm = TRUE),
            Units = sum(Units, na.rm = TRUE),
            DosageUnits = sum(DosageUnits, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(Sales = round(Sales, 2),
         Units = round(Units),
         DosageUnits = round(DosageUnits)) %>% 
  filter(Sales > 0, Units > 0, DosageUnits > 0) %>% 
  arrange(Date, Province, City, Pack_ID)

msd.universe <- msd.2019 %>% 
  group_by(Pack_ID, Channel, Province = "National", City = "National", 
           Date, ATC3, MKT, Molecule_Desc, Prod_Desc, Pck_Desc, Corp_Desc) %>% 
  summarise(Sales = sum(Sales, na.rm = TRUE),
            Units = sum(Units, na.rm = TRUE),
            DosageUnits = sum(DosageUnits, na.rm = TRUE)) %>% 
  ungroup()

adj.factor <- read.xlsx("02_Inputs/Adjust_Factor.xlsx") %>% 
  mutate(Pack_ID = stri_pad_left(Pack_ID, 7, 0)) %>% 
  setDT() %>% 
  melt(id.vars = c("Prod_Desc", "Pack_ID"), 
       variable.name = "City", 
       value.name = "factor", 
       variable.factor = FALSE)

msd.result <- msd.2019 %>% 
  filter(City %in% target.city) %>% 
  bind_rows(msd.universe) %>% 
  group_by(Pack_ID, Channel, Province, City, Year = stri_sub(Date, 1, 4), 
           ATC3, MKT, Molecule_Desc, Prod_Desc, Pck_Desc, Corp_Desc) %>% 
  summarise(Sales = sum(Sales, na.rm = TRUE),
            Units = sum(Units, na.rm = TRUE),
            DosageUnits = sum(DosageUnits, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(adj.factor, by = c("Prod_Desc", "Pack_ID", "City")) %>% 
  mutate(factor = if_else(is.na(factor), 1, factor),
         Sales = round(Sales * factor, 2),
         Units = round(Units * factor),
         DosageUnits = round(DosageUnits * factor)) %>% 
  select(-factor) %>% 
  arrange(Year, Province, City, Pack_ID)

write_feather(msd.result, "03_Outputs/06_MSD_CHC_OAD_2019.feather")
write.xlsx(msd.result, "03_Outputs/06_MSD_CHC_OAD_2019.xlsx")


# msd.dashboard <- msd.result %>% 
#   setDT() %>% 
#   melt(id.vars = c("Pack_ID", "Channel", "Province", "City", "Year", "ATC3", "MKT", 
#                    "Molecule_Desc", "Prod_Desc", "Pck_Desc", "Corp_Desc"), 
#        measure.vars = c("Sales", "Units", "DosageUnits"), 
#        variable.name = "Measurement", 
#        value.name = "Value", 
#        variable.factor = FALSE)
# 
# write.xlsx(msd.dashboard, "03_Outputs/MSD_CHC_OAD_2019_Dashboard.xlsx")
