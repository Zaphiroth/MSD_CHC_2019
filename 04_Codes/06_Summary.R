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

write_feather(msd.2019, "03_Outputs/06_MSD_CHC_OAD_2017Q1_2019Q4.feather")
write.xlsx(msd.2019, "03_Outputs/06_MSD_CHC_OAD_2017Q1_2019Q4.xlsx")

# worksheet
msd.target <- msd.target.city %>% 
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

msd.universe <- msd.price %>% 
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
  mutate(Sales = round(Sales, 2),
         Units = round(Units),
         DosageUnits = round(DosageUnits)) %>% 
  filter(Sales > 0, Units > 0, DosageUnits > 0) %>% 
  arrange(Date, Province, City, Pack_ID)

wb <- createWorkbook()
addWorksheet(wb, "City")
addWorksheet(wb, "National")
writeDataTable(wb, "City", msd.target)
writeDataTable(wb, "National", msd.universe)
saveWorkbook(wb, "03_Outputs/06_MSD_CHC_OAD_2019_Split.xlsx")



