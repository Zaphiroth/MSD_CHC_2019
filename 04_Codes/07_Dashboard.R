# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  MSD CHC 2019
# Purpose:      Dashboard
# programmer:   Zhe Liu
# Date:         2020-06-24
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


msd.adj <- read.xlsx("02_Inputs/06_MSD_CHC_OAD_2019_0624.xlsx")
dly.dosage <- read.xlsx("02_Inputs/OAD_PDot转换关系.xlsx", startRow = 4)
dly.dosage1 <- read.xlsx("02_Inputs/PROD_NAME_Not_Matching1.xlsx")
dly.dosage2 <- read.xlsx("02_Inputs/PROD_NAME_Not_Matching2.xlsx")
msd.category <- read.xlsx("02_Inputs/MSD交付匹配.xlsx", cols = 1:2)
city.en <- read.xlsx("02_Inputs/MSD交付匹配.xlsx", cols = 4:7)

dly.dosage <- dly.dosage %>% 
  mutate(PROD_NAME = gsub(" \\S*$", "", PROD_NAME)) %>% 
  distinct()

dly.dosage.pack <- msd.adj %>% 
  mutate(PROD_NAME = paste0(Prod_Desc, " ", Pck_Desc), 
         PROD_NAME = gsub("\\s+", " ", str_trim(PROD_NAME))) %>% 
  left_join(dly.dosage, by = "PROD_NAME") %>% 
  bind_rows(dly.dosage1, dly.dosage2) %>% 
  filter(!is.na(DLY_DOSAGE)) %>% 
  distinct(Pack_ID, DLY_DOSAGE)

msd.dashboard <- msd.adj %>% 
  mutate(Corp_Desc = if_else(Corp_Desc == "LVYE GROUP", "LUYE GROUP", Corp_Desc), 
         PROD_NAME = paste0(Prod_Desc, " ", Pck_Desc), 
         PROD_NAME = gsub("\\s+", " ", str_trim(PROD_NAME))) %>% 
  left_join(dly.dosage.pack, by = "Pack_ID") %>% 
  left_join(msd.category, by = "ATC3") %>% 
  left_join(city.en, by = c("Province", "City")) %>% 
  mutate(Date = Year, 
         Province = Province_EN, 
         City = City_EN, 
         PDot = DosageUnits / DLY_DOSAGE) %>% 
  setDT() %>% 
  melt(id.vars = c("Channel", "MKT", "Date", "Province", "City", "ATC3", 
                   "Category", "Molecule_Desc", "Prod_Desc", "Pck_Desc", 
                   "Pack_ID", "Corp_Desc"),
       measure.vars = c("Sales", "Units", "DosageUnits", "PDot"),
       variable.name = "Measurement",
       value.name = "Value",
       variable.factor = FALSE)

write.xlsx(msd.dashboard, "03_Outputs/07_MSD_Dashboard_Data.xlsx")

# adjustment
msd.dashboard <- read.xlsx("03_Outputs/07_MSD_Dashboard_Data.xlsx")

msd.dashboard.adj <- msd.dashboard %>% 
  mutate(Pack_ID = if_else(Pack_ID == '4777502', '5890602', Pack_ID), 
         Corp_Desc = if_else(Pack_ID == '4777502', 'ASTRAZENECA GROUP', Corp_Desc)) %>% 
  group_by(Channel, MKT, Date, Province, City, ATC3, Category, Molecule_Desc, 
           Prod_Desc, Pck_Desc, Pack_ID, Corp_Desc, Measurement) %>% 
  summarise(Value = sum(Value, na.rm = TRUE)) %>% 
  ungroup()

write.xlsx(msd.dashboard.adj, '03_Outputs/07_MSD_Dashboard_Data_Adjustment.xlsx')

# chk <- msd.dashboard %>% 
#   filter(is.na(DLY_DOSAGE)) %>% 
#   distinct(Pack_ID, PROD_NAME) %>% 
#   arrange(Pack_ID)
# 
# write.xlsx(chk, "05_Internal_Review/PROD_NAME_Not_Matching.xlsx")

# Beijing sample
servier.raw.2020 <- read_feather('02_Inputs/Servier_CHC_Raw.feather')

msd.bj <- servier.raw.2020 %>% 
  filter(city == '北京', 
         molecule_desc %in% market.def$Molecule, 
         quarter %in% c('2019Q1', '2019Q2', '2019Q3', '2019Q4', '2020Q1')) %>% 
  mutate(packid = if_else(packid == '4777502', '5890602', packid)) %>% 
  left_join(msd.category, by = c('atc3' = 'ATC3')) %>% 
  left_join(prod.desc, by = 'packid') %>% 
  left_join(corp.pack, by = 'packid') %>% 
  group_by(Channel = 'CHC', MKT = market, Date = quarter, ATC3 = atc3, 
           Category, Molecule_Desc = molecule_desc, Prod_Desc = Prd_desc, 
           Pck_Desc, Pack_ID = packid, Corp_Desc, Measurement = 'sales') %>% 
  summarise(Value = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

write.xlsx(msd.bj, '03_Outputs/MSD_CHC_OAD_Sample_2019Q1_2020Q1_Beijing.xlsx')

