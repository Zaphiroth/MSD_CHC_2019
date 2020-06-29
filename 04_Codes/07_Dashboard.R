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

# chk <- msd.dashboard %>% 
#   filter(is.na(DLY_DOSAGE)) %>% 
#   distinct(Pack_ID, PROD_NAME) %>% 
#   arrange(Pack_ID)
# 
# write.xlsx(chk, "05_Internal_Review/PROD_NAME_Not_Matching.xlsx")





