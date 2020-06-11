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
  select(packid = Pack_Id, Prd_desc, Pck_Desc, Corp_Desc, PckSize_Desc)


##---- Result ----
# target city result
servier.chc.raw <- read.xlsx("02_Inputs/CHC_MAX_16Q419Q4_0317.xlsx")

msd.target.city <- servier.chc %>% 
  filter(stri_sub(Date, 1, 4) == "2019", 
         City %in% target.city, 
         MKT == "OAD", 
         Molecule_Desc %in% market.def$Molecule) %>% 
  mutate(Pack_ID = stri_pad_left(Pack_ID, 7, 0)) %>% 
  select(Pack_ID, Channel, Province, City, Date, ATC3, MKT, Molecule_Desc, 
         Prod_Desc, Pck_Desc, Corp_Desc, Sales, Units, DosageUnits)

# final result
msd.2019 <- msd.price %>% 
  filter(!(city %in% target.city)) %>% 
  left_join(corp.pack, by = "packid") %>% 
  mutate(Channel = "CHC",
         dosageunits = PckSize_Desc * units) %>% 
  group_by(Pack_ID = packid, Channel, Province = province, City = city, Date = quarter, ATC3 = atc3, 
           MKT = market, Molecule_Desc = molecule_desc, Prod_Desc = Prd_desc, Pck_Desc, Corp_Desc) %>% 
  summarise(Sales = sum(sales, na.rm = TRUE),
            Units = sum(units, na.rm = TRUE),
            DosageUnits = sum(dosageunits, na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(msd.target.city) %>% 
  mutate(Sales = round(Sales, 2),
         Units = round(Units),
         DosageUnits = round(DosageUnits)) %>% 
  arrange(Date, Province, City, Pack_ID)

write_feather(msd.2019, "03_Outputs/06_MSD_CHC_OAD_2019.feather")
write.xlsx(msd.2019, "03_Outputs/06_MSD_CHC_OAD_2019.xlsx")


