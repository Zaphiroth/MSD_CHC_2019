# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  MSD CHC 2019
# Purpose:      Readin
# programmer:   Zhe Liu
# Date:         2020-06-10
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin data ----
# PCHC code
pchc.universe <- read.xlsx("02_Inputs/Universe_PCHCCode_20200507.xlsx", sheet = "PCHC")

pchc.mapping1 <- pchc.universe %>% 
  filter(!is.na(`单位名称`), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `单位名称`) %>% 
  summarise(pchc1 = first(PCHC_Code)) %>% 
  ungroup()

pchc.mapping2 <- pchc.universe %>% 
  filter(!is.na(ZS_Servier.name), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `ZS_Servier.name`) %>% 
  summarise(pchc2 = first(PCHC_Code)) %>% 
  ungroup()

pchc.mapping3 <- pchc.mapping1 %>% 
  group_by(pchc = pchc1) %>% 
  summarise(province = first(na.omit(province)),
            city = first(na.omit(city)),
            district = first(na.omit(district))) %>% 
  ungroup()

# market definition
market.def <- read.xlsx("02_Inputs/默沙东CMAX 项目通知书-2020-0605.xlsx", sheet = "市场定义") %>% 
  distinct(Molecule, Molecule_CN) %>% 
  filter(!is.na(Molecule), !is.na(Molecule_CN)) %>% 
  mutate(Molecule = toupper(Molecule))

# history
servier.chc.raw <- read.xlsx("02_Inputs/CHC_MAX_16Q419Q4_0317.xlsx")

servier.chc <- servier.chc.raw %>% 
  group_by(Pack_ID, Channel, Province, City, Date, ATC3, MKT, Molecule_Desc, 
           Prod_Desc, Pck_Desc, Corp_Desc, `Period-MAT`, `CITY-EN`, 
           TherapeuticClsII, Prod_CN_Name,  Package, Dosage, Quantity, 
           `是否是4+7城市`, `是否进入带量采购`, `是否是原研`, `是否是中标品种`, 
           `是否是MNC`, `ATC3中文分类`) %>% 
  summarise(Sales = sum(Sales, na.rm = TRUE),
            Units = sum(Units, na.rm = TRUE),
            DosageUnits = sum(DosageUnits, na.rm = TRUE)) %>% 
  ungroup()

servier.chc.mole <- servier.chc %>% 
  distinct(Molecule_Desc) %>% 
  mutate(Molecule_Desc = toupper(Molecule_Desc),
         label = 1)

molecule.check <- servier.chc.mole %>% 
  right_join(market.def, by = c("Molecule_Desc" = "Molecule")) %>% 
  filter(is.na(label))

# IMS pack info
ims.pack.raw <- read.xlsx("02_Inputs/ims_chpa_to20Q1.xlsx", startRow = 4, cols = 1:21)

ims.pack.info <- ims.pack.raw %>% 
  distinct(packid = Pack_ID, atc4 = ATC4_Code, molecule = Molecule_Desc)

# target city
target.city <- c("北京", "上海", "广州", "杭州", "苏州", "南京", "福州", "宁波")


##---- Formatting ----
# Shanghai new sample
servier.sh.raw <- read.xlsx("02_Inputs/raw data/shanghai_201805_202004_packid_moleinfo_PCHC.xlsx")

servier.sh <- servier.sh.raw %>% 
  mutate(year = as.character(Year),
         date = as.character(Month),
         quarter = ifelse(stri_sub(date, 5, 6) %in% c("01", "02", "03"), paste0(year, "Q1"), 
                          ifelse(stri_sub(date, 5, 6) %in% c("04", "05", "06"), paste0(year, "Q2"), 
                                 ifelse(stri_sub(date, 5, 6) %in% c("07", "08", "09"), paste0(year, "Q3"), 
                                        ifelse(stri_sub(date, 5, 6) %in% c("10", "11", "12"), paste0(year, "Q4"), 
                                               NA_character_)))),
         province = "上海",
         city =`城市`,
         pchc = PCHC_Code,
         atc3 = stri_sub(ATC4_Code, 1, 4),
         molecule_desc = Molecule_Desc,
         packid = packcode) %>% 
  distinct() %>% 
  filter(Molecule_Desc %in% market.def$Molecule) %>% 
  mutate(market = "OAD") %>% 
  select(year, date, quarter, province, city, pchc, market, atc3, molecule_desc, 
         packid, units = `数量`, sales = `金额`)

# bind
servier.raw <- read_feather("02_Inputs/raw data/Servier_CHC_Total_Raw_2017-2019.feather")

msd.raw <- bind_rows(servier.raw, servier.sh) %>% 
  filter(market == "OAD", 
         molecule_desc %in% market.def$Molecule, 
         year %in% c("2018", "2019")) %>% 
  mutate(packid = if_else(packid == '4777502', '5890602', packid)) %>% 
  group_by(year, date, quarter, pchc, market, atc3, molecule_desc, packid) %>% 
  summarise(province = first(na.omit(province)),
            city = first(na.omit(city)),
            units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

write_feather(msd.raw, "03_Outputs/01_MSD_CHC_Raw.feather")


prod.desc <- read.csv("02_Inputs/pfc与ims数据对应.csv") %>% 
  mutate(Pack_Id = stri_pad_left(Pack_Id, 7, 0)) %>% 
  select(packid = Pack_Id, Prd_desc = `商品名`)

raw.check <- servier.raw %>% 
  filter(market == "OAD", 
         molecule_desc %in% market.def$Molecule, 
         year %in% c("2019")) %>% 
  left_join(prod.desc, by = 'packid') %>% 
  mutate(packid = if_else(packid == '4777502', '5890602', packid), 
         packid = if_else(Prd_desc == 'GLUCOPHAGE', 
                          stri_paste('64895', stri_sub(packid, 6, 7)), 
                          packid)) %>% 
  group_by(city) %>% 
  mutate(pchc_n = length(unique(pchc))) %>% 
  ungroup() %>% 
  group_by(year, date, quarter, province, city, pchc_n, market, atc3, 
           molecule_desc, Prd_desc) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

write.xlsx(raw.check, '05_Internal_Review/MSD_CHC_2019_Raw.xlsx')



