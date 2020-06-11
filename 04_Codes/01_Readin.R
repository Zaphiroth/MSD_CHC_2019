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
  right_join(market.def, by = c("Molecule_Desc" = "Molecule"))

# IMS pack info
ims.pack.raw <- read.xlsx("02_Inputs/ims_chpa_to20Q1.xlsx", startRow = 4, cols = 1:21)

ims.pack.info <- ims.pack.raw %>% 
  distinct(packid = Pack_ID, atc4 = ATC4_Code, molecule = Molecule_Desc)

# target city
target.city <- c("北京", "上海", "广州", "杭州", "苏州", "南京", "福州", "宁波")


##---- Formatting ----
servier.raw <- read_feather("02_Inputs/raw data/Servier_CHC_Total_Raw_2017-2019.feather")

msd.raw <- servier.raw %>% 
  filter(market == "OAD", molecule_desc %in% market.def$Molecule, year %in% c("2018", "2019"))

write_feather(msd.raw, "03_Outputs/01_MSD_CHC_Raw.feather")



