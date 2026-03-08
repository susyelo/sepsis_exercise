library(readxl)
library(dplyr)
library(stringr)
library(janitor)

### 1. Economical variable 
# Niveau de vie et pauvreté par région
##https://www.insee.fr/fr/statistiques/7941411?sommaire=7941491
url1 <- "https://www.insee.fr/fr/statistiques/fichier/7941411/RPM2024-F21.xlsx"
dest1 <- "./data_raw/INSEE/RPM2024-F21.xlsx"

download.file(url1, destfile = dest1, mode = "wb")

# 1) Read the Excel sheet
poverty_dat<- read_excel(dest1, sheet = "Figure 1", col_names = FALSE)

# 2) Extract the relevant columns
poverty_region_df <- 
  poverty_dat %>%
  transmute(
    region_raw = as.character(...1),
    niveau_vie_median_annuel = suppressWarnings(as.numeric(...3)),
    intensite_pauvrete_pct = suppressWarnings(as.numeric(...7))
  ) %>%
  mutate(
    region_raw = str_replace_all(region_raw, "\n", " "),
    region_raw = str_squish(region_raw)
  ) %>%
  filter(
    !is.na(region_raw),
    !is.na(niveau_vie_median_annuel),
    !str_detect(region_raw, "^ns"),
    !str_detect(region_raw, "^1\\."),
    !str_detect(region_raw, "^Note"),
    !str_detect(region_raw, "^Lecture"),
    !str_detect(region_raw, "^Champ"),
    !str_detect(region_raw, "^Sources")
  )

# 3) Name harmonisation
poverty_region_df <- 
  poverty_region_df %>%
  mutate(region_name = harmonise_region(region_raw)) %>%
  filter(region_name != "FRANCE METROPOLITAINE, MARTINIQUE ET LA REUNION") %>%
  select(region_name, niveau_vie_median_annuel, intensite_pauvrete_pct)

### 2. pop data: 
### pop data over 15 years old and total population 
### Estimation de la population au 1ᵉʳ janvier 2026 Séries par région, département, sexe et âge de 1975 à 2026
## https://www.insee.fr/fr/statistiques/8721456
url2 <- "https://www.insee.fr/fr/statistiques/fichier/8721456/estim-pop-nreg-sexe-aq-1975-2026.xlsx"
dest2 <- "./data_raw/INSEE/estim-pop-nreg-sexe-aq-1975-2026.xlsx"

download.file(url2, destfile = dest2, mode = "wb")

# 1) Read the Excel sheet
pop_dat <- 
  read_excel(dest2, sheet = "2022", skip = 4) %>%
  janitor::clean_names()


# Columns corresponding to ages 15+
age15_cols <- c("x15_a_19_ans_5", "x20_a_24_ans_6", "x25_a_29_ans_7", "x30_a_34_ans_8", "x35_a_39_ans_9", "x40_a_44_ans_10", "x45_a_49_ans_11", "x50_a_54_ans_12", "x55_a_59_ans_13", "x60_a_64_ans_14", "x65_a_69_ans_15", "x70_a_74_ans_16","x75_a_79_ans_17", "x80_a_84_ans_18", "x85_a_89_ans_19", "x90_a_94_ans_20", "x95_ans_et_plus_21")

total_pop_cols <- c("x0_a_4_ans_2", "x5_a_9_ans_3", "x10_a_14_ans_4", "x15_a_19_ans_5", "x20_a_24_ans_6", "x25_a_29_ans_7", "x30_a_34_ans_8", "x35_a_39_ans_9", "x40_a_44_ans_10", "x45_a_49_ans_11", "x50_a_54_ans_12", "x55_a_59_ans_13", "x60_a_64_ans_14", "x65_a_69_ans_15", "x70_a_74_ans_16","x75_a_79_ans_17", "x80_a_84_ans_18", "x85_a_89_ans_19", "x90_a_94_ans_20", "x95_ans_et_plus_21")

pop_region_df <- 
  pop_dat %>%
  rename(
    region_name = x1) %>% 
  filter(region_name != "France métropolitaine") %>% 
  filter(region_name != "DOM") %>% 
  filter(region_name != "France métropolitaine et DOM") %>% 
  filter(region_name != "Source : Insee - Estimations de population (résultats définitifs arrêtés fin 2024)") %>% 
  mutate(region_name = harmonise_region(region_name))
   
pop_region_total <-
  pop_region_df %>%
  mutate(
    pop15plus = rowSums(across(all_of(age15_cols)), na.rm = TRUE), 
    total = rowSums(across(all_of(total_pop_cols)), na.rm = TRUE) 
  ) |>
  select(region_name, total, pop15plus)


### 3. Educational data
### Formation en 2021 Recensement de la population - Base des tableaux détaillés
## https://www.insee.fr/fr/statistiques/8202328?sommaire=8202334
url3 <- "https://www.insee.fr/fr/statistiques/fichier/8202328/TD_FOR2_2021_xlsx.zip"
dest3 <- "./data_raw/INSEE/TD_FOR2_2021_xlsx.zip"

download.file(url3, destfile = dest3, mode = "wb")
unzip("./data_raw/INSEE/TD_FOR2_2021_xlsx.zip", exdir = "./data_raw/INSEE/")

# 1) Read the Excel sheet
edu_dat <-
  read_excel("./data_raw/INSEE/TD_FOR2_2021.xlsx", sheet = "COM", skip = 10) %>%
  janitor::clean_names() %>% 
  mutate(
    COM = str_pad(as.character(codgeo), width = 5, side = "left", pad = "0")
  )

edu_comm_df <- 
  edu_dat %>% 
  mutate(edu_pop15plus = rowSums(across(-c(codgeo, COM, libgeo)), na.rm = TRUE)) %>% 
  select(-contains("ageq"))

# save data ---------------------------------------------------------------
saveRDS(poverty_region_df, "./data/derived/02_insee_poverty_REG.RDS")
saveRDS(pop_region_total, "./data/derived/02_insee_population_REG.RDS")
saveRDS(edu_comm_df, "./data/derived/02_insee_education_COM.RDS")



