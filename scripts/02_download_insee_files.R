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
### Formation en 2021 Recensement de la population - Base des tableaux détaillés
## https://www.insee.fr/fr/statistiques/8202328?sommaire=8202334
url2 <- "https://www.insee.fr/fr/statistiques/fichier/2012692/TCRD_021.xlsx"
dest2 <- "./data_raw/INSEE/TCRD_021.xlsx"

download.file(url2, destfile = dest2, mode = "wb")

# 1) Read the Excel sheet
pop_dat <- 
  read_excel(dest2, sheet = "REG", skip = 2) %>%
  janitor::clean_names()

pop_region_df <- 
  pop_dat %>%
  rename(
    code_region = x1,
    region_name = x2
  ) %>% 
  mutate(region_name = harmonise_region(region_name)) %>% 
  filter(region_name != "FRANCE METROPOLITAINE") %>% 
  filter(region_name != "FRANCE") 

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
  mutate(total_pop15plus = rowSums(across(-c(codgeo, COM, libgeo)), na.rm = TRUE)) %>% 
  select(-contains("ageq"))

# save data ---------------------------------------------------------------
saveRDS(poverty_region_df, "./data/derived/02_insee_poverty_REG.RDS")
saveRDS(pop_region_df, "./data/derived/02_insee_population_REG.RDS")
saveRDS(edu_comm_df, "./data/derived/02_insee_education_COM.RDS")



