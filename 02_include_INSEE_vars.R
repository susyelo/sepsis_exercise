library(readxl)
library(dplyr)
library(stringr)
library(janitor)

### 1. Economical variable 
file_path <- "./INSEE/RPM2024-F21.xlsx"

# 1) Read the Excel sheet
raw_reg <- read_excel(
  file_path,
  sheet = "Figure 1",
  col_names = FALSE
)

# 2) Extract the relevant columns
poverty_region_df <- raw_reg %>%
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

## check 
anti_join(seps_dat, poverty_region_df, by = "region_name")


### 2. pop data
file_path <- "./INSEE/TCRD_021.xlsx"

# 1) Read the Excel sheet

reg_data <- 
  read_excel(file_path, sheet = "REG", skip = 2) %>%
  janitor::clean_names()

pop_data <- 
  reg_data %>%
  rename(
    code_region = x1,
    region_name = x2
  ) %>% 
  mutate(region_name = harmonise_region(region_name)) %>% 
  filter(region_name != "FRANCE METROPOLITAINE") %>% 
  filter(region_name != "FRANCE") 

### 3. Educational data
file_path <- "./INSEE/TD_FOR2_2021.xlsx"

reg_data <- 
  read_excel(file_path, sheet = "COM", skip = 10) %>%
  janitor::clean_names() %>% 
  mutate(
    CODGEO = str_pad(as.character(codgeo), width = 5, side = "left", pad = "0")
  )


## Commune and regions classifications
com_reg <- read.csv("./INSEE/20230823-communes-departement-region.csv")
com_reg <-
  com_reg %>% 
  mutate(
    CODGEO = str_pad(as.character(code_commune_INSEE), width = 5, side = "left", pad = "0")
  ) %>% 
  select(CODGEO, nom_commune, code_region, nom_region)


## join data 
edu_with_region <- 
  reg_data %>%
  left_join(com_reg, by = "CODGEO")

tmp <- edu_with_region %>% select(CODGEO, libgeo, ageq65015_dipl_19a_sexe1, nom_commune, code_region, nom_region)

## sum data 
edu_region <- 
  edu_with_region %>%
  group_by(code_region, nom_region) %>%
  summarise(
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    n_communes = n(),
    .groups = "drop"
  )


unmatched_communes <- 
  edu_with_region %>%
  filter(is.na(nom_region)) %>%
  select(codgeo, libgeo)

unmatched_communes
