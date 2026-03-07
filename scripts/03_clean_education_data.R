### Calculate the population non scolarised by commune, there are some communes with same number and different name
## I am adding all the numbers to have an added value per commune

## la population non scolarisée comprend les personnes non inscrites dans un établissement d'enseignement.
edu_comm_df <- readRDS("./data/derived/02_insee_education_COM.RDS")

## Since there are some communes that were old and fusion with others I will sum everything along the commune code number
edu_data_COM <- 
  edu_comm_df %>% 
  group_by(COM) %>%
  summarise(
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )

### using CODGEO as the join reference
## https://www.insee.fr/fr/information/8377162 source from the lookup data
## Commune and regions classifications
com_reg <- 
  read.csv("./data/lookup/v_commune_2025.csv") %>% 
  filter(!is.na(REG)) %>% 
  distinct(COM, REG)

reg_info <- 
  read.csv("./data/lookup/v_region_2025.csv") %>% 
  distinct(REG,  REG_NAME = NCC)

## join data 
edu_with_region <- 
  edu_data_COM %>%
  left_join(com_reg, by = "COM") %>% 
  left_join(reg_info, by = "REG")

### All of these communes did not have information about the regions classification, either because they are not longer a commune or because the code reported in the data table does not correspond to the commune region data
unmatched_communes <- 
  edu_with_region %>%
  filter(is.na(REG_NAME))

## table with the communes that have data in the education excel but does not have a match in the lookup file
saveRDS(unmatched_communes, "outputs/tables/03_edu_communes_no_reg_match.RDS")

## sum data 
edu_region <- 
  edu_with_region %>%
  group_by(REG, REG_NAME) %>%
  summarise(
    across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
    n_communes = n(),
    .groups = "drop"
  )

saveRDS(edu_region, "data/derived/03_insee_education_REG.RDS")