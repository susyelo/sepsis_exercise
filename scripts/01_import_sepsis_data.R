# libs --------------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(readr)
library(curl)
library(stringi)


# functions ---------------------------------------------------------------
source("./R/fun.R")

# data --------------------------------------------------------------------
seps_dat <- read_xlsx("./data_raw/sepsis/sepsis_resume_reg.xlsx", skip = 1)

### Change the names to the variables 
## type_sejour_sepsis = "1 - Sepsis sans choc septique" "2 - Choc septique" 
## nb_sejours = number of hospital stays related to sepsis
## dms_nuitees = Durée moyenne de séjour (DMS). Average number of nights per hospitalization
## age_moyen = average age of patients hospitalized for sepsis in that region,
## tx_sejours_sepsis_100k = Sepsis hospitalization rate. (nb_sejours/pop)*1e5

original_labs <- names(seps_dat)

seps_dat <- 
  seps_dat %>%
  rename(
    region = `Région`,
    type_sejour_sepsis = `Type de séjours de prise en charge de sepsis`,
    nb_sejours = `Nombre de séjours`,
    age_moyen = `Age moyen`,
    dms_nuitees = `DMS en nuitées`,
    pct_deces = `% de décès`,
    tx_sejours_sepsis_100k = `Nombre de séjours pour sepsis des habitants de la région pour 100 000 habitants`
  ) %>% 
  mutate(region_name = gsub("^\\s*\\d{2}\\s*-\\s*", "", region))


### Download FRANCE regions shapefiles ---------------------------------------------------------------
#regions_sf <- download_FRA_REG(page_url = "https://geoservices.ign.fr/telechargement-api/ADMIN-EXPRESS-COG?page=1")

regions_sf <- read_REG_data(folder = "./data_raw/spatial/data_ign", subfolder = "admin_express_cog")

## Fixing names mismatched! 
# Align formatting to your shapefile style + fix the truncation
seps_dat$region_name <- harmonise_region(seps_dat$region_name)
seps_dat$region_name <- sub("PROVENCE-ALPES-COTE D'AZU$", "PROVENCE-ALPES-COTE D'AZUR", seps_dat$region_name)

regions_joined <- 
  regions_sf %>%
  left_join(seps_dat, by = c("NOM_M" = "region_name"))


# save data ----------------------------------------------------------------
saveRDS(seps_dat, "./data/derived/01_sepsis_dat.RDS")
saveRDS(regions_joined, "./data/derived/01_sepsis_spatial.RDS")
saveRDS(original_labs, "./data/derived/01_original_sepsis_labels.RDS")
