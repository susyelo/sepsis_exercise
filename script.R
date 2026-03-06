# libs --------------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(sf)
library(readr)
library(curl)
library(stringi)
library(patchwork)

# functions ---------------------------------------------------------------
source("fun.R")


# data --------------------------------------------------------------------
seps_dat <- read_xlsx("sepsis_resume_reg.xlsx", skip = 1)

original_labs <- names(seps_dat)

### Change the names to the variables 
## type_sejour_sepsis = "1 - Sepsis sans choc septique" "2 - Choc septique" 
## nb_sejours = number of hospital stays related to sepsis
## dms_nuitees = Durée moyenne de séjour (DMS). Average number of nights per hospitalization
## age_moyen = average age of patients hospitalized for sepsis in that region,
## tx_sejours_sepsis_100k = Sepsis hospitalization rate. (nb_sejours/pop)*1e5

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


labels_names <- data.frame(original = c(original_labs, "region_name"), var_names = names(seps_dat))
labels_names<- 
  labels_names[
    labels_names$var_names%in%c("region", "type_sejour_sepsis", "region_name")==FALSE,
  ]

### Plot each variable
for (i in 1:nrow(labels_names)){

  p <- plot_one_var(data = seps_dat, 
               var_name = labels_names$var_names[i], 
               var_label = labels_names$original[i])
  
  print(p)
  
}

## All variables at the same time (scale to see effect)
vars <- labels_names$var_names[labels_names$var_names!="nb_sejours"]

seps_long <- 
  seps_dat %>%
  pivot_longer(all_of(vars), names_to = "variable", values_to = "value")

ggplot(seps_long, aes(x = reorder(region, value), y = value, fill = type_sejour_sepsis)) +
  geom_col(position = "dodge", width = 0.75) +
  coord_flip() +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())


### Download FRANCE regions shapefiles 
#regions_sf <- download_FRA_REG(page_url = "https://geoservices.ign.fr/telechargement-api/ADMIN-EXPRESS-COG?page=1")

regions_sf <- read_REG_data()

## Fixing names mismatched! 
# Align formatting to your shapefile style + fix the truncation
seps_dat$region_name <- harmonise_region(seps_dat$region_name)
seps_dat$region_name <- sub("PROVENCE-ALPES-COTE D'AZU$", "PROVENCE-ALPES-COTE D'AZUR", seps_dat$region_name)

regions_joined <- 
  regions_sf %>%
  left_join(seps_dat, by = c("NOM_M" = "region_name"))

## Map each reponse variable
plot_maps_var(regions_joined, 
              var = "tx_sejours_sepsis_100k", 
              label_var = "Nombre de séjours pour sepsis des habitants de la région pour 100 000 habitants")