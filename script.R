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


plot_one_var <- function(data, 
                         var_name, 
                         var_label,
                         region_col = "region",
                         group_col  = "type_sejour_sepsis") {
  
  ggplot(
    data,
    aes(x = .data[[region_col]], y = .data[[var_name]], fill = .data[[group_col]])
  ) +
    geom_col(position = "dodge", width = 0.75) +
    coord_flip() +
    labs(x = NULL, y = var_label, fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

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

ggplot(seps_long, aes(x = region, y = value, fill = type_sejour_sepsis)) +
  geom_col(position = "dodge", width = 0.75) +
  coord_flip() +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

# Map regional variables France -------------------------------------------

page_url <- "https://geoservices.ign.fr/telechargement-api/ADMIN-EXPRESS-COG?page=1"

html <- readr::read_file(curl::curl(page_url))

# Extract all .7z URLs from the page
urls <- str_extract_all(html, "https://data\\.geopf\\.fr/telechargement/download/ADMIN-EXPRESS-COG/[^\\s\"']+\\.7z")[[1]]

# Keep only WGS84G France archives
wgs84_fra <- urls[str_detect(urls, "SHP_WGS84G_FRA")]

# Pick the "latest" by date in filename (YYYY-MM-DD) if present
get_date <- function(u) str_extract(u, "\\d{4}-\\d{2}-\\d{2}")
dates <- as.Date(get_date(wgs84_fra))
download_url <- wgs84_fra[which.max(dates)]

# ---- B) Download (wget equivalent) ----
dir.create("data_ign", showWarnings = FALSE)
dest_7z <- file.path("data_ign", basename(download_url))

curl::curl_download(download_url, destfile = dest_7z, mode = "wb")


###---- C) Extract the .7z ----
# You need 7-Zip installed on your system (command: 7z).
# Ubuntu: sudo apt-get install p7zip-full
out_dir <- file.path("data_ign", "admin_express_cog")
dir.create(out_dir, showWarnings = FALSE)

system2("7z", c("x", shQuote(dest_7z), paste0("-o", shQuote(out_dir)), "-y"))

# ---- D) Read the REGION layer ----
# Find the REGION shapefile inside extracted folders
shp_files <- list.files(out_dir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)
region_shp <- shp_files[str_detect(toupper(shp_files), "REGION")][1]

regions_sf <- st_read(region_shp, quiet = TRUE)


## Fixing names mismatched! 
seps_dat$region_name <- stri_trans_general(
  stri_enc_toutf8(seps_dat$region_name),
  "Latin-ASCII"
)

# Align formatting to your shapefile style + fix the truncation
seps_dat$region_name <- toupper(seps_dat$region_name)
seps_dat$region_name <- sub("PROVENCE-ALPES-COTE D'AZU$", "PROVENCE-ALPES-COTE D'AZUR", seps_dat$region_name)


regions_joined <- 
  regions_sf %>%
  left_join(seps_dat, by = c("NOM_M" = "region_name"))


g1 <- "1 - Sepsis sans choc septique"   
g2 <- "2 - Choc septique"  


left  <- plot_fr_var_insets(
  regions_joined, 
  "tx_sejours_sepsis_100k", 
  "Nombre de séjours pour sepsis des habitants de la région pour 100 000 habitants",
  group_level = g1)

right <- plot_fr_var_insets(
  regions_joined, 
  "tx_sejours_sepsis_100k", 
  "Nombre de séjours pour sepsis des habitants de la région pour 100 000 habitants",
  group_level = g2
)


wrap_plots(left | right, ncol = 1)
