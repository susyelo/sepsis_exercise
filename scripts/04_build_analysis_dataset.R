# read data ---------------------------------------------------------------
## 1. incidence data
sep_dat <- readRDS("./data/derived/01_sepsis_dat.RDS")
regions_joined <- readRDS("data/derived/01_sepsis_spatial.RDS")

## 2. INSEE data
pop_dat <- readRDS("data/derived/02_insee_population_REG.RDS")
edu_dat <- readRDS("data/derived/03_insee_education_REG.RDS")
poverty_dat <- readRDS("data/derived/02_insee_poverty_REG.RDS")

density_dat <- read.csv("data_raw/INSEE/density.csv")


## education data has a different region name format. I am changing it to the same format as all the data
ref_table <- data.frame(region_name_ref = unique(sep_dat$region_name))

edu_dat <- 
ref_table %>% 
  mutate(REG_NAME = gsub("-", " ", region_name_ref)) %>% 
  mutate(REG_NAME = gsub("'", " ", REG_NAME)) %>% 
  left_join(edu_dat, by = "REG_NAME") %>% 
  select(region_name = region_name_ref, edu_pop15plus)
  

pop_dat$region_name <- gsub("CENTRE-VAL-DE-LOIRE", "CENTRE-VAL DE LOIRE", pop_dat$region_name)

### Joininig education and population data
edu_pop_dat <- 
  edu_dat %>% 
  left_join(pop_dat, by = "region_name")

## joining poverty data
all_insee_dat <- 
  edu_pop_dat %>% 
  left_join(poverty_dat, by = "region_name") %>% 
  select(region_name, 
         pop_non_scol15 = edu_pop15plus , 
         total_population = total, 
         plus15_population = pop15plus,
         intensite_pauvrete_pct, 
         niveau_vie_median_annuel)

## Calculate proportion of non scolarised population over 15 years old. 
all_insee_dat <- 
  all_insee_dat %>% 
  mutate(pct_non_scol15 = pop_non_scol15 / plus15_population * 100) %>% 
  left_join(density_dat, by = "region_name")


## Join sepsis incidence data
combined_all <- 
  all_insee_dat %>% 
  full_join(sep_dat %>% 
              mutate(code_region = gsub(" .*", "", region)), by = "region_name")

combined_NO_chock <- 
all_insee_dat %>% 
  left_join(sep_dat %>% filter(type_sejour_sepsis == "1 - Sepsis sans choc septique"))

combined_chock <- 
  all_insee_dat %>% 
  left_join(sep_dat %>% filter(type_sejour_sepsis == "2 - Choc septique"))

## insee spatial 
regions_unique <- 
  regions_joined %>%
  group_by(NOM_M, INSEE_REG) %>%
  summarise(geometry = sf::st_union(geometry), .groups = "drop")

insee_spatial <- 
  regions_unique %>% 
  mutate(region_name = NOM_M) %>% 
  left_join(insee_all, by = "region_name")





# savedata ----------------------------------------------------------------
saveRDS(regions_unique, "./data/derived/04_spatial_dat.RDS")
saveRDS(all_insee_dat, "./data/derived/04_all_insee_dat.RDS")
saveRDS(insee_spatial, "data/derived/04_insee_spatial.RDS")
saveRDS(combined_NO_chock, "./data/derived/04_combined_NO_chock.RDS")
saveRDS(combined_chock, "./data/derived/04_combined_chock.RDS")
saveRDS(combined_all, "./data/derived/04_combined_all.RDS")


