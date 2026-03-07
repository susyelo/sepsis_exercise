# read data ---------------------------------------------------------------
## 1. incidence data
sep_dat <- readRDS("./data/derived/01_sepsis_dat.RDS")

## 2. INSEE data
pop_dat <- readRDS("data/derived/02_insee_population_REG.RDS")
edu_dat <- readRDS("data/derived/03_insee_education_REG.RDS")
poverty_dat <- readRDS("data/derived/02_insee_poverty_REG.RDS")


anti_join(seps_dat, poverty_region_df, by = "region_name")
### Joininig education and population data
edu_pop_dat <- 
  edu_dat %>% 
  mutate(code_region = str_pad(as.character(REG), width = 2, pad = "0")) %>% 
  select(code_region, 
         total_pop15plus) %>% 
  right_join(
    pop_dat %>% select(code_region, region_name, ensemble), 
    by = "code_region")

## joining poverty data
all_insee_dat <- 
  edu_pop_dat %>% 
  left_join(poverty_dat, by = "region_name") %>% 
  select(code_region, region_name, 
         pop_non_scol15 = total_pop15plus , 
         total_population = ensemble, 
         intensite_pauvrete_pct, 
         niveau_vie_median_annuel)

## Join sepsis incidence data
combined_all <- 
  all_insee_dat %>% 
  left_join(sep_dat)

combined_NO_chock <- 
all_insee_dat %>% 
  left_join(sep_dat %>% filter(type_sejour_sepsis == "1 - Sepsis sans choc septique"))

combined_chock <- 
  all_insee_dat %>% 
  left_join(sep_dat %>% filter(type_sejour_sepsis == "2 - Choc septique"))


# savedata ----------------------------------------------------------------
saveRDS(all_insee_dat, "./data/derived/04_all_insee_dat.RDS")
saveRDS(combined_NO_chock, "./data/derived/04_combined_NO_chock.RDS")
saveRDS(combined_chock, "./data/derived/04_combined_chock.RDS")
saveRDS(combined_all, "./data/derived/04_combined_all.RDS")


