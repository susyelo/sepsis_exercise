# libs --------------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)


# data --------------------------------------------------------------------
seps_dat <- read_xlsx("sepsis_resume_reg.xlsx", skip = 1)

original_labs <- names(seps_dat)

### Change the names to the variables 
## type_sejour_sepsis = "1 - Sepsis sans choc septique" "2 - Choc septique" 
## nb_sejours = number of hospital stays related to sepsis
## dms_nuitees = Durée moyenne de séjour (DMS). Average number of nights per hospitalization
## age_moyen = average age of patients hospitalized for sepsis in that region,
## tx_sejours_sepsis_100k = Sepsis hospitalization rate. (nb_sejours/pop)*1e5

seps_dat <- seps_dat %>%
  rename(
    region = `Région`,
    type_sejour_sepsis = `Type de séjours de prise en charge de sepsis`,
    nb_sejours = `Nombre de séjours`,
    age_moyen = `Age moyen`,
    dms_nuitees = `DMS en nuitées`,
    pct_deces = `% de décès`,
    tx_sejours_sepsis_100k = `Nombre de séjours pour sepsis des habitants de la région pour 100 000 habitants`
  )


labels_names <- data.frame(original = original_labs, var_names = names(seps_dat))
labels_names<- labels_names[labels_names$var_names%in%c("region", "type_sejour_sepsis")==FALSE,]


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