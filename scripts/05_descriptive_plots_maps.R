library(patchwork)
library(ggplot2)

# plot sepsis incidence ---------------------------------------------------
seps_dat <- readRDS("./data/derived/01_sepsis_dat.RDS")

original_labs <- readRDS("./data/derived/01_original_sepsis_labels.RDS")

### plots
to_plot <- names(select_if(seps_dat, is.numeric))

labels_names <- data.frame(original = original_labs, var_names = names(seps_dat))
labels_names<- labels_names[labels_names$var_names%in%to_plot,]


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


## Map each reponse variable
regions_joined <- readRDS("./data/derived/01_sepsis_spatial.RDS")

plot_maps_var(regions_joined, 
              var = "tx_sejours_sepsis_100k", 
              label_var = "Nombre de séjours pour sepsis des habitants de la région pour 100 000 habitants")


plot_maps_var(regions_joined, 
              var = "tx_sejours_sepsis_100k", 
              label_var = "Nombre de séjours pour sepsis des habitants de la région pour 100 000 habitants", 
              shared_legend = FALSE)

