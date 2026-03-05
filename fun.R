library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)

# helper: expand bbox a bit (so polygons don't touch panel edges)
.expand_bbox <- function(x, expand = 0.05) {
  bb <- st_bbox(x)
  xr <- as.numeric(bb$xmax - bb$xmin)
  yr <- as.numeric(bb$ymax - bb$ymin)
  st_bbox(c(
    xmin = bb$xmin - expand * xr,
    ymin = bb$ymin - expand * yr,
    xmax = bb$xmax + expand * xr,
    ymax = bb$ymax + expand * yr
  ), crs = st_crs(x))
}

.expand_bbox <- function(x, expand = 0.05) {
  bb <- st_bbox(x)
  xr <- as.numeric(bb$xmax - bb$xmin)
  yr <- as.numeric(bb$ymax - bb$ymin)
  st_bbox(c(
    xmin = bb$xmin - expand * xr, ymin = bb$ymin - expand * yr,
    xmax = bb$xmax + expand * xr, ymax = bb$ymax + expand * yr
  ), crs = st_crs(x))
}

plot_fr_var_insets <- function(sf_data, var, label,
                               name_col = "NOM_M",
                               group_col = "type_sejour_sepsis",
                               group_level = NULL,     # e.g. "Avec choc" / "Sans choc"
                               dom = c("GUADELOUPE","MARTINIQUE","GUYANE","LA REUNION","MAYOTTE")) {
  
  dat <- sf_data
  if (!is.null(group_level)) dat <- dat %>% filter(.data[[group_col]] == group_level)
  
  # Transform to avoid domination for counts/rates
  v <- dat[[var]]
  v_plot <- if (var %in% c("nb_sejours", "tx_sejours_sepsis_100k")) log1p(v) else v
  dat <- dat %>% mutate(.vplot = v_plot)
  
  # Quantile classes (robust, map-friendly)
  br <- unique(quantile(dat$.vplot, probs = seq(0, 1, 0.2), na.rm = TRUE))
  if (length(br) < 3) br <- pretty(range(dat$.vplot, na.rm = TRUE), n = 5)
  dat <- dat %>% mutate(.class = cut(.vplot, breaks = br, include.lowest = TRUE))
  
  metro <- dat %>% filter(!.data[[name_col]] %in% dom)
  domsf <- dat %>% filter(.data[[name_col]] %in% dom)
  
  bb_m <- .expand_bbox(metro, 0.03)
  
  p_metro <- ggplot(metro) +
    geom_sf(aes(fill = .class), color = "white", linewidth = 0.15) +
    coord_sf(xlim = c(bb_m["xmin"], bb_m["xmax"]),
             ylim = c(bb_m["ymin"], bb_m["ymax"]),
             expand = FALSE) +
    scale_fill_viridis_d(drop = FALSE) +
    labs(
      title = paste0(label, if (!is.null(group_level)) paste0(" — ", group_level) else ""),
      fill = NULL
    ) +
    theme_void() +
    theme(legend.position = "right", plot.margin = margin(2,2,2,2))
  
  inset_one <- function(nm) {
    x <- domsf %>% filter(.data[[name_col]] == nm)
    if (nrow(x) == 0) return(NULL)
    bb <- .expand_bbox(x, 0.15)
    
    ggplot(x) +
      geom_sf(aes(fill = .class), color = "white", linewidth = 0.15) +
      coord_sf(xlim = c(bb["xmin"], bb["xmax"]),
               ylim = c(bb["ymin"], bb["ymax"]),
               expand = FALSE) +
      scale_fill_viridis_d(drop = FALSE) +
      labs(title = nm) +
      theme_void() +
      theme(
        plot.title = element_text(size = 9, hjust = 0.5),
        legend.position = "none",
        plot.margin = margin(1,1,1,1)
      )
  }
  
  insets <- lapply(dom, inset_one)
  insets <- insets[!vapply(insets, is.null, logical(1))]
  
  (p_metro | wrap_plots(insets, ncol = 1)) + plot_layout(widths = c(4.2, 1.1))
}