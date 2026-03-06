library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)


plot_one_var <- function(data, 
                         var_name, 
                         var_label,
                         region_col = "region",
                         group_col  = "type_sejour_sepsis") {
  
  ggplot(
    data,
    aes(x = reorder(.data[[region_col]], .data[[var_name]]),
        y = .data[[var_name]], 
        fill = .data[[group_col]])
  ) +
    geom_col(position = "dodge", width = 0.75) +
    coord_flip() +
    labs(x = NULL, y = var_label, fill = NULL) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}


## read regions spatial data (shapefiles)


# Find the REGION shapefile inside extracted folders
read_REG_data <-function(pattern = "\\.shp$", folder = "data_ign", subfolder = "admin_express_cog"){
  
  out_dir <- file.path(folder, subfolder)
  shp_files <- list.files(out_dir, pattern = pattern, recursive = TRUE, full.names = TRUE)
  region_shp <- shp_files[str_detect(toupper(shp_files), "REGION")][1]
  
  regions_sf <- st_read(region_shp, quiet = TRUE)
  
  regions_sf
}


# Map regional variables France -------------------------------------------
download_FRA_REG <-function(page_url = "https://geoservices.ign.fr/telechargement-api/ADMIN-EXPRESS-COG?page=1"){
  
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
  read_REG_data()
}



# helper: expand bbox a bit (so polygons don't touch panel edges)
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
                               group_level = NULL,
                               dom = c("GUADELOUPE","MARTINIQUE","GUYANE","LA REUNION","MAYOTTE")) {
  
  dat <- sf_data
  if (!is.null(group_level)) dat <- dat %>% filter(.data[[group_col]] == group_level)
  
  # Transform to avoid domination for counts/rates
  v <- dat[[var]]
  v_plot = v
  #v_plot <- if (var %in% c("nb_sejours", "tx_sejours_sepsis_100k")) log1p(v) else v
  dat <- dat %>% mutate(.vplot = v_plot)
  
  # Quantile classes (robust, map-friendly)
  br <- unique(quantile(dat$.vplot, probs = seq(0, 1, 0.2), na.rm = TRUE))
  if (length(br) < 3) br <- pretty(range(dat$.vplot, na.rm = TRUE), n = 5)
  dat <- dat %>% mutate(.class = cut(.vplot, breaks = br, include.lowest = TRUE))
  
  metro <- dat %>% filter(!.data[[name_col]] %in% dom) 
  domsf <- dat %>% filter(.data[[name_col]] %in% dom)
  
  
  metro_2154 <- st_transform(metro, 2154) %>%
    mutate(label_pt = st_point_on_surface(geometry))
  
  
  bb_m <- .expand_bbox(metro, 0.03)
  
  p_metro <- ggplot(metro_2154) +
    geom_sf(aes(fill = .class), color = "white", linewidth = 0.15) +
    geom_sf_text(aes(geometry = label_pt, label = NOM_M), size = 2.6) +
    coord_sf(crs = st_crs(2154), expand = FALSE) +
    coord_sf(xlim = c(bb_m["xmin"], bb_m["xmax"]),
             ylim = c(bb_m["ymin"], bb_m["ymax"]),
             expand = FALSE) +
    scale_fill_viridis_d(drop = FALSE) +
    labs(
      title = paste0(if (!is.null(group_level)) paste0(" — ", group_level) else ""),
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
  
  (wrap_plots(insets, ncol = 1) | p_metro) + plot_layout(widths = c(0.5, 4.5))
}

plot_maps_var <- function(sf_data, 
                          var = "tx_sejours_sepsis_100k", 
                          label_var = "Nombre de séjours pour sepsis des habitants de la région pour 100 000 habitants"){
  
  g1 <- "1 - Sepsis sans choc septique"   
  g2 <- "2 - Choc septique"  
  
  p1  <- plot_fr_var_insets(
    sf_data, 
    var, 
    label_var,
    group_level = g1)
  
  p2 <- plot_fr_var_insets(
    sf_data, 
    var, 
    label_var,
    group_level = g2
  )
  
  (wrap_elements(p1) | wrap_elements(p2)) + 
    plot_layout(guides = "keep") +
    plot_annotation(
      title = label_var
    ) &
    theme(
      plot.title = element_text(hjust = 0.5)  # centers titles (global + panel titles)
    )
  
  
}
