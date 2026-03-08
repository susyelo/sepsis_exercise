library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)
library(ggrepel)



harmonise_region <- function(x) {
  x <- x %>%
    toupper() %>% 
    str_replace_all("\n", " ") %>%
    str_squish() %>%
    str_trim()
  
  stri_trans_general(
    stri_enc_toutf8(x),
    "Latin-ASCII"
  )
  
}


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
  dir.create("./data_raw/spatial/data_ign", showWarnings = FALSE)
  dest_7z <- file.path("./data_raw/spatial/data_ign", basename(download_url))
  
  curl::curl_download(download_url, destfile = dest_7z, mode = "wb")
  
  
  ###---- C) Extract the .7z ----
  # You need 7-Zip installed on your system (command: 7z).
  # Ubuntu: sudo apt-get install p7zip-full
  out_dir <- file.path("./data_raw/spatial/data_ign", "admin_express_cog")
  dir.create(out_dir, showWarnings = FALSE)
  
  system2("7z", c("x", shQuote(dest_7z), paste0("-o", shQuote(out_dir)), "-y"))
  
  # ---- D) Read the REGION layer ----
  # Find the REGION shapefile inside extracted folders
  read_REG_data()
}


# Find the REGION shapefile inside extracted folders
read_REG_data <-function(pattern = "\\.shp$", folder = "data_ign", subfolder = "admin_express_cog"){
  
  out_dir <- file.path(folder, subfolder)
  shp_files <- list.files(out_dir, pattern = pattern, recursive = TRUE, full.names = TRUE)
  region_shp <- shp_files[str_detect(toupper(shp_files), "REGION")][1]
  
  regions_sf <- st_read(region_shp, quiet = TRUE)
  
  regions_sf
}


read_COMM_data <-function(pattern = "\\.shp$", folder = "data_ign", subfolder = "admin_express_cog"){
  
  out_dir <- file.path(folder, subfolder)
  shp_files <- list.files(out_dir, pattern = pattern, recursive = TRUE, full.names = TRUE)
  com_shp <- shp_files[str_detect(toupper(shp_files), "CHFLIEU_COMMUNE")][1]
  
  com_shp <- st_read(com_shp, quiet = TRUE)
  
  com_shp
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
                               dom = c("GUADELOUPE","MARTINIQUE","GUYANE","LA REUNION","MAYOTTE"),
                               limits = NULL,
                               show_legend = TRUE) {
  
  dat <- sf_data
  if (!is.null(group_level)) dat <- dplyr::filter(dat, .data[[group_col]] == group_level)
  
  # >>> KEY FIX: if limits not provided, compute from ALL data used in this plot (metro + DOM)
  if (is.null(limits)) {
    limits <- range(dat[[var]], na.rm = TRUE)
  }
  
  metro <- dat %>% dplyr::filter(!.data[[name_col]] %in% dom)
  domsf <- dat %>% dplyr::filter(.data[[name_col]] %in% dom)
  
  metro_2154 <- sf::st_transform(metro, 2154) %>%
    dplyr::mutate(label_pt = sf::st_point_on_surface(geometry))
  
  bb_m <- .expand_bbox(metro_2154, 0.03)
  
  scale_fill <- ggplot2::scale_fill_viridis_c(limits = limits, oob = scales::squish)
  
  p_metro <- ggplot2::ggplot(metro_2154) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data[[var]]), color = "white", linewidth = 0.15) +
    ggplot2::geom_sf_text(ggplot2::aes(geometry = label_pt, label = .data[[name_col]]), size = 2.6) +
    ggplot2::coord_sf(
      xlim = c(bb_m["xmin"], bb_m["xmax"]),
      ylim = c(bb_m["ymin"], bb_m["ymax"]),
      crs = sf::st_crs(2154),
      expand = FALSE
    ) +
    scale_fill +
    ggplot2::labs(title = group_level, fill = NULL) +
    ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = if (show_legend) "right" else "none",
      plot.margin = ggplot2::margin(2, 2, 2, 2)
    )
  
  inset_one <- function(nm) {
    x <- domsf %>% dplyr::filter(.data[[name_col]] == nm)
    if (nrow(x) == 0) return(NULL)
    bb <- .expand_bbox(x, 0.15)
    
    ggplot2::ggplot(x) +
      ggplot2::geom_sf(ggplot2::aes(fill = .data[[var]]), color = "white", linewidth = 0.15) +
      ggplot2::coord_sf(
        xlim = c(bb["xmin"], bb["xmax"]),
        ylim = c(bb["ymin"], bb["ymax"]),
        expand = FALSE
      ) +
      scale_fill +
      ggplot2::labs(title = nm) +
      ggplot2::theme_void() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 9, hjust = 0.5),
        legend.position = "none",
        plot.margin = ggplot2::margin(1, 1, 1, 1)
      )
  }
  
  insets <- lapply(dom, inset_one)
  insets <- insets[!vapply(insets, is.null, logical(1))]
  
  (patchwork::wrap_plots(insets, ncol = 1) | p_metro) +
    patchwork::plot_layout(widths = c(0.5, 4.5))
}

plot_maps_var <- function(sf_data,
                          var = "tx_sejours_sepsis_100k",
                          label_var = "Nombre de sejours pour sepsis des habitants de la region pour 100 000 habitants",
                          shared_legend = TRUE) {
  
  g1 <- "1 - Sepsis sans choc septique"
  g2 <- "2 - Choc septique"
  
  if (shared_legend) {
    # One common scale for both types -> one legend
    lims <- sf_data %>%
      dplyr::filter(type_sejour_sepsis %in% c(g1, g2)) %>%
      dplyr::pull(.data[[var]]) %>%
      range(na.rm = TRUE)
    
    p1 <- plot_fr_var_insets(sf_data, var, label_var, group_level = g1, limits = lims, show_legend = TRUE)
    p2 <- plot_fr_var_insets(sf_data, var, label_var, group_level = g2, limits = lims, show_legend = FALSE)
    
  } else {
    # Different scales (and legends) per type
    p1 <- plot_fr_var_insets(sf_data, var, label_var, group_level = g1, limits = NULL, show_legend = TRUE)
    p2 <- plot_fr_var_insets(sf_data, var, label_var, group_level = g2, limits = NULL, show_legend = TRUE)
  }
  
  (patchwork::wrap_elements(p1) | patchwork::wrap_elements(p2)) +
    patchwork::plot_annotation(title = label_var) &
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                   legend.position = "right")
}


plot_scatter_y_outliers <- function(data,
                                    xvar,
                                    yvar,
                                    group_var = NULL,
                                    label_var = "region",
                                    detect_within_group = FALSE,
                                    method = c("iqr","zscore"),
                                    outlier_side = c("both","upper","lower"),
                                    iqr_mult = 1.5,
                                    z_cut = 2.5) {
  
  method <- match.arg(method)
  outlier_side <- match.arg(outlier_side)
  
  dat <- data %>%
    mutate(.y = .data[[yvar]])
  
  # Grouping only if group_var exists
  if (!is.null(group_var) && detect_within_group) {
    dat <- dat %>% group_by(across(all_of(group_var)))
  }
  
  # ---- Outlier detection ----
  
  if (method == "iqr") {
    
    dat <- dat %>%
      mutate(
        q1 = quantile(.y, 0.25, na.rm = TRUE),
        q3 = quantile(.y, 0.75, na.rm = TRUE),
        iqr = IQR(.y, na.rm = TRUE),
        lower = q1 - iqr_mult * iqr,
        upper = q3 + iqr_mult * iqr
      )
    
    dat <- dat %>%
      mutate(
        is_outlier = case_when(
          outlier_side == "both"  ~ .y < lower | .y > upper,
          outlier_side == "upper" ~ .y > upper,
          outlier_side == "lower" ~ .y < lower
        )
      )
    
  } else {
    
    dat <- dat %>%
      mutate(
        z = (.y - mean(.y, na.rm = TRUE)) / sd(.y, na.rm = TRUE)
      )
    
    dat <- dat %>%
      mutate(
        is_outlier = case_when(
          outlier_side == "both"  ~ abs(z) > z_cut,
          outlier_side == "upper" ~ z > z_cut,
          outlier_side == "lower" ~ z < -z_cut
        )
      )
  }
  
  dat <- dat %>% ungroup()
  
  # ---- Plot ----
  
  if (is.null(group_var)) {
    
    p <- ggplot(dat, aes(x = .data[[xvar]], y = .data[[yvar]])) +
      geom_point(data = dat %>% filter(!is_outlier),
                 size = 2.5, alpha = 0.75) +
      
      geom_point(data = dat %>% filter(is_outlier),
                 shape = 21,
                 size = 4,
                 stroke = 1.2,
                 fill = "red",
                 colour = "black")
    
  } else {
    
    p <- ggplot(dat,
                aes(x = .data[[xvar]],
                    y = .data[[yvar]],
                    colour = .data[[group_var]])) +
      
      geom_point(data = dat %>% filter(!is_outlier),
                 size = 2.5,
                 alpha = 0.75) +
      
      geom_point(data = dat %>% filter(is_outlier),
                 aes(fill = .data[[group_var]]),
                 shape = 21,
                 size = 4,
                 stroke = 1.2,
                 colour = "black")
  }
  
  p +
    geom_text_repel(
      data = dat %>% filter(is_outlier),
      aes(label = .data[[label_var]]),
      colour = "black",
      size = 3.5,
      max.overlaps = Inf,
      show.legend = FALSE
    ) +
    labs(
      x = xvar,
      y = yvar,
      colour = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
}

