# 7. FIGURES FINALES ----------------------------------------------------

# -----------------------------------------------------------------------
# Préparation des objets spatiaux pour les figures
# -----------------------------------------------------------------------

sites_vect <- terra::vect(sites_sf)

emprise_sites <- terra::ext(sites_vect)

emprise_sites_buffer <- terra::ext(
  emprise_sites$xmin - 0.2,
  emprise_sites$xmax + 0.2,
  emprise_sites$ymin - 0.2,
  emprise_sites$ymax + 0.2
)

## 7.1 Figure 2 - Panel des rasters environnementaux --------------------

library(terra)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)

# -----------------------------------------------------------------------
# 1. Sélection de la période d'étude MISArbO
# -----------------------------------------------------------------------



periode_debut <- as.Date("2025-04-01")
periode_fin   <- as.Date("2025-10-23")

dates_temp <- as.Date(
  gsub("^X", "", names(temp_daily)),
  format = "%Y.%m.%d"
)

dates_precip <- as.Date(
  gsub("^X", "", names(precip_daily)),
  format = "%Y.%m.%d"
)

dates_rad <- as.Date(
  gsub("^X", "", names(rad_daily)),
  format = "%Y.%m.%d"
)

# -----------------------------------------------------------------------
# 2. Conservation des couches correspondant à la période d'étude
# -----------------------------------------------------------------------

temp_period <- temp_daily[[
   which(
     dates_temp >= periode_debut &
       dates_temp <= periode_fin
   )
]]

precip_period <- precip_daily[[
   which(
     dates_precip >= periode_debut &
       dates_precip <= periode_fin
   )
]]

rad_period <- rad_daily[[
   which(
     dates_rad >= periode_debut &
       dates_rad <= periode_fin
   )
]]

cat("\nPériode température utilisée pour Figure 2 :\n")
print(
  range(
    dates_temp[
       dates_temp >= periode_debut &
         dates_temp <= periode_fin
    ]
  )
)

cat("\nNombre de jours température Figure 2 :\n")
print(nlyr(temp_period))

cat("\nPériode précipitations utilisée pour Figure 2 :\n")
print(
  range(
    dates_precip[
       dates_precip >= periode_debut &
         dates_precip <= periode_fin
    ]
  )
)

cat("\nNombre de jours précipitations Figure 2 :\n")
print(nlyr(precip_period))

cat("\nPériode rayonnement utilisée pour Figure 2 :\n")
print(
  range(
    dates_rad[
       dates_rad >= periode_debut &
         dates_rad <= periode_fin
    ]
  )
)

cat("\nNombre de jours rayonnement Figure 2 :\n")
print(nlyr(rad_period))

# -----------------------------------------------------------------------


# 3. Création des rasters synthétiques sur la période d'étude
# -----------------------------------------------------------------------

temp_mean_period <- mean(
  temp_period,
  na.rm = TRUE
)

precip_sum_period <- sum(
  precip_period,
  na.rm = TRUE
)

rad_mean_period <- mean(
  rad_period,
  na.rm = TRUE
)

# -----------------------------------------------------------------------
# 4. Crop sur l'emprise des sites MISArbO
# -----------------------------------------------------------------------

temp_crop <- terra::crop(
  temp_mean_period,
  emprise_sites_buffer
)

precip_crop <- terra::crop(
  precip_sum_period,
  emprise_sites_buffer
)

rad_crop <- terra::crop(
  rad_mean_period,
  emprise_sites_buffer
)

bri_crop <- terra::crop(
  bri_mean,
  emprise_sites_buffer
)

culex_presence_crop <- terra::crop(
  raster_culex,
  emprise_sites_buffer
)

culex_abund_crop <- terra::crop(
  raster_culex_abund,
  emprise_sites_buffer
)

# -----------------------------------------------------------------------
# 5. Trait de côte Méditerranée
# -----------------------------------------------------------------------

world <- ne_countries(
  scale = "medium",
  returnclass = "sf"
)

coast_sf <- world %>%
  filter(admin %in% c("France", "Spain")) %>%
  st_transform(4326)

coast_vect <- terra::vect(coast_sf)

coast_lines <- terra::as.lines(coast_vect)

# -----------------------------------------------------------------------
# 6. Export Figure 2
# -----------------------------------------------------------------------

png(
  filename = file.path(
     out_dir_figures,
     "Figure_2_panel_rasters_environnementaux.png"
  ),
  width = 6000,
  height = 4000,
  res = 300
)



par(
  mfrow = c(2, 3),
  mar = c(3, 3, 4, 5),
  cex.main = 1.25
)

# A. Température

plot(
  temp_crop,
  main = "A. Température moyenne (°C)"
)

plot(
  sites_vect,
  add = TRUE,
  pch = 16,
  cex = 0.35,
  col = "black"
)

plot(
  coast_lines,
  add = TRUE,
  col = "black",
  lwd = 1
)

# B. Précipitations

plot(
  precip_crop,
  main = "B. Précipitations cumulées (mm)"
)

plot(
  sites_vect,
  add = TRUE,
  pch = 16,
  cex = 0.35,
  col = "black"
)

plot(
  coast_lines,
  add = TRUE,
  col = "black",
  lwd = 1
)

# C. Rayonnement solaire

plot(
  rad_crop,
  main = "C. Rayonnement solaire moyen (MJ/m²/j)"
)

plot(
  sites_vect,
  add = TRUE,
  pch = 16,
  cex = 0.35,
  col = "black"
)

plot(
  coast_lines,
  add = TRUE,
  col = "black",
  lwd = 1
)

# D. Bird Risk Index

plot(
  bri_crop,
  main = "D. Bird Risk Index moyen"
)

plot(
  sites_vect,
  add = TRUE,
  pch = 16,


    cex = 0.35,
    col = "black"
)

plot(
  coast_lines,
  add = TRUE,
  col = "black",
  lwd = 1
)

# E. Probabilité de présence de Culex pipiens

plot(
  culex_presence_crop,
  main = "E. Probabilité de présence de Culex pipiens"
)

plot(
  sites_vect,
  add = TRUE,
  pch = 16,
  cex = 0.35,
  col = "black"
)

plot(
  coast_lines,
  add = TRUE,
  col = "black",
  lwd = 1
)

# F. Abondance prédite de Culex pipiens

plot(
  culex_abund_crop,
  main = "F. Abondance prédite de Culex pipiens"
)

plot(
  sites_vect,
  add = TRUE,
  pch = 16,
  cex = 0.35,
  col = "black"
)

plot(
  coast_lines,
  add = TRUE,
  col = "black",
  lwd = 1
)

dev.off()

cat("\nFigure 2 exportée :\n")
print(
  file.path(
    out_dir_figures,
    "Figure_2_panel_rasters_environnementaux.png"
  )
)

## 7.2 Figures spatiales et cartographies ------------------------------

# Préparation raster BRI pour les cartographies

bri_mean_crop <- terra::crop(
  bri_mean,
  emprise_sites_buffer
)

# Préparation des sites positifs WNV / USUV pour les cartographies

sites_wnv_pos <- DB_WNV %>%
  filter(Pos_Neg_WNV == "POS")

sites_usuv_pos <- DB_USUV %>%
  filter(Pos_Neg_USUV == "POS")

sites_wnv_pos_sf <- st_as_sf(


    sites_wnv_pos,
    coords = c("x", "y"),
    crs = 4326,
    remove = FALSE
)

sites_usuv_pos_sf <- st_as_sf(
  sites_usuv_pos,
  coords = c("x", "y"),
  crs = 4326,
  remove = FALSE
)

sites_wnv_pos_vect <- terra::vect(sites_wnv_pos_sf)

sites_usuv_pos_vect <- terra::vect(sites_usuv_pos_sf)

cat("\nNombre de prélèvements/sites WNV positifs cartographiés :\n")
print(nrow(sites_wnv_pos))

cat("\nNombre de prélèvements/sites USUV positifs cartographiés :\n")
print(nrow(sites_usuv_pos))

### 7.2.1 Carte simple des sites WNV positifs --------------------------

plot(

    sites_vect,

    pch = 16,

    cex = 0.5,

    col = "grey40",

    main = "Sites MISArbO et détections WNV positives"

)

plot(

    sites_wnv_pos_vect,

    add = TRUE,

    pch = 16,

    cex = 1,

    col = "red"

)



### 7.2.2 Carte WNV et raster abondance Culex --------------------------

# Sites WNV positifs

sites_wnv_pos <- DB_WNV %>%
  filter(Pos_Neg_WNV == "POS")

# Conversion en objet spatial

sites_wnv_pos_sf <- st_as_sf(
  sites_wnv_pos,
  coords = c("x", "y"),
  crs = 4326,
  remove = FALSE
)

sites_wnv_pos_vect <- terra::vect(
  sites_wnv_pos_sf
)

# Affichage raster abondance Culex

plot(
  culex_abund_crop,
  main = "Détections WNV positives et abondance écologique de Culex"
)

# Tous les sites MISArbO



plot(
  sites_vect,
  add = TRUE,
  pch = 16,
  cex = 0.4,
  col = "black"
)

# Sites WNV positifs

plot(
  sites_wnv_pos_vect,
  add = TRUE,
  pch = 16,
  cex = 1,
  col = "red"
)

# Légende

legend(
  "bottomleft",
  legend = c(
     "Tous les sites",
     "Sites WNV positifs"
  ),
  pch = 16,
  col = c(
     "black",
     "red"
  ),
  bty = "n"
)

### 7.2.3 Carte simple des sites USUV positifs -------------------------

plot(

    sites_vect,

    pch = 16,

    cex = 0.5,

    col = "grey40",

    main = "Sites MISArbO et détections USUV positives"

)

plot(

    sites_usuv_pos_vect,

    add = TRUE,

    pch = 16,

    cex = 1,

    col = "red"

)

### 7.2.4 Carte USUV et raster abondance Culex -------------------------

# Sites USUV positifs

sites_usuv_pos <- DB_USUV %>%
  filter(Pos_Neg_USUV == "POS")

# Conversion en objet spatial

sites_usuv_pos_sf <- st_as_sf(
  sites_usuv_pos,
  coords = c("x", "y"),
  crs = 4326,
  remove = FALSE
)

sites_usuv_pos_vect <- terra::vect(
  sites_usuv_pos_sf
)



# Affichage raster abondance Culex

plot(
  culex_abund_crop,
  main = "Détections USUV positives et abondance écologique de Culex pipiens"
)

# Tous les sites MISArbO

plot(
  sites_vect,
  add = TRUE,
  pch = 16,
  cex = 0.4,
  col = "black"
)

# Sites USUV positifs

plot(
  sites_usuv_pos_vect,
  add = TRUE,
  pch = 16,
  cex = 1,
  col = "red"
)

# Légende

legend(
  "bottomleft",
  legend = c(
     "Tous les sites",
     "Sites USUV positifs"
  ),
  pch = 16,
  col = c(
     "black",
     "red"
  ),
  bty = "n"
)

### 7.2.5 Carte finale WNV sur fond BRI --------------------------------

# Palette couleurs

cols_bri <- hcl.colors(

    100,

    palette = "YlGnBu"

)



plot(

    bri_mean_crop,

    col = cols_bri,

    main = "Sites WNV positifs sur fond Bird Risk Index",

    axes = TRUE

)

# Tous les sites

plot(

    sites_vect,

    add = TRUE,

    pch = 16,

    cex = 0.5,

    col = "grey30"



)

# Sites WNV positifs

plot(

    sites_wnv_pos_vect,

    add = TRUE,

    pch = 16,

    cex = 1,

    col = "red"

)

# Légende

legend(

    "bottomleft",

    legend = c(

      "Tous les sites",

      "Sites WNV+"

    ),

    pch = 16,

    col = c(

      "grey30",

      "red"

    ),

    bty = "n"

)




### 7.2.6 Carte finale USUV sur fond BRI -------------------------------

plot(

    bri_mean_crop,

    col = cols_bri,

    main = "Sites USUV positifs sur fond Bird Risk Index",

    axes = TRUE

)



# Tous les sites

plot(

    sites_vect,

    add = TRUE,

    pch = 16,

    cex = 0.5,

    col = "grey30"

)



# Sites USUV positifs

plot(



    sites_usuv_pos_vect,

    add = TRUE,

    pch = 16,

    cex = 1.2,

    col = "darkorange"

)



legend(

    "bottomleft",

    legend = c(

      "Tous les sites",

      "Sites USUV+"

    ),

    pch = 16,

    col = c(

      "grey30",

      "darkorange"

    ),

    bty = "n"

)

