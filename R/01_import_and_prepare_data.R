# 1. IMPORT ET PREPARATION DES DONNEES --------------------------------

## 1.1 MISARBO ---------------------------------------------------------

### 1.1.1 Import MISARBO brut ------------------------------------------

db_raw <- read.csv2(
  path_misarbo_raw,
  stringsAsFactors = FALSE
)

cat("\nBase brute MISArbO importée :\n")
print(dim(db_raw))

cat("\nAperçu des colonnes de la base brute :\n")
print(names(db_raw))

### 1.1.2 Nettoyage MISARBO --------------------------------------------

db_misarbo <- db_raw



cat("\nDimensions base brute avant nettoyage :\n")
print(dim(db_misarbo))

cat("\nRépartition mission :\n")
print(table(db_misarbo$mission, useNA = "ifany"))

cat("\nRépartition région :\n")
print(table(db_misarbo$region, useNA = "ifany"))

cat("\nRépartition MatrixType :\n")
print(table(db_misarbo$MatrixType, useNA = "ifany"))

cat("\nRépartition type :\n")
print(table(db_misarbo$type, useNA = "ifany"))

cat("\nRépartition année date_releve brute :\n")
date_tmp <- as.POSIXct(
  db_misarbo$date_releve,
  format = "%Y/%m/%d %H:%M:%S",
  tz = "UTC"
)
print(table(format(as.Date(date_tmp), "%Y"), useNA = "ifany"))

### 1.1.3 Filtrage MISARBO 2025 Occitanie ------------------------------

# Conversion des dates

db_misarbo$date_releve_dt <- as.POSIXct(
  db_misarbo$date_releve,
  format = "%Y/%m/%d %H:%M:%S",
  tz = "UTC"
)

db_misarbo$date_only <- as.Date(db_misarbo$date_releve_dt)

# Extraction de l’année

db_misarbo$annee <- format(db_misarbo$date_only, "%Y")

# Contrôle dimensions avant filtrage

cat("\nDimensions avant filtrage :\n")
print(dim(db_misarbo))

# Filtrage analytique principal
# Conservation uniquement :
# - projet MISARBO
# - région Occitanie
# - année 2025
# - matrices Zones humides et Xénomonitoring
# - échantillonnage de réseau uniquement
# - période analytique avril à octobre 2025

db_misarbo <- db_misarbo %>%
  filter(
    mission == "MISARBO",
    region == "OCCITANIE",
    annee == "2025",
    MatrixType %in% c("Zones humides", "Xénomonitoring"),
    CaractereEchant == "Reseau",
    date_only >= as.Date("2025-04-01"),
    date_only <= as.Date("2025-10-23")
  )

# Contrôle dimensions après filtrage

cat("\nDimensions après filtrage complet :\n")
print(dim(db_misarbo))

# Contrôle matrices conservées

cat("\nMatrices conservées :\n")
print(table(db_misarbo$MatrixType, useNA = "ifany"))

# Contrôle caractère d’échantillonnage

cat("\nCaractereEchant conservé :\n")
print(table(db_misarbo$CaractereEchant, useNA = "ifany"))

# Contrôle période analytique

cat("\nPériode conservée :\n")


print(range(db_misarbo$date_only, na.rm = TRUE))

# Contrôle nombre de sites

cat("\nNombre de sites conservés :\n")
print(length(unique(db_misarbo$site_acronyme)))

### 1.1.4 Nettoyage des variables principales --------------------------

db_misarbo$x <- as.numeric(gsub(",", ".", db_misarbo$x))
db_misarbo$y <- as.numeric(gsub(",", ".", db_misarbo$y))

db_misarbo$site_acronyme <- as.character(db_misarbo$site_acronyme)
db_misarbo$nom_commune <- as.character(db_misarbo$nom_commune)
db_misarbo$MatrixType <- as.character(db_misarbo$MatrixType)



### 1.1.5 Variables de positivité --------------------------------------

db_misarbo$Pos_Neg_WNV <- factor(
  db_misarbo$Pos_Neg_WNV,
  levels = c("NEG", "POS")
)

db_misarbo$Pos_Neg_USUV <- factor(
  db_misarbo$Pos_Neg_USUV,
  levels = c("NEG", "POS")
)

db_misarbo$WNV_bin <- ifelse(db_misarbo$Pos_Neg_WNV == "POS", 1, 0)
db_misarbo$USUV_bin <- ifelse(db_misarbo$Pos_Neg_USUV == "POS", 1, 0)



### 1.1.6 Variables temporelles ----------------------------------------

db_misarbo$mois_num <- format(db_misarbo$date_only, "%m")

db_misarbo$mois <- factor(
  db_misarbo$mois_num,
  levels = c("04", "05", "06", "07", "08", "09", "10"),
  labels = c("Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre")
)

db_misarbo$semaine <- format(db_misarbo$date_only, "%Y-%U")



### 1.1.7 Coordonnées et sites -----------------------------------------

sites_misarbo <- db_misarbo %>%
  group_by(site_acronyme) %>%
  summarise(
    nom_commune = first(nom_commune),
    x = mean(x, na.rm = TRUE),
    y = mean(y, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nNombre de sites MISARBO :\n")
print(nrow(sites_misarbo))



### 1.1.8 Variables entomologiques -------------------------------------

entomo_vars <- names(db_misarbo)[grepl("adultes_|nbr_ad", names(db_misarbo))]

cat("\nVariables entomologiques détectées :\n")
print(entomo_vars)



### 1.1.9 Contrôles qualité MISARBO ------------------------------------

cat("\nContrôle WNV :\n")
print(table(db_misarbo$Pos_Neg_WNV, useNA = "ifany"))

cat("\nContrôle USUV :\n")
print(table(db_misarbo$Pos_Neg_USUV, useNA = "ifany"))

cat("\nContrôle MatrixType :\n")
print(table(db_misarbo$MatrixType, useNA = "ifany"))

cat("\nContrôle mois :\n")
print(table(db_misarbo$mois, useNA = "ifany"))



cat("\nNombre de lignes MISARBO prêtes :\n")
print(nrow(db_misarbo))

## 1.2 METEO -----------------------------------------------------------

### 1.2.1 Inventaire des fichiers météo ERA5-Land ----------------------

# Objectif :
# identifier les fichiers météo disponibles avant extraction/jointure
# avec la base MISARBO filtrée.

cat("\nFichiers température ERA5-Land :\n")
print(list.files(dir_temperature))

cat("\nFichiers précipitations ERA5-Land :\n")
print(list.files(dir_precipitation))

cat("\nFichiers rayonnement ERA5-Land :\n")
print(list.files(dir_radiation))

### 1.2.2 Vérification des fichiers météo ERA5-Land --------------------

# Lecture test d’un fichier par variable météo
# Objectif : vérifier les noms de variables NetCDF et la structure spatiale/temporelle.

temp_test <- terra::rast(file.path(dir_temperature, "temp_2025_04.nc"))
precip_test <- terra::rast(file.path(dir_precipitation, "precip_2025_04.nc"))
rad_test <- terra::rast(file.path(dir_radiation, "rad_2025_04.nc"))

cat("\nStructure fichier température test :\n")
print(temp_test)

cat("\nStructure fichier précipitation test :\n")
print(precip_test)

cat("\nStructure fichier rayonnement test :\n")
print(rad_test)

cat("\nNoms des couches température :\n")
print(names(temp_test)[1:5])

cat("\nNoms des couches précipitation :\n")
print(names(precip_test)[1:5])

cat("\nNoms des couches rayonnement :\n")
print(names(rad_test)[1:5])



### 1.2.3 Variables climatiques Copernicus -----------------------------

# Fonction pour extraire les dates horaires depuis les noms des couches ERA5

extract_era5_time <- function(r) {

    if (any(grepl("valid_time=", names(r)))) { # ancienne version Terra : t2m_valid_time=1743465600
      timestamps <- as.numeric(
        gsub(".*valid_time=", "", names(r))
      )
    } else {
      timestamps <- terra::time(r) # nouveau version Terra : t2m_1, t2m_2, etc.
      return(as.POSIXct(timestamps, tz = "UTC"))
    }
    as.POSIXct(
      timestamps,
      origin = "1970-01-01",
      tz = "UTC"
    )
}



# Fonction d’agrégation journalière température
# t2m : Kelvin -> degrés Celsius
# Agrégation : moyenne journalière

daily_temperature <- function(r) {

    times <- extract_era5_time(r)

    dates <- as.Date(times)

    r_celsius <- r - 273.15

    tapp(


        r_celsius,
        index = dates,
        fun = mean,
        na.rm = TRUE
    )
}



# Fonction d’agrégation journalière précipitations
# tp : mètres -> millimètres
# Agrégation : maximum journalier

daily_precipitation <- function(r) {

    times <- extract_era5_time(r)

    dates <- as.Date(times)

    r_mm <- r * 1000

    tapp(
      r_mm,
      index = dates,
      fun = max,
      na.rm = TRUE
    )
}



# Fonction d’agrégation journalière rayonnement solaire
# ssrd : J/m² -> MJ/m²/jour
# Agrégation : maximum journalier

daily_radiation <- function(r) {

    times <- extract_era5_time(r)

    dates <- as.Date(times)

    r_mj <- r / 1e6

    tapp(
      r_mj,
      index = dates,
      fun = max,
      na.rm = TRUE
    )
}

### 1.2.4 Agrégation temporelle météo ----------------------------------

# Liste des fichiers ERA5-Land

temp_files <- list.files(
  dir_temperature,
  pattern = "\\.nc$",
  full.names = TRUE
)

precip_files <- list.files(
  dir_precipitation,
  pattern = "\\.nc$",
  full.names = TRUE
)

rad_files <- list.files(
  dir_radiation,
  pattern = "\\.nc$",
  full.names = TRUE
)

cat("\nNombre fichiers température :\n")
print(length(temp_files))

cat("\nNombre fichiers précipitations :\n")
print(length(precip_files))

cat("\nNombre fichiers rayonnement :\n")
print(length(rad_files))



# Import et agrégation journalière température



temp_daily_list <- lapply(temp_files, function(f) {

  r <- rast(f)

  daily_temperature(r)
})

temp_daily <- rast(temp_daily_list)

cat("\nRaster température journalière :\n")
print(temp_daily)



# Import et agrégation journalière précipitations

precip_daily_list <- lapply(precip_files, function(f) {

  r <- rast(f)

  daily_precipitation(r)
})

precip_daily <- rast(precip_daily_list)

cat("\nRaster précipitations journalières :\n")
print(precip_daily)



# Import et agrégation journalière rayonnement

rad_daily_list <- lapply(rad_files, function(f) {

  r <- rast(f)

  daily_radiation(r)
})

rad_daily <- rast(rad_daily_list)

cat("\nRaster rayonnement journalier :\n")
print(rad_daily)

### 1.2.5 Extraction météo sur fenêtres pré-prélèvement ----------------

# Préparation des points de prélèvement
points_misarbo <- terra::vect(
  db_misarbo,
  geom = c("x", "y"),
  crs = "EPSG:4326"
)

# Import des rasters horaires complets
temp_hourly <- rast(temp_files)
precip_hourly <- rast(precip_files)
rad_hourly <- rast(rad_files)

# Attribution des temps horaires aux rasters
time(temp_hourly) <- extract_era5_time(temp_hourly)
time(precip_hourly) <- extract_era5_time(precip_hourly)
time(rad_hourly) <- extract_era5_time(rad_hourly)

# Conversion des unités
temp_hourly <- temp_hourly - 273.15       # Kelvin -> °C
precip_hourly <- precip_hourly * 1000     # m -> mm
rad_hourly <- rad_hourly / 1e6            # J/m² -> MJ/m²

# Fonction d'extraction sur fenêtre temporelle avant prélèvement
extract_window_value <- function(r, points_vect, date_time, window_hours, fun) {

  r_time <- time(r)
  values <- rep(NA_real_, length(date_time))

  for (i in seq_along(date_time)) {

     t_end <- date_time[i]
     t_start <- t_end - lubridate::hours(window_hours)

     layer_id <- which(r_time > t_start & r_time <= t_end)

     if (length(layer_id) > 0) {

       r_window <- r[[layer_id]]



            r_agg <- tapp(
              r_window,
              index = rep(1, nlyr(r_window)),
              fun = fun,
              na.rm = TRUE
            )

            extracted <- terra::extract(
              r_agg,
              points_vect[i, ],
              ID = FALSE,
              search_radius = 20000
            )

            values[i] <- extracted[[1]]
        }
    }

    values
}

# Température moyenne sur les 15 jours précédant le prélèvement
db_misarbo$cop_temp_mean_15d <- extract_window_value(
  temp_hourly,
  points_misarbo,
  db_misarbo$date_releve_dt,
  window_hours = 360,
  fun = mean
)

# Précipitations cumulées sur les 15 jours précédant le prélèvement
db_misarbo$cop_precip_sum_15d <- extract_window_value(
  precip_hourly,
  points_misarbo,
  db_misarbo$date_releve_dt,
  window_hours = 360,
  fun = sum
)

# Rayonnement solaire cumulé sur les 24 h précédant le prélèvement
db_misarbo$cop_rad_sum_24h <- extract_window_value(
  rad_hourly,
  points_misarbo,
  db_misarbo$date_releve_dt,
  window_hours = 24,
  fun = sum
)

### 1.2.6 Contrôles qualité météo --------------------------------------

meteo_vars <- c(
  "cop_temp_mean_15d",
  "cop_precip_sum_15d",
  "cop_rad_sum_24h"
)

cat("\nRésumé variables météo pré-prélèvement retenues :\n")
print(summary(db_misarbo[meteo_vars]))

cat("\nNA variables météo pré-prélèvement retenues :\n")
print(colSums(is.na(db_misarbo[meteo_vars])))

cat("\nPlage température moyenne 15 jours :\n")
print(range(db_misarbo$cop_temp_mean_15d, na.rm = TRUE))

cat("\nPlage précipitations cumulées 15 jours :\n")
print(range(db_misarbo$cop_precip_sum_15d, na.rm = TRUE))

### 1.2.7 Export base météo intégrée -------------------------------

path_export_meteo <- file.path(
  out_dir_dbfull,
  "DB_MISARBO_2025_METEO.csv"
)

write.csv(
  db_misarbo,
  path_export_meteo,
  row.names = FALSE
)

cat("\nBase MISARBO + météo exportée :\n")
print(path_export_meteo)



cat("\nDimensions base exportée :\n")
print(dim(db_misarbo))

## 1.3 BIRD RISK INDEX -------------------------------------------------



### 1.3.1 Import rasters BRI -------------------------------------------

# Import des rasters Bird Risk Index (BRI)
# BRI_mean : risque aviaire moyen
# BRI_CV   : variabilité du risque aviaire

bri_mean <- rast(path_bri_mean)

bri_cv <- rast(path_bri_cv)



cat("\nRaster BRI mean importé :\n")
print(bri_mean)

cat("\nRaster BRI CV importé :\n")
print(bri_cv)

### 1.3.2 Vérification structure rasters BRI ----------------------------

cat("\nRésumé raster BRI mean :\n")
print(summary(bri_mean))

cat("\nRésumé raster BRI CV :\n")
print(summary(bri_cv))

cat("\nEmprise raster BRI mean :\n")
print(ext(bri_mean))

cat("\nEmprise raster BRI CV :\n")
print(ext(bri_cv))

cat("\nProjection raster BRI mean :\n")
print(crs(bri_mean))

cat("\nProjection raster BRI CV :\n")
print(crs(bri_cv))

### 1.3.3 Préparation des points MISARBO --------------------------------

# Création des points MISARBO pour extraction du BRI
# Les rasters BRI sont en WGS84 / EPSG:4326, comme les coordonnées x/y.

points_bri <- terra::vect(
  sites_misarbo,
  geom = c("x", "y"),
  crs = "EPSG:4326"
)

cat("\nPoints MISARBO préparés pour extraction BRI :\n")
print(points_bri)

cat("\nNombre de sites pour extraction BRI :\n")
print(nrow(sites_misarbo))

cat("\nCRS points MISARBO :\n")
print(crs(points_bri))

### 1.3.4 Extraction BRI aux sites --------------------------------------

# Extraction robuste des valeurs BRI aux sites MISARBO.
# search_radius récupère la cellule non-NA la plus proche si le point tombe
# sur une cellule littorale/lagunaire sans valeur.

extract_bri_value <- function(r, points_vect) {

    extracted <- terra::extract(
      r,
      points_vect,
      ID = FALSE,
      search_radius = 20000
    )

    extracted[[1]]
}

# Création de la table sites enrichie avec les indicateurs BRI


sites_bri <- sites_misarbo

sites_bri$bri_mean <- extract_bri_value(
  bri_mean,
  points_bri
)

sites_bri$bri_cv <- extract_bri_value(
  bri_cv,
  points_bri
)

cat("\nAperçu table sites + BRI :\n")
print(head(sites_bri))

cat("\nRésumé BRI mean :\n")
print(summary(sites_bri$bri_mean))

cat("\nRésumé BRI CV :\n")
print(summary(sites_bri$bri_cv))

cat("\nSites avec BRI manquant :\n")
print(
  sites_bri %>%
    filter(is.na(bri_mean) | is.na(bri_cv))
)

### 1.3.5 Jointure BRI à la base météo ----------------------------------

# Jointure des variables BRI site-niveau à la base MISARBO + météo.
# Les valeurs BRI sont constantes pour un site donné.

db_misarbo <- db_misarbo %>%
  left_join(
    sites_bri %>%
      select(site_acronyme, bri_mean, bri_cv),
    by = "site_acronyme"
  )

cat("\nDimensions après jointure BRI :\n")
print(dim(db_misarbo))

cat("\nColonnes BRI ajoutées :\n")
print(names(db_misarbo)[grepl("bri", names(db_misarbo))])

### 1.3.6 Contrôles qualité BRI -----------------------------------------

bri_vars <- c(
  "bri_mean",
  "bri_cv"
)

cat("\nRésumé variables BRI dans db_misarbo :\n")
print(summary(db_misarbo[bri_vars]))

cat("\nNA BRI :\n")
print(colSums(is.na(db_misarbo[bri_vars])))

cat("\nRésumé BRI par site :\n")
print(
  db_misarbo %>%
    group_by(site_acronyme) %>%
    summarise(
       bri_mean = first(bri_mean),
       bri_cv = first(bri_cv),
       .groups = "drop"
    ) %>%
    summary()
)

### 1.3.7 Export base météo + BRI ---------------------------------------

path_export_meteo_bri <- file.path(
  out_dir_dbfull,
  "DB_MISARBO_2025_METEO_BRI.csv"
)

write.csv(
  db_misarbo,
  path_export_meteo_bri,
  row.names = FALSE
)



cat("\nBase MISARBO + météo + BRI exportée :\n")
print(path_export_meteo_bri)

cat("\nDimensions base exportée :\n")
print(dim(db_misarbo))



## 1.4 CORINE LAND COVER ----------------------------------------------

### 1.4.1 Import tables CLC ---------------------------------------------

library(readxl)

cat("\nPrésence fichier CLC :\n")
print(file.exists(path_clc))

cat("\nFeuilles disponibles dans le fichier CLC :\n")
print(excel_sheets(path_clc))

clc_2000 <- read_excel(
  path_clc,
  sheet = "CLC2000"
)

clc_150 <- read_excel(
  path_clc,
  sheet = "CLC150"
)

cat("\nDimensions CLC 2000 m :\n")
print(dim(clc_2000))

cat("\nDimensions CLC 150 m :\n")
print(dim(clc_150))

# -----------------------------------------------------------------------
# Correction of the CLC file for a subset of sites
# -----------------------------------------------------------------------
# The site codes are withheld in this public version: they identify the real
# sampling locations of the MISArbO network.

sites_a_corriger <- c(
  # "site_code_1", "site_code_2", ...  (list not shared)
)

clc_2000 <- clc_2000 %>%
  mutate(
    AREA = ifelse(code_prelev %in% sites_a_corriger,
                  AREA / 2,
                  AREA),
    `%` = ifelse(code_prelev %in% sites_a_corriger,
                 `%` / 2,
                 `%`)
  )

clc_150 <- clc_150 %>%
  mutate(
    AREA = ifelse(code_prelev %in% sites_a_corriger,
                  AREA / 2,
                  AREA),
    `%` = ifelse(code_prelev %in% sites_a_corriger,
                 `%` / 2,
                 `%`)
  )

### 1.4.2 Contrôle compatibilité CLC / MISARBO --------------------------

clc_2000_match <- clc_2000 %>%
  filter(code_prelev %in% db_misarbo$code_prelev)

clc_150_match <- clc_150 %>%
  filter(code_prelev %in% db_misarbo$code_prelev)



cat("\nNombre code_prelev MISARBO :\n")
print(length(unique(db_misarbo$code_prelev)))

cat("\nNombre code_prelev compatibles CLC 2000 m :\n")
print(length(unique(clc_2000_match$code_prelev)))

cat("\nNombre code_prelev compatibles CLC 150 m :\n")
print(length(unique(clc_150_match$code_prelev)))

cat("\nCodes MISARBO absents du CLC 2000 m :\n")
print(
  setdiff(
    unique(db_misarbo$code_prelev),
    unique(clc_2000$code_prelev)
  )
)

cat("\nCodes MISARBO absents du CLC 150 m :\n")
print(
  setdiff(
    unique(db_misarbo$code_prelev),
    unique(clc_150$code_prelev)
  )
)

### 1.4.3 Construction variables paysagères CLC -------------------------

# Objectif :
# transformer les tables longues CLC en variables larges exploitables
# dans les modèles statistiques.
#
# code_1 = niveau CLC 1
# code_2 = niveau CLC 2
# code_12 = niveau CLC 3 original
#
# Les proportions sont contenues dans la colonne "%".

library(tidyr)

# Normalisation des proportions CLC par code_prelev
# Objectif : chaque buffer doit sommer à 1.

clc_150 <- clc_150 %>%
  group_by(code_prelev) %>%
  mutate(prop_clc = `%` / sum(`%`, na.rm = TRUE)) %>%
  ungroup()

clc_2000 <- clc_2000 %>%
  group_by(code_prelev) %>%
  mutate(prop_clc = `%` / sum(`%`, na.rm = TRUE)) %>%
  ungroup()

# -----------------------------------------------------------------------
# Variables CLC niveau 1 - buffer 150 m
# -----------------------------------------------------------------------

clc150_code1 <- clc_150 %>%
  select(code_prelev, code_1, prop_clc) %>%
  group_by(code_prelev, code_1) %>%
  summarise(
    prop = sum(prop_clc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = code_1,
    values_from = prop,
    values_fill = 0,
    names_prefix = "clc150_code1_"
  )

# -----------------------------------------------------------------------
# Variables CLC niveau 1 - buffer 2000 m
# -----------------------------------------------------------------------

clc2000_code1 <- clc_2000 %>%
  select(code_prelev, code_1, prop_clc) %>%
  group_by(code_prelev, code_1) %>%
  summarise(
    prop = sum(prop_clc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = code_1,


      values_from = prop,
      values_fill = 0,
      names_prefix = "clc2000_code1_"
  )

# -----------------------------------------------------------------------
# Variables CLC niveau 2 - buffer 150 m
# -----------------------------------------------------------------------

clc150_code2 <- clc_150 %>%
  select(code_prelev, code_2, prop_clc) %>%
  group_by(code_prelev, code_2) %>%
  summarise(
    prop = sum(prop_clc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = code_2,
    values_from = prop,
    values_fill = 0,
    names_prefix = "clc150_code2_"
  )

# -----------------------------------------------------------------------
# Variables CLC niveau 2 - buffer 2000 m
# -----------------------------------------------------------------------

clc2000_code2 <- clc_2000 %>%
  select(code_prelev, code_2, prop_clc) %>%
  group_by(code_prelev, code_2) %>%
  summarise(
    prop = sum(prop_clc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = code_2,
    values_from = prop,
    values_fill = 0,
    names_prefix = "clc2000_code2_"
  )

cat("\nVariables CLC niveau 1 - 150 m :\n")
print(names(clc150_code1))

cat("\nVariables CLC niveau 1 - 2000 m :\n")
print(names(clc2000_code1))

cat("\nVariables CLC niveau 2 - 150 m :\n")
print(names(clc150_code2))

cat("\nVariables CLC niveau 2 - 2000 m :\n")
print(names(clc2000_code2))

# -----------------------------------------------------------------------
# Variables CLC code_12 sélectionnées - buffer 2000 m
# -----------------------------------------------------------------------

codes_clc12_keep <- c(
  "112", "211", "213", "221", "242",
  "311", "321", "411", "421", "521"
)

clc2000_code12_selected <- clc_2000 %>%
  select(code_prelev, code_12, `%`) %>%
  group_by(code_prelev, code_12) %>%
  summarise(
    prop = sum(`%`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = code_12,
    values_from = prop,
    values_fill = 0,
    names_prefix = "clc2000_code12_"
  ) %>%
  mutate(
    clc2000_code12_311 = clc2000_code12_311 + clc2000_code12_313,
    clc2000_code12_321 = clc2000_code12_321 + clc2000_code12_323,
    clc2000_code12_242 = clc2000_code12_242 + clc2000_code12_243
  ) %>%
  select(
    code_prelev,
    all_of(


          paste0(
            "clc2000_code12_",
            codes_clc12_keep
          )
      )
  )

cat("\nVariables CLC code_12 sélectionnées - 2000 m :\n")
print(names(clc2000_code12_selected))

### 1.4.4 Jointure CLC à la base ---------------------------------------

# Jointure des variables CLC à la base MISArbO.
# Les variables code_1 sont utilisées pour les premiers modèles CLC
# car elles sont plus synthétiques et limitent les problèmes de colinéarité.
# Les variables code_2 sont conservées pour exploration ultérieure.

DB_FULL_CLC <- db_misarbo %>%
  left_join(clc150_code1, by = "code_prelev") %>%
  left_join(clc2000_code1, by = "code_prelev") %>%
  left_join(clc150_code2, by = "code_prelev") %>%
  left_join(clc2000_code2, by = "code_prelev") %>%
  left_join(clc2000_code12_selected, by = "code_prelev")

cat("\nDimensions DB_FULL_CLC :\n")
print(dim(DB_FULL_CLC))

cat("\nVariables CLC ajoutées à DB_FULL_CLC :\n")
print(names(DB_FULL_CLC)[grepl("clc", names(DB_FULL_CLC))])



### 1.4.5 Contrôles qualité CLC ----------------------------------------

clc_vars <- names(DB_FULL_CLC)[grepl("clc", names(DB_FULL_CLC))]

cat("\nNombre de variables CLC ajoutées :\n")
print(length(clc_vars))

cat("\nNA variables CLC :\n")
print(
  colSums(
    is.na(
       DB_FULL_CLC[clc_vars]
    )
  )
)

cat("\nContrôle somme proportions CLC code_1 - 150 m :\n")
print(
  summary(
    rowSums(
       DB_FULL_CLC[
          names(DB_FULL_CLC)[grepl("^clc150_code1_", names(DB_FULL_CLC))]
       ],
       na.rm = TRUE
    )
  )
)

cat("\nContrôle somme proportions CLC code_1 - 2000 m :\n")
print(
  summary(
    rowSums(
       DB_FULL_CLC[
          names(DB_FULL_CLC)[grepl("^clc2000_code1_", names(DB_FULL_CLC))]
       ],
       na.rm = TRUE
    )
  )
)



### 1.4.6 Export base météo + BRI + CLC --------------------------------

path_export_meteo_bri_clc <- file.path(
  out_dir_dbfull,
  "DB_MISARBO_2025_METEO_BRI_CLC.csv"
)

write.csv(
  DB_FULL_CLC,
  path_export_meteo_bri_clc,
  row.names = FALSE


)

cat("\nBase MISArbO + météo + BRI + CLC exportée :\n")
print(path_export_meteo_bri_clc)

cat("\nDimensions base exportée météo + BRI + CLC :\n")
print(dim(DB_FULL_CLC))

