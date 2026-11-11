
# SCRIPT GLOBAL MISARBO 2025 -------------------------------------------
# Auteur : Kany Tadjou | M2 Eco-épidémiologie | Altopictus -------------
# Projet : MISArbO 2025 ------------------------------------------------
# Objet : Construction de DB_FULL et analyses --------------------------



# 0. LIBRAIRIES ET CHEMINS ---------------------------------------------

## 0.1 Packages --------------------------------------------------------

rm(list = ls())

library(dplyr)
library(ggplot2)
library(lubridate)
library(terra)
library(lme4)
library(sjPlot)
library(glmmTMB)
library(gt)
library(scales)
library(readxl)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(splines)
library(DHARMa)
library(broom.mixed)
library(ggeffects)

## 0.2 Chemins des données ---------------------------------------------

path_misarbo_raw <- "data_raw/Mergin_Full_wide.xlsx.csv"

dir_temperature <- "data_raw/temperature_ERA5Land"
dir_precipitation <- "data_raw/precipitation_ERA5Land"
dir_radiation <- "data_raw/radiation_ERA5Land"

path_bri_mean <- "data_raw/rasters_BRI/BRI_mean.tif"
path_bri_cv <- "data_raw/rasters_BRI/BRI_CV.tif"

path_clc <- "data_raw/CLC_MISARBO_150_2000_code12_code1_code2.xlsx"

path_culex <- "rasters_Culex/pip10xyPABALBRTRFMEANmsk2y3qunsuit.tif"
path_culex_abund <- "rasters_Culex/pip1xylnevbpsbrtRFMEANjan20ok.tif"

## 0.3 Dossiers exports ------------------------------------------------

out_dir_root <- "results"

out_dir_dbfull <- file.path(out_dir_root, "DB_FULL")
out_dir_figures <- file.path(out_dir_root, "figures")
out_dir_tables <- file.path(out_dir_root, "tables")
out_dir_models <- file.path(out_dir_root, "models")

dir.create(out_dir_dbfull, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir_figures, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir_tables, recursive = TRUE, showWarnings = FALSE)
dir.create(out_dir_models, recursive = TRUE, showWarnings = FALSE)

cat("\nDossiers d'export prêts.\n")



