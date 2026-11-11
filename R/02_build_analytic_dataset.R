# 2. CONSTRUCTION DB_FULL ----------------------------------------------

## 2.1 Sélection variables analytiques ---------------------------------

cat("\nDimensions actuelles db_misarbo :\n")
print(dim(db_misarbo))

cat("\nColonnes disponibles :\n")
print(names(db_misarbo))

cat("\nNombre de NA par variable :\n")
print(
  sort(
    colSums(is.na(db_misarbo)),
    decreasing = TRUE
  )
)

# Variables entomologiques

vars_entomo <- c(
  "nbr_ad",
  names(db_misarbo)[grepl("adultes_", names(db_misarbo))]
)

# Variables retenues pour DB_FULL

vars_analysis <- c(

    # Identifiants
    "num_releve",
    "num_releve_All",
    "code_prelev",

    # Informations spatiales
    "region",
    "dpt",
    "code_dpt",
    "insee",
    "nom_commune",
    "site_acronyme",
    "GeoCluster",
    "x",
    "y",

    # Informations temporelles
    "date_releve",
    "date_releve_dt",
    "date_only",
    "annee",
    "mois_num",
    "mois",
    "semaine",

    # Informations prélèvement
    "MatrixType",
    "type",
    "CaractereEchant",

    # Variables virales
    "Pos_Neg_WNV",
    "Pos_Neg_USUV",
    "WNV_bin",
    "USUV_bin",

    # Variables météo pré-prélèvement retenues
    "cop_temp_mean_15d",
    "cop_precip_sum_15d",
    "cop_rad_sum_24h",

    # Variables aviaires
    "bri_mean",
    "bri_cv",

    # Variables entomologiques


    vars_entomo
)

# Vérification existence variables

vars_analysis <- intersect(
  vars_analysis,
  names(db_misarbo)
)

# Construction DB_FULL

DB_FULL <- db_misarbo %>%
  select(all_of(vars_analysis))

cat("\nDimensions DB_FULL :\n")
print(dim(DB_FULL))

cat("\nColonnes DB_FULL :\n")
print(names(DB_FULL))

# Ordre chronologique des mois

DB_FULL <- DB_FULL %>%
  mutate(
    mois = factor(
      mois,
      levels = c(
        "Avril",
        "Mai",
        "Juin",
        "Juillet",
        "Août",
        "Septembre",
        "Octobre"
      )
    )
  )

# Standardisation des variables météorologiques
# Ces variables standardisées sont utilisées dans les modèles
# afin d'éviter les problèmes d'échelle entre prédicteurs.

DB_FULL <- DB_FULL %>%
  mutate(
    temp_s = as.numeric(scale(cop_temp_mean_15d)),
    precip_s = as.numeric(scale(cop_precip_sum_15d)),
    rad_s = as.numeric(scale(cop_rad_sum_24h))
  )



## 2.2 Vérification types variables ------------------------------------

cat("\nStructure DB_FULL :\n")
str(DB_FULL)

cat("\nClasses des variables DB_FULL :\n")
print(sapply(DB_FULL, class))

## 2.3 Gestion valeurs manquantes --------------------------------------

cat("\nNA par variable dans DB_FULL :\n")
print(
  sort(
    colSums(is.na(DB_FULL)),
    decreasing = TRUE
  )
)

cat("\nVariables sans aucun NA :\n")
print(
  names(DB_FULL)[colSums(is.na(DB_FULL)) == 0]
)

cat("\nVariables avec au moins un NA :\n")
print(
  names(DB_FULL)[colSums(is.na(DB_FULL)) > 0]
)

## 2.4 Sélection variables entomologiques ------------------------------

na_entomo <- data.frame(
  variable = vars_entomo,


    n_na = colSums(is.na(DB_FULL[vars_entomo])),
    prop_na = colSums(is.na(DB_FULL[vars_entomo])) / nrow(DB_FULL)
)

na_entomo <- na_entomo %>%
  arrange(prop_na)

cat("\nNA variables entomologiques :\n")
print(na_entomo)

## 2.5 Construction DB_FULL analytique ---------------------------------

vars_entomo_keep <- c(
  "nbr_ad"
)

vars_analysis_final <- c(

    # Identifiants
    "num_releve",
    "num_releve_All",
    "code_prelev",

    # Spatial
    "region",
    "dpt",
    "code_dpt",
    "insee",
    "nom_commune",
    "site_acronyme",
    "GeoCluster",
    "x",
    "y",

    # Temporel
    "date_releve_dt",
    "date_only",
    "annee",
    "mois",
    "semaine",

    # Type prélèvement
    "MatrixType",
    "type",

    # Réponse virale
    "Pos_Neg_WNV",
    "Pos_Neg_USUV",
    "WNV_bin",
    "USUV_bin",

    # Météo pré-prélèvement retenue
    "cop_temp_mean_15d",
    "cop_precip_sum_15d",
    "cop_rad_sum_24h",

    # Météo standardisée pour les modèles
    "temp_s",
    "precip_s",
    "rad_s",

    # Aviaire
    "bri_mean",
    "bri_cv",

    # Entomo retenue
    vars_entomo_keep
)

DB_FULL <- DB_FULL %>%
  select(all_of(vars_analysis_final))

cat("\nDimensions DB_FULL analytique :\n")
print(dim(DB_FULL))

cat("\nColonnes DB_FULL analytique :\n")
print(names(DB_FULL))

## 2.6 Contrôles qualité DB_FULL ---------------------------------------

cat("\nDimensions finales DB_FULL :\n")
print(dim(DB_FULL))



cat("\nNA finaux DB_FULL :\n")
print(
  sort(
    colSums(is.na(DB_FULL)),
    decreasing = TRUE
  )
)

cat("\nRépartition WNV :\n")
print(table(DB_FULL$Pos_Neg_WNV, useNA = "ifany"))

cat("\nRépartition USUV :\n")
print(table(DB_FULL$Pos_Neg_USUV, useNA = "ifany"))

cat("\nRépartition MatrixType :\n")
print(table(DB_FULL$MatrixType, useNA = "ifany"))

cat("\nRépartition mois :\n")
print(table(DB_FULL$mois, useNA = "ifany"))

cat("\nNombre de sites :\n")
print(length(unique(DB_FULL$site_acronyme)))

cat("\nVariables météo/BRI conservées dans DB_FULL :\n")
print(
  names(DB_FULL)[names(DB_FULL) %in% c(
     "cop_temp_mean_15d",
     "cop_precip_sum_15d",
     "cop_rad_sum_24h",
     "temp_s",
     "precip_s",
     "rad_s",
     "bri_mean",
     "bri_cv"
  )]
)

# 3. CONTROLES QUALITE DB_FULL -----------------------------------------

## 3.1 Structure générale -----------------------------------------------

cat("\nDimensions DB_FULL :\n")
print(dim(DB_FULL))

cat("\nAperçu DB_FULL :\n")
print(head(DB_FULL))

cat("\nStructure DB_FULL :\n")
str(DB_FULL)

cat("\nTypes de variables :\n")
print(sapply(DB_FULL, class))

cat("\nNombre de sites uniques :\n")
print(length(unique(DB_FULL$site_acronyme)))

cat("\nNombre de prélèvements uniques :\n")
print(length(unique(DB_FULL$num_releve)))

cat("\nNombre de code_prelev uniques :\n")
print(length(unique(DB_FULL$code_prelev)))



## 3.2 Vérification variables virales -------------------------------

cat("\nRépartition WNV :\n")
print(table(DB_FULL$Pos_Neg_WNV, useNA = "ifany"))

cat("\nRépartition USUV :\n")
print(table(DB_FULL$Pos_Neg_USUV, useNA = "ifany"))

cat("\nPrévalence WNV (%) :\n")
print(
  prop.table(
    table(DB_FULL$Pos_Neg_WNV)
  ) * 100
)

cat("\nPrévalence USUV (%) :\n")
print(
  prop.table(
    table(DB_FULL$Pos_Neg_USUV)
  ) * 100


)

cat("\nWNV par MatrixType :\n")
print(
  table(
    DB_FULL$MatrixType,
    DB_FULL$Pos_Neg_WNV,
    useNA = "ifany"
  )
)

cat("\nUSUV par MatrixType :\n")
print(
  table(
    DB_FULL$MatrixType,
    DB_FULL$Pos_Neg_USUV,
    useNA = "ifany"
  )
)

cat("\nWNV par mois :\n")
print(
  table(
    DB_FULL$mois,
    DB_FULL$Pos_Neg_WNV,
    useNA = "ifany"
  )
)

cat("\nUSUV par mois :\n")
print(
  table(
    DB_FULL$mois,
    DB_FULL$Pos_Neg_USUV,
    useNA = "ifany"
  )
)

cat("\nNombre total de positifs WNV :\n")
print(sum(DB_FULL$Pos_Neg_WNV == "POS", na.rm = TRUE))

cat("\nNombre total de positifs USUV :\n")
print(sum(DB_FULL$Pos_Neg_USUV == "POS", na.rm = TRUE))

## 3.3 Vérification couverture temporelle ---------------------------

cat("\nNombre de prélèvements par mois :\n")
print(
  DB_FULL %>%
    count(mois)
)

cat("\nNombre de prélèvements par semaine :\n")
print(
  DB_FULL %>%
    count(semaine)
)

cat("\nPériode couverte :\n")
print(range(DB_FULL$date_only))

cat("\nNombre de prélèvements par MatrixType et mois :\n")
print(
  DB_FULL %>%
    count(MatrixType, mois)
)

cat("\nNombre de sites actifs par mois :\n")
print(
  DB_FULL %>%
    group_by(mois) %>%
    summarise(
       n_sites = n_distinct(site_acronyme)
    )
)

## 3.4 Vérification couverture spatiale -----------------------------

cat("\nNombre de prélèvements par département :\n")
print(
  DB_FULL %>%
    count(dpt, sort = TRUE)
)



cat("\nNombre de prélèvements par GeoCluster :\n")
print(
  DB_FULL %>%
    count(GeoCluster, sort = TRUE)
)

cat("\nNombre de prélèvements par site :\n")
print(
  DB_FULL %>%
    count(site_acronyme, sort = TRUE)
)

cat("\nNombre de code_prelev par site :\n")
print(
  DB_FULL %>%
    group_by(site_acronyme) %>%
    summarise(
       n_code_prelev = n_distinct(code_prelev)
    ) %>%
    arrange(desc(n_code_prelev))
)

cat("\nCoordonnées spatiales :\n")
print(summary(DB_FULL[, c("x", "y")]))



## 3.5 Vérification variables météo -------------------------------

meteo_vars <- c(
  "cop_temp_mean_15d",
  "cop_precip_sum_15d",
  "cop_rad_sum_24h"
)

cat("\nRésumé variables météo :\n")
print(summary(DB_FULL[meteo_vars]))

cat("\nNA variables météo :\n")
print(colSums(is.na(DB_FULL[meteo_vars])))

cat("\nTempérature par mois :\n")
print(
  DB_FULL %>%
    group_by(mois) %>%
    summarise(
       temp_mean = mean(cop_temp_mean_15d, na.rm = TRUE),
       temp_min = min(cop_temp_mean_15d, na.rm = TRUE),
       temp_max = max(cop_temp_mean_15d, na.rm = TRUE)
    )
)

cat("\nPrécipitations par mois :\n")
print(
  DB_FULL %>%
    group_by(mois) %>%
    summarise(
       precip_mean = mean(cop_precip_sum_15d, na.rm = TRUE),
       precip_max = max(cop_precip_sum_15d, na.rm = TRUE)
    )
)

cat("\nRadiation par mois :\n")
print(
  DB_FULL %>%
    group_by(mois) %>%
    summarise(
       rad_mean = mean(cop_rad_sum_24h, na.rm = TRUE),
       rad_min = min(cop_rad_sum_24h, na.rm = TRUE),
       rad_max = max(cop_rad_sum_24h, na.rm = TRUE)
    )
)

## 3.6 Vérification variables aviaires ----------------------------

aviaire_vars <- c(
  "bri_mean",
  "bri_cv"
)

cat("\nRésumé variables aviaires :\n")
print(summary(DB_FULL[aviaire_vars]))



cat("\nNA variables aviaires :\n")
print(colSums(is.na(DB_FULL[aviaire_vars])))

cat("\nBRI moyen par site :\n")
print(
  DB_FULL %>%
    group_by(site_acronyme) %>%
    summarise(
       bri_mean_site = mean(bri_mean, na.rm = TRUE),
       bri_cv_site = mean(bri_cv, na.rm = TRUE)
    ) %>%
    arrange(desc(bri_mean_site))
)

cat("\nDistribution BRI mean :\n")
print(table(cut(DB_FULL$bri_mean, breaks = 5)))

cat("\nDistribution BRI CV :\n")
print(table(cut(DB_FULL$bri_cv, breaks = 5)))

cat("\nNombre de sites avec BRI disponible :\n")
print(
  DB_FULL %>%
    filter(!is.na(bri_mean)) %>%
    summarise(n_sites = n_distinct(site_acronyme))
)

## 3.7 Relations exploratoires virus ↔ environnement -------------------

### 3.7.1 WNV -----------------------------------------------------------

cat("\nTempérature selon statut WNV et matrice :\n")
print(
  DB_FULL %>%
    group_by(MatrixType, Pos_Neg_WNV) %>%
    summarise(
       n = n(),
       temp_mean = mean(cop_temp_mean_15d, na.rm = TRUE),
       temp_sd = sd(cop_temp_mean_15d, na.rm = TRUE),
       .groups = "drop"
    )
)

cat("\nPrécipitations selon statut WNV et matrice :\n")
print(
  DB_FULL %>%
    group_by(MatrixType, Pos_Neg_WNV) %>%
    summarise(
       n = n(),
       precip_mean = mean(cop_precip_sum_15d, na.rm = TRUE),
       precip_sd = sd(cop_precip_sum_15d, na.rm = TRUE),
       .groups = "drop"
    )
)

cat("\nRadiation selon statut WNV et matrice :\n")
print(
  DB_FULL %>%
    group_by(MatrixType, Pos_Neg_WNV) %>%
    summarise(
       n = n(),
       rad_mean = mean(cop_rad_sum_24h, na.rm = TRUE),
       rad_sd = sd(cop_rad_sum_24h, na.rm = TRUE),
       .groups = "drop"
    )
)

cat("\nBRI selon statut WNV et matrice :\n")
print(
  DB_FULL %>%
    group_by(MatrixType, Pos_Neg_WNV) %>%
    summarise(
       n = n(),
       bri_mean_mean = mean(bri_mean, na.rm = TRUE),
       bri_mean_sd = sd(bri_mean, na.rm = TRUE),
       bri_cv_mean = mean(bri_cv, na.rm = TRUE),
       bri_cv_sd = sd(bri_cv, na.rm = TRUE),
       .groups = "drop"
    )
)



### 3.7.2 USUV ----------------------------------------------------------



cat("\nTempérature selon statut USUV et matrice :\n")
print(
  DB_FULL %>%
    group_by(MatrixType, Pos_Neg_USUV) %>%
    summarise(
       n = n(),
       temp_mean = mean(cop_temp_mean_15d, na.rm = TRUE),
       temp_sd = sd(cop_temp_mean_15d, na.rm = TRUE),
       .groups = "drop"
    )
)

cat("\nPrécipitations selon statut USUV et matrice :\n")
print(
  DB_FULL %>%
    group_by(MatrixType, Pos_Neg_USUV) %>%
    summarise(
       n = n(),
       precip_mean = mean(cop_precip_sum_15d, na.rm = TRUE),
       precip_sd = sd(cop_precip_sum_15d, na.rm = TRUE),
       .groups = "drop"
    )
)

cat("\nRadiation selon statut USUV et matrice :\n")
print(
  DB_FULL %>%
    group_by(MatrixType, Pos_Neg_USUV) %>%
    summarise(
       n = n(),
       rad_mean = mean(cop_rad_sum_24h, na.rm = TRUE),
       rad_sd = sd(cop_rad_sum_24h, na.rm = TRUE),
       .groups = "drop"
    )
)

cat("\nBRI selon statut USUV et matrice :\n")
print(
  DB_FULL %>%
    group_by(MatrixType, Pos_Neg_USUV) %>%
    summarise(
       n = n(),
       bri_mean_mean = mean(bri_mean, na.rm = TRUE),
       bri_mean_sd = sd(bri_mean, na.rm = TRUE),
       bri_cv_mean = mean(bri_cv, na.rm = TRUE),
       bri_cv_sd = sd(bri_cv, na.rm = TRUE),
       .groups = "drop"
    )
)



# 4. EXPORTS FINAUX ----------------------------------------------------

## 4.1 Export DB_FULL CSV ----------------------------------------------

path_export_dbfull <- file.path(
  out_dir_dbfull,
  "DB_FULL_MISARBO_2025.csv"
)

write.csv(
  DB_FULL,
  path_export_dbfull,
  row.names = FALSE
)

cat("\nDB_FULL exportée :\n")
print(path_export_dbfull)

cat("\nDimensions DB_FULL exportée :\n")
print(dim(DB_FULL))



## 4.2 Export tables intermédiaires ------------------------------------

write.csv(
  na_entomo,
  file.path(out_dir_tables, "Table_NA_variables_entomologiques.csv"),
  row.names = FALSE
)




## 4.3 Export tables descriptives --------------------------------------

table_virus_matrice <- DB_FULL %>%
  count(MatrixType, Pos_Neg_WNV, Pos_Neg_USUV)

write.csv(
  table_virus_matrice,
  file.path(out_dir_tables, "Table_virus_par_matrice.csv"),
  row.names = FALSE
)

table_mois_matrice <- DB_FULL %>%
  count(MatrixType, mois)

write.csv(
  table_mois_matrice,
  file.path(out_dir_tables, "Table_prelevements_par_mois_matrice.csv"),
  row.names = FALSE
)

cat("\nExports finaux réalisés.\n")

