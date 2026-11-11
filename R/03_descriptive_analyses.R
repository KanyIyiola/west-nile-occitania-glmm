# 5. ANALYSES DESCRIPTIVES ---------------------------------------------

# Force l'ordre chronologique des mois pour tous les graphiques à venir
DB_FULL$mois <- factor(
  DB_FULL$mois,
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

## 5.1 Dynamique temporelle WNV ----------------------------------------

# Construction table WNV par mois

wnv_mois <- DB_FULL %>%

  filter(!is.na(Pos_Neg_WNV)) %>%

  group_by(mois, Pos_Neg_WNV) %>%

  summarise(
    n = n(),
    .groups = "drop"
  ) %>%

  group_by(mois) %>%

  mutate(
    prop = n / sum(n)
  )

cat("\nTable WNV par mois :\n")
print(wnv_mois)



# Ordre des statuts WNV

wnv_mois$Pos_Neg_WNV <- factor(
  wnv_mois$Pos_Neg_WNV,
  levels = c("NEG", "POS")
)



# Visualisation dynamique temporelle WNV
# Proportions mensuelles avec effectifs par statut

ggplot(
  wnv_mois,
  aes(
    x = mois,
    y = prop,
    fill = Pos_Neg_WNV
  )
) +



  geom_col(
    position = "fill",
    width = 0.9
  ) +

  geom_text(
    aes(label = n),
    position = position_fill(vjust = 0.5),
    color = "white",
    size = 4
  ) +

  scale_y_continuous(
    labels = percent_format()
  ) +

  scale_fill_manual(
    values = c(
      "NEG" = "#d9d2c3",
      "POS" = "#c96b2c"
    )
  ) +

  labs(
    title = "Distribution mensuelle des détections WNV",
    subtitle = "Proportions mensuelles et effectifs par statut",
    x = "Mois",
    y = "Proportion",
    fill = "Statut WNV"
  ) +

  theme_minimal() +

  theme(
    plot.title = element_text(
       face = "bold",
       size = 16
    ),

      plot.subtitle = element_text(
         size = 11
      ),

      axis.title = element_text(
         face = "bold"
      ),

      legend.title = element_text(
        face = "bold"
      )
  )

# Nombre de détections positives WNV par mois

wnv_pos_mois <- DB_FULL %>%
  filter(!is.na(Pos_Neg_WNV)) %>%
  group_by(mois) %>%
  summarise(
    n_pos = sum(Pos_Neg_WNV == "POS", na.rm = TRUE),
    n_total = n(),
    .groups = "drop"
  )

cat("\nNombre de positifs WNV par mois :\n")
print(wnv_pos_mois)

ggplot(
  wnv_pos_mois,
  aes(
    x = mois,
    y = n_pos
  )
) +
  geom_col(
    fill = "#c96b2c",
    width = 0.8
  ) +
  geom_text(
    aes(label = n_pos),
    vjust = -0.4,
    size = 4
  ) +
  labs(


    title = "Nombre mensuel de détections positives WNV",
    x = "Mois",
    y = "Nombre de positifs WNV"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold")
  )

## 5.2 Dynamique temporelle USUV ---------------------------------------

# Construction table USUV par mois

usuv_mois <- DB_FULL %>%

  filter(!is.na(Pos_Neg_USUV)) %>%

  group_by(mois, Pos_Neg_USUV) %>%

  summarise(
    n = n(),
    .groups = "drop"
  ) %>%

  group_by(mois) %>%

  mutate(
    prop = n / sum(n)
  )

cat("\nTable USUV par mois :\n")

print(usuv_mois)



# Ordre des statuts USUV

usuv_mois$Pos_Neg_USUV <- factor(
  usuv_mois$Pos_Neg_USUV,
  levels = c("NEG", "POS")
)



# Visualisation dynamique temporelle USUV
# Proportions mensuelles avec effectifs par statut

ggplot(
  usuv_mois,
  aes(
    x = mois,
    y = prop,
    fill = Pos_Neg_USUV
  )
) +

  geom_col(
    position = "fill"
  ) +

  geom_text(
    aes(label = n),
    position = position_fill(vjust = 0.5),
    color = "white",
    size = 4
  ) +

  scale_y_continuous(
    labels = percent_format()
  ) +

  scale_fill_manual(
    values = c(
      "NEG" = "#d8d2c4",
      "POS" = "#7b4b2a"
    )
  ) +

  labs(
    title = "Distribution mensuelle des détections USUV",
    subtitle = "Proportions mensuelles et effectifs par statut",
    x = "Mois",
    y = "Proportion",


    fill = "Statut USUV"
  ) +

  theme_minimal()

# Construction table positifs USUV par mois

usuv_pos_mois <- DB_FULL %>%
  filter(!is.na(Pos_Neg_USUV)) %>%
  group_by(mois) %>%
  summarise(
    n_pos = sum(Pos_Neg_USUV == "POS", na.rm = TRUE),
    n_total = n(),
    .groups = "drop"
  )

cat("\nPositifs USUV par mois :\n")

print(usuv_pos_mois)

# Visualisation nombre de positifs USUV par mois

ggplot(
  usuv_pos_mois,
  aes(
    x = mois,
    y = n_pos
  )
) +

  geom_col(
    fill = "#7b4b2a"
  ) +

  geom_text(
    aes(label = n_pos),
    vjust = -0.3,
    size = 5
  ) +

  labs(
    title = "Nombre mensuel de détections positives USUV",
    x = "Mois",
    y = "Nombre de positifs USUV"
  ) +

  theme_minimal()

## 5.3 Répartition spatiale WNV ----------------------------------------

# Construction table WNV par site

wnv_site <- DB_FULL %>%

  filter(!is.na(Pos_Neg_WNV)) %>%

  group_by(
    site_acronyme,
    x,
    y
  ) %>%

  summarise(
    n_total = n(),
    n_pos = sum(Pos_Neg_WNV == "POS", na.rm = TRUE),
    prop_pos = n_pos / n_total,
    .groups = "drop"
  )

cat("\nTable WNV par site :\n")

print(wnv_site)



# Vérification nombre de sites

cat("\nNombre de sites WNV :\n")

print(nrow(wnv_site))

cat("\nNombre de sites avec au moins un positif WNV :\n")

print(


    sum(wnv_site$n_pos > 0)
)

cat("\nNombre total de détections positives WNV :\n")

print(
  sum(wnv_site$n_pos)
)

# Visualisation spatiale WNV
# Taille = nombre positifs
# Couleur = proportion de positifs

ggplot(
  wnv_site,
  aes(
    x = x,
    y = y
  )
) +

    geom_point(
      aes(
         size = n_pos,
         color = prop_pos
      ),
      alpha = 0.8
    ) +

    scale_size_continuous(
      name = "Nombre positifs WNV"
    ) +

    scale_color_gradient(
      low = "#d8d2c4",
      high = "#d95f02",
      name = "Proportion\npositifs"
    ) +

    labs(
      title = "Répartition spatiale des détections WNV",
      subtitle = "Sites MISARBO 2025",
      x = "Longitude",
      y = "Latitude"
    ) +

    theme_minimal()

## 5.4 Répartition spatiale USUV ---------------------------------------

# Construction table USUV par site

usuv_site <- DB_FULL %>%

    filter(!is.na(Pos_Neg_USUV)) %>%

    group_by(
      site_acronyme,
      x,
      y
    ) %>%

    summarise(
      n_total = n(),
      n_pos = sum(Pos_Neg_USUV == "POS", na.rm = TRUE),
      prop_pos = n_pos / n_total,
      .groups = "drop"
    )

cat("\nTable USUV par site :\n")

print(usuv_site)

# Vérification nombre de sites

cat("\nNombre de sites USUV :\n")

print(nrow(usuv_site))

cat("\nNombre de sites avec au moins un positif USUV :\n")

print(
  sum(usuv_site$n_pos > 0)


)

cat("\nNombre total de détections positives USUV :\n")

print(
  sum(usuv_site$n_pos)
)

# Visualisation spatiale USUV
# Taille = nombre positifs
# Couleur = proportion de positifs

ggplot(
  usuv_site,
  aes(
    x = x,
    y = y
  )
) +

    geom_point(
      aes(
         size = n_pos,
         color = prop_pos
      ),
      alpha = 0.8
    ) +

    scale_size_continuous(
      name = "Nombre positifs USUV"
    ) +

    scale_color_gradient(
      low = "#d8d2c4",
      high = "#7b4b2a",
      name = "Proportion\npositifs"
    ) +

    labs(
      title = "Répartition spatiale des détections USUV",
      subtitle = "Sites MISARBO 2025",
      x = "Longitude",
      y = "Latitude"
    ) +

    theme_minimal()

## 5.5 Matrices environnementales --------------------------------------

# Répartition des prélèvements par MatrixType

matrix_counts <- DB_FULL %>%

    count(MatrixType)

cat("\nRépartition MatrixType :\n")

print(matrix_counts)

cat("\nProportion des matrices (%):\n")

print(
  matrix_counts %>%
    mutate(
       prop = round(100 * n / sum(n), 1),
    )
)

# Visualisation des matrices environnementales

ggplot(
  matrix_counts,
  aes(
    x = MatrixType,
    y = n,
    fill = MatrixType
  )
) +

    geom_col(
      width = 0.7
    ) +



  geom_text(
    aes(label = n),
    vjust = -0.3,
    size = 5
  ) +

  scale_fill_manual(
    values = c(
      "Zones humides" = "#6c8c6b",
      "Xénomonitoring" = "#8c6d5a"
    )
  ) +

  labs(
    title = "Répartition des matrices environnementales",
    subtitle = "Prélèvements MISARBO 2025",
    x = "Type de matrice",
    y = "Nombre de prélèvements",
    fill = "Matrice"
  ) +

  theme_minimal()

# WNV selon MatrixType

wnv_matrix <- DB_FULL %>%

  filter(!is.na(Pos_Neg_WNV)) %>%

  group_by(MatrixType, Pos_Neg_WNV) %>%

  summarise(
    n = n(),
    .groups = "drop"
  ) %>%

  group_by(MatrixType) %>%

  mutate(
    prop = n / sum(n)
  )

cat("\nWNV selon MatrixType :\n")

print(wnv_matrix)

cat("\nNombre total de positifs WNV par matrice :\n")

print(
  DB_FULL %>%
    filter(Pos_Neg_WNV == "POS") %>%
    count(MatrixType)
)

# Visualisation WNV par matrice

ggplot(
  wnv_matrix,
  aes(
    x = MatrixType,
    y = prop,
    fill = Pos_Neg_WNV
  )
) +

  geom_col(
    position = "fill"
  ) +

  geom_text(
    aes(label = n),
    position = position_fill(vjust = 0.5),
    color = "white",
    size = 5
  ) +

  scale_fill_manual(
    values = c(
      "NEG" = "#d8d2c4",
      "POS" = "#d95f02"
    )
  ) +



  scale_y_continuous(
    labels = percent_format()
  ) +

  labs(
    title = "Détections WNV selon la matrice environnementale",
    subtitle = "Proportions et effectifs",
    x = "Type de matrice",
    y = "Proportion",
    fill = "Statut WNV"
  ) +

  theme_minimal()

# USUV selon MatrixType

usuv_matrix <- DB_FULL %>%

  filter(!is.na(Pos_Neg_USUV)) %>%

  group_by(MatrixType, Pos_Neg_USUV) %>%

  summarise(
    n = n(),
    .groups = "drop"
  ) %>%

  group_by(MatrixType) %>%

  mutate(
    prop = n / sum(n)
  )

cat("\nUSUV selon MatrixType :\n")

print(usuv_matrix)

cat("\nNombre total de positifs USUV par matrice :\n")

print(
  DB_FULL %>%
    filter(Pos_Neg_USUV == "POS") %>%
    count(MatrixType)
)

# Visualisation USUV par matrice

ggplot(
  usuv_matrix,
  aes(
    x = MatrixType,
    y = prop,
    fill = Pos_Neg_USUV
  )
) +

  geom_col(
    position = "fill",
    width = 0.8
  ) +

  geom_text(
    aes(label = n),
    position = position_fill(vjust = 0.5),
    color = "white",
    size = 5
  ) +

  scale_fill_manual(
    values = c(
      "NEG" = "#d8d2c4",
      "POS" = "#7b4b2a"
    )
  ) +

  scale_y_continuous(
    labels = percent_format()
  ) +

  labs(
    title = "Détections USUV selon la matrice environnementale",
    subtitle = "Proportions et effectifs",
    x = "Type de matrice",


    y = "Proportion",
    fill = "Statut USUV"
  ) +

  theme_minimal()

## 5.6 Variables météorologiques --------------------------------------

# Température selon statut WNV

ggplot(
  DB_FULL %>%
    filter(!is.na(Pos_Neg_WNV)),

  aes(
    x = Pos_Neg_WNV,
    y = cop_temp_mean_15d,
    fill = Pos_Neg_WNV
  )
) +

  geom_boxplot(
    alpha = 0.8
  ) +

  scale_fill_manual(
    values = c(
      "NEG" = "#d8d2c4",
      "POS" = "#d95f02"
    )
  ) +

  labs(
    title = "Température selon le statut WNV",
    subtitle = "Distribution des températures moyennes sur 15 jours",
    x = "Statut WNV",
    y = "Température moyenne sur 15 jours (°C)",
    fill = "Statut WNV"
  ) +

  theme_minimal()




# Précipitations selon statut WNV

ggplot(
  DB_FULL %>%
    filter(!is.na(Pos_Neg_WNV)),

  aes(
    x = Pos_Neg_WNV,
    y = cop_precip_sum_15d,
    fill = Pos_Neg_WNV
  )
) +

  geom_boxplot(
    alpha = 0.8
  ) +

  scale_fill_manual(
    values = c(
      "NEG" = "#d8d2c4",
      "POS" = "#d95f02"
    )
  ) +

  labs(
    title = "Précipitations selon le statut WNV",
    subtitle = "Distribution des précipitations cumulées sur 15 jours",
    x = "Statut WNV",
    y = "Précipitations cumulées sur 15 jours (mm)",
    fill = "Statut WNV"
  ) +

  theme_minimal()




# Radiation selon statut WNV

ggplot(


  DB_FULL %>%
    filter(!is.na(Pos_Neg_WNV)),

  aes(
    x = Pos_Neg_WNV,
    y = cop_rad_sum_24h,
    fill = Pos_Neg_WNV
  )
) +

  geom_boxplot(
    alpha = 0.8
  ) +

  scale_fill_manual(
    values = c(
      "NEG" = "#d8d2c4",
      "POS" = "#d95f02"
    )
  ) +

  labs(
    title = "Rayonnement solaire selon le statut WNV",
    subtitle = "Distribution du rayonnement cumulé sur 24 h",
    x = "Statut WNV",
    y = "Rayonnement solaire cumulé sur 24 h (MJ/m²)",
    fill = "Statut WNV"
  ) +

  theme_minimal()




# BRI mean selon statut WNV

ggplot(
  DB_FULL %>%
    filter(!is.na(Pos_Neg_WNV)),

  aes(
    x = Pos_Neg_WNV,
    y = bri_mean,
    fill = Pos_Neg_WNV
  )
) +

  geom_boxplot(
    alpha = 0.8
  ) +

  scale_fill_manual(
    values = c(
      "NEG" = "#d8d2c4",
      "POS" = "#d95f02"
    )
  ) +

  labs(
    title = "Bird Risk Index moyen selon le statut WNV",
    subtitle = "Distribution des valeurs BRI mean",
    x = "Statut WNV",
    y = "BRI mean",
    fill = "Statut WNV"
  ) +

  theme_minimal()




# BRI CV selon statut WNV

ggplot(
  DB_FULL %>%
    filter(!is.na(Pos_Neg_WNV)),

  aes(
    x = Pos_Neg_WNV,
    y = bri_cv,
    fill = Pos_Neg_WNV
  )
) +

  geom_boxplot(


    alpha = 0.8
  ) +

  scale_fill_manual(
    values = c(
      "NEG" = "#d8d2c4",
      "POS" = "#d95f02"
    )
  ) +

  labs(
    title = "Variabilité du Bird Risk Index selon le statut WNV",
    subtitle = "Distribution des valeurs BRI CV",
    x = "Statut WNV",
    y = "BRI CV",
    fill = "Statut WNV"
  ) +

  theme_minimal()

## 5.7 Variables entomologiques ----------------------------------------



# Résumé variable entomologique retenue

cat("\nRésumé nbr_ad :\n")

print(summary(DB_FULL$nbr_ad))

cat("\nNombre de prélèvements avec nbr_ad renseigné :\n")

print(
  sum(!is.na(DB_FULL$nbr_ad))
)

cat("\nNombre de prélèvements avec nbr_ad > 0 :\n")

print(
  sum(DB_FULL$nbr_ad > 0, na.rm = TRUE)
)

# Distribution globale du nombre d'adultes

ggplot(
  DB_FULL,
  aes(x = nbr_ad)
) +

  geom_histogram(
    bins = 30,
    fill = "#7c9b7c",
    color = "white"
  ) +

  labs(
    title = "Distribution du nombre d'adultes collectés",
    subtitle = "Variable entomologique nbr_ad",
    x = "Nombre d'adultes",
    y = "Fréquence"
  ) +

  theme_minimal()

# Nombre d'adultes selon statut WNV
# Echelle logarithmique

cat("\nNombre d'adultes selon statut WNV :\n")

print(
  DB_FULL %>%
    filter(
       !is.na(Pos_Neg_WNV),
       nbr_ad > 0
    ) %>%
    group_by(Pos_Neg_WNV) %>%
    summarise(
       n = n(),
       mean_ad = mean(nbr_ad, na.rm = TRUE),
       median_ad = median(nbr_ad, na.rm = TRUE),
       .groups = "drop"
    )
)



ggplot(
  DB_FULL %>%
    filter(
       !is.na(Pos_Neg_WNV),
       nbr_ad > 0
    ),

  aes(
    x = Pos_Neg_WNV,
    y = nbr_ad,
    fill = Pos_Neg_WNV
  )
) +

  geom_boxplot(
    alpha = 0.9
  ) +

  scale_y_log10() +

  scale_fill_manual(
    values = c(
      "NEG" = "#d8d2c7",
      "POS" = "#c46f32"
    )
  ) +

  labs(
    title = "Nombre d'adultes selon le statut WNV",
    subtitle = "Distribution des abondances entomologiques (échelle logarithmique)",
    x = "Statut WNV",
    y = "Nombre d'adultes (log10)"
  ) +

  theme_minimal()

# Nombre d'adultes selon statut USUV
# Echelle logarithmique

cat("\nNombre d'adultes selon statut USUV :\n")

print(
  DB_FULL %>%
    filter(
       !is.na(Pos_Neg_USUV),
       nbr_ad > 0
    ) %>%
    group_by(Pos_Neg_USUV) %>%
    summarise(
       n = n(),
       mean_ad = mean(nbr_ad, na.rm = TRUE),
       median_ad = median(nbr_ad, na.rm = TRUE),
       .groups = "drop"
    )
)

ggplot(
  DB_FULL %>%
    filter(
       !is.na(Pos_Neg_USUV),
       nbr_ad > 0
    ),

  aes(
    x = Pos_Neg_USUV,
    y = nbr_ad,
    fill = Pos_Neg_USUV
  )
) +

  geom_boxplot(
    alpha = 0.9
  ) +

  scale_y_log10() +

  scale_fill_manual(
    values = c(
      "NEG" = "#d8d2c7",
      "POS" = "#7b4b2a"
    )
  ) +



  labs(
    title = "Nombre d'adultes selon le statut USUV",
    subtitle = "Distribution des abondances entomologiques (échelle logarithmique)",
    x = "Statut USUV",
    y = "Nombre d'adultes (log10)"
  ) +

  theme_minimal()

## 5.8 Variables aviaires ----------------------------------------------

# Résumé variables aviaires

cat("\nRésumé BRI :\n")

print(
  summary(
    DB_FULL[, c("bri_mean", "bri_cv")]
  )
)



# Distribution BRI mean

ggplot(
  DB_FULL,
  aes(x = bri_mean)
) +

  geom_histogram(
    bins = 25,
    fill = "#7c8c6f",
    color = "white"
  ) +

  labs(
    title = "Distribution du Bird Risk Index moyen",
    subtitle = "Variable aviaire bri_mean",
    x = "BRI mean",
    y = "Fréquence"
  ) +

  theme_minimal()

# Distribution BRI CV

ggplot(
  DB_FULL,
  aes(x = bri_cv)
) +

  geom_histogram(
    bins = 25,
    fill = "#9b7d5c",
    color = "white"
  ) +

  labs(
    title = "Distribution de la variabilité du Bird Risk Index",
    subtitle = "Variable aviaire bri_cv",
    x = "BRI CV",
    y = "Fréquence"
  ) +

  theme_minimal()

# Résumé BRI selon statut USUV

cat("\nBRI selon statut USUV :\n")

print(
  DB_FULL %>%
    filter(!is.na(Pos_Neg_USUV)) %>%
    group_by(Pos_Neg_USUV) %>%
    summarise(
       n = n(),
       bri_mean_mean = mean(bri_mean, na.rm = TRUE),
       bri_mean_median = median(bri_mean, na.rm = TRUE),
       bri_cv_mean = mean(bri_cv, na.rm = TRUE),
       bri_cv_median = median(bri_cv, na.rm = TRUE),
       .groups = "drop"
    )
)



# BRI mean selon statut USUV

ggplot(
  DB_FULL %>%
    filter(!is.na(Pos_Neg_USUV)),

  aes(
    x = Pos_Neg_USUV,
    y = bri_mean,
    fill = Pos_Neg_USUV
  )
) +

  geom_boxplot(
    alpha = 0.9
  ) +

  scale_fill_manual(
    values = c(
      "NEG" = "#d8d2c7",
      "POS" = "#7b4b2a"
    )
  ) +

  labs(
    title = "Bird Risk Index moyen selon le statut USUV",
    subtitle = "Distribution des valeurs bri_mean",
    x = "Statut USUV",
    y = "BRI mean"
  ) +

  theme_minimal()

# BRI CV selon statut USUV

ggplot(
  DB_FULL %>%
    filter(!is.na(Pos_Neg_USUV)),

  aes(
    x = Pos_Neg_USUV,
    y = bri_cv,
    fill = Pos_Neg_USUV
  )
) +

  geom_boxplot(
    alpha = 0.9
  ) +

  scale_fill_manual(
    values = c(
      "NEG" = "#d8d2c7",
      "POS" = "#7b4b2a"
    )
  ) +

  labs(
    title = "Variabilité du Bird Risk Index selon le statut USUV",
    subtitle = "Distribution des valeurs bri_cv",
    x = "Statut USUV",
    y = "BRI CV"
  ) +

  theme_minimal()

# Résumé BRI selon statut WNV

cat("\nBRI selon statut WNV :\n")

print(
  DB_FULL %>%
    filter(!is.na(Pos_Neg_WNV)) %>%
    group_by(Pos_Neg_WNV) %>%
    summarise(
       n = n(),
       bri_mean_mean = mean(bri_mean, na.rm = TRUE),
       bri_mean_median = median(bri_mean, na.rm = TRUE),
       bri_cv_mean = mean(bri_cv, na.rm = TRUE),
       bri_cv_median = median(bri_cv, na.rm = TRUE),
       .groups = "drop"
    )


)

# BRI CV selon statut WNV

ggplot(
  DB_FULL %>%
    filter(!is.na(Pos_Neg_WNV)),

  aes(
    x = Pos_Neg_WNV,
    y = bri_cv,
    fill = Pos_Neg_WNV
  )
) +

    geom_boxplot(
      alpha = 0.9
    ) +

    scale_fill_manual(
      values = c(
        "NEG" = "#d8d2c7",
        "POS" = "#c46f32"
      )
    ) +

    labs(
      title = "Variabilité du Bird Risk Index selon le statut WNV",
      subtitle = "Distribution des valeurs bri_cv",
      x = "Statut WNV",
      y = "BRI CV"
    ) +

    theme_minimal()

## 5.9 Figure 3 - Taux de positivité mensuel par virus et matrice -------

# -----------------------------------------------------------------------
# FIGURE 3 - Positivité mensuelle par virus et matrice
# -----------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# -----------------------------------------------------------------------
# 1. Préparation des données
# -----------------------------------------------------------------------

fig3_data <- DB_FULL %>%
  select(
    mois,
    MatrixType,
    WNV_bin,
    USUV_bin
  ) %>%
  pivot_longer(
    cols = c(WNV_bin, USUV_bin),,
    names_to = "Virus",
    values_to = "positif"
  ) %>%
  mutate(
    Virus = case_when(
       Virus == "WNV_bin" ~ "WNV",
       Virus == "USUV_bin" ~ "USUV",
       TRUE ~ Virus
    ),
    Matrice = case_when(
       MatrixType == "Xénomonitoring" ~ "MX",
       MatrixType == "Zones humides" ~ "ZH",
       TRUE ~ MatrixType
    ),
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

# -----------------------------------------------------------------------
# 2. Résumé statistique
# -----------------------------------------------------------------------

fig3_summary <- fig3_data %>%
  group_by(
    Virus,
    Matrice,
    mois
  ) %>%
  summarise(
    n_total = n(),
    n_pos = sum(
       positif == 1,
       na.rm = TRUE
    ),
    taux_pos = 100 * n_pos / n_total,
    .groups = "drop"
  ) %>%
  mutate(
    label_n = paste0("n=", n_total),
    panel = factor(
       paste(
          Virus,
          Matrice,
          sep = " - "
       ),
       levels = c(
          "WNV - MX",
          "WNV - ZH",
          "USUV - MX",
          "USUV - ZH"
       )
    )
  )

# -----------------------------------------------------------------------
# 3. Couleurs
# -----------------------------------------------------------------------

couleurs_panel <- c(
  "WNV - MX" = "#7B2CBF",    # violet foncé
  "WNV - ZH" = "#C77DFF",    # violet clair
  "USUV - MX" = "#E85D04",   # orange foncé
  "USUV - ZH" = "#FFBA08"    # orange clair
)

# -----------------------------------------------------------------------
# 4. Figure
# -----------------------------------------------------------------------

fig3 <- ggplot(
  fig3_summary,
  aes(
    x = mois,
    y = taux_pos,
    fill = panel
  )
) +

  geom_col(
    width = 0.75
  ) +

  geom_text(
    aes(
       label = label_n
    ),
    size = 2.4,
    colour = "grey25",
    vjust = -0.35
  ) +

  facet_wrap(
    ~ panel,
    ncol = 2,
    scales = "free_y"
  ) +



  scale_fill_manual(
    values = couleurs_panel,
    guide = "none"
  ) +

  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(
      mult = c(
        0,
        0.18
      )
    )
  ) +

  labs(
    title = NULL,
    x = "Mois de prélèvement",
    y = "Taux de positivité (%)",
    caption = "Les effectifs indiqués au-dessus des barres correspondent au nombre total de
prélèvements analysés."
  ) +

  theme_minimal() +

  theme(
    plot.title = element_text(
       face = "bold",
       size = 15,
       hjust = 0.5
    ),
    axis.title = element_text(
       face = "bold"
    ),
    axis.text.x = element_text(
       angle = 45,
       hjust = 1
    ),
    strip.text = element_text(
       face = "bold",
       size = 12
    ),
    plot.caption = element_text(
       size = 9,
       hjust = 0
    )
  )

# -----------------------------------------------------------------------
# 5. Affichage
# -----------------------------------------------------------------------

fig3

# -----------------------------------------------------------------------
# 6. Export
# -----------------------------------------------------------------------

ggsave(
  filename = file.path(
     out_dir_figures,
     "Figure_3_taux_positivite_virus_matrice.png"
  ),
  plot = fig3,
  width = 12,
  height = 8,
  dpi = 300
)

