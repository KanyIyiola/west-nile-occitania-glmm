# 9. Modélisation de la quantification de l'ARN viral WNV
# ============================================================
# Objectif :
# Étudier les déterminants environnementaux associés à la
# concentration en ARN du WNV (copies/mL) dans les matrices
# environnementales.
#
# La structure des modèles reprend celle utilisée pour les
# analyses de détection afin de faciliter la comparaison des
# résultats.
#
# Les échantillons négatifs sont conservés et leur concentration
# est fixée à 0 copie/mL.
# ============================================================



# ============================================================
# 9.1 Préparation de la base d'analyse
# ============================================================

# Conservation des prélèvements issus du réseau de surveillance

DB_Mis <- DB_FULL_CLC[
  DB_FULL_CLC$CaractereEchant == "Reseau",
]

# Ajout des variables écologiques Culex

DB_Mis$culex_wint <- valeurs_culex[, 2]
DB_Mis$culex_abund_wint <- valeurs_culex_abund[, 2]

# Ordre chronologique des mois

DB_Mis <- DB_Mis %>%
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

DB_Mis <- DB_Mis %>%


  mutate(
    temp_s = as.numeric(scale(cop_temp_mean_15d)),
    precip_s = as.numeric(scale(cop_precip_sum_15d)),
    rad_s = as.numeric(scale(cop_rad_sum_24h))
  )

# Variables spatiales et saisonnières

DB_Mis <- DB_Mis %>%
  mutate(
    doy = as.integer(format(date_releve_dt, "%j")),
    sy = as.numeric(scale(y)),
    sx = as.numeric(scale(x))
  )

# Les prélèvements négatifs sont codés à 0 copie/mL

DB_Mis[
  DB_Mis$Pos_Neg_WNV == "NEG" &
    !is.na(DB_Mis$Pos_Neg_WNV),
]$Conc._WNV_Cop.mL.1 <- 0

# ============================================================
# 9.2 Modèles météorologiques de quantification du WNV
# ============================================================
# Objectif :
# Évaluer l'association entre la concentration en ARN du WNV
# (copies/mL) et les conditions météorologiques.
#
# Variables d'intérêt :
# - Température moyenne sur les 15 jours précédant le prélèvement
# - Précipitations cumulées sur les 15 jours précédant le prélèvement
# - Rayonnement solaire cumulé sur les 24 heures précédant le prélèvement
#
# Les modèles sont ajustés sur :
# - les coordonnées spatiales (x, y),
# - la saisonnalité (spline naturelle sur le jour de l'année),
# - un effet aléatoire du site de prélèvement.
# ============================================================

glmm_wnv_ZHQ <- glmmTMB(
  Conc._WNV_Cop.mL.1 ~
     temp_s +
     precip_s +
     rad_s +
     sy +
     sx +
     ns(doy, df = 4) +
     (1 | site_acronyme),
  data = DB_Mis[
     DB_Mis$MatrixType == "Zones humides" &
       !is.na(DB_Mis$Pos_Neg_WNV),
  ],
  family = tweedie(link = "log")
)

summary(glmm_wnv_ZHQ)

sjPlot::tab_model(
  glmm_wnv_ZHQ
)



glmm_wnv_MXQ <- glmmTMB(
  Conc._WNV_Cop.mL.1 ~
     temp_s +
     precip_s +
     rad_s +
     sy +
     sx +
     ns(doy, df = 4) +
     (1 | site_acronyme),
  data = DB_Mis[
     DB_Mis$MatrixType == "Xénomonitoring" &
       !is.na(DB_Mis$Pos_Neg_WNV),
  ],
  family = tweedie(link = "log")
)

summary(glmm_wnv_MXQ)

sjPlot::tab_model(
  glmm_wnv_MXQ


)

# ============================================================
# 9.3 Modèles biologiques de quantification du WNV
# ============================================================
# Objectif :
# Évaluer l'association entre la concentration en ARN du WNV
# (copies/mL) et les facteurs biologiques impliqués dans son
# cycle de transmission.
#
# Variables d'intérêt :
# - Bird Risk Index (BRI)
# - Probabilité de présence de Culex pipiens
# - Abondance prédite de Culex pipiens
#
# Les modèles sont ajustés sur :
# - les coordonnées spatiales (x, y),
# - la saisonnalité (spline naturelle sur le jour de l'année),
# - un effet aléatoire du site de prélèvement.
# ============================================================

glmm_wnv_ZHQ_Bio <- glmmTMB(
  Conc._WNV_Cop.mL.1 ~
     bri_mean +
     culex_wint +
     culex_abund_wint +
     sy +
     sx +
     ns(doy, df = 4) +
     (1 | site_acronyme),
  data = DB_Mis[
     DB_Mis$MatrixType == "Zones humides" &
       !is.na(DB_Mis$Pos_Neg_WNV),
  ],
  family = tweedie(link = "log")
)

summary(glmm_wnv_ZHQ_Bio)

sjPlot::tab_model(
  glmm_wnv_ZHQ_Bio
)



glmm_wnv_MXQ_Bio <- glmmTMB(
  Conc._WNV_Cop.mL.1 ~
     bri_mean +
     culex_wint +
     culex_abund_wint +
     sy +
     sx +
     ns(doy, df = 4) +
     (1 | site_acronyme),
  data = DB_Mis[
     DB_Mis$MatrixType == "Xénomonitoring" &
       !is.na(DB_Mis$Pos_Neg_WNV),
  ],
  family = tweedie(link = "log")
)

summary(glmm_wnv_MXQ_Bio)

sjPlot::tab_model(
  glmm_wnv_MXQ_Bio
)

# ============================================================
# 9.4 Modèles paysagers de quantification du WNV – CLC 150 m
# ============================================================
# Objectif :
# Évaluer l'association entre la concentration en ARN du WNV
# (copies/mL) et la composition paysagère locale autour des sites.
#
# Variables d'intérêt :
# - Proportions des grandes classes d'occupation du sol
#   dans un rayon de 150 m autour du point de prélèvement
# - Niveau CLC code 1
#
# La classe « Zones artificialisées » (CLC code 1) est retirée du modèle afin de servir de
# référence implicite et d'éviter la colinéarité parfaite entre
# proportions paysagères.
#
# Les modèles sont ajustés sur :


# - les coordonnées spatiales (x, y),
# - la saisonnalité (spline naturelle sur le jour de l'année),
# - un effet aléatoire du site de prélèvement.
# ============================================================

glmm_wnv_ZHQ_CLC150 <- glmmTMB(
  Conc._WNV_Cop.mL.1 ~
     clc150_code1_2 +
     clc150_code1_3 +
     clc150_code1_4 +
     sy +
     sx +
     ns(doy, df = 4) +
     (1 | site_acronyme),
  data = DB_Mis[
     DB_Mis$MatrixType == "Zones humides" &
       !is.na(DB_Mis$Pos_Neg_WNV),
  ],
  family = tweedie(link = "log")
)

summary(glmm_wnv_ZHQ_CLC150)

sjPlot::tab_model(
  glmm_wnv_ZHQ_CLC150
)



glmm_wnv_MXQ_CLC150 <- glmmTMB(
  Conc._WNV_Cop.mL.1 ~
     clc150_code1_2 +
     clc150_code1_3 +
     clc150_code1_4 +
     sy +
     sx +
     ns(doy, df = 4) +
     (1 | site_acronyme),
  data = DB_Mis[
     DB_Mis$MatrixType == "Xénomonitoring" &
       !is.na(DB_Mis$Pos_Neg_WNV),
  ],
  family = tweedie(link = "log")
)

summary(glmm_wnv_MXQ_CLC150)

sjPlot::tab_model(
  glmm_wnv_MXQ_CLC150
)



# ============================================================
# 9.5 Modèles paysagers de quantification du WNV – CLC 2000 m
# ============================================================
# Objectif :
# Évaluer l'association entre la concentration en ARN du WNV
# (copies/mL) et la composition paysagère à l'échelle du paysage.
#
# Variables d'intérêt :
# - Proportions des grandes classes d'occupation du sol
#   dans un rayon de 2000 m autour du point de prélèvement
# - Niveau CLC code 1
#
# La classe « Zones artificialisées » (CLC code 1) est utilisée
# comme catégorie de référence. Elle est retirée du modèle afin
# d'éviter la colinéarité parfaite entre les proportions
# d'occupation du sol.
#
# Les modèles sont ajustés sur :
# - les coordonnées spatiales (x, y),
# - la saisonnalité (spline naturelle sur le jour de l'année),
# - un effet aléatoire du site de prélèvement.
# ============================================================

glmm_wnv_ZHQ_CLC2000 <- glmmTMB(
  Conc._WNV_Cop.mL.1 ~
    clc2000_code1_2 +
    clc2000_code1_3 +
    clc2000_code1_4 +
    sy +
    sx +
    ns(doy, df = 4) +
    (1 | site_acronyme),


    data = DB_Mis[
       DB_Mis$MatrixType == "Zones humides" &
         !is.na(DB_Mis$Pos_Neg_WNV),
    ],
    family = tweedie(link = "log")
)

summary(glmm_wnv_ZHQ_CLC2000)

sjPlot::tab_model(
  glmm_wnv_ZHQ_CLC2000
)



glmm_wnv_MXQ_CLC2000 <- glmmTMB(
  Conc._WNV_Cop.mL.1 ~
     clc2000_code1_2 +
     clc2000_code1_3 +
     clc2000_code1_4 +
     sy +
     sx +
     ns(doy, df = 4) +
     (1 | site_acronyme),
  data = DB_Mis[
     DB_Mis$MatrixType == "Xénomonitoring" &
       !is.na(DB_Mis$Pos_Neg_WNV),
  ],
  family = tweedie(link = "log")
)

summary(glmm_wnv_MXQ_CLC2000)

sjPlot::tab_model(
  glmm_wnv_MXQ_CLC2000
)

# ============================================================
# 9.6 Évaluation de l'effet global de la saisonnalité
# ============================================================
# Objectif :
# Évaluer la contribution globale de la composante saisonnière
# dans chacun des modèles de quantification.
#
# Pour chaque modèle, le modèle complet est comparé à un modèle
# identique dont la spline naturelle sur le jour de l'année
# (DOY) a été retirée.
#
# Les deux modèles sont comparés à l'aide d'un test du rapport
# de vraisemblance (Likelihood Ratio Test).
#
# H0 : la composante saisonnière n'améliore pas significativement
#      l'ajustement du modèle.
#
# H1 : l'ajout de la composante saisonnière améliore
#      significativement le modèle.
# ============================================================

# ============================================================
# 9.6 Évaluation de l'effet global de la saisonnalité
# ============================================================
# Objectif :
# Évaluer la contribution globale de la composante saisonnière
# dans chacun des modèles de quantification.
#
# Pour chaque modèle, le modèle complet est comparé à un modèle
# identique dont la spline naturelle sur le jour de l'année
# (DOY) a été retirée.
#
# Les deux modèles sont comparés à l'aide d'un test du rapport
# de vraisemblance.
# ============================================================

cat("\n==============================\n")
cat("METEO ZH\n")
cat("==============================\n")

model_cut_meteo_ZH <- update(
  glmm_wnv_ZHQ,
  . ~ . - ns(doy, df = 4)
)

anova(
  model_cut_meteo_ZH,


    glmm_wnv_ZHQ,
    test = "Chisq"
)



cat("\n==============================\n")
cat("METEO MX\n")
cat("==============================\n")

model_cut_meteo_MX <- update(
  glmm_wnv_MXQ,
  . ~ . - ns(doy, df = 4)
)

anova(
  model_cut_meteo_MX,
  glmm_wnv_MXQ,
  test = "Chisq"
)



cat("\n==============================\n")
cat("BIOTIQUE ZH\n")
cat("==============================\n")

model_cut_bio_ZH <- update(
  glmm_wnv_ZHQ_Bio,
  . ~ . - ns(doy, df = 4)
)

anova(
  model_cut_bio_ZH,
  glmm_wnv_ZHQ_Bio,
  test = "Chisq"
)



cat("\n==============================\n")
cat("BIOTIQUE MX\n")
cat("==============================\n")

model_cut_bio_MX <- update(
  glmm_wnv_MXQ_Bio,
  . ~ . - ns(doy, df = 4)
)

anova(
  model_cut_bio_MX,
  glmm_wnv_MXQ_Bio,
  test = "Chisq"
)



cat("\n==============================\n")
cat("CLC150 ZH\n")
cat("==============================\n")

model_cut_clc150_ZH <- update(
  glmm_wnv_ZHQ_CLC150,
  . ~ . - ns(doy, df = 4)
)

anova(
  model_cut_clc150_ZH,
  glmm_wnv_ZHQ_CLC150,
  test = "Chisq"
)



cat("\n==============================\n")
cat("CLC150 MX\n")
cat("==============================\n")

model_cut_clc150_MX <- update(
  glmm_wnv_MXQ_CLC150,
  . ~ . - ns(doy, df = 4)
)

anova(
  model_cut_clc150_MX,
  glmm_wnv_MXQ_CLC150,
  test = "Chisq"
)




cat("\n==============================\n")
cat("CLC2000 ZH\n")
cat("==============================\n")

model_cut_clc2000_ZH <- update(
  glmm_wnv_ZHQ_CLC2000,
  . ~ . - ns(doy, df = 4)
)

anova(
  model_cut_clc2000_ZH,
  glmm_wnv_ZHQ_CLC2000,
  test = "Chisq"
)



cat("\n==============================\n")
cat("CLC2000 MX\n")
cat("==============================\n")

model_cut_clc2000_MX <- update(
  glmm_wnv_MXQ_CLC2000,
  . ~ . - ns(doy, df = 4)
)

anova(
  model_cut_clc2000_MX,
  glmm_wnv_MXQ_CLC2000,
  test = "Chisq"
)

# ============================================================
# 9.7 Figure temporelle de positivité WNV par matrice
# ============================================================
# Objectif :
# Visualiser l'évolution temporelle du taux de positivité WNV
# selon le type de matrice environnementale.
#
# La figure combine :
# - le taux de positivité WNV par semaine,
# - le nombre total de prélèvements,
# - une séparation entre Zones humides et Xénomonitoring.
# ============================================================

test_semaine <- DB_Mis %>%
  filter(
    MatrixType %in% c("Zones humides", "Xénomonitoring"),
    !is.na(Pos_Neg_WNV)
  ) %>%
  group_by(MatrixType, semaine) %>%
  summarise(
    n_total = n(),
    n_pos = sum(Pos_Neg_WNV == "POS"),
    taux_pos = 100 * n_pos / n_total,
    .groups = "drop"
  )

# Ordre chronologique des semaines

test_semaine$semaine <- factor(
  test_semaine$semaine,
  levels = sort(unique(test_semaine$semaine))
)

# Facteur de mise à l'échelle pour afficher les effectifs
# sur l'axe secondaire

coef_scale <- max(test_semaine$n_total) / 50

ggplot(
  test_semaine,
  aes(x = semaine)
) +
  geom_col(
    aes(
       y = n_total / coef_scale,
       fill = MatrixType
    ),
    alpha = 0.25,
    width = 0.8
  ) +


  geom_text(
    aes(
       y = n_total / coef_scale + 1,
       label = n_total
    ),
    size = 3
  ) +
  geom_line(
    aes(
       y = taux_pos,
       colour = MatrixType,
       group = MatrixType
    ),
    linewidth = 1.3
  ) +
  geom_point(
    aes(
       y = taux_pos,
       colour = MatrixType
    ),
    size = 3
  ) +
  facet_wrap(
    ~ MatrixType,
    ncol = 1
  ) +
  scale_fill_manual(
    values = c(
       "Zones humides" = "#7AA35A",
       "Xénomonitoring" = "#E8892E"
    )
  ) +
  scale_colour_manual(
    values = c(
       "Zones humides" = "#2F6B2F",
       "Xénomonitoring" = "#D96C00"
    )
  ) +
  scale_y_continuous(
    name = "Taux de positivité WNV (%)",
    sec.axis = sec_axis(
       ~ . * coef_scale,
       name = "Nombre de prélèvements"
    )
  ) +
  labs(
    x = "Semaines de prélèvement"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    strip.text = element_text(
       face = "bold",
       size = 15
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# ============================================================
# 9.8 Exports des modèles de quantification WNV
# ============================================================

saveRDS(
  glmm_wnv_ZHQ,
  file = file.path(out_dir_models, "Modele_quantification_WNV_meteo_ZH.rds")
)

saveRDS(
  glmm_wnv_MXQ,
  file = file.path(out_dir_models, "Modele_quantification_WNV_meteo_MX.rds")
)

saveRDS(
  glmm_wnv_ZHQ_Bio,
  file = file.path(out_dir_models, "Modele_quantification_WNV_biotique_ZH.rds")
)

saveRDS(
  glmm_wnv_MXQ_Bio,
  file = file.path(out_dir_models, "Modele_quantification_WNV_biotique_MX.rds")
)



saveRDS(
  glmm_wnv_ZHQ_CLC150,
  file = file.path(out_dir_models, "Modele_quantification_WNV_CLC150_ZH.rds")
)

saveRDS(
  glmm_wnv_MXQ_CLC150,
  file = file.path(out_dir_models, "Modele_quantification_WNV_CLC150_MX.rds")
)

saveRDS(
  glmm_wnv_ZHQ_CLC2000,
  file = file.path(out_dir_models, "Modele_quantification_WNV_CLC2000_ZH.rds")
)

saveRDS(
  glmm_wnv_MXQ_CLC2000,
  file = file.path(out_dir_models, "Modele_quantification_WNV_CLC2000_MX.rds")
)

cat("\nModèles de quantification WNV exportés.\n")

# ============================================================
# 10. Contrôles finaux
# ============================================================
# Objectif :
# Vérifier que les principales bases, figures et modèles produits
# par le script existent bien dans l'environnement R.
# ============================================================

cat("\n==============================\n")
cat("CONTRÔLES FINAUX\n")
cat("==============================\n")

cat("\nDimensions DB_FULL :\n")
print(dim(DB_FULL))

cat("\nDimensions DB_FULL_CLC :\n")
print(dim(DB_FULL_CLC))

cat("\nDimensions DB_Mis :\n")
print(dim(DB_Mis))

cat("\nModèles de quantification WNV créés :\n")
print(
  c(
     exists("glmm_wnv_ZHQ"),
     exists("glmm_wnv_MXQ"),
     exists("glmm_wnv_ZHQ_Bio"),
     exists("glmm_wnv_MXQ_Bio"),
     exists("glmm_wnv_ZHQ_CLC150"),
     exists("glmm_wnv_MXQ_CLC150"),
     exists("glmm_wnv_ZHQ_CLC2000"),
     exists("glmm_wnv_MXQ_CLC2000")
  )
)

cat("\nScript terminé sans erreur bloquante.\n")




