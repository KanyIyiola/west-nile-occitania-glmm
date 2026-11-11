# 6. MODELISATION STATISTIQUE ------------------------------------------

## 6.1 Préparation des bases analytiques -------------------------------

### 6.1.1 Base WNV -----------------------------------------------------



# Construction base analytique WNV

DB_WNV <- DB_FULL %>%

  filter(!is.na(WNV_bin))




cat("\nDimensions DB_WNV :\n")

print(dim(DB_WNV))

cat("\nNombre de sites DB_WNV :\n")

print(
  n_distinct(DB_WNV$site_acronyme)
)

cat("\nRépartition WNV_bin :\n")

print(table(DB_WNV$WNV_bin))

cat("\nNombre de positifs WNV :\n")

print(
  sum(DB_WNV$WNV_bin == 1, na.rm = TRUE)
)

cat("\nPrévalence WNV (%) :\n")

print(
  round(
    100 * mean(DB_WNV$WNV_bin == 1, na.rm = TRUE),
    2
  )
)

# Vérification classes variables

cat("\nClasses DB_WNV :\n")

print(
  sapply(DB_WNV, class)
)

# Conversion variables catégorielles

DB_WNV <- DB_WNV %>%

  mutate(

      site_acronyme = as.factor(site_acronyme),

      mois = as.factor(mois),

      MatrixType = as.factor(MatrixType),

      dpt = as.factor(dpt),

      GeoCluster = as.factor(GeoCluster)

  )

# Vérification finale facteurs

cat("\nStructure facteurs DB_WNV :\n")

print(
  str(
    DB_WNV[
       ,
       c(
          "site_acronyme",
          "mois",
          "MatrixType",
          "dpt",
          "GeoCluster"
       )
    ]
  )
)

# Résumé variables explicatives WNV

cat("\nRésumé variables explicatives WNV :\n")

print(
  summary(
    DB_WNV[
       ,
       c(


                "cop_temp_mean_15d",
                "cop_precip_sum_15d",
                "cop_rad_sum_24h",
                "bri_mean",
                "bri_cv"
            )
        ]
    )
)

# Vérification NA base WNV

cat("\nNA DB_WNV :\n")

print(
  sort(
    colSums(is.na(DB_WNV)),
    decreasing = TRUE
  )
)

### 6.1.2 Base USUV ----------------------------------------------------



# Construction base analytique USUV

DB_USUV <- DB_FULL %>%

    filter(!is.na(USUV_bin))



cat("\nDimensions DB_USUV :\n")

print(dim(DB_USUV))

cat("\nNombre de sites DB_USUV :\n")

print(
  n_distinct(DB_USUV$site_acronyme)
)

cat("\nRépartition USUV_bin :\n")

print(table(DB_USUV$USUV_bin))

cat("\nNombre de positifs USUV :\n")

print(
  sum(DB_USUV$USUV_bin == 1, na.rm = TRUE)
)

cat("\nPrévalence USUV (%) :\n")

print(
  round(
    100 * mean(DB_USUV$USUV_bin == 1, na.rm = TRUE),
    2
  )
)

# Vérification classes variables

cat("\nClasses DB_USUV :\n")

print(
  sapply(DB_USUV, class)
)

# Conversion variables catégorielles

DB_USUV <- DB_USUV %>%

    mutate(

        site_acronyme = as.factor(site_acronyme),

        mois = as.factor(mois),

        MatrixType = as.factor(MatrixType),

        dpt = as.factor(dpt),

        GeoCluster = as.factor(GeoCluster)



  )

# Vérification structure facteurs

cat("\nStructure facteurs DB_USUV :\n")

print(
  str(
    DB_USUV[
       ,
       c(
          "site_acronyme",
          "mois",
          "MatrixType",
          "dpt",
          "GeoCluster"
       )
    ]
  )
)

cat("\nRésumé variables explicatives USUV :\n")

print(
  summary(
    DB_USUV[
       ,
       c(
          "cop_temp_mean_15d",
          "cop_precip_sum_15d",
          "cop_rad_sum_24h",
          "bri_mean",
          "bri_cv"
       )
    ]
  )
)

# Vérification NA base USUV

cat("\nNA DB_USUV :\n")

print(
  sort(
    colSums(is.na(DB_USUV)),
    decreasing = TRUE
  )
)

## 6.2 GLMM WNV --------------------------------------------------------

### 6.2.1 Corrélations variables explicatives --------------------------



# Installation du package corrplot si nécessaire

if (!requireNamespace("corrplot", quietly = TRUE)) {
  install.packages("corrplot")
}

library(corrplot)

# Sélection des variables numériques candidates

vars_num <- DB_WNV %>%

  select(
    cop_temp_mean_15d,
    cop_precip_sum_15d,
    cop_rad_sum_24h,
    bri_mean,
    bri_cv,
    nbr_ad
  )

# Matrice de corrélation de Spearman

cor_matrix <- cor(
  vars_num,
  use = "complete.obs",
  method = "spearman"
)



cat("\nMatrice de corrélation de Spearman :\n")

print(round(cor_matrix, 2))

# Visualisation de la matrice de corrélation

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  number.cex = 0.7
)

### 6.2.2 Modèles GLMM WNV --------------------------------------------

if (!requireNamespace("lme4", quietly = TRUE)) {
  install.packages("lme4")
}

if (!requireNamespace("DHARMa", quietly = TRUE)) {
  install.packages("DHARMa")
}

library(lme4)
library(DHARMa)

# Coordonnées standardisées
DB_WNV <- DB_WNV %>%
  mutate(
    sy = as.numeric(scale(y)),
    sx = as.numeric(scale(x))
  )

# Modèle sans mois ni coordonnées
glmm_wnv_0 <- glmer(
  Pos_Neg_WNV ~
    temp_s +
    precip_s +
    rad_s +
    (1 | site_acronyme),
  data = DB_WNV,
  family = binomial
)

# Modèle avec mois
glmm_wnv_1 <- glmer(
  Pos_Neg_WNV ~
    temp_s +
    precip_s +
    rad_s +
    mois +
    (1 | site_acronyme),
  data = DB_WNV,
  family = binomial
)

# Modèle avec coordonnées spatiales sx/sy
glmm_wnv_2 <- glmer(
  Pos_Neg_WNV ~
    temp_s +
    precip_s +
    rad_s +
    sy +
    sx +
    (1 | site_acronyme),
  data = DB_WNV,
  family = binomial
)

# Modèle avec mois + coordonnées spatiales sx/sy
glmm_wnv_3 <- glmer(
  Pos_Neg_WNV ~
    temp_s +
    precip_s +
    rad_s +
    mois +
    sy +
    sx +
    (1 | site_acronyme),


    data = DB_WNV,
    family = binomial
)

# Comparaison AIC
cat("\nComparaison AIC modèles WNV :\n")
print(
  AIC(
    glmm_wnv_0,
    glmm_wnv_1,
    glmm_wnv_2,
    glmm_wnv_3
  )
)

# Diagnostics DHARMa
res_WNV_0 <- simulateResiduals(glmm_wnv_0)
res_WNV_1 <- simulateResiduals(glmm_wnv_1)
res_WNV_2 <- simulateResiduals(glmm_wnv_2)
res_WNV_3 <- simulateResiduals(glmm_wnv_3)

cat("\nDiagnostics DHARMa modèle WNV 0 :\n")
print(testUniformity(res_WNV_0))
print(testDispersion(res_WNV_0))
print(testOutliers(res_WNV_0))

cat("\nDiagnostics DHARMa modèle WNV 1 :\n")
print(testUniformity(res_WNV_1))
print(testDispersion(res_WNV_1))
print(testOutliers(res_WNV_1))

cat("\nDiagnostics DHARMa modèle WNV 2 :\n")
print(testUniformity(res_WNV_2))
print(testDispersion(res_WNV_2))
print(testOutliers(res_WNV_2))

cat("\nDiagnostics DHARMa modèle WNV 3 :\n")
print(testUniformity(res_WNV_3))
print(testDispersion(res_WNV_3))
print(testOutliers(res_WNV_3))

# Résumé du modèle complet
cat("\nRésumé GLMM WNV modèle avec mois + coordonnées spatiales :\n")
summary(glmm_wnv_3)

### 6.2.3 Odds ratios WNV ----------------------------------------------

# Section reportée.
# Les Odds Ratios finaux seront calculés à partir du modèle spline retenu
# (glmm_wnv_6) après validation du modèle final.

### 6.2.4 Visualisation des Odds Ratios et intervalles de confiance WNV ----

# Section reportée.
# La visualisation finale des Odds Ratios sera produite à partir
# du modèle spline retenu (glmm_wnv_6).

### 6.2.5 Diagnostics modèle WNV ---------------------------------------

# Vérification colinéarité

if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}

library(car)



# Modèle GLM pour calcul VIF

glm_vif_wnv <- glm(

    Pos_Neg_WNV ~

      cop_temp_mean_15d +
      cop_precip_sum_15d +
      cop_rad_sum_24h +
      bri_mean,

    data = DB_WNV,

    family = binomial



)

# Calcul VIF

vif_wnv <- vif(glm_vif_wnv)

cat("\nVariance Inflation Factors (VIF) :\n")

print(vif_wnv)

cat("\nVIF maximum :\n")

print(max(vif_wnv))

# Tous les VIF < 2 :
# absence de colinéarité problématique entre variables explicatives

### 6.2.6 Visualisation effets WNV -------------------------------------

# Section reportée.
# Les effets marginaux seront recalculés à partir du modèle spline retenu
# (glmm_wnv_6) avec les variables météorologiques finales :
# cop_temp_mean_15d, cop_precip_sum_15d et cop_rad_sum_24h.

### 6.2.7 Modèles WNV avec spline saisonnière ---------------------------

library(splines)
library(DHARMa)

# -----------------------------------------------------------------------
# 1. Création des bases par matrice
# -----------------------------------------------------------------------

DB_WNV_ZHMX <- DB_WNV %>%
  mutate(
    doy = as.integer(format(date_releve_dt, "%j")),
    sx = as.numeric(scale(x)),
    sy = as.numeric(scale(y)),
    site_acronyme = as.factor(site_acronyme)
  )

DB_WNV_ZHMX <- DB_WNV_ZHMX %>%
  mutate(
    temp_s = as.numeric(scale(cop_temp_mean_15d)),
    precip_s = as.numeric(scale(cop_precip_sum_15d)),
    rad_s = as.numeric(scale(cop_rad_sum_24h))
  )

DB_WNV_ZH <- DB_WNV_ZHMX %>%
  filter(MatrixType == "Zones humides")

DB_WNV_MX <- DB_WNV_ZHMX %>%
  filter(MatrixType == "Xénomonitoring")

cat("\nEffectifs par matrice - WNV :\n")
print(table(DB_WNV_ZHMX$MatrixType))

cat("\nNombre de lignes ZH :\n")
print(nrow(DB_WNV_ZH))

cat("\nNombre de lignes MX :\n")
print(nrow(DB_WNV_MX))

cat("\nNombre de positifs WNV par matrice :\n")
print(
  DB_WNV_ZHMX %>%
    group_by(MatrixType) %>%
    summarise(
       n_total = n(),
       n_pos = sum(Pos_Neg_WNV == "POS", na.rm = TRUE),
       .groups = "drop"
    )
)

# -----------------------------------------------------------------------
# 2. Modèle toutes matrices : ZH + MX
# -----------------------------------------------------------------------

glmm_wnv_6 <- glmer(
  Pos_Neg_WNV ~
    temp_s +
    precip_s +
    rad_s +


      sy +
      sx +
      ns(doy, df = 4) +
      (1 | site_acronyme),
    data = DB_WNV_ZHMX,
    family = binomial
)

summary(glmm_wnv_6)

res_WNV_6 <- DHARMa::simulateResiduals(glmm_wnv_6)

cat("\nDiagnostics DHARMa WNV spline - toutes matrices :\n")
print(DHARMa::testUniformity(res_WNV_6))
print(DHARMa::testDispersion(res_WNV_6))
print(DHARMa::testOutliers(res_WNV_6))

# Le test d'autocorrélation temporelle peut échouer si plusieurs
# observations partagent le même jour de l'année (DOY).
# Il est donc exécuté uniquement si les temps sont uniques.
if (anyDuplicated(DB_WNV_ZHMX$doy) == 0) {
  print(
    DHARMa::testTemporalAutocorrelation(
      res_WNV_6,
      time = DB_WNV_ZHMX$doy
    )
  )
} else {
  cat("\nAutocorrélation temporelle non testée pour WNV ZH+MX : DOY non uniques.\n")
}

# -----------------------------------------------------------------------
# 3. Modèle Zones humides uniquement
# -----------------------------------------------------------------------

glmm_wnv_6_ZH <- glmer(
  Pos_Neg_WNV ~
    temp_s +
    precip_s +
    rad_s +
    sy +
    sx +
    ns(doy, df = 4) +
    (1 | site_acronyme),
  data = DB_WNV_ZH,
  family = binomial
)

summary(glmm_wnv_6_ZH)

res_WNV_6_ZH <- DHARMa::simulateResiduals(glmm_wnv_6_ZH)

cat("\nDiagnostics DHARMa WNV spline - Zones humides :\n")
print(DHARMa::testUniformity(res_WNV_6_ZH))
print(DHARMa::testDispersion(res_WNV_6_ZH))
print(DHARMa::testOutliers(res_WNV_6_ZH))

if (anyDuplicated(DB_WNV_ZH$doy) == 0) {
  print(
    DHARMa::testTemporalAutocorrelation(
      res_WNV_6_ZH,
      time = DB_WNV_ZH$doy
    )
  )
} else {
  cat("\nAutocorrélation temporelle non testée pour WNV ZH : DOY non uniques.\n")
}

# -----------------------------------------------------------------------
# 4. Modèle Xénomonitoring uniquement
# -----------------------------------------------------------------------

glmm_wnv_6_MX <- glmer(
  Pos_Neg_WNV ~
    temp_s +
    precip_s +
    rad_s +
    sy +
    sx +
    ns(doy, df = 4) +
    (1 | site_acronyme),
  data = DB_WNV_MX,
  family = binomial


)

summary(glmm_wnv_6_MX)

res_WNV_6_MX <- DHARMa::simulateResiduals(glmm_wnv_6_MX)

cat("\nDiagnostics DHARMa WNV spline - Xénomonitoring :\n")
print(DHARMa::testUniformity(res_WNV_6_MX))
print(DHARMa::testDispersion(res_WNV_6_MX))
print(DHARMa::testOutliers(res_WNV_6_MX))

if (anyDuplicated(DB_WNV_MX$doy) == 0) {
  print(
    DHARMa::testTemporalAutocorrelation(
      res_WNV_6_MX,
      time = DB_WNV_MX$doy
    )
  )
} else {
  cat("\nAutocorrélation temporelle non testée pour WNV MX : DOY non uniques.\n")
}

# -----------------------------------------------------------------------
# 5. Comparaison AIC
# -----------------------------------------------------------------------

cat("\nComparaison AIC modèles spline WNV :\n")
print(
  AIC(
    glmm_wnv_6,
    glmm_wnv_6_ZH,
    glmm_wnv_6_MX
  )
)

cat("\nAttention : les AIC ci-dessus concernent des sous-jeux différents.\n")
cat("Ils ne doivent pas être interprétés comme une comparaison stricte de modèles concurrents.\n")

# -----------------------------------------------------------------------
# 6. Fonction de prédiction saisonnière WNV
# -----------------------------------------------------------------------

make_saison_pred <- function(model, data, label) {

    Data_sim <- data.frame(
      doy = seq(
         min(data$doy, na.rm = TRUE),
         max(data$doy, na.rm = TRUE),
         length.out = 200
      ),

        # Variables météo standardisées fixées à leur moyenne (= 0)
        temp_s = 0,
        precip_s = 0,
        rad_s = 0,

        # Coordonnées standardisées fixées à leur moyenne (= 0)
        sx = 0,
        sy = 0
    )

    pred_link <- predict(
      model,
      newdata = Data_sim,
      type = "link",
      re.form = NA,
      se.fit = TRUE
    )

    Data_sim$fit <- plogis(pred_link$fit)
    Data_sim$lwr <- plogis(pred_link$fit - 1.96 * pred_link$se.fit)
    Data_sim$upr <- plogis(pred_link$fit + 1.96 * pred_link$se.fit)
    Data_sim$Matrice <- label

    Data_sim
}

# Prédictions saisonnières séparées par matrice

pred_ZH <- make_saison_pred(
  glmm_wnv_6_ZH,
  DB_WNV_ZH,
  "Zones humides"


)

pred_MX <- make_saison_pred(
  glmm_wnv_6_MX,
  DB_WNV_MX,
  "Xénomonitoring"
)

pred_saison_wnv <- bind_rows(
  pred_ZH,
  pred_MX
)

# Figure de saisonnalité WNV — version finale avec axe mois + jour
fig_saison_wnv <- ggplot(
  pred_saison_wnv,
  aes(
    x = doy,
    y = fit,
    color = Matrice,
    fill = Matrice
  )
) +
  geom_ribbon(
    aes(
       ymin = lwr,
       ymax = upr
    ),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(
    linewidth = 1.1
  ) +
  scale_x_continuous(
    breaks = c(100, 150, 200, 250, 300),
    labels = c("100\n(avr.)", "150\n(mai-juin)", "200\n(juil.)", "250\n(sept.)", "300\n(oct.)")
  ) +
  labs(
    title = "Effet de la saisonnalité sur la probabilité de détection du WNV",
    x = "Jour de l'année",
    y = "Probabilité prédite de détection",
    color = "Matrice",
    fill = "Matrice"
  ) +
  theme_minimal()

print(fig_saison_wnv)

ggsave(
  filename = file.path(
     out_dir_figures,
     "Figure8_saisonnalite_WNV_ZH_MX.png"
  ),
  plot = fig_saison_wnv,
  width = 9,
  height = 5.5,
  dpi = 300
)
# -----------------------------------------------------------------------
# 7. Figure effet saisonnier comparé
# -----------------------------------------------------------------------

fig_saison_wnv <- ggplot(
  pred_saison_wnv,
  aes(
    x = doy,
    y = fit
  )
) +
  geom_ribbon(
    aes(
       ymin = lwr,
       ymax = upr
    ),
    alpha = 0.2
  ) +
  geom_line(
    linewidth = 1
  ) +
  facet_wrap(
    ~ Matrice,
    ncol = 1


  ) +
  labs(
    x = "Jour de l'année (DOY)",
    y = "Probabilité prédite de détection du WNV"
  ) +
  theme_bw() +
  xlim(c(1, 365))

fig_saison_wnv

ggsave(
  filename = file.path(out_dir_figures, "Figure_saison_WNV_spline_matrices.png"),
  plot = fig_saison_wnv,
  width = 8,
  height = 9,
  dpi = 300
)

### 6.2.8 Odds ratios du modèle spline WNV ---------------------------

library(broom.mixed)
library(dplyr)
library(ggplot2)

coef_wnv_spline <- broom.mixed::tidy(
  glmm_wnv_6,
  effects = "fixed",
  conf.int = TRUE,
  exponentiate = TRUE
) %>%
  filter(
    term %in% c(
       "cop_temp_mean_15d",
       "cop_precip_sum_15d",
       "cop_rad_sum_24h",
       "bri_mean"
    )
  ) %>%
  mutate(
    Variable = case_when(
       term == "cop_temp_mean_15d"     ~ "Température moyenne (15 jours)",
       term == "cop_precip_sum_15d" ~ "Précipitations cumulées (15 jours)",
       term == "cop_rad_sum_24h"     ~ "Rayonnement cumulé (24 h)",
       term == "bri_mean"            ~ "Bird Risk Index"
    ),
    Variable = factor(
       Variable,
       levels = rev(c(
          "Température moyenne (15 jours)",
          "Précipitations cumulées (15 jours)",
          "Rayonnement cumulé (24 h)",
          "Bird Risk Index"
       ))
    )
  )

cat("\nOdds ratios modèle spline WNV :\n")
print(coef_wnv_spline)

fig_or_wnv_spline <- ggplot(
  coef_wnv_spline,
  aes(
    x = estimate,
    y = Variable
  )
) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    colour = "grey50"
  ) +
  geom_errorbarh(
    aes(
       xmin = conf.low,
       xmax = conf.high
    ),
    height = 0.20,
    linewidth = 0.8
  ) +
  geom_point(
    size = 3
  ) +
  scale_x_log10() +


  labs(
    x = "Odds ratio (IC95 %, échelle logarithmique)",
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(face = "bold"),
    axis.text.y = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

fig_or_wnv_spline

ggsave(
  filename = file.path(
     out_dir_figures,
     "Figure_4_OR_GLMM_WNV_spline.png"
  ),
  plot = fig_or_wnv_spline,
  width = 8,
  height = 5,
  dpi = 300
)

## 6.3 Analyses exploratoires USUV -------------------------------------

# NOTE : La structure hiérarchique (GLMM) a été testée initialement.
# Le test de variance de l'effet aléatoire a révélé une singularité (variance nulle).
# Décision méthodologique : Retenir le GLM simple pour garantir la robustesse statistique.

### 6.3.1 Variables explicatives USUV ----------------------------------

# Construction GLMM exploratoire USUV

glmm_wnv_6_MX <- glmer(
  Pos_Neg_WNV ~
    temp_s +
    precip_s +
    rad_s +
    sy +
    sx +
    ns(doy, df = 4) +
    (1 | site_acronyme),
  data = DB_WNV_MX,
  family = binomial
)

# cat("\nRésumé GLMM exploratoire USUV :\n")
# summary(glm_usuv_1)

### 6.3.2 Modèles exploratoires USUV -----------------------------------

glm_usuv_1 <- glm(
  Pos_Neg_USUV ~
    temp_s +
    precip_s +
    rad_s,
  data = DB_USUV,
  family = binomial
)

summary(glm_usuv_1)

### 6.3.3 Sélection modèle final USUV ----------------------------------

# Comparaison AIC

cat("\nComparaison AIC modèles USUV :\n")

print(
  AIC(
    glm_usuv_1
  )
)

# Le GLMM présentait une singularité avec une variance
# quasi nulle de l’effet aléatoire site_acronyme.
# Le GLM simple a donc été retenu comme modèle final
# pour USUV.

cat("\nModèle retenu pour USUV : glm_usuv_1\n")




### 6.3.4 Odds ratios et intervalles de confiance USUV -----------------

# Extraction coefficients modèle

coef_glm_usuv <- summary(glm_usuv_1)$coefficients



# Construction table Odds Ratios

OR_USUV <- data.frame(

    Variable = rownames(coef_glm_usuv),

    Estimate = coef_glm_usuv[, "Estimate"],

    Std_Error = coef_glm_usuv[, "Std. Error"],

    p_value = coef_glm_usuv[, "Pr(>|z|)"]

)

# Calcul Odds Ratios

OR_USUV <- OR_USUV %>%

    mutate(

        OR = exp(Estimate),

        IC_low = exp(Estimate - 1.96 * Std_Error),

        IC_high = exp(Estimate + 1.96 * Std_Error)

    )

cat("\nTable Odds Ratios USUV :\n")

print(OR_USUV)

cat("\nVariables significatives USUV (p < 0.05) :\n")

print(
  OR_USUV %>%
    filter(
       Variable != "(Intercept)",
       p_value < 0.05
    )
)

cat("\nVariables tendance USUV (p < 0.10) :\n")

print(
  OR_USUV %>%
    filter(
       Variable != "(Intercept)",
       p_value < 0.10
    )
)

### 6.3.5 Forest plot Odds Ratios USUV ---------------------------------

# Préparation table OR pour visualisation

OR_USUV_plot <- OR_USUV %>%

    filter(Variable != "(Intercept)") %>%

    mutate(

        Variable_label = dplyr::recode(
           Variable,
           "cop_temp_mean_15d" = "Température moyenne (15 jours)",
           "cop_precip_sum_15d" = "Précipitations cumulées (15 jours)",
           "cop_rad_sum_24h" = "Rayonnement cumulé (24 h)",
           "bri_mean" = "BRI moyen"
        ),

        Variable_label = factor(
          Variable_label,
          levels = rev(c(
            "Température moyenne (15 jours)",
            "Précipitations cumulées (15 jours)",
            "Rayonnement cumulé (24 h)",


            "BRI moyen"
          ))
      )
  )

# Forest plot Odds Ratios USUV

ggplot(
  OR_USUV_plot,
  aes(
    x = OR,
    y = Variable_label
  )
) +

  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "grey40"
  ) +

  geom_errorbarh(
    aes(
       xmin = IC_low,
       xmax = IC_high
    ),
    height = 0.2,
    color = "#4a4038"
  ) +

  geom_point(
    size = 3.5,
    color = "#7b4b2a"
  ) +

  scale_x_log10() +

  labs(
    title = "Odds Ratios du modèle USUV",
    subtitle = "GLM binomial",
    x = "Odds Ratio, échelle logarithmique",
    y = "Variables explicatives"
  ) +

  theme_minimal() +

  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title = element_text(face = "bold")
  )

### 6.3.6 Visualisation effets USUV ------------------------------------

# Effet température

eff_temp_usuv <- ggpredict(
  glm_usuv_1,
  terms = "temp_s"
)

plot(eff_temp_usuv) +

  labs(
    title = "Effet de la température moyenne sur la probabilité USUV",
    x = "Température moyenne sur 15 jours (°C)",
    y = "Probabilité prédite USUV"
  ) +

  theme_minimal()




# Effet précipitations

eff_precip_usuv <- ggpredict(
  glm_usuv_1,
  terms = "precip_s"
)

plot(eff_precip_usuv) +

  labs(
    title = "Effet des précipitations sur la probabilité USUV",


    x = "Précipitations cumulées sur 15 jours (mm)",
    y = "Probabilité prédite USUV"
  ) +

  theme_minimal()



# Effet rayonnement solaire

eff_rad_usuv <- ggpredict(
  glm_usuv_1,
  terms = "rad_s"
)

plot(eff_rad_usuv) +

  labs(
    title = "Effet du rayonnement solaire sur la probabilité USUV",
    x = "Rayonnement cumulé sur 24 h (MJ/m²)",
    y = "Probabilité prédite USUV"
  ) +

  theme_minimal()

## 6.4 Analyses complémentaires ----------------------------------------

### 6.4.1 Vérification colinéarité modèles ------------------------------



# Vérification colinéarité WNV

vif_wnv <- vif(glm_vif_wnv)

cat("\nVariance Inflation Factors (VIF) WNV :\n")

print(vif_wnv)

cat("\nVIF maximum WNV :\n")

print(max(vif_wnv))

# Vérification colinéarité USUV

vif_usuv <- vif(glm_usuv_1)

cat("\nVariance Inflation Factors (VIF) USUV :\n")

print(vif_usuv)

cat("\nVIF maximum USUV :\n")

print(max(vif_usuv))

# Tous les VIF observés sont inférieurs à 5,
# et la majorité sont largement inférieurs à 2,
# suggérant l'absence de colinéarité problématique.

### 6.4.2 Corrélations variables ---------------------------------------

vars_corr <- DB_WNV %>%
  select(
    cop_temp_mean_15d,
    cop_precip_sum_15d,
    cop_rad_sum_24h
  )

cor_matrix <- cor(
  vars_corr,
  use = "complete.obs",
  method = "pearson"
)

cat("\nMatrice de corrélation :\n")

print(cor_matrix)

cat("\nCorrélation absolue maximale :\n")

print(
  max(
    abs(cor_matrix[upper.tri(cor_matrix)]),
    na.rm = TRUE
  )


)

# Les corrélations observées entre variables explicatives
# restent modérées (< 0.7) suggérant l’absence de redondance
# forte entre covariables environnementales.

### 6.4.3 Sélection variables ------------------------------------------

# Les variables environnementales retenues dans les modèles
# ont été sélectionnées selon :
# - leur pertinence écologique vis-à-vis de la dynamique
#   des moustiques vecteurs et de la circulation virale ;
# - leur disponibilité homogène sur l’ensemble des sites ;
# - l’absence de colinéarité problématique observée
#   entre covariables environnementales.
#
# Les variables climatiques retenues correspondent aux
# fenêtres temporelles présentant le meilleur intérêt
# biologique et statistique pour l’étude :
# - température moyenne sur les 15 jours précédant le prélèvement ;
# - précipitations cumulées sur les 15 jours précédant le prélèvement ;
# - rayonnement solaire cumulé sur les 24 heures précédant le prélèvement.
#
# Des variables biotiques complémentaires ont également
# été explorées, notamment le Bird Risk Index (BRI)
# ainsi que des rasters de présence et d’abondance
# potentielle de Culex pipiens.
#
# Ces variables sont présentées dans la section 6.5.

### 6.5 Modèles biotiques : BRI et Culex -------------------------------

# Analyse exploratoire des déterminants biotiques :
# Bird Risk Index (BRI)
# Présence potentielle de Culex pipiens
# Abondance potentielle de Culex pipiens

library(terra)
library(sf)

# -----------------------------------------------------------------------
# 1. Extraction raster présence potentielle Culex
# -----------------------------------------------------------------------

raster_culex <- rast(path_culex)

cat("\nCRS raster Culex présence :\n")
print(crs(raster_culex))

sites_sf <- st_as_sf(
  DB_FULL,
  coords = c("x", "y"),
  crs = 4326,
  remove = FALSE
)

sites_proj <- st_transform(
  sites_sf,
  crs = crs(raster_culex)
)

valeurs_culex <- terra::extract(
  raster_culex,
  terra::vect(sites_proj)
)

DB_FULL$culex_wint <- valeurs_culex[, 2]

cat("\nRésumé variable Culex Wint :\n")
print(summary(DB_FULL$culex_wint))

cat("\nNombre de NA variable Culex Wint :\n")
print(sum(is.na(DB_FULL$culex_wint)))



# -----------------------------------------------------------------------
# 2. Extraction raster abondance potentielle Culex
# -----------------------------------------------------------------------

raster_culex_abund <- rast(path_culex_abund)

cat("\nCRS raster Culex abondance :\n")
print(crs(raster_culex_abund))



sites_proj_abund <- st_transform(
  sites_sf,
  crs = crs(raster_culex_abund)
)

valeurs_culex_abund <- terra::extract(
  raster_culex_abund,
  terra::vect(sites_proj_abund)
)

DB_FULL$culex_abund_wint <- valeurs_culex_abund[, 2]

cat("\nRésumé variable abondance Culex Wint :\n")
print(summary(DB_FULL$culex_abund_wint))

cat("\nNombre de NA abondance Culex Wint :\n")
print(sum(is.na(DB_FULL$culex_abund_wint)))



# -----------------------------------------------------------------------
# 3. Mise à jour bases analytiques avec variables Culex
# -----------------------------------------------------------------------

  DB_WNV_culex <- DB_FULL %>%
  filter(
    !is.na(Pos_Neg_WNV),
    !is.na(culex_wint),
    !is.na(culex_abund_wint)
  ) %>%
  mutate(
    site_acronyme = as.factor(site_acronyme),
    doy = as.integer(format(date_releve_dt, "%j")),
    sx = as.numeric(scale(x)),
    sy = as.numeric(scale(y))
  )

DB_USUV_culex <- DB_FULL %>%
  filter(
    !is.na(Pos_Neg_USUV),
    !is.na(culex_wint),
    !is.na(culex_abund_wint)
  ) %>%
  mutate(
    site_acronyme = as.factor(site_acronyme)
  )

DB_WNV_culex_ZH <- DB_WNV_culex %>%
  filter(MatrixType == "Zones humides")

DB_WNV_culex_MX <- DB_WNV_culex %>%
  filter(MatrixType == "Xénomonitoring")

cat("\nDimensions DB_WNV_culex :\n")
print(dim(DB_WNV_culex))

cat("\nRépartition WNV dans DB_WNV_culex :\n")
print(table(DB_WNV_culex$Pos_Neg_WNV))

cat("\nDimensions DB_USUV_culex :\n")
print(dim(DB_USUV_culex))

cat("\nRépartition USUV dans DB_USUV_culex :\n")
print(table(DB_USUV_culex$Pos_Neg_USUV))

cat("\nDimensions DB_WNV_culex_ZH :\n")
print(dim(DB_WNV_culex_ZH))

cat("\nRépartition WNV dans DB_WNV_culex_ZH :\n")
print(table(DB_WNV_culex_ZH$Pos_Neg_WNV))

cat("\nDimensions DB_WNV_culex_MX :\n")
print(dim(DB_WNV_culex_MX))

cat("\nRépartition WNV dans DB_WNV_culex_MX :\n")
print(table(DB_WNV_culex_MX$Pos_Neg_WNV))

cat("\nVérification MatrixType DB_WNV_culex_ZH :\n")
print(table(DB_WNV_culex_ZH$MatrixType))

cat("\nVérification MatrixType DB_WNV_culex_MX :\n")
print(table(DB_WNV_culex_MX$MatrixType))



# -----------------------------------------------------------------------
# 4. Modèles biotiques : BRI + Culex
# -----------------------------------------------------------------------

glmm_wnv_culex <- glmer(
  Pos_Neg_WNV ~
    bri_mean +
    culex_wint +
    culex_abund_wint +
    sy +
    sx +
    ns(doy, df = 4) +
    (1 | site_acronyme),
  data = DB_WNV_culex,
  family = binomial
)

cat("\nRésumé GLMM WNV biotique :\n")
summary(glmm_wnv_culex)

cat("\nComparaison AIC WNV modèle météo / modèle biotique :\n")
print(
  AIC(
    glmm_wnv_6,
    glmm_wnv_culex
  )
)

glm_usuv_culex <- glm(
  Pos_Neg_USUV ~
    bri_mean +
    culex_wint +
    culex_abund_wint,
  data = DB_USUV_culex,
  family = binomial
)

cat("\nRésumé GLM USUV biotique :\n")
summary(glm_usuv_culex)

cat("\nComparaison AIC USUV modèle météo / modèle biotique :\n")
print(
  AIC(
    glm_usuv_1,
    glm_usuv_culex
  )
)

### 6.5.1 Extraction raster abondance potentielle Culex ---------------

raster_culex_abund <- rast(path_culex_abund)

cat("\nCRS raster Culex abondance :\n")
print(crs(raster_culex_abund))

sites_proj_abund <- st_transform(
  sites_sf,
  crs = crs(raster_culex_abund)
)

valeurs_culex_abund <- terra::extract(
  raster_culex_abund,
  terra::vect(sites_proj_abund)
)

DB_FULL$culex_abund_wint <- valeurs_culex_abund[, 2]

cat("\nRésumé variable abondance Culex Wint :\n")
print(summary(DB_FULL$culex_abund_wint))

cat("\nNombre de NA abondance Culex Wint :\n")
print(sum(is.na(DB_FULL$culex_abund_wint)))

# -----------------------------------------------------------------------
# Modèle biotique WNV - Zones humides
# -----------------------------------------------------------------------

glmm_wnv_culex_ZH <- glmer(
  Pos_Neg_WNV ~
    bri_mean +
    culex_wint +
    culex_abund_wint +
    sy +


      sx +
      ns(doy, df = 4) +
      (1 | site_acronyme),
    data = DB_WNV_culex_ZH,
    family = binomial
)

cat("\nRésumé GLMM WNV biotique ZH :\n")
summary(glmm_wnv_culex_ZH)



# -----------------------------------------------------------------------
# Modèle biotique WNV - Xénomonitoring
# -----------------------------------------------------------------------

glmm_wnv_culex_MX <- glmer(
  Pos_Neg_WNV ~
    bri_mean +
    culex_wint +
    culex_abund_wint +
    sy +
    sx +
    ns(doy, df = 4) +
    (1 | site_acronyme),
  data = DB_WNV_culex_MX,
  family = binomial
)

cat("\nRésumé GLMM WNV biotique MX :\n")
summary(glmm_wnv_culex_MX)

### 6.5.3 Synthèse interprétative des rasters environnementaux --------

# Les rasters environnementaux de présence et d’abondance
# écologique de Culex pipiens (Wint et al., 2020)
# ainsi que le Bird Risk Index (BRI) ont été explorés
# spatialement sur l’emprise des sites MISArbO.
#
# Les visualisations montraient une cohérence écologique
# globale entre les zones favorables aux vecteurs,
# les gradients environnementaux littoraux
# et la distribution spatiale des détections WNV/USUV.
#
# Cependant, malgré cette cohérence visuelle,
# les variables raster intégrées dans les modèles
# statistiques n’amélioraient pas les performances
# des GLMM/GLM selon les critères AIC et ne présentaient
# pas d’effet statistiquement significatif.
#
# Ces résultats suggèrent que les dynamiques locales
# et temporelles de circulation virale observées
# dans MISArbO sont probablement davantage influencées
# par des conditions environnementales fines
# et des variations hebdomadaires locales
# que par des gradients écologiques continentaux statiques.
#
# Les variables météorologiques locales restent les covariables
# privilégiées dans les modèles finaux, suggérant un rôle potentiel
# des conditions abiotiques à courte échelle spatio-temporelle.

### 6.6 Analyses exploratoires CLC -------------------------------------

# Les variables CLC sont conservées dans DB_FULL_CLC.
# Les modèles ci-dessous testent plusieurs niveaux de catégorisation CLC :
# - code 1 : grandes classes paysagères
# - code 2 : catégorisation plus fine
#
# Une catégorie CLC est retirée dans chaque modèle afin d'éviter
# la colinéarité parfaite entre proportions d'occupation du sol.

DB_WNV_CLC <- DB_FULL_CLC %>%
  filter(!is.na(Pos_Neg_WNV)) %>%
  mutate(
    site_acronyme = as.factor(site_acronyme),
    doy = as.integer(format(date_releve_dt, "%j")),
    sx = as.numeric(scale(x)),
    sy = as.numeric(scale(y))
  )

DB_WNV_CLC_ZH <- DB_WNV_CLC %>%
  filter(MatrixType == "Zones humides")

DB_WNV_CLC_MX <- DB_WNV_CLC %>%


  filter(MatrixType == "Xénomonitoring")



# -----------------------------------------------------------------------
# Modèles CLC code 1
# -----------------------------------------------------------------------

glmm_wnv_clc_ZH <- glmer(
  Pos_Neg_WNV ~
    clc150_code1_1 +
    clc150_code1_2 +
    clc150_code1_3 +
    clc150_code1_4 +
    sx +
    sy +
    ns(doy, df = 4) +
    (1 | site_acronyme),
  data = DB_WNV_CLC_ZH,
  family = binomial
)

glmm_wnv_clc_MX <- glmer(
  Pos_Neg_WNV ~
    clc150_code1_1 +
    clc150_code1_2 +
    clc150_code1_3 +
    clc150_code1_4 +
    sx +
    sy +
    ns(doy, df = 4) +
    (1 | site_acronyme),
  data = DB_WNV_CLC_MX,
  family = binomial
)



# -----------------------------------------------------------------------
# Modèles CLC code 2
# -----------------------------------------------------------------------

glmm_wnv_clc2_ZH <- glmer(
  Pos_Neg_WNV ~
    clc150_code2_11 +
    clc150_code2_12 +
    clc150_code2_13 +
    clc150_code2_14 +
    clc150_code2_21 +
    clc150_code2_22 +
    clc150_code2_23 +
    clc150_code2_24 +
    clc150_code2_31 +
    clc150_code2_32 +
    clc150_code2_33 +
    clc150_code2_41 +
    clc150_code2_42 +
    clc150_code2_51 +
    sx +
    sy +
    ns(doy, df = 4) +
    (1 | site_acronyme),
  data = DB_WNV_CLC_ZH,
  family = binomial
)

glmm_wnv_clc2_MX <- glmer(
  Pos_Neg_WNV ~
    clc150_code2_11 +
    clc150_code2_12 +
    clc150_code2_13 +
    clc150_code2_14 +
    clc150_code2_21 +
    clc150_code2_22 +
    clc150_code2_23 +
    clc150_code2_24 +
    clc150_code2_31 +
    clc150_code2_32 +
    clc150_code2_33 +
    clc150_code2_41 +
    clc150_code2_42 +
    clc150_code2_51 +
    sx +
    sy +
    ns(doy, df = 4) +


      (1 | site_acronyme),
    data = DB_WNV_CLC_MX,
    family = binomial
)

# -----------------------------------------------------------------------
# Modèles CLC code 12
# -----------------------------------------------------------------------

glmm_wnv_clc12_ZH <- glmer(
  Pos_Neg_WNV ~
    clc2000_code12_112 +
    clc2000_code12_211 +
    clc2000_code12_213 +
    clc2000_code12_221 +
    clc2000_code12_242 +
    clc2000_code12_311 +
    clc2000_code12_321 +
    clc2000_code12_411 +
    clc2000_code12_421 +
    sx +
    sy +
    ns(doy, df = 4) +
    (1 | site_acronyme),
  data = DB_WNV_CLC_ZH,
  family = binomial
)

glmm_wnv_clc12_MX <- glmer(
  Pos_Neg_WNV ~
    clc2000_code12_112 +
    clc2000_code12_211 +
    clc2000_code12_213 +
    clc2000_code12_221 +
    clc2000_code12_242 +
    clc2000_code12_311 +
    clc2000_code12_321 +
    clc2000_code12_411 +
    clc2000_code12_421 +
    sx +
    sy +
    ns(doy, df = 4) +
    (1 | site_acronyme),
  data = DB_WNV_CLC_MX,
  family = binomial
)

# -----------------------------------------------------------------------
# Résumés modèles CLC WNV
# -----------------------------------------------------------------------

cat("\nRésumé GLMM WNV CLC code1 ZH :\n")
summary(glmm_wnv_clc_ZH)

cat("\nRésumé GLMM WNV CLC code2 ZH :\n")
summary(glmm_wnv_clc2_ZH)

cat("\nRésumé GLMM WNV CLC code1 MX :\n")
summary(glmm_wnv_clc_MX)

cat("\nRésumé GLMM WNV CLC code2 MX :\n")
summary(glmm_wnv_clc2_MX)

cat("\nRésumé GLMM WNV CLC code12 ZH :\n")
summary(glmm_wnv_clc12_ZH)

cat("\nRésumé GLMM WNV CLC code12 MX :\n")
summary(glmm_wnv_clc12_MX)

# -----------------------------------------------------------------------
# Comparaison AIC CLC code1 vs code2
# -----------------------------------------------------------------------

cat("\nComparaison AIC CLC code1 vs code2 ZH :\n")
print(
  AIC(
    glmm_wnv_clc_ZH,
    glmm_wnv_clc2_ZH
  )
)

cat("\nComparaison AIC CLC code1 vs code2 MX :\n")
print(


    AIC(
      glmm_wnv_clc_MX,
      glmm_wnv_clc2_MX
    )
)

cat("\nComparaison AIC CLC code1 vs code2 vs code12 ZH :\n")
print(
  AIC(
    glmm_wnv_clc_ZH,
    glmm_wnv_clc2_ZH,
    glmm_wnv_clc12_ZH
  )
)

cat("\nComparaison AIC CLC code1 vs code2 vs code12 MX :\n")
print(
  AIC(
    glmm_wnv_clc_MX,
    glmm_wnv_clc2_MX,
    glmm_wnv_clc12_MX
  )
)

# -----------------------------------------------------------------------
# Comparaison AIC des modèles WNV séparés par matrice
# -----------------------------------------------------------------------

cat("\nComparaison AIC WNV - modèles ZH et MX :\n")
print(
  AIC(
    glmm_wnv_6_ZH,
    glmm_wnv_6_MX,
    glmm_wnv_culex_ZH,
    glmm_wnv_culex_MX,
    glmm_wnv_clc_ZH,
    glmm_wnv_clc_MX,
    glmm_wnv_clc2_ZH,
    glmm_wnv_clc2_MX
  )
)

DB_USUV_CLC <- DB_FULL_CLC %>%
  filter(!is.na(Pos_Neg_USUV))

glm_usuv_clc <- glm(
  Pos_Neg_USUV ~
    clc150_code1_1 +
    clc150_code1_2 +
    clc150_code1_3 +
    clc150_code1_4,
  data = DB_USUV_CLC,
  family = binomial
)

cat("\nRésumé GLM USUV exploratoire CLC :\n")
summary(glm_usuv_clc)

cat("\nComparaison AIC USUV météo / biotique / CLC :\n")
print(
  AIC(
    glm_usuv_1,
    glm_usuv_culex,
    glm_usuv_clc
  )
)

### 6.6.2 Modèles exploratoires CLC 2000 m -----------------------------

# Modèles exploratoires intégrant les variables CLC niveau 1
# dans un buffer de 2000 m autour des points de prélèvement.
# Ces modèles sont préparés afin de pouvoir être relancés
# dès réception du fichier CLC corrigé.

glmm_wnv_clc2000 <- glmer(
  Pos_Neg_WNV ~
    clc2000_code1_1 +
    clc2000_code1_2 +
    clc2000_code1_3 +
    clc2000_code1_4 +
    sx +
    sy +
    ns(doy, df = 4) +


      (1 | site_acronyme),
    data = DB_WNV_CLC,
    family = binomial
)

cat("\nRésumé GLMM WNV exploratoire CLC 2000 m :\n")
summary(glmm_wnv_clc2000)

cat("\nComparaison AIC WNV - 6 modèles séparés par matrice :\n")
print(
  AIC(
    glmm_wnv_6_ZH,
    glmm_wnv_6_MX,
    glmm_wnv_culex_ZH,
    glmm_wnv_culex_MX,
    glmm_wnv_clc_ZH,
    glmm_wnv_clc_MX
  )
)

glm_usuv_clc2000 <- glm(
  Pos_Neg_USUV ~
    clc2000_code1_1 +
    clc2000_code1_2 +
    clc2000_code1_3 +
    clc2000_code1_4,
  data = DB_USUV_CLC,
  family = binomial
)

cat("\nRésumé GLM USUV exploratoire CLC 2000 m :\n")
summary(glm_usuv_clc2000)

cat("\nComparaison AIC USUV météo / biotique / CLC 150 m / CLC 2000 m :\n")
print
  AIC(
    glmm_wnv_6_ZH,
    glmm_wnv_6_MX,
    glmm_wnv_culex_ZH,
    glmm_wnv_culex_MX,
    glmm_wnv_clc_ZH,
    glmm_wnv_clc_MX
  )

### 6.6.3 Modèles exploratoires CLC 2000 m - code_12 sélectionnés -------

# Modèles exploratoires utilisant les classes CLC code_12 sélectionnées
# avec mes encadrants, après regroupement de certaines classes :
# 311 + 313, 321 + 323, 242 + 243.

glmm_wnv_clc2000_code12 <- glmer(
  Pos_Neg_WNV ~
    clc2000_code12_112 +
    clc2000_code12_211 +
    clc2000_code12_213 +
    clc2000_code12_221 +
    clc2000_code12_242 +
    clc2000_code12_311 +
    clc2000_code12_321 +
    clc2000_code12_411 +
    clc2000_code12_421 +
    clc2000_code12_521 +
    sx +
    sy +
    ns(doy, df = 4) +
    (1 | site_acronyme),
  data = DB_WNV_CLC,
  family = binomial
)

cat("\nRésumé GLMM WNV exploratoire CLC 2000 m code_12 sélectionnés :\n")
summary(glmm_wnv_clc2000_code12)

cat("\nComparaison AIC WNV - 6 modèles séparés par matrice :\n")
print(
  AIC(
    glmm_wnv_6_ZH,
    glmm_wnv_6_MX,
    glmm_wnv_culex_ZH,
    glmm_wnv_culex_MX,
    glmm_wnv_clc_ZH,
    glmm_wnv_clc_MX
  )


)

glm_usuv_clc2000_code12 <- glm(
  Pos_Neg_USUV ~
    clc2000_code12_112 +
    clc2000_code12_211 +
    clc2000_code12_213 +
    clc2000_code12_221 +
    clc2000_code12_242 +
    clc2000_code12_311 +
    clc2000_code12_321 +
    clc2000_code12_411 +
    clc2000_code12_421 +
    clc2000_code12_521,
  data = DB_USUV_CLC,
  family = binomial
)

cat("\nRésumé GLM USUV exploratoire CLC 2000 m code_12 sélectionnés :\n")
summary(glm_usuv_clc2000_code12)

cat("\nComparaison AIC USUV modèles CLC :\n")
print(
  AIC(
    glm_usuv_1,
    glm_usuv_culex,
    glm_usuv_clc,
    glm_usuv_clc2000,
    glm_usuv_clc2000_code12
  )
)

### 6.7 Graphes des estimates des modèles ------------------------------

library(broom.mixed)
library(dplyr)
library(ggplot2)

plot_estimates <- function(model, model_name, output_name) {

    est <- broom.mixed::tidy(
      model,
      effects = "fixed",
      conf.int = TRUE
    ) %>%
      filter(term != "(Intercept)") %>%
      mutate(
        modele = model_name
      )

    fig <- ggplot(
      est,
      aes(
        x = estimate,
        y = reorder(term, estimate)
      )
    ) +
      geom_vline(
        xintercept = 0,
        linetype = "dashed"
      ) +
      geom_errorbarh(
        aes(
           xmin = conf.low,
           xmax = conf.high
        ),
        height = 0.2
      ) +
      geom_point(
        size = 3
      ) +
      labs(
        title = model_name,
        x = "Estimate logit",
        y = NULL
      ) +
      theme_minimal()

    print(fig)

    ggsave(
      filename = file.path(
        out_dir_figures,


           output_name
        ),
        plot = fig,
        width = 8,
        height = 5,
        dpi = 300
    )

    return(est)
}

# Graphes WNV

est_wnv_meteo_ZH <- plot_estimates(
  glmm_wnv_6_ZH,
  "WNV - modèle météo ZH",
  "Figure_estimates_WNV_meteo_ZH.png"
)

est_wnv_meteo_MX <- plot_estimates(
  glmm_wnv_6_MX,
  "WNV - modèle météo MX",
  "Figure_estimates_WNV_meteo_MX.png"
)

est_wnv_biotique_ZH <- plot_estimates(
  glmm_wnv_culex_ZH,
  "WNV - modèle biotique ZH",
  "Figure_estimates_WNV_biotique_ZH.png"
)

est_wnv_biotique_MX <- plot_estimates(
  glmm_wnv_culex_MX,
  "WNV - modèle biotique MX",
  "Figure_estimates_WNV_biotique_MX.png"
)

est_wnv_clc_ZH <- plot_estimates(
  glmm_wnv_clc_ZH,
  "WNV - modèle CLC ZH",
  "Figure_estimates_WNV_CLC_ZH.png"
)

est_wnv_clc_MX <- plot_estimates(
  glmm_wnv_clc_MX,
  "WNV - modèle CLC MX",
  "Figure_estimates_WNV_CLC_MX.png"
)



# Graphes USUV

#est_usuv_meteo <- plot_estimates(
# glm_usuv_1,
#"USUV - modèle météo",
# "Figure_estimates_USUV_meteo.png"
#)

#est_usuv_biotique <- plot_estimates(
# glm_usuv_culex,
# "USUV - modèle biotique BRI + Culex",
# "Figure_estimates_USUV_biotique.png"
#)

#est_usuv_clc <- plot_estimates(
# glm_usuv_clc,
# "USUV - modèle CLC",
# "Figure_estimates_USUV_CLC.png"
#)

### 6.8 Tableaux des modèles comparatifs --------------------------------

library(parameters)

## Modèles météorologiques
model_parameters(glmm_wnv_6_ZH, exponentiate = TRUE, ci = 0.95)
model_parameters(glmm_wnv_6_MX, exponentiate = TRUE, ci = 0.95)

## Modèles biotiques / entomologiques
model_parameters(glmm_wnv_culex_ZH, exponentiate = TRUE, ci = 0.95)
model_parameters(glmm_wnv_culex_MX, exponentiate = TRUE, ci = 0.95)

## Modèles occupation du sol — CLC 150 m, grandes classes code1


model_parameters(glmm_wnv_clc_ZH, exponentiate = TRUE, ci = 0.95)
model_parameters(glmm_wnv_clc_MX, exponentiate = TRUE, ci = 0.95)

## Modèles occupation du sol — CLC 150 m, classes détaillées code2
model_parameters(glmm_wnv_clc2_ZH, exponentiate = TRUE, ci = 0.95)
model_parameters(glmm_wnv_clc2_MX, exponentiate = TRUE, ci = 0.95)

## Modèles occupation du sol — CLC 2000 m, classes code12
model_parameters(glmm_wnv_clc12_ZH, exponentiate = TRUE, ci = 0.95)
model_parameters(glmm_wnv_clc12_MX, exponentiate = TRUE, ci = 0.95)

cat("\n--- WNV ZH | CLC 150 m | code1 ---\n")
model_parameters(glmm_wnv_clc_ZH, exponentiate = TRUE, ci = 0.95)

cat("\n--- WNV MX | CLC 150 m | code1 ---\n")
model_parameters(glmm_wnv_clc_MX, exponentiate = TRUE, ci = 0.95)

cat("\n--- WNV ZH | CLC 150 m | code2 ---\n")
model_parameters(glmm_wnv_clc2_ZH, exponentiate = TRUE, ci = 0.95)

cat("\n--- WNV MX | CLC 150 m | code2 ---\n")
model_parameters(glmm_wnv_clc2_MX, exponentiate = TRUE, ci = 0.95)

cat("\n--- WNV ZH | CLC 2000 m | code12 ---\n")
model_parameters(glmm_wnv_clc12_ZH, exponentiate = TRUE, ci = 0.95)

cat("\n--- WNV MX | CLC 2000 m | code12 ---\n")
model_parameters(glmm_wnv_clc12_MX, exponentiate = TRUE, ci = 0.95)

library(sjPlot)

model_parameters(glmm_wnv_clc_ZH, exponentiate = TRUE, ci = 0.95)

sjPlot::tab_model(
  glmm_wnv_6_ZH,
  glmm_wnv_6_MX,
  glmm_wnv_culex_ZH,
  glmm_wnv_culex_MX,
  glmm_wnv_clc_ZH,
  glmm_wnv_clc_MX,
  show.ci = TRUE,
  show.se = FALSE,
  show.re.var = TRUE,
  transform = "exp",
  dv.labels = c(
     "WNV météo ZH",
     "WNV météo MX",
     "WNV biotique ZH",
     "WNV biotique MX",
     "WNV CLC ZH",
     "WNV CLC MX"
  ),
  file = file.path(out_dir_tables, "Tableau_6_modeles_WNV.html")
)

sjPlot::tab_model(
  glm_usuv_1,
  glm_usuv_culex,
  glm_usuv_clc,
  show.ci = TRUE,
  show.se = FALSE,
  transform = "exp",
  dv.labels = c(
     "USUV - météo",
     "USUV - biotique",
     "USUV - CLC"
  ),
  file = file.path(out_dir_tables, "Tableau_modeles_USUV.html")
)

# -----------------------------------------------------------------------
# EXPLORATION DESCRIPTIVE DE LA QUANTIFICATION VIRALE (dPCR)
# -----------------------------------------------------------------------

## Préparation : conserver uniquement les prélèvements positifs avec
## une concentration virale renseignée et non nulle

DB_conc_WNV <- DB_FULL %>%
  filter(
    Pos_Neg_WNV == "POS",
    !is.na(culex_wint) | TRUE   # placeholder, retiré si non pertinent
  )



# Note : la variable de concentration n'est pas encore dans DB_FULL,
# elle vient de db_misarbo / db_raw (Conc._WNV_Cop.mL.1, Conc._USUV_Cop.mL.1)
# On la rejoint via num_releve

conc_vars <- db_misarbo %>%
  select(
    num_releve,
    Conc._WNV_Cop.mL.1,
    Conc._USUV_Cop.mL.1
  )

DB_FULL_CONC <- DB_FULL %>%
  left_join(conc_vars, by = "num_releve")

cat("\nNA concentration WNV :\n")
print(sum(is.na(DB_FULL_CONC$Conc._WNV_Cop.mL.1)))

cat("\nNA concentration USUV :\n")
print(sum(is.na(DB_FULL_CONC$Conc._USUV_Cop.mL.1)))

## Résumé concentration WNV chez les positifs uniquement

conc_wnv_pos <- DB_FULL_CONC %>%
  filter(
    Pos_Neg_WNV == "POS",
    !is.na(Conc._WNV_Cop.mL.1),
    Conc._WNV_Cop.mL.1 > 0
  )

cat("\nNombre de positifs WNV avec concentration renseignée :\n")
print(nrow(conc_wnv_pos))

cat("\nRésumé concentration WNV (positifs) :\n")
print(summary(conc_wnv_pos$Conc._WNV_Cop.mL.1))

## Concentration WNV par mois

conc_wnv_mois <- conc_wnv_pos %>%
  group_by(mois) %>%
  summarise(
    n = n(),
    conc_mean = mean(Conc._WNV_Cop.mL.1, na.rm = TRUE),
    conc_median = median(Conc._WNV_Cop.mL.1, na.rm = TRUE),
    conc_max = max(Conc._WNV_Cop.mL.1, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nConcentration WNV par mois (positifs uniquement) :\n")
print(conc_wnv_mois)

## Concentration WNV par matrice

conc_wnv_matrice <- conc_wnv_pos %>%
  group_by(MatrixType) %>%
  summarise(
    n = n(),
    conc_mean = mean(Conc._WNV_Cop.mL.1, na.rm = TRUE),
    conc_median = median(Conc._WNV_Cop.mL.1, na.rm = TRUE),
    conc_max = max(Conc._WNV_Cop.mL.1, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nConcentration WNV par matrice (positifs uniquement) :\n")
print(conc_wnv_matrice)

## Même chose pour USUV

conc_usuv_pos <- DB_FULL_CONC %>%
  filter(
    Pos_Neg_USUV == "POS",
    !is.na(Conc._USUV_Cop.mL.1),
    Conc._USUV_Cop.mL.1 > 0
  )

cat("\nNombre de positifs USUV avec concentration renseignée :\n")
print(nrow(conc_usuv_pos))

cat("\nRésumé concentration USUV (positifs) :\n")
print(summary(conc_usuv_pos$Conc._USUV_Cop.mL.1))

conc_usuv_mois <- conc_usuv_pos %>%
  group_by(mois) %>%
  summarise(


      n = n(),
      conc_mean = mean(Conc._USUV_Cop.mL.1, na.rm = TRUE),
      conc_median = median(Conc._USUV_Cop.mL.1, na.rm = TRUE),
      .groups = "drop"
  )

cat("\nConcentration USUV par mois (positifs uniquement) :\n")
print(conc_usuv_mois)

## Visualisation : concentration WNV par mois (échelle log)

ggplot(
  conc_wnv_pos,
  aes(x = mois, y = Conc._WNV_Cop.mL.1)
) +
  geom_boxplot(fill = "#c96b2c", alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
  scale_y_log10() +
  labs(
    title = "Concentration virale WNV selon le mois (échantillons positifs)",
    x = "Mois",
    y = "Concentration WNV (copies/mL, log10)"
  ) +
  theme_minimal()

## Visualisation : concentration WNV par matrice (échelle log)

ggplot(
  conc_wnv_pos,
  aes(x = MatrixType, y = Conc._WNV_Cop.mL.1, fill = MatrixType)
) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
  scale_y_log10() +
  scale_fill_manual(
    values = c("Zones humides" = "#6c8c6b", "Xénomonitoring" = "#8c6d5a")
  ) +
  labs(
    title = "Concentration virale WNV selon la matrice (échantillons positifs)",
    x = "Type de matrice",
    y = "Concentration WNV (copies/mL, log10)"
  ) +
  theme_minimal()

## Test statistique simple : la concentration diffère-t-elle entre matrices ?
## Wilcoxon car effectifs faibles et distribution probablement non normale

test_conc_matrice <- wilcox.test(
  Conc._WNV_Cop.mL.1 ~ MatrixType,
  data = conc_wnv_pos
)

cat("\nTest Wilcoxon concentration WNV ~ matrice :\n")
print(test_conc_matrice)

# ANALYSE CONCENTRATIONS USUV
conc_usuv_pos <- DB_FULL_CONC %>%
  filter(
    Pos_Neg_USUV == "POS",
    !is.na(Conc._USUV_Cop.mL.1),
    Conc._USUV_Cop.mL.1 > 0
  )

cat("\nNombre de positifs USUV avec concentration renseignée :\n")
print(nrow(conc_usuv_pos))

cat("\nRésumé concentration USUV (positifs) :\n")
print(summary(conc_usuv_pos$Conc._USUV_Cop.mL.1))

conc_usuv_mois <- conc_usuv_pos %>%
  group_by(mois) %>%
  summarise(
    n = n(),
    conc_mean = mean(Conc._USUV_Cop.mL.1, na.rm = TRUE),
    conc_median = median(Conc._USUV_Cop.mL.1, na.rm = TRUE),
    conc_max = max(Conc._USUV_Cop.mL.1, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nConcentration USUV par mois (positifs uniquement) :\n")
print(conc_usuv_mois)

conc_usuv_matrice <- conc_usuv_pos %>%


  group_by(MatrixType) %>%
  summarise(
    n = n(),
    conc_mean = mean(Conc._USUV_Cop.mL.1, na.rm = TRUE),
    conc_median = median(Conc._USUV_Cop.mL.1, na.rm = TRUE),
    conc_max = max(Conc._USUV_Cop.mL.1, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nConcentration USUV par matrice (positifs uniquement) :\n")
print(conc_usuv_matrice)

ggplot(
  conc_usuv_pos,
  aes(x = mois, y = Conc._USUV_Cop.mL.1)
) +
  geom_boxplot(fill = "#5c85d6", alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
  scale_y_log10() +
  labs(
    title = "Concentration virale USUV selon le mois (échantillons positifs)",
    x = "Mois",
    y = "Concentration USUV (copies/mL, log10)"
  ) +
  theme_minimal()

ggplot(
  conc_usuv_pos,
  aes(x = MatrixType, y = Conc._USUV_Cop.mL.1, fill = MatrixType)
) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
  scale_y_log10() +
  scale_fill_manual(
    values = c(
      "Zones humides" = "#6c8c6b",
      "Xénomonitoring" = "#8c6d5a"
    )
  ) +
  labs(
    title = "Concentration virale USUV selon la matrice (échantillons positifs)",
    x = "Type de matrice",
    y = "Concentration USUV (copies/mL, log10)"
  ) +
  theme_minimal()

test_conc_usuv_matrice <- wilcox.test(
  Conc._USUV_Cop.mL.1 ~ MatrixType,
  data = conc_usuv_pos
)

cat("\nTest Wilcoxon concentration USUV ~ matrice :\n")
print(test_conc_usuv_matrice)

