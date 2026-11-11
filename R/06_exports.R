# 8. EXPORTS FINAUX ----------------------------------------------------

## 8.1 Export tables finales -------------------------------------------

# Export Odds Ratios WNV modèle spline

write.csv(
  coef_wnv_spline,
  file = file.path(
     out_dir_figures,
     "Table_OR_WNV_spline.csv"
  ),
  row.names = FALSE
)

# Export Odds Ratios USUV modèle final

write.csv(
  OR_USUV,
  file = file.path(
     out_dir_figures,
     "Table_OR_USUV.csv"
  ),
  row.names = FALSE
)

# Export VIF WNV

write.csv(
  data.frame(
     Variable = names(vif_wnv),
     VIF = as.numeric(vif_wnv)
  ),
  file = file.path(
     out_dir_figures,
     "Table_VIF_WNV.csv"
  ),
  row.names = FALSE
)

# Export VIF USUV

write.csv(


    data.frame(
       Variable = names(vif_usuv),
       VIF = as.numeric(vif_usuv)
    ),
    file = file.path(
       out_dir_figures,
       "Table_VIF_USUV.csv"
    ),
    row.names = FALSE
)

# Export table Figure 3

write.csv(
  fig3_summary,
  file = file.path(
     out_dir_figures,
     "Table_Figure_3_taux_positivite_virus_matrice.csv"
  ),
  row.names = FALSE
)



# Export prédictions saisonnières WNV spline

write.csv(
  pred_saison_wnv,
  file = file.path(
     out_dir_figures,
     "Table_predictions_saison_WNV_spline.csv"
  ),
  row.names = FALSE
)

## 8.2 Export modèles finaux -------------------------------------------

# Modèle final WNV

saveRDS(
  glmm_wnv_6,
  file = file.path(
    out_dir_figures,
    "Modele_final_GLMM_WNV_spline.rds"
  )
)
# Modèle final USUV

saveRDS(
  glm_usuv_1,
  file = file.path(
    out_dir_figures,
    "Modele_final_GLM_USUV.rds"
  )
)

# Modèles exploratoires Culex

saveRDS(
  glmm_wnv_culex,
  file = file.path(
    out_dir_figures,
    "Modele_exploratoire_GLMM_WNV_Culex.rds"
  )
)

saveRDS(
  glm_usuv_culex,
  file = file.path(
    out_dir_figures,
    "Modele_exploratoire_GLM_USUV_Culex.rds"
  )
)

## 8.3 Export données cartographiques ----------------------------------

data_sites_carto_2025 <- DB_FULL %>%
  filter(
    !is.na(Pos_Neg_WNV),
    !is.na(Pos_Neg_USUV)
  ) %>%
  group_by(
    site_acronyme,
    x,


    y
  ) %>%
  summarise(
    n_total = n(),
    n_pos_WNV = sum(Pos_Neg_WNV == "POS", na.rm = TRUE),
    n_pos_USUV = sum(Pos_Neg_USUV == "POS", na.rm = TRUE),
    .groups = "drop"
  )

write.csv(
  data_sites_carto_2025,
  file = file.path(
     out_dir_figures,
     "data_sites_carto_2025.csv"
  ),
  row.names = FALSE,
  quote = FALSE
)

## 8.4 Sauvegarde environnement R --------------------------------------

save.image(
  file = file.path(
    out_dir_figures,
    "Environnement_R_final.RData"
  )
)

cat("\nExports finaux terminés.\n")

# ============================================================
