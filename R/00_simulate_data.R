# 00_simulate_data.R ---------------------------------------------------
# Generates a SYNTHETIC dataset with the same structure as DB_FULL
# (the analytic dataset built in 02_build_analytic_dataset.R).
#
# The real MISArbO data belong to the project partners and are not shared.
# Orders of magnitude are kept close to the real study (2025 season):
#   - 44 sites in Occitania: 33 wetland-water sites (ZH), 11 xenomonitoring sites (MX)
#   - 812 samples, April to October 2025 (676 ZH + 136 MX)
#   - 90 WNV-positive and 13 USUV-positive samples
#   - strong seasonality: WNV early in the season for ZH, later for MX
#   - NO true effect of weather or bird index on detection (as in the real analysis)
# All values are random: nothing here can be interpreted biologically.
# ---------------------------------------------------------------------

set.seed(2025)

dir.create("data/simulated", recursive = TRUE, showWarnings = FALSE)

# 1. Sites --------------------------------------------------------------

n_zh <- 33
n_mx <- 11

departements <- data.frame(
  dpt      = c("Hérault", "Gard", "Aude", "Pyrénées-Orientales"),
  code_dpt = c("34", "30", "11", "66"),
  lon_min  = c(3.3, 4.0, 2.8, 2.8),
  lon_max  = c(4.2, 4.5, 3.2, 3.1),
  lat_min  = c(43.3, 43.4, 43.0, 42.5),
  lat_max  = c(43.7, 43.7, 43.3, 42.9),
  stringsAsFactors = FALSE
)

make_sites <- function(n, prefix) {
  d <- sample(seq_len(nrow(departements)), n, replace = TRUE,
              prob = c(0.45, 0.25, 0.2, 0.1))
  data.frame(
    site_acronyme = sprintf("%s%02d", prefix, seq_len(n)),
    dpt           = departements$dpt[d],
    code_dpt      = departements$code_dpt[d],
    x             = runif(n, departements$lon_min[d], departements$lon_max[d]),
    y             = runif(n, departements$lat_min[d], departements$lat_max[d]),
    stringsAsFactors = FALSE
  )
}

sites <- rbind(make_sites(n_zh, "ZH"), make_sites(n_mx, "MX"))
sites$MatrixType <- rep(c("Zones humides", "Xénomonitoring"), c(n_zh, n_mx))
sites$type       <- rep(c("ZH", "MX"), c(n_zh, n_mx))
sites$nom_commune <- sprintf("Commune_%02d", sample(1:30, nrow(sites), replace = TRUE))
sites$insee       <- sprintf("%s%03d", sites$code_dpt, sample(1:300, nrow(sites)))
sites$GeoCluster  <- as.integer(cut(sites$x, breaks = 4))

# Site-level covariates (constant in time)
sites$bri_mean <- round(runif(nrow(sites), 0.1, 0.9), 3)
sites$bri_cv   <- round(runif(nrow(sites), 0.05, 0.6), 3)

# 2. Samples: dates per site --------------------------------------------
# 676 ZH samples (about 20-21 per site) and 136 MX samples (12-13 per site)

n_per_site <- c(
  rep(c(21, 20), length.out = n_zh),
  rep(c(13, 12), length.out = n_mx)
)
# adjust to reach exactly 676 and 136
diff_zh <- 676 - sum(n_per_site[1:n_zh])
diff_mx <- 136 - sum(n_per_site[(n_zh + 1):(n_zh + n_mx)])
n_per_site[1] <- n_per_site[1] + diff_zh
n_per_site[n_zh + 1] <- n_per_site[n_zh + 1] + diff_mx

start_zh <- as.Date("2025-04-01"); end_zh <- as.Date("2025-10-23")
start_mx <- as.Date("2025-05-06"); end_mx <- as.Date("2025-10-23")

DB <- do.call(rbind, lapply(seq_len(nrow(sites)), function(i) {
  s <- sites[i, ]
  rng <- if (s$type == "ZH") c(start_zh, end_zh) else c(start_mx, end_mx)
  dates <- sort(sample(seq(rng[1], rng[2], by = "day"), n_per_site[i]))
  cbind(s[rep(1, n_per_site[i]), ], date_only = dates, row.names = NULL)
}))

DB <- DB[order(DB$date_only, DB$site_acronyme), ]
rownames(DB) <- NULL
n <- nrow(DB)

# 3. Identifiers and time variables --------------------------------------

DB$num_releve     <- seq_len(n)
DB$num_releve_All <- 1000 + seq_len(n)
DB$code_prelev    <- sprintf("%s_%04d", DB$type, seq_len(n))
DB$region         <- "OCCITANIE"
DB$date_releve_dt <- as.POSIXct(DB$date_only, tz = "UTC") + 3600 * 10
DB$annee          <- 2025L
DB$mois <- factor(
  format(DB$date_only, "%m"),
  levels = c("04", "05", "06", "07", "08", "09", "10"),
  labels = c("Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre")
)
DB$semaine <- format(DB$date_only, "%Y-%U")
doy <- as.numeric(format(DB$date_only, "%j"))

# 4. Weather (seasonal pattern + noise; same for all, no link with virus) ---

season <- sin(pi * (doy - 60) / 200)   # smooth rise and fall over the season
DB$cop_temp_mean_15d  <- round(14 + 10 * season + rnorm(n, 0, 1.5), 2)
DB$cop_precip_sum_15d <- round(rgamma(n, shape = 1.2, scale = 12), 1)
DB$cop_rad_sum_24h    <- round(1.2e7 + 0.8e7 * season + rnorm(n, 0, 1.5e6), 0)

DB$temp_s   <- as.numeric(scale(DB$cop_temp_mean_15d))
DB$precip_s <- as.numeric(scale(DB$cop_precip_sum_15d))
DB$rad_s    <- as.numeric(scale(DB$cop_rad_sum_24h))

# 5. Mosquitoes caught (xenomonitoring only) ------------------------------

DB$nbr_ad <- ifelse(DB$type == "MX",
                    rpois(n, lambda = 5 + 25 * pmax(season, 0)), NA_integer_)

# 6. Virus detection --------------------------------------------------------
# Exactly 90 WNV positives and 13 USUV positives, drawn with probabilities that
# depend on matrix, season and a site random effect only.

site_effect <- rnorm(nrow(sites), 0, 0.7)
names(site_effect) <- sites$site_acronyme
re <- site_effect[DB$site_acronyme]

# WNV: early peak for ZH (April-May), later and weaker for MX
logit_wnv <- ifelse(
  DB$type == "ZH",
  -3 + 3.0 * exp(-((doy - 100) / 45)^2),
  -4 + 1.5 * exp(-((doy - 215) / 50)^2)
) + re
p_wnv <- plogis(logit_wnv)

# USUV: later in the season (peak in August)
logit_usuv <- -6 + 2.5 * exp(-((doy - 230) / 35)^2) + 0.3 * re
p_usuv <- plogis(logit_usuv)

pick_exact <- function(p, k) {
  idx <- sample(seq_along(p), size = k, prob = p)
  out <- rep(0L, length(p))
  out[idx] <- 1L
  out
}

DB$WNV_bin  <- pick_exact(p_wnv, 90)
DB$USUV_bin <- pick_exact(p_usuv, 13)
DB$Pos_Neg_WNV  <- factor(ifelse(DB$WNV_bin == 1, "POS", "NEG"),  levels = c("NEG", "POS"))
DB$Pos_Neg_USUV <- factor(ifelse(DB$USUV_bin == 1, "POS", "NEG"), levels = c("NEG", "POS"))

# 7. Column order (same as DB_FULL) and export ----------------------------

DB_FULL <- DB[, c(
  "num_releve", "num_releve_All", "code_prelev",
  "region", "dpt", "code_dpt", "insee", "nom_commune", "site_acronyme",
  "GeoCluster", "x", "y",
  "date_releve_dt", "date_only", "annee", "mois", "semaine",
  "MatrixType", "type",
  "Pos_Neg_WNV", "Pos_Neg_USUV", "WNV_bin", "USUV_bin",
  "cop_temp_mean_15d", "cop_precip_sum_15d", "cop_rad_sum_24h",
  "temp_s", "precip_s", "rad_s",
  "bri_mean", "bri_cv",
  "nbr_ad"
)]

write.csv(DB_FULL, "data/simulated/DB_FULL_simulated.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

# 8. Sanity checks against the real study --------------------------------

cat("\nSimulated DB_FULL:", nrow(DB_FULL), "samples,",
    length(unique(DB_FULL$site_acronyme)), "sites\n")
print(table(DB_FULL$MatrixType))
cat("\nWNV positives:", sum(DB_FULL$WNV_bin),
    "| USUV positives:", sum(DB_FULL$USUV_bin), "\n")
cat("\nWNV positivity by month:\n")
print(round(100 * tapply(DB_FULL$WNV_bin, DB_FULL$mois, mean), 1))
