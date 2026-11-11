# Run the full MISArbO analysis pipeline, in order.
# Requires the data folder (see README); with simulated data, run 00_simulate_data.R first.
scripts <- c(
  "00_setup.R",
  "01_import_and_prepare_data.R",
  "02_build_analytic_dataset.R",
  "03_descriptive_analyses.R",
  "04_models_detection.R",
  "05_final_figures.R",
  "06_exports.R",
  "07_viral_load_models.R"
)
for (s in scripts) {
  message("Running ", s)
  source(file.path("R", s), encoding = "UTF-8")
}
