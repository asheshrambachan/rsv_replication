# Paired cross-fitted RSV stages on exactly the same draws/predictions.
for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)
source("code/smartcards/03_sims_noexpoutcomes_secondary/settings.R")
source("code/smartcards/03_sims_noexpoutcomes_secondary/simulation_utils.R")
cfg <- secondary_sim_settings()
params <- readRDS(file.path(cfg$interim_dir, "params.Rds"))
secondary_sim_run(cfg, params, "rsv")
