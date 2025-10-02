source("code/poverty/01_data_prep/clean_base_data.R")
source("code/poverty/01_data_prep/clean_shapefiles.R")
source("code/poverty/01_data_prep/get_features.R")
source("code/poverty/01_data_prep/merge_features.R")

source("code/poverty/02_analysis/01_compute_pca_features.R")
source("code/poverty/02_analysis/02_rsv_synthD_synthS.R")
source("code/poverty/02_analysis/03_surrogate_synthD_synthS.R")
source("code/poverty/02_analysis/04_rsv_realD_synthS.R")
source("code/poverty/02_analysis/05_rsv_realD_realS.R")
source("code/poverty/02_analysis/06_benchmark_realD.R")
source("code/poverty/02_analysis/07_rsv_realD_synthS_wo_spillover.R")
source("code/poverty/02_analysis/08_rsv_realD_realS_wo_spillover.R")
source("code/poverty/02_analysis/09_benchmark_realD_wo_spillover.R")

source("code/poverty/02_analysis/10_summarize.R")

source("code/poverty/03_outputs/generate_summary_stats.R")
source("code/poverty/03_outputs/plot_maps.R")
source("code/poverty/03_outputs/plot_rsv_distribution.R")
source("code/poverty/03_outputs/plot_synthD_synthS_bias_rmse.R")

source("code/poverty/03_outputs/plot_realD_denominator.R")
source("code/poverty/03_outputs/plot_realD_te.R")
source("code/poverty/03_outputs/plot_realD_representation.R")

source("code/poverty/03_outputs/plot_realD_denominator_wo_spillover.R")
source("code/poverty/03_outputs/plot_realD_te_wo_spillover.R")

source("code/poverty/03_outputs/plot_realD_denominator_by_spillover.R")
source("code/poverty/03_outputs/plot_realD_te_by_spillover.R")
