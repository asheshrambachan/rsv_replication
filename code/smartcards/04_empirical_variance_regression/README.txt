SMARTCARDS: VARIANCE REGRESSION WITHOUT SECONDARY CALIBRATION

From the project root:
  source("code/smartcards/04_empirical_variance_regression/00_run_all.R")

Edit settings.R for outcomes, sample restrictions, RF parameters, bootstrap
draws, confidence level and output paths. Defaults match 04_empirical_secondary:
  Ycons, Ylowinc, Ymidinc; full and no-spillover samples; Stilde roles;
  all luminosity/satellite predictors; full-sample training and estimation;
  RF with 1,000 trees, min.node.size=20, max.depth=10, seed=42;
  variance-floor quantile=0.01; 1,000 mandal draws; 90% normal CIs;
  whole mandals resampled without wave stratification (strata=NULL).

No secondary measurement is required. Y is masked outside the observational
role; D is masked outside the experimental role. Overlapping records contribute
to both roles without duplication. No benchmark is formed from masked outcomes.

Stages
  uncalibrated_unit: H0 constructed with sigma2=1, with a preliminary effect
    estimated from the observed estimating moments. The same initial forests
    are reused; no secondary calibration is performed.
  uncalibrated_updated: regress (delta_e - tau_preliminary * delta_o)^2 on R,
    including the overlap cross term, floor predictions at quantile 0.01, and
    use H0/sigma2 to estimate the effect. Calls rsv(sigma2.method="regression").

The preliminary effect differs from the secondary workflow's calibrated unit
effect, because this workflow removes calibration at both stages. It also
differs from the original plugin initialization based on predicted contrasts.

Inference
  add_se(method="score_bootstrap") keeps all prediction functions, including
  the variance forest, fixed. Each draw rebuilds normalization frequencies,
  H and the estimating moments. The full-sample quantile floor is recomputed
  from the resampled raw variance predictions. Both stages use the same seeds
  and mandal samples. No forests are fitted within bootstrap draws.
  Reports withhold a stage's SE/CI if any effect draw failed or was nonfinite.
  Fitted-prediction learning uncertainty is excluded; full-sample RF validity
  and finite-sample coverage are not established by these calculations.

Outputs (separate from the original and secondary empirical workflows)
  data/interim/smartcards/empirical_variance_regression/{full,nospillover}/{Y_var}.Rds
    A bundle with the final fit, both stage fits, masked observations/IDs,
    settings and bootstrap draws. Point estimates are checkpointed before SEs;
    each completed stage is saved before moving to the next bootstrap.
  data/clean/smartcards/empirical_variance_regression/
    results.csv: effect estimates, SEs, CIs, separation, sample sizes and status.
    confidence_intervals.csv: normal effect CIs for both stages.
    variance_diagnostics.csv: preliminary effect, floor, floored share, H range.
    representations.csv: both H scores, IDs, masks and variance predictions.
    relevance.csv: denominators, separation, SEs and normal denominator CIs.

01_empirical.R fits and bootstraps. 02-04 regenerate reports from saved fits,
without refitting or resampling. A confidence-level change only needs 02-04.
Nothing is added to the parent Smartcards pipeline automatically.
