# Shared LaTeX tables for stability conditional on a binary secondary outcome.
# Reads saved clustered-test results only; does not rerun tests or rebuild data.
source("code/utils/stability_binary_secondary.R")

tab_stability_binary_secondary <- function(
    secondary_var, sample_definition, threshold = NULL, percentile = NULL,
    direction = "above", results_csv = NULL, output_dir = "tables/smartcards") {
  if (missing(secondary_var) || !is.character(secondary_var) ||
      length(secondary_var) != 1L || is.na(secondary_var) || !nzchar(secondary_var)) {
    stop("Supply the name of the secondary-outcome column.")
  }
  sample_definition <- match.arg(sample_definition, c("S", "Stilde"))
  direction <- match.arg(direction, c("above", "at-or-above", "below", "at-or-below"))
  if (!is.null(threshold) && !is.null(percentile)) {
    stop("Specify either threshold or percentile, not both.")
  }
  if (!is.null(threshold) &&
      (!is.numeric(threshold) || length(threshold) != 1L || !is.finite(threshold))) {
    stop("threshold must be a single finite number.")
  }
  if (!is.null(percentile) &&
      (!is.numeric(percentile) || length(percentile) != 1L ||
       !is.finite(percentile) || percentile < 0 || percentile > 100)) {
    stop("percentile must be a number between 0 and 100.")
  }
  if (is.null(threshold) && is.null(percentile) && direction != "above") {
    stop("direction requires a threshold or percentile.")
  }

  tag <- if (!is.null(percentile)) {
    paste("percentile", percentile, direction, sep = "_")
  } else if (!is.null(threshold)) {
    paste("threshold", threshold, direction, sep = "_")
  } else ""
  spec_name <- gsub("[^[:alnum:]_-]", "_", secondary_var)
  if (nzchar(tag)) spec_name <- paste0(spec_name, "_", gsub("[^[:alnum:]_-]", "_", tag))
  if (is.null(results_csv)) {
    results_csv <- paste0("data/clean/smartcards/assumption_stability_",
                          sample_definition, "_secondary_", spec_name, ".csv")
    if (!file.exists(results_csv)) {
      legacy_csv <- paste0(
        "data/clean/smartcards/assumption_stability_binary_secondary_", spec_name, ".csv"
      )
      if (file.exists(legacy_csv)) results_csv <- legacy_csv
    }
  }
  if (!file.exists(results_csv)) stop("Run the matching stability test first: ", results_csv)
  ks <- utils::read.csv(results_csv, stringsAsFactors = FALSE)
  required <- c(
    "sample_definition", "outcome", "Y", "secondary", "secondary_value",
    "secondary_method", "secondary_cutoff", "secondary_percentile",
    "secondary_direction", "n_exp", "n_obs", "ks_stat", "cluster_pval", "status", "B"
  )
  if (!all(required %in% names(ks))) stop("Test results lack required columns.")
  ks <- ks[which(ks$sample_definition == sample_definition), ]
  method <- if (!is.null(percentile)) "percentile" else if (!is.null(threshold)) {
    "fixed_threshold"
  } else "binary_input"
  equal_number <- function(x, value) {
    !anyNA(x) && all(is.finite(x)) &&
      all(abs(x - value) <= 1e-12 * max(1, abs(value)))
  }
  if (!nrow(ks) || anyNA(ks$secondary) || any(ks$secondary != secondary_var) ||
      anyNA(ks$secondary_method) || any(ks$secondary_method != method)) {
    stop("Test results do not match the selected secondary measurement or conversion.")
  }
  if (method != "binary_input") {
    cutoff <- unique(ks$secondary_cutoff)
    if (length(cutoff) != 1L || !is.finite(cutoff) ||
        anyNA(ks$secondary_direction) || any(ks$secondary_direction != direction) ||
        (!is.null(threshold) && !equal_number(ks$secondary_cutoff, threshold)) ||
        (!is.null(percentile) && !equal_number(ks$secondary_percentile, percentile))) {
      stop("Test results do not match the requested cutoff or direction.")
    }
  }
  Y_vars <- c("Ycons", "Ylowinc", "Ymidinc")
  labels <- c(Ycons = "Consumption", Ylowinc = "Low income", Ymidinc = "Middle income")
  if (anyNA(ks$outcome) || any(!ks$outcome %in% Y_vars) ||
      anyNA(ks$Y) || any(!ks$Y %in% c(0, 1)) ||
      anyNA(ks$secondary_value) || any(!ks$secondary_value %in% c(0, 1)) ||
      anyDuplicated(ks[c("outcome", "Y", "secondary_value")]) || nrow(ks) != 12L) {
    stop("Expected exactly one row for each outcome, Y, and secondary-outcome cell.")
  }
  if (length(unique(ks$B)) != 1L || anyNA(ks$B) || any(ks$B < 1)) {
    stop("Test results must use one positive bootstrap draw count.")
  }
  if (anyNA(ks$status)) stop("Test results lack cell statuses.")

  latex_escape <- function(x) {
    chars <- strsplit(x, "", fixed = TRUE)[[1]]
    replacements <- c(
      "\\" = "\\textbackslash{}", "{" = "\\{", "}" = "\\}",
      "_" = "\\_", "%" = "\\%", "&" = "\\&", "#" = "\\#",
      "$" = "\\$", "^" = "\\textasciicircum{}", "~" = "\\textasciitilde{}"
    )
    hit <- match(chars, names(replacements))
    chars[!is.na(hit)] <- replacements[hit[!is.na(hit)]]
    paste0(chars, collapse = "")
  }
  fmt_stat <- function(x) if (is.finite(x)) sprintf("$%.3f$", x) else "--"
  fmt_p <- function(x) {
    if (!is.finite(x)) return("--")
    if (x < 0.001) return("$<0.001$")
    sprintf("$%.3f$", x)
  }
  role <- if (sample_definition == "S") "S" else "\\widetilde{S}"
  conversion <- if (method == "binary_input") {
    sprintf("$\\widetilde{Y}$ is the supplied binary column \\texttt{%s}.",
            latex_escape(secondary_var))
  } else {
    op <- c(above = ">", "at-or-above" = "\\geq", below = "<", "at-or-below" = "\\leq")
    paste0(
      "$\\widetilde{Y}=1$ when \\texttt{", latex_escape(secondary_var), "} $",
      op[[direction]], "$ ", format(cutoff, digits = 10, trim = TRUE),
      ", and zero otherwise.",
      if (method == "percentile") paste0(
        " The cutoff is the ", percentile,
        "th percentile of all nonmissing input-column values (quantile type 7)."
      ) else " The cutoff is fixed in the original units."
    )
  }

  lines <- c(
    "\\begin{threeparttable}", "\\begin{tabular}{lrrrr}", "\\toprule",
    "Outcome & $n_1$ & $n_2$ & KS & $p$-value \\\\", "\\midrule"
  )
  for (y in c(0, 1)) {
    for (t in c(0, 1)) {
      if (y != 0 || t != 0) lines <- c(lines, "\\addlinespace[0.6em]")
      lines <- c(lines, sprintf(
        "\\multicolumn{5}{l}{$D=0,\\,Y=%d,\\,\\widetilde{Y}=%d$}\\\\", y, t
      ))
      for (v in Y_vars) {
        r <- ks[ks$outcome == v & ks$Y == y & ks$secondary_value == t, ]
        lines <- c(lines, sprintf(
          "\\quad %s & $%d$ & $%d$ & %s & %s \\\\",
          labels[[v]], r$n_exp, r$n_obs, fmt_stat(r$ks_stat), fmt_p(r$cluster_pval)
        ))
      }
    }
  }
  notes <- paste0(
    "\\item Notes: Each row compares the distribution of $R_{\\mathrm{PC1}}$ ",
    "between experimental ($", role, "=e$) and observational ($", role,
    "=o$) units, conditional on $D=0$, $Y$, and $\\widetilde{Y}$. ",
    "$n_1$ and $n_2$ are the corresponding sample sizes. ",
    "The $p$-values use a centered mandal-level multiplier bootstrap with $B=",
    unique(ks$B), "$; the secondary-outcome cutoff is held fixed. ",
    if (sample_definition == "Stilde") {
      "Buffer villages contribute to both roles and share mandal multipliers. "
    } else "Buffer villages contribute only to the experimental role. ",
    conversion,
    " Unavailable statistics or $p$-values are shown as --. ",
    "Failure to reject does not establish stability."
  )
  if (any(ks$status != "ok")) {
    flagged <- ks[ks$status != "ok", ]
    descriptions <- paste0(
      flagged$outcome, " (Y=", flagged$Y, ", secondary=", flagged$secondary_value,
      "): ", flagged$status
    )
    notes <- paste0(notes, " Flagged cells: ",
                    paste(vapply(descriptions, latex_escape, character(1)), collapse = "; "), ".")
    warning("Flagged cells in table: ", paste(descriptions, collapse = "; "), call. = FALSE)
  }
  lines <- c(lines, "\\bottomrule", "\\end{tabular}",
             "\\begin{tablenotes}[flushleft]", "\\small", notes,
             "\\end{tablenotes}", "\\end{threeparttable}")
  out_path <- file.path(output_dir, paste0(
    "tab_stability_", sample_definition, "_secondary_", spec_name, ".tex"
  ))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, out_path)
  message("Saved: ", out_path)
  invisible(out_path)
}

run_stability_secondary_tables_cli <- function(
    sample_definition, args = commandArgs(trailingOnly = TRUE)) {
  sample_definition <- match.arg(sample_definition, c("S", "Stilde"))
  script <- if (sample_definition == "S") "08_tab_stability_S_secondary.R" else {
    "09_tab_stability_Stilde_secondary.R"
  }
  options <- parse_binary_stability_args(args, script)
  if (!is.null(options$help)) {
    cat("Usage: Rscript", script,
        "COLUMN [--percentile P | --threshold VALUE] [--direction RULE]\n")
    return(invisible(NULL))
  }
  do.call(tab_stability_binary_secondary, c(
    options[c("secondary_var", "threshold", "percentile", "direction")],
    list(sample_definition = sample_definition)
  ))
}
