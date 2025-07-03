# Third: For this figure, could you output the version
# that does not normalize the X-axis (i.e., the x-axis just goes from [0,1])? 
# Mapping to the previous figures, do you know what version of the empirical 
# application this corresponds to (i.e., Random subset or buffer and holdout)?
  
rm(list = ls())
library(ggpubr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(binsreg)

font <- "Times"

y_cons_raw <- read.csv("data/clean/antipoverty/data_wo_features.csv") %>%
  filter(
    tot_p >= 100, # Identify and remove small shrid based on population size i.e filter out all shrids with pop size of 100 or lower
    !(is.na(y_05k) | is.na(y_10k) | is.na(y_cons)) # Remove rows with missing values in target variable
  ) %>% .$y_cons_raw

for (S in c("random", "buffer_holdout")){
  filename <- sprintf("data/clean/antipoverty/regressions_based_on_original/rsv_cons_%s.rds", S)
  rds <- readRDS(filename)
  Y <- rds$first_step$pred_test$Y
  H <- scale(rds$H)
  
  data <- data.frame(
    Y = Y,
    H = H,
    consumption = log(y_cons_raw)
  ) 
  
  binscatter <- binsreg(
    data = data, 
    x = Y, y = H, 
    randcut=1,
    polyreg=3,
    dots=T,
    # cb = T,
    # ci = T,
    # level=95,
    # vce = "HC1",
    # noplot = T
  )
  
  fig <- ggplot() + 
    geom_hline(yintercept = 0, linewidth=0.3, color="gray") +
    # geom_vline(xintercept = 0, linewidth=0.3, color="gray") +
    geom_point(data=data, aes(x=Y, y=H, color = "Raw data"), size=0.01, alpha=0.2) +
    # geom_abline(intercept = 0, linetype="dashed", color="red", linewidth=0.3) +
    geom_point(data=binscatter$data.plot$`Group Full Sample`$data.dots, aes(x=x, y=fit, color = "Binscatter"), size=0.8) +
    geom_line(data=binscatter$data.plot$`Group Full Sample`$data.poly, aes(x=x, y=fit, color = "Polynomial fit"), linewidth=0.5) +
    
    # geom_smooth(data=data, aes(x=Y, y=H, color = "Polynomial fit"), formula = y ~ poly(x, 3), se = FALSE) +
    labs(
      x = "Prediction of low consumption",
      y = "Optimal represention *H(R)*",
      colour = NULL
    ) +
    scale_color_manual(
      breaks = c("Raw data", "Binscatter", "Polynomial fit"),
      values = c("#8DB1CE", "#2D2F2E", "#2D2F2E")
    ) +
    scale_x_continuous(minor_breaks = seq(-5,5,0.2)) +
    scale_y_continuous(limits = c(-2, 4), minor_breaks = seq(-5,5,0.2)) +
    theme_bw() + 
    theme(
      legend.position = "none",
      panel.grid.major=element_line(linewidth=0.2),
      panel.grid.minor=element_line(linewidth=0.1),
      text = element_text(color="#2D2F2E", size=14, family=font),
      axis.title = element_markdown(color="#2D2F2E", size=14, family=font),
      axis.text = element_text(color="#2D2F2E", size=12, family=font)
    ) + 
    guides(color = guide_legend(override.aes = list(size=1)))
  
  
  output_path <- sprintf("output/figures/antipoverty_representation_binscatter/%s.jpeg", S)
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  ggsave(output_path, plot = fig, height = 4, width = 4.6)
  cat(sprintf("Saved figure to: %s\n", output_path))
  
}
