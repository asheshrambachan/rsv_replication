library(dplyr)
library(ggplot2)

# Load custom theme/font
source("code/utils/fte_theme.R")

# Output directory
output_dir <- "figures"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Load and summarize publication data
pub_data <- read.csv("data/clean/publications/data.csv") %>%
  group_by(year) %>%
  summarize(n_pub = n()) 

# Create publication count plot
fig <- ggplot(pub_data, aes(x = year, y = n_pub)) +
  geom_line(linewidth = 0.65, color = palette$darkblue) +
  scale_x_continuous(
    limits = c(2015, 2024),
    breaks = seq(2015, 2024, 1)
  ) +
  scale_y_continuous(
    limits = c(0, 20),
    minor_breaks = seq(0, 20, 1)
  ) +
  labs(
    x = "Year", 
    y = "Number of publications"
  ) +
  theme_bw() + 
  theme(
    text = element_text(size=14, family=font, color="black"),
    legend.text = element_text(size=12, family=font, color="black"),
    axis.text = element_text(size=12, family=font, color="black"),
    axis.title = element_text(size=14, family=font, color="black"),
    panel.grid.major = element_line(linewidth=0.3),
    panel.grid.minor = element_line(linewidth=0.1),
    panel.grid.minor.x = element_blank(),
  )

# Display the plot
print(fig)

# Save the plot
ggsave(
  filename = file.path(output_dir, "publication_counts.jpeg"),
  plot = fig,
  width = 6,
  height = 3.5
)
