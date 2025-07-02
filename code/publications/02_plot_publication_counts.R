library(dplyr)
library(ggplot2)

# Load custom theme/font
source("code/common/ggplot_theme.r")

# Load and summarize publication data
pub_data <- read.csv("data/clean/publication_data.csv") %>%
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
  linechart_theme
  
# Display the plot
print(fig)

# Save the plot
ggsave(
  filename = here("output/figures/publication_counts.jpeg"),
  plot = fig,
  width = 6,
  height = 3.5
)
