font <- "Times" 

palette <- list(
  darkblue = "#43739D",
  blue = "#8DB1CE", 
  darkgreen = "#618564", 
  green = "#B4C992",
  gray = "#EBEBEB"
)

linechart_theme <- theme_bw() + 
  theme(
    text = element_text(size=14, family=font, color="black"),
    legend.text = element_text(size=12, family=font, color="black"),
    axis.text = element_text(size=12, family=font, color="black"),
    axis.title = element_text(size=14, family=font, color="black"),
    panel.grid.major = element_line(linewidth=0.3),
    panel.grid.minor = element_line(linewidth=0.1),
    panel.grid.minor.x = element_blank(),
  )  

cropburn_map_theme <- theme_void() +
  theme(
    plot.margin = unit(c(-0.4,-1.75,-0.4,0.7), "cm"),
    legend.text = element_markdown(size=12, family=font, color="black"),
    legend.position = "inside",
    legend.direction = "vertical",
    legend.key.spacing.y = unit(0.2, "cm"),
    legend.key.size = unit(0.5, "cm"),
  ) 
