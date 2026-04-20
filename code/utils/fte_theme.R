require(RColorBrewer)
require(ggplot2)

font <- "Times"

palette <- list(
  darkblue   = "#43739D",
  blue       = "#8DB1CE",
  darkgreen  = "#618564",
  green      = "#B4C992",
  lightgreen = "#D4E1C4",
  gray       = "#EBEBEB",
  red        = "#ff6666",
  black      = "black"
)
fte_theme <- function() {

  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = "black" #pallette[6]
  color.axis.title = "black"
  color.title = palette[9]

  # Begin construction of chart
  theme_bw(base_size=12) +

    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    #theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +

    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_line(color=color.grid.major,size=.25)) +
    #theme(panel.grid.minor=element_blank()) +
    #theme(axis.ticks=element_blank()) +

    # Format the legend, but hide by default
    #theme(legend.position="none") +
    #theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=10,color=color.axis.title)) +
    theme(legend.title = element_text(size=10,color=color.axis.title)) +

    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=14, vjust=1.25, hjust = 0.5)) +
    theme(axis.text.x=element_text(size=10,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=10,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=10,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=10,color=color.axis.title, vjust=1.25)) +

    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

bias_coverage_theme <- function(guide, legend.position.inside = c(0.05, 0.1)) {
  list(
    scale_color_manual(name = NULL, breaks = guide$breaks, labels = guide$labels, values = guide$colors),
    scale_fill_manual( name = NULL, breaks = guide$breaks, labels = guide$labels, values = guide$colors),
    scale_shape_manual(name = NULL, breaks = guide$breaks, labels = guide$labels, values = guide$shapes),
    theme_bw(),
    theme(
      legend.position        = "inside",
      legend.justification   = c(0, 0),
      legend.position.inside = legend.position.inside,
      legend.box.background  = element_rect(fill = "white"),
      text                   = element_text(size = 14, family = font, color = "black"),
      legend.text            = element_text(size = 12, family = font, color = "black"),
      axis.text              = element_text(size = 12, family = font, color = "black"),
      axis.title             = element_text(size = 14, family = font, color = "black"),
      panel.grid.major       = element_line(linewidth = 0.3),
      panel.grid.minor       = element_line(linewidth = 0.1),
      panel.grid.minor.x     = element_blank()
    )
  )
}
