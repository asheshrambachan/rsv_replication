
library(ggtext)

font <- "Times" 

palette <- list(
  darkblue = "#43739D",
  blue = "#8DB1CE", 
  darkgreen = "#618564", 
  green = "#B4C992",
  gray = "#EBEBEB"
)

# Line chart theme
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

# Density plots theme
density_theme <- theme_bw() +
  theme(
    text = element_text(size=14, family=font),
    legend.text = element_markdown(size=14, family=font),
    axis.title = element_markdown(size=14, family=font),
    axis.text=element_markdown(size=12),
    legend.position = "inside",
    legend.justification = c(0, 1), 
    legend.position.inside = c(0.04, 0.95),
    legend.key.spacing.y = unit(0.2, "cm"),
    legend.key.size = unit(0.5, "cm"),
    legend.background = element_blank(),
    panel.grid.major.x=element_blank(), 
    panel.grid.minor.x=element_blank(), 
    panel.grid.major.y=element_line(linewidth=0.3),
    panel.grid.minor.y=element_line(linewidth=0.1),
    axis.ticks=element_line(linewidth=0.3),
  )

# Cropburn base map function
cropburn_base_map <- function(data, fill, guide, legend.position.inside = c(0.73,0.234)) {
  
  # Load shapefiles
  districts <- st_read("data/processed/cropburn/districts/districts.shp", quiet=T)
  villages <- st_read("data/processed/cropburn/villages/villages.shp", quiet=T) 
  data <- right_join(villages, data, by = "pc11_tv_id")
  
  list(
    geom_sf(data = districts, fill = palette$gray, color = NA),
    geom_sf(data = data, aes(fill = !!ensym(fill)), linewidth = 0.2, color = "white"),
    geom_sf(data = villages, fill = NA, color = "white", linewidth = 0.2),
    geom_sf(data = districts, fill = NA, color = "white", linewidth = 0.5),
    scale_fill_manual(name = NULL, breaks = guide$breaks, labels = guide$labels, values = guide$colors),
    theme_void(),
    theme(
      legend.text = element_markdown(size=12, family=font, color="black"),
      legend.direction = "vertical",
      legend.box.just = "top",
      legend.key.spacing.y = unit(0.2, "cm"),
      legend.key.size = unit(0.5, "cm"),
      legend.position = "inside",
      plot.margin = unit(c(-0.4,-1.75,-0.4,0.7), "cm"), # shift map to the right a bit for legend
      legend.position.inside = legend.position.inside
    ) 
  )
}

# Anti-poverty base map function
antipoverty_base_map <- function(data, fill, guide, legend.position.inside = c(0.73,0.234)) {
  
  # Load shapefiles
  state <- st_read("data/processed/poverty/state/state.shp", quiet=T)
  districts <- st_read("data/processed/poverty/districts/districts.shp", quiet=T)
  shrids <- st_read("data/processed/poverty/shrids/shrids.shp", quiet=T)
  data <- right_join(shrids, data, by="shrid2")
  
  list(
    geom_sf(data = state, fill=palette$gray, color=NA),
    geom_sf(data = data, aes(fill=!!ensym(fill)), color="white", linewidth=0.04),
    geom_sf(data = districts, fill=NA, color="white", linewidth=0.3),
    scale_fill_manual(name=NULL, breaks = guide$breaks, labels = guide$labels, values = guide$colors),
    theme_void(),
    theme(
      legend.text = element_markdown(size=14, family=font, color="black"),
      legend.direction = "vertical",
      legend.box.just = "top",
      legend.key.spacing.y = unit(0.2, "cm"),
      legend.key.size = unit(0.5, "cm"),
      legend.position = "inside",
      legend.position.inside = legend.position.inside
    ) 
  )
}

