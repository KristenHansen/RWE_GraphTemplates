#Templating R Source file
#Guide is a RMD file

plot_theme <- function(background = "black",legend_location = "bottom", gridlines = FALSE, grid_line_pattern = NULL, font = "Times", fontsize_title = 15,fontsize_axes_title = 8, fontsize_axes = 6, title_centered = TRUE, map_plot = FALSE){
  library(ggplot2)
  library(dplyr)
  #line types solid, dashed, dotted
  if(background == "black"){
    saved_theme <- theme_bw() + 
      theme(
        # Text
        text = element_text(family = font),
        # Axes
        
        axis.title = element_text(face = "bold", size = fontsize_axes_title),
        axis.line = element_line(color="black", linewidth = .25),
        axis.text.y = element_text(color="black", size = fontsize_axes),
        axis.text.x = element_text(color="black", size = fontsize_axes),
        axis.ticks = element_line(color="black", linewidth = .25),
      
        # Title
        plot.title = element_text(face = "bold", size = fontsize_title, hjust = ifelse(title_centered, 0.5, 0)),
        # Legends
        legend.title =  element_text(color = "white", size = fontsize_axes),
        legend.position = legend_location,
        legend.text = element_text(color = "white", size = fontsize_axes - 1),
        legend.key.size = unit(0.4, "lines"),
        legend.box.spacing = unit(0, "cm"),
        legend.background = element_rect(fill = "gray15"),
        # Panel
        panel.border = element_blank(),
        panel.background = element_rect(fill=background),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  } else {
    saved_theme <- theme_grey() + 
      theme(
        # Text
        text = element_text(family = font),
        # Axes
        axis.title = element_text(face = "bold", size = fontsize_axes_title),
        axis.line = element_line(color="black", linewidth = .25),
        axis.text.y = element_text(color="black", size = fontsize_axes),
        axis.text.x = element_text(color="black", size = fontsize_axes),
        axis.ticks = element_line(color="black", linewidth = .25),
        # Title
        plot.title = element_text(face = "bold", size = fontsize_title, hjust = ifelse(title_centered, 0.5, 0)),
        # Legends
        legend.title =  element_text(color = "black", size = fontsize_axes),
        legend.position = "bottom",
        legend.text = element_text(size = fontsize_axes - 1),
        legend.key.size = unit(0.4, "lines"),
        legend.box.spacing = unit(0, "cm"),
        legend.background = element_rect(fill = "grey95"),
        # Panel
        panel.border = element_blank(),
        panel.background = element_rect(fill=background),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
    
  }
  if(map_plot){
    saved_theme <- saved_theme + theme(
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks = element_blank()
    )
  }
  if(gridlines){
    saved_theme <- saved_theme + theme(
      panel.grid.major = element_line(color="grey", linewidth = .10, linetype = grid_line_pattern),
      panel.grid.minor = element_line(color="grey", linewidth = .10, linetype = grid_line_pattern),
      axis.text.y = element_text(color="black", size = fontsize_axes),
      axis.text.x = element_text(color="black", size = fontsize_axes),
      axis.ticks = element_line(color="black", linewidth = .25)
    )
  }
  saved_theme %>% saveRDS('saved_theme.rds')
  
  print("Theme Saved in Working Directory")
  return(NA)
}

## save it as RDS
