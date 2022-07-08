# Base ggplot2 theme ------------------------------------------------------
library(ggpubr)
library(ggtext)
plot_theme <- function(...) {
  theme_pubr() +
    cowplot::theme_half_open(12) +
    cowplot::background_grid()+
    theme(
      plot.margin = margin(0.2, 0.2, 0.2, 0.2, "in"),
      plot.title = element_text(size = 16, face = "bold"),
      plot.caption = element_text(size = 16),
      plot.subtitle = element_text(size = 16),
      plot.background = element_rect(fill="white"),
      axis.title = element_text(size = 16, face = "bold"),
      axis.title.y = element_text(margin = margin(r = 10, l = -10)),
      axis.title.x = element_text(margin = margin(t = 10, b = -10)),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1,
        size = 16
      ),
      axis.text.y = element_text(size = 16),
      # strip.text = element_text(size=8),
      legend.title = element_text(size = 16, face = "bold"),
      legend.text = element_text(size = 16),
      strip.background = element_blank(),
      strip.text = element_textbox(
        size = 16,
        face = "bold",
        color = "#000000",
        fill = "#FFFFFF",
        box.color = "#000000",
        halign = 0.5,
        linetype = 1,
        r = unit(2, "pt"),
        width = unit(1, "npc"),
        padding = margin(2, 0, 1, 0),
        margin = margin(3, 3, 3, 3)
      ),
      
      #additional settings passed to theme()
      ...
    )
  
}

#Make default theme for all .R and .Rmd files
theme_set(plot_theme())
