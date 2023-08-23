if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,viridis)

temp_DO_function1 <- function(x) {(14.7-(0.0017*0))*exp(-0.0225*x)} 
temp_DO_function2 <- function(x) {(14.7-(0.0017*500))*exp(-0.0225*x)}
temp_DO_function3 <- function(x) {(14.7-(0.0017*1000))*exp(-0.0225*x)}
temp_DO_function4 <- function(x) {(14.7-(0.0017*1500))*exp(-0.0225*x)}
temp_DO_function5 <- function(x) {(14.7-(0.0017*2000))*exp(-0.0225*x)}
temp_DO_function6 <- function(x) {(14.7-(0.0017*2500))*exp(-0.0225*x)}
temp_DO_function7 <- function(x) {(14.7-(0.0017*3000))*exp(-0.0225*x)}
temp_DO_function8 <- function(x) {(14.7-(0.0017*3500))*exp(-0.0225*x)}
temp_DO_function9 <- function(x) {(14.7-(0.0017*4000))*exp(-0.0225*x)}

function_output <- data.frame(x = 3:34,            # Create data for ggplot2
                       values = c(temp_DO_function1(3:34),
                                  temp_DO_function2(3:34),
                                  temp_DO_function3(3:34),
                                  temp_DO_function4(3:34),
                                  temp_DO_function5(3:34),
                                  temp_DO_function6(3:34),
                                  temp_DO_function7(3:34),
                                  temp_DO_function8(3:34),
                                  temp_DO_function9(3:34)),
                       elevation = rep(c(0,
                                         500,
                                         1000,
                                         1500,
                                         2000,
                                         2500,
                                         3000,
                                         3500,
                                         4000), each = 32)) %>%
  rename(`Elevation (m)` = elevation)



function_plot <- ggplot(function_output,
       aes(x, values, group = `Elevation (m)`, col = `Elevation (m)`)) +
  geom_line()+
  scale_color_viridis(discrete = F, option = "C")+
  theme_light()+
  labs(x = "Water Temperature (C)", y = "Dissolved Oxygen (mg/L)")

ggsave(function_plot, path = ".",
       filename = "./figures/temp_do_elevation_concept_plot.jpg",
       width = 8, height = 6, device='jpg', dpi=2000)


