# create_NPP_plots_Fig1.r

# Load libraries
library(ggpubr) # Add correlation and regression
library(plotbiomes) # Whittaker biome plot

# Figure 1a is adapted from Hanbury-Brown et al., (2022)
# Load data from Supplementary information 
RNPP_sites <- read.csv(paste0(path_to_HB2022_data)) %>% 
  mutate(RRL = R/(R+L)) %>% 
  # Retain sites with NPP estimates
  filter(!is.na(NPP))

# Set text sizes
title_size = 14
axis_size = 10


get_RNPP_corrplot <- function(RNPP_sites, title_size = 14, axis_size = 10){
  fig <- RNPP_sites %>% 
  ggplot(aes(x = R.NPP, 
             y = RRL)) +
  geom_point(aes(x = R.NPP, 
                 y = RRL, 
                 color = Whittaker_biome)) +
  scale_color_manual(name = "Whittaker biomes",
                     breaks = names(Ricklefs_colors),
                     labels = names(Ricklefs_colors),
                     values = Ricklefs_colors) +
  geom_smooth(method = "lm",
              color = "grey32") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.y = .58, size = 5) +
  stat_regline_equation(label.x = .003, label.y = .53, size = 5) +
  ylab("R/(R+L)") +
  xlab("R/NPP") +
  theme_classic() +
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5, size = title_size),
        strip.text.x = element_text(size = axis_size),
        axis.title.x = element_text (size = axis_size), 
        axis.title.y = element_text (size = axis_size),
        axis.title.y.right = element_text (size = axis_size, color = "black"),
        axis.text.x = element_text (size = axis_size, colour = "black"),
        axis.text.y = element_text (size = axis_size, colour = "black") )

return(fig)
}



get_logRNPP_plot <- function(RNPP_sites, title_size = 14, axis_size = 10) {

  fig <- RNPP_sites %>% 
  ggplot(aes(x = log(R.NPP), 
             y = log(RRL))) +
  geom_point(aes(x = log(R.NPP), 
                 y = log(RRL), 
                 color = Whittaker_biome)) +
  scale_color_manual(name = "Whittaker biomes",
                     breaks = names(Ricklefs_colors),
                     labels = names(Ricklefs_colors),
                     values = Ricklefs_colors) +
  geom_smooth(method = "lm",
              color = "grey32") + 
  stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
           label.y = -0.5, size = 5) +
  stat_regline_equation(label.y = -1, size = 5) + # label.x = .003, 
  ylab("log(R/(R+L))") +
  xlab("log(R/NPP)") +
  theme_bw() +
  theme(aspect.ratio = 1,
        plot.title = element_text(hjust = 0.5, size = title_size),
        strip.text.x = element_text(size = axis_size),
        axis.title.x = element_text (size = axis_size), 
        axis.title.y = element_text (size = axis_size),
        axis.title.y.right = element_text (size = axis_size, color = "black"),
        axis.text.x = element_text (size = axis_size, colour = "black"),
        axis.text.y = element_text (size = axis_size, colour = "black") )

  return(fig)

}


get_denom_corr_plot <- function(RNPP_sites, title_size = 14, axis_size = 10){

  fig <- RNPP_sites %>% 
      ggplot(aes(x = log(NPP), 
                y = log(R + L ))) +
      geom_point(aes(x = log(NPP), 
                    y = log(R+L), 
                    color = Whittaker_biome)) +
      scale_color_manual(name = "Whittaker biomes",
                        breaks = names(Ricklefs_colors),
                        labels = names(Ricklefs_colors),
                        values = Ricklefs_colors) +
      geom_smooth(method = "lm",
                  color = "grey32") + 
      stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.y=6.5,size = 5) + 
      stat_regline_equation(label.y = 6.35, size = 5) +
      ylab("log(R+L)") +
      xlab("log(NPP)") +
      theme_bw() +
      theme(aspect.ratio = 1,
            plot.title = element_text(hjust = 0.5, size = title_size),
            strip.text.x = element_text(size = axis_size),
            axis.title.x = element_text (size = axis_size), 
            axis.title.y = element_text (size = axis_size),
            axis.title.y.right = element_text (size = axis_size, color = "black"),
            axis.text.x = element_text (size = axis_size, colour = "black"),
            axis.text.y = element_text (size = axis_size, colour = "black") )

  return(fig)

}
