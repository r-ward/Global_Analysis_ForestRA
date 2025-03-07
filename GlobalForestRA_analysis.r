#  GlobalForestRA_analysis.r

# Clear the workspace
rm(list = ls())

# Set working directory 
setwd("/Users/rachelward/Desktop/Projects/VS_workspace/GlobalForestRA_final")

# Load libraries
library(tidyverse) # data wrangling, plotting
library(datawizard) # data standardization
library(MASS) # boxcox transformation
library(nlme) # model fitting
library(sjPlot) # model summary tables

############################## Load data ###############################

df <- read.csv("Data/GlobalForestRA_data.csv")

# Add geo_site, unique latitude and longitude
df$geo_site <- paste0(df$Latitude, "_", df$Longitude)

# For ease, rename col R/(R+L)
df$RRL <- df$'R..R.L.' # this is the R/(R+L) variable

# Make biome and age group a factor
df$Biome <- factor(df$Biome, levels = c("Boreal", "Temperate", "Tropical")) 
df$Forest_age_group <- factor(df$Forest_age_group, levels = c("young", "mid", "old"))


############################### Summary stats ###############################
# Number of observations in the dataset
nrow(df) # 824

# Number of unique geo_site values
length(unique(df$geo_site)) # 393

# Number of unique sites 
length(unique(df$Site)) # 239

# Number of unique subsites
length(unique(paste0(df$Site, df$Subsite))) # 469

# Range of duration
range(df$Sampling_duration) # 1-24
sum(df$Sampling_duration) # 1583

# Range of response variable 
range(df$RRL) # 0.00140647-0.54561102

# N sites where lat lon is adjusted
df %>% 
    filter(LatLonAdjusted == 1) %>% 
    nrow(.) # 9

# N sites where Forest age is reported
df %>% 
  filter(!is.na(Forest_age)) %>% 
  nrow(.)

# N sites where Successional stage is reported
df %>% 
  filter(!is.na(Successional_stage)) %>% 
  nrow(.)

# N sites where both are reported
df %>% 
  filter(!is.na(Forest_age) & !is.na(Successional_stage)) %>% 
  nrow(.)

# Mean and sd R/(R+L) by biome  
df %>% 
  group_by(Biome) %>% 
  summarise(mean_RRL = mean(RRL, na.rm = TRUE),
            sd_RRL = sd(RRL, na.rm = T)) 

######################### Box-Cox transformation ########################

# The response variable is not log-normal, so we use a Box-Cox transformation
# which is performed for the response variable R/(R+L) as well as R and L, separately
# note: boxcox() will return a `NOT RUN (closure error)` if the dataframe is called "df"
# any other name seems to be fine
df1 <- df

# R/(R+L)
b <- boxcox(lm(df1$RRL ~ 1)) # box cox transformation 
lambda <- b$x[which.max(b$y)] # 0.3838384
# R (reproductive flux) 
br <- boxcox(lm(df1$Repro_flux_Mghayr ~ 1)) # box cox lambda for R
lambda_r <- br$x[which.max(br$y)] # 0.2626263
# L (leaf flux)
bl <- boxcox(lm(df1$Leaf_flux_Mghayr ~ 1)) # box cox lambda for L
lambda_l <- bl$x[which.max(bl$y)] # 0.4646465

# Add lambda values to the dataframe
df$lambda <- lambda
df$lambda_r <- lambda_r
df$lambda_l <- lambda_l

# Calculate the box-cox transformed values and save in dataframe
df$bc_RRL <- ((df$RRL ^ lambda) - 1) / lambda
df$bc_R <- ((df$Repro_flux_Mghayr ^ lambda_r) - 1) / lambda_r
df$bc_L <- ((df$Leaf_flux_Mghayr ^ lambda_l) - 1) / lambda_l


##################### Fig1 - Distribution of sites in Whittaker biome and geographic space #####################

#Fig 1a is adapted from Hanbury-Brown et al., (2022) 
source(Functions/create_NPP_plot_Fig1.r)
Fig1a <- get_RNPP_corrplot(RNPP_sites)

# Fig 1b 
source(Functions/create_Whittaker_biome_plot.r)
Fig1b <- get_whittakerbiome_plot(Whittaker_df)

# Fig 1c
source(Functions/create_forest_extent_map.r)
Fig1c <- forest_sitemap

# Arrange plots
#  - remove legend from Fig1a but keep Fig1b with its legend
Fig1a <- Fig1a + theme(legend.position = "none")

# Adjust the legend size in Fig1b
Fig1b <- Fig1b +
  theme(
    legend.key.size = unit(0.9, "lines"),  # Make legend keys smaller
    legend.text = element_text(size = 9),  # Make legend text smaller
    legend.title = element_text(size = 10)  # Make legend title smaller
  )

# Align the left edges
plot_layout <- Fig1a + Fig1b + Fig1c +
  plot_layout(
    design = "
    AB
    CC
    ",
    widths = c(1, 1.1),    # Adjust width ratio of columns
    heights = c(0.7, 1)    # Top row is 70% height of bottom row
  )

# Add labels to the plots
final_plot <- plot_layout + 
  plot_annotation(tag_levels = 'a', tag_suffix = ')') &
  theme(plot.tag.position = c(0, 1),      # Position tags at top-left
        plot.tag = element_text(hjust = 0, vjust = 1))  # Align tags

# Save the plot
ggsave(filename = "Output/Figure1_NPPcorr_and_maps.jpeg", plot = final_plot, 
       width = 12, height = 9, units = "in", dpi = 300)



############################### Fig2 - biome differences ###############################
# How does R/(R+L) differ by biome, biome x leaf morphology, biome x age group?

source("Functions/create_boxplots_Fig2.r")

# Fig 2a) How does R/(R+L) vary across biome? 
Fig2a_biomefig <- create_biome_plot_letters(df) +
  labs(title = "a)")

# Fig 2b
# Set up biomes + leaf morphology groups to compare
biome_pfts <- c("Tropical_broadleaf", "Temperate_broadleaf", "Temperate_needleleaf", "Boreal_needleleaf")

# df_biome will contain only the biome_pft groups we want to compare in the figure
df_biome <- df %>%
  mutate(biome_leaftype = paste(Biome, Leaf_type, sep = "_")) %>%
  filter(biome_leaftype %in% biome_pfts) %>%
  mutate(biome_leaftype = gsub("_", " ", biome_leaftype)) %>%
  mutate(biome_leaftype = factor(biome_leaftype, 
                                levels = gsub("_", " ", rev(biome_pfts)))) %>%
  # Add this to check if we have empty factor levels
  droplevels()

nrow(df_biome) # 716 sites

# Create figure
Fig2b_pftfig <- create_pft_plot_letters(df_biome) +
  labs(title = "b)") 

# Histogram of biome 
Fig2c_biome_age_plot <- create_biome_age_plot(df, col_biome = "Biome", col_age = "Forest_age_group", colors_biome = colors_biome) +
  labs(title = "c)")

# Combine the plots
Fig2_combined <- (Fig2a_biomefig + Fig2b_pftfig + plot_layout(ncol = 2, widths = c(1, 1.2))) /
  Fig2c_biome_age_plot +
  plot_layout(nrow = 2, heights = c(1, 1))  & 
theme(plot.margin = margin(t = 0, r = 5, b = 0, l = 5, unit = "pt"))

# Save Figure 2
# ggsave("Output/Figure2_boxplots.jpeg", Fig2_combined, width = 8, height = 8.5)  # Adjust dimensions as needed


############################### Model fitting ###############################
d_mod <- df %>%
  # Select variables
  dplyr::select(ID, Site, Subsite, 
                bc_RRL, bc_R, bc_L,
                MAT_C, MAP_mm,
                Soil_pH, Soil_N, Soil_CEC, Soil_texture_index,
                Forest_age_group,
                geo_site,
                Sampling_duration) %>%
  # Rename variables
  rename(RRL = bc_RRL, R = bc_R, L = bc_L,
         MAT = MAT_C, MAP = MAP_mm,
         Forest_age = Forest_age_group,
         site = geo_site) %>%
  # Create squared terms
  mutate(MAT2 = MAT^2,
         MAP2 = MAP^2) %>%
  # Standardize all numeric variables at once
  datawizard::standardize(
    select = c("MAT", "MAP", "MAT2", "MAP2", 
               "Soil_pH", "Soil_N", "Soil_CEC", "Soil_texture_index")
  ) %>%
  # Factor conversion for Forest_age and square duration 
  mutate(Forest_age = factor(Forest_age, levels = c("young", "mid", "old")),
         sqrt_Duration = sqrt(Sampling_duration))


# Set reference level for Forest_age
d_mod$Forest_age <- relevel(d_mod$Forest_age, ref = "young")  #  relevel

# Fit models 
# RRL
mod_full_RRL <- lme(RRL ~ MAT + MAT2 + MAP + MAP2 + MAP*MAT + Soil_pH + Soil_N + Soil_texture_index + Soil_CEC + Soil_texture_index*MAP + Forest_age, random = ~1|site, weights = varFixed(~sqrt_Duration), data = d_mod)
mod_RRL <- lme(RRL ~ MAT + MAT2 + MAP + MAP2 + MAT:MAP + Soil_pH + Soil_N  + Soil_texture_index + Soil_texture_index*MAP + Forest_age, random = ~1|site, weights = varFixed(~sqrt_Duration), data = d_mod)
tab_model(mod_full_RRL, mod_RRL)

# R
mod_full_R <- lme(R ~ MAT + MAT2 + MAP + MAP2 + MAP*MAT + Soil_pH + Soil_N + Soil_texture_index + Soil_CEC + Soil_texture_index*MAP + Forest_age, random = ~1|site, weights = varFixed(~sqrt_Duration), data = d_mod)
mod_R <- lme(R ~ MAT + MAT2 + MAP + MAP2 + MAT:MAP + Soil_texture_index  + Forest_age, random = ~1|site, weights = varFixed(~sqrt_Duration), data = d_mod)
tab_model(mod_full_R, mod_R)

# L 
mod_full_L <- lme(L ~ MAT + MAT2 + MAP + MAP2 + MAP*MAT + Soil_pH + Soil_N + Soil_texture_index + Soil_CEC + Soil_texture_index*MAP + Forest_age, random = ~1|site, weights = varFixed(~sqrt_Duration), data = d_mod)
mod_full_L_noMAT2 <- lme(L ~ MAT + MAP + MAP2 + MAP*MAT + Soil_pH + Soil_N + Soil_texture_index + Soil_CEC + Soil_texture_index*MAP + Forest_age, random = ~1|site, weights = varFixed(~sqrt_Duration), data = d_mod)
mod_L <- lme(L ~ MAT + MAP + MAP2 + MAT:MAP + Soil_pH + Soil_N + Soil_texture_index , random = ~1|site, weights = varFixed(~sqrt_Duration), data = d_mod)
tab_model(mod_full_L, mod_full_L_noMAT2, mod_L)

# Compare full models
tab_model(mod_full_RRL, mod_full_R, mod_full_L,
  dv.labels = c("R/(R+L)", "R", "L"),
  show.aic = TRUE
) 

# Comare best fit models
tab_model(mod_RRL, mod_R, mod_L,
  dv.labels = c("R/(R+L)", "R", "L"),
  show.aic = T)

# Note that summary tables are saved in the GlobalForestRA_SI script

############################### Fig3 - compare model coefficient estimates ###############################

source("Functions/create_model_est_Fig3.r")

# Define the order of the terms on the y axis
mod_order <- c("MAT", "MAT^2", "MAP", "MAP^2","MAT x MAP", "MAP x \nSoil texture index", "Soil texture index","Soil pH", "Soil N", "Forest age (old)", "Forest age (mid)")

# Choose colors 
red_color <- "#21908CFF" #colors_viridis[3] # viridis(6, option= "mako", begin = 0, end = 0.9)[5]
blue_color <- "#E16462FF" #colors_age[1] #viridis(6, option= "magma", begin = 0, end = 0.9)[5] 

# Get the model data and plot the effect sizes for best fit models
mod_est_RRL <- get_mod_est_data(mod_RRL)
EF_RRL <- plot_EF_fig(mod_est_RRL, "R/(R+L)", c(-1, 1), mod_order, red_color, blue_color)

mod_est_R <- get_mod_est_data(mod_R)
EF_R <- plot_EF_fig(mod_est_R, "R (reproductive flux)", c(-1.5, 1.5), mod_order, red_color, blue_color)

mod_est_L <- get_mod_est_data(mod_L)
EF_L <- plot_EF_fig(mod_est_L, "L (leaf flux)", c(-1.25, 1.25), mod_order, red_color, blue_color)


# Put these together in a grid: 
EF_grid <- grid.arrange(
  # First plot
  EF_RRL + 
     theme(
       plot.title = element_text(hjust = 0.5)
     ), 
  # Second plot
  EF_R + 
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.title = element_text(hjust = 0.5)
    ) +
    scale_x_continuous(breaks = seq(-1, 1, 0.5)),
  
  # Third plot
  EF_L + 
    theme(
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      plot.title = element_text(hjust = 0.5)
    ) +
    scale_x_continuous(breaks = seq(-1, 1, 0.5)),
  
  ncol = 3,
  widths = c(1.6, 1, 1)
)

# ggsave("Output/Figure3_modeffects.jpeg", EF_grid, width = 16, height = 10, units = "in", dpi = 300, bg = "white")


############################### Fig4 - compare predictions over climate space ###############################

source("Functions/create_heatmap_pred_Fig4.r")

# Generate data for figure 4 using best fit models for R/(R+L), R, and L
 RRL_tile <- get_new_data(mod_RRL, bc_limits, unique(df$lambda), "RRL") 
 R_tile <- get_new_data(mod_full_R, bc_r_limits, unique(df$lambda_r), "R")
 L_tile <- get_new_data(mod_full_L, bc_l_limits, unique(df$lambda_l), "L")

# Use data to make heat plot tile
tile_plot_RRL <- create_tile_plot(RRL_tile, "RRL", "R/(R+L)")
tile_plot_R <- create_tile_plot(R_tile, "R", "R")
tile_plot_L <- create_tile_plot(L_tile, "L", "L")

# Define axes limits for contour plot - the range of MAT and MAP
mod_mat_axes <- c(-4.2, 27)
mod_map_axes <- c(199, 3550)

# Create the convex hull heat map 
RRL_heatmap <- get_convex_hull_heatmap(RRL_tile, hull_points, "RRL", "R/(R+L)", "R/(R+L)", df_points, mod_mat_axes, mod_map_axes)
R_heatmap  <- get_convex_hull_heatmap(R_tile, hull_points, "R", "R\n(Mg/hayr)", "R (reproductive flux)", df_points,  mod_mat_axes, mod_map_axes)
L_heatmap <- get_convex_hull_heatmap(L_tile, hull_points, "L", "L\n(Mg/hayr)", "L (leaf flux)", df_points,  mod_mat_axes, mod_map_axes)

# Arrange in a grid 
Figure_4 <- grid.arrange(RRL_heatmap, R_heatmap, L_heatmap, ncol = 3)

# Save plot
# ggsave("Output/Figure4_heatmap.jpeg", Figure_4_test, width = 16, height = 6.5, units = "in", dpi = 300, bg = "white")


############################### Fig5 - compare predictions over MAT ###############################

source("Functions/create_prediction_plots_Fig5.r")

# First, create list of  plots generated by source function
plot_list <- list(p1, p2, p3, p4, p5, p6)

# Add labels to each plot
labels <- c("a)", "b)", "c)", "d)", "e)", "f)")

# Add labels to each plot
plot_list_labeled <- lapply(seq_along(plot_list), function(i) {
  plot_list[[i]] + 
    theme(plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm")) +  # top, right, bottom, left margins
    annotate("text", x = -Inf, y = Inf, 
             label = labels[i], 
             hjust = 1.3, vjust = -1.1,
             size = 4) +
    coord_cartesian(clip = "off")  # Allow plotting outside the panel

})

# Arrange all plots
Fig5_combined <- grid.arrange(
  # First row with MAP plots and legend
  arrangeGrob(p1, p2, p3, map_legend,
              widths = c(1, 1, 1, 0.4),
              nrow = 1),
  # Second row with FA plots and legend
  arrangeGrob(p4, p5, p6, fa_legend,
              widths = c(1, 1, 1, 0.4),
              nrow = 1),
  heights = c(1, 1),
  nrow = 2
)

# Arrange plots with labels 
Fig5_combined_labeled <- grid.arrange(
  # First row with MAP plots and legend
  arrangeGrob(plot_list_labeled[[1]], plot_list_labeled[[2]], 
              plot_list_labeled[[3]], map_legend,
              widths = c(1, 1, 1, 0.4),
              nrow = 1),
  # Second row with FA plots and legend
  arrangeGrob(plot_list_labeled[[4]], plot_list_labeled[[5]], 
              plot_list_labeled[[6]], fa_legend,
              widths = c(1, 1, 1, 0.4),
              nrow = 1),
  heights = c(1, 1),
  nrow = 2
)

# Save plot
# ggsave(filename = "GlobalRA_scripts/Paper_figs/Figure5_prediction_plots.jpeg",Fig5_combined_labeled, width = 10, height = 6.5, units = "in", dpi = 300)


############################### Session info ###############################
# sessionInfo()

# R version 4.4.0 (2024-04-24)
# Platform: x86_64-apple-darwin20
# Running under: macOS Ventura 13.3

# Matrix products: default
# BLAS:   /Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/lib/libRblas.0.dylib 
# LAPACK: /Library/Frameworks/R.framework/Versions/4.4-x86_64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0

# locale:
# [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

# time zone: America/Los_Angeles
# tzcode source: internal

# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     

# other attached packages:
#  [1] sp_2.1-4            gridExtra_2.3       ggbeeswarm_0.7.2   
#  [4] patchwork_1.2.0     multcompView_0.1-10 viridis_0.6.5      
#  [7] viridisLite_0.4.2   gghalves_0.1.4      sjPlot_2.8.16      
# [10] nlme_3.1-164        MASS_7.3-60.2       lubridate_1.9.3    
# [13] forcats_1.0.0       stringr_1.5.1       dplyr_1.1.4        
# [16] purrr_1.0.2         readr_2.1.5         tidyr_1.3.1        
# [19] tibble_3.2.1        ggplot2_3.5.1       tidyverse_2.0.0    

# loaded via a namespace (and not attached):
#  [1] gtable_0.3.5       beeswarm_0.4.0     xfun_0.45          bayestestR_0.13.2 
#  [5] insight_0.20.1     lattice_0.22-6     tzdb_0.4.0         vctrs_0.6.5       
#  [9] sjstats_0.19.0     tools_4.4.0        generics_0.1.3     datawizard_0.11.0 
# [13] sandwich_3.1-0     fansi_1.0.6        pkgconfig_2.0.3    Matrix_1.7-0      
# [17] RColorBrewer_1.1-3 ggeffects_1.7.0    lifecycle_1.0.4    compiler_4.4.0    
# [21] farver_2.1.2       sjmisc_2.8.10      munsell_0.5.1      codetools_0.2-20  
# [25] vipor_0.4.7        nloptr_2.0.3       pillar_1.9.0       boot_1.3-30       
# [29] multcomp_1.4-26    tidyselect_1.2.1   sjlabelled_1.2.0   performance_0.12.0
# [33] mvtnorm_1.2-5      stringi_1.8.4      labeling_0.4.3     splines_4.4.0     
# [37] grid_4.4.0         colorspace_2.1-0   cli_3.6.3          magrittr_2.0.3    
# [41] survival_3.5-8     utf8_1.2.4         TH.data_1.1-2      withr_3.0.0       
# [45] scales_1.3.0       timechange_0.3.0   estimability_1.5.1 emmeans_1.10.6    
# [49] lme4_1.1-35.3      zoo_1.8-12         hms_1.1.3          coda_0.19-4.1     
# [53] knitr_1.48         parameters_0.22.0  rlang_1.1.4        isoband_0.2.7     
# [57] Rcpp_1.0.12        xtable_1.8-4       glue_1.7.0         effectsize_0.8.8  
# [61] minqa_1.2.7        jsonlite_1.8.8     R6_2.5.1          