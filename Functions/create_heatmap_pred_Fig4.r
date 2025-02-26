#create_heatmap_pred_figs.r

library(sp)

######################## Inverse Box-Cox ########################
bc_limits <- range(df$bc_RRL)
bc_r_limits <- range(df$bc_R)
bc_l_limits <- range(df$bc_L)

# Function to do the inverse Box-Cox transformation
inverse_bc <- function(x, lambda ) {
  if (lambda == 0) {
    return(exp(x))
  } else {
    return(exp(log(lambda * x + 1) / lambda))
  }
}

# Save variables to use for scaling new data to match the model
mean_MAT <- mean(df$MAT_C)
sd_MAT <- sd(df$MAT_C)
mean_MAT2 <- mean(df$MAT_C^2)
sd_MAT2 <- sd(df$MAT_C^2)
mean_MAP <- mean(df$MAP_mm)
sd_MAP <- sd(df$MAP_mm)
mean_MAP2 <- mean(df$MAP_mm^2)
sd_MAP2 <- sd(df$MAP_mm^2)


# Generate new data over which to make predictions

get_new_data <- function(model, bc_limits, lambda_val, response_name){

    new_data <- expand.grid(MAT_unsc = seq(min(df$MAT_C), max(df$MAT_C), length.out = 1000),
                               MAP_unsc = seq(min(df$MAP_mm), max(df$MAP_mm), length.out = 1000))

    # Add MAT2 and M_surplus2
    new_data$MAT2_unsc = new_data$MAT_unsc^2
    new_data$MAP2_unsc = new_data$MAP_unsc^2

    # Scale the data to match the model
    new_data$MAT <- (new_data$MAT_unsc - mean_MAT) / sd_MAT
    new_data$MAT2 <- (new_data$MAT2_unsc - mean_MAT2) / sd_MAT2
    new_data$MAP <- (new_data$MAP_unsc - mean_MAP) / sd_MAP
    new_data$MAP2 <- (new_data$MAP2_unsc - mean_MAP2) / sd_MAP2

    # Add mean (0) values for the other variables
    new_data$Soil_CEC <- 0
    new_data$Soil_pH <- 0
    new_data$Soil_N <- 0
    new_data$Soil_texture_index  <- 0

    # Add the forest age categorical variable
    new_data$Forest_age <- factor("mid") 
    new_data$Sampling_duration <- 1

    # Predict the RA proxy
    new_data$bc_response <- predict(model, new_data, level = 0) # re.form = NA

    # Inverse Box-Cox transform the predictions to get R/(R+L)
    new_data$bc_response_trimmed <- ifelse(new_data$bc_response < bc_limits[1],
                                                bc_limits[1], 
                                                ifelse(new_data$bc_response > bc_limits[2], 
                                                         bc_limits[2], 
                                                         new_data$bc_response))

    new_data$response <- inverse_bc(new_data$bc_response_trimmed, lambda = lambda_val)
    
    # Rename response variable
    new_data <- new_data %>%
    rename(!! paste0("bc_", response_name) := bc_response) %>%
    rename(!! paste0("bc_", response_name, "_trimmed") := bc_response_trimmed) %>%
    rename(!!response_name := response)

    return(new_data)

}

# Create predictions over range of new data, 
# - result are a heat map tile of the response variable
# - generate data for figure 4 using best fit models for R/(R+L), R, and L
 RRL_tile <- get_new_data(mod_RRL, bc_limits, unique(df$lambda), "RRL") # test the function
 R_tile <- get_new_data(mod_R, bc_r_limits, unique(df$lambda_r), "R")
 L_tile <- get_new_data(mod_L, bc_l_limits, unique(df$lambda_l), "L")

# Save tile data - optional 
# write.csv(RRLtile_MS, "GLobalRA_scripts/Figs/using_Ms/heat_map/RRLtile_MS.csv")
# write.csv(Rtile_MS, "GLobalRA_scripts/Figs/using_Ms/heat_map/Rtile_MS.csv")
# write.csv(Ltile_MS, "GLobalRA_scripts/Figs/using_Ms/heat_map/Ltile_MS.csv")


# Create the tile plots to test the function
create_tile_plot <- function(data, z_var, z_label) {
  ggplot(data, aes(x = MAT_unsc, y = MAP_unsc, z = .data[[z_var]])) +
    geom_tile(aes(fill = .data[[z_var]]), alpha = 0.9) +
    geom_contour(linetype = "dashed", color = "grey32") +
    scale_fill_viridis_c() +
    labs(x = "MAT", y = "MAP", fill = z_label) +
    theme_minimal() +
    theme(
      text = element_text(size = 22),
      axis.text = element_text(size = 22),
      axis.title = element_text(size = 26),
      legend.text = element_text(size = 22),
      legend.title = element_text(size = 22),
      plot.title = element_text(size = 28)
    ) 
}


################ create tile plots #########################
# in main script 
# might want to include the unclipped heat plot tiles in the SI 

# tile_plot_RRL <- create_tile_plot(RRL_tile, "RRL", "R/(R+L)")
# tile_plot_R <- create_tile_plot(R_tile, "R", "R")
# tile_plot_L <- create_tile_plot(L_tile, "L", "L")


########################### Heat map of convex hull ###########################

# Compute the convex hull of the original data points
df_unique <- unique(df[, c("MAT_C", "MAP_mm")])
colnames(df_unique) <- c("MAT_unsc", "MAP_unsc")
hull_indices <- chull(df_unique$MAT_unsc, df_unique$MAP_unsc)
hull_points <- df_unique[hull_indices, ]
df_points <- df %>% dplyr::select(MAT_C, MAP_mm, Sampling_duration) %>% 
                            rename(MAT_unsc = MAT_C,
                                MAP_unsc = MAP_mm)


# Function to plot the response variable (using tile data) over the convex hull
get_convex_hull_heatmap <- function(tile_data, hull_points, z_var, z_label, z_title, df_points, xlims, ylims) {

    # Create a mask for the convex hull
    grid <- tile_data
    points_in_hull <- point.in.polygon(grid$MAT_unsc, grid$MAP_unsc, hull_points$MAT_unsc, hull_points$MAP_unsc)

    # Keep only the points inside the convex hull
    grid <- grid[points_in_hull > 0, ]

    # Plot the heatmap 
    convex_hull_heatmap <- ggplot() +
        geom_tile(data = grid, aes(x = MAT_unsc, y = MAP_unsc, fill = !!sym(z_var)), alpha = .9) +
            scale_fill_viridis_c() +
        geom_polygon(data = hull_points, aes(x = MAT_unsc, y = MAP_unsc), color = "black", fill = NA) +
        geom_contour(data = grid, aes(x = MAT_unsc, y = MAP_unsc, z = !!sym(z_var)), color = "black") +
        geom_point(data = df_points, 
                            aes(x = MAT_unsc, y = MAP_unsc, size = Sampling_duration), color = "black", shape = 21, fill = "#ebe7eb", alpha = .6) +
        labs(title = paste(z_title), x = "MAT", y = "MAP", fill = z_label, size = "Sampling\n duration") +
        theme_bw() + 
        theme(
          # legend.position = "bottom"
            axis.title = element_text(size = 14)
          ) +
        guides(fill = guide_colorbar(order = 1)) #+
       # theme_bw() 
        
    return(convex_hull_heatmap)
}

