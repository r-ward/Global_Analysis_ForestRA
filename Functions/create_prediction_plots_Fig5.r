# Create Figure 5

library(viridis)
library(gird)
library(gridExtra)
library(ggplot2)

# For ease: save some variables to use for scaling new data to match model data
mean_MAT <- mean(df$MAT_C)
sd_MAT <- sd(df$MAT_C)
mean_MAT2 <- mean(df$MAT_C^2)
sd_MAT2 <- sd(df$MAT_C^2)
mean_MAP <- mean(df$MAP_mm)
sd_MAP <- sd(df$MAP_mm)
mean_MAP2 <- mean(df$MAP_mm^2)
sd_MAP2 <- sd(df$MAP_mm^2)

# For ease: make sure we have saved unscaled var in d_mod
d_mod$MAT_unsc <- (d_mod$MAT * sd_MAT) + mean_MAT
d_mod$MAP_unsc <- (d_mod$MAP * sd_MAP) + mean_MAP

# Get MAP values to use for predictions 
mean_MAP <- round(mean(df$MAP_mm),0)
high_MAP <- round(mean(df$MAP_mm) + sd(df$MAP_mm),0) # 698.5 # below this, not much forest
low_MAP <- round(mean(df$MAP_mm) - sd(df$MAP_mm),0) # 1999 # w/in range most moist tropical forest

# Labels for different MAP cases
meanMAPlab <- paste(mean_MAP, "(mean)")
highMAPlab <- paste(high_MAP, "(+1 SD)")
lowMAPlab <- paste(low_MAP, "(-1 SD)")

######################## Inverse Box-Cox transform ########################

# Function to do the inverse box cox transformation
inverse_bc <- function(x, lambda ) {
  if (lambda == 0) {
    return(exp(x))
  } else {
    return(exp(log(lambda * x + 1) / lambda))
  }
}

##### Generate predictions over MAT using a range of MAP values

get_newMATdata_fig5 <- function(model, bc_limits, lambda_val, response_name){
  # Create MAT sequence and MAP cases
  new_data <- expand.grid(
    MAT_unsc = seq(min(df$MAT_C), max(df$MAT_C), length.out = 100),
    MAP_case = c("low", "mean", "high")
  )
  # Assign MAP values based on case
  new_data$MAP_unsc <- case_when(
    new_data$MAP_case == "low" ~ mean(df$MAP_mm) - sd(df$MAP_mm),
    new_data$MAP_case == "mean" ~ mean(df$MAP_mm),
    new_data$MAP_case == "high" ~ mean(df$MAP_mm) + sd(df$MAP_mm)
  )
  # Calculate squared terms
  new_data$MAT2_unsc = new_data$MAT_unsc^2
  new_data$MAP2_unsc = new_data$MAP_unsc^2
  # Scale the variables to match the model
  new_data$MAT <- (new_data$MAT_unsc - mean_MAT) / sd_MAT
  new_data$MAT2 <- (new_data$MAT2_unsc - mean_MAT2) / sd_MAT2
  new_data$MAP <- (new_data$MAP_unsc - mean_MAP) / sd_MAP
  new_data$MAP2 <- (new_data$MAP2_unsc - mean_MAP2) / sd_MAP2
  # Add other predictor variables at their reference values, 0
  new_data$Soil_CEC <- 0
  new_data$Soil_pH <- 0
  new_data$Soil_N <- 0
  new_data$Soil_texture_index <- 0
  new_data$Forest_age <- factor("mid")
  new_data$Sampling_duration <- 1
  
  # Generate predictions
  new_data$bc_response <- predict(model, new_data, level = 0)
  # Inverse transform the predictions
  new_data$response <- inverse_bc(new_data$bc_response, lambda = lambda_val)
  # Rename column
  new_data <- new_data %>%
    rename(!!response_name := response)
  
  return(new_data)
}

####### Generate predictions over MAT and Forest Age 

get_newFAdata_fig5 <- function(model, bc_limits, lambda_val, response_name){
  # Create MAT sequence and Forest age cases
  new_data <- expand.grid(
    MAT_unsc = seq(min(df$MAT_C), max(df$MAT_C), length.out = 100),
    Forest_age = factor(c("young", "mid", "old"), levels = levels(d_mod$Forest_age))
  )
  # Add MAP at its mean value
  new_data$MAP_unsc <- mean(df$MAP_mm)
  # Calculate squared terms
  new_data$MAT2_unsc = new_data$MAT_unsc^2
  new_data$MAP2_unsc = new_data$MAP_unsc^2
  # Scale the variables to match the model
  new_data$MAT <- (new_data$MAT_unsc - mean_MAT) / sd_MAT
  new_data$MAT2 <- (new_data$MAT2_unsc - mean_MAT2) / sd_MAT2
  new_data$MAP <- (new_data$MAP_unsc - mean_MAP) / sd_MAP
  new_data$MAP2 <- (new_data$MAP2_unsc - mean_MAP2) / sd_MAP2
  # Add other predictor variables at their reference values
  new_data$Soil_CEC <- 0
  new_data$Soil_pH <- 0
  new_data$Soil_N <- 0
  new_data$Soil_texture_index <- 0
  new_data$Sampling_duration <- 1
  # Generate predictions
  new_data$bc_response <- predict(model, new_data, level = 0)
  # Inverse transform the predictions  
    new_data$response <- inverse_bc(new_data$bc_response, lambda = lambda_val)
  # Rename column
  new_data <- new_data %>%
    rename(!!response_name := response)
  
  return(new_data)
}


plot_MATresponse <- function(data, y_var, y_lab, colors_MAP) {
  ggplot(data, aes(x = MAT_unsc, y = .data[[y_var]], color = MAP_case)) +
    geom_line(linewidth = 1) +
    scale_color_manual(
      values = colors_MAP,
      breaks = c("low", "mean", "high"),
      labels = c(lowMAPlab, meanMAPlab, highMAPlab)
    ) +
    theme_bw() +
    labs(
      x = "MAT (°C)",
      y = y_lab,
      color = "MAP (mm/yr)"
    ) +
    theme(
      legend.position = "right",
      panel.grid.minor = element_blank()
    )
}

# Plot forest age over temperature

plot_FA_responses <- function(data, y_var, y_lab, colors_age) {
  ggplot(data, aes(x = MAT_unsc, y = .data[[y_var]], color = Forest_age)) +
    geom_line(linewidth = 1) +
    scale_color_manual(
      values = colors_age,
      labels = c("Young", "Mid", "Old")
    ) +
    theme_bw() +
    labs(
      x = "MAT (°C)",
      y = y_lab,
      color = "Forest Age"
    ) +
    theme(
      legend.position = "right",
      panel.grid.minor = element_blank()
    )
}



###### Make MAT, MAP plots
# Get predictions: 
RRL_MAT_preds <- get_newMATdata_fig5(mod_RRL, bc_limits, unique(df$lambda), "RRL")
R_MAT_preds <- get_newMATdata_fig5(mod_R, bc_r_limits, unique(df$lambda_r), "R")
L_MAT_preds <- get_newMATdata_fig5(mod_L, bc_l_limits, unique(df$lambda_l), "L")
# Plot
RRL_FA_data <- get_newFAdata_fig5(mod_RRL, bc_limits, unique(df$lambda), "RRL")
R_FA_data <- get_newFAdata_fig5(mod_R, bc_r_limits, unique(df$lambda_r), "R")
L_FA_data <- get_newFAdata_fig5(mod_L, bc_l_limits, unique(df$lambda_l), "L")


# Define colors 
colors_MAP <- viridis(3, begin = 0.35, end = .85, direction = -1) # dark purple endpoint 0, less yellow upper end 0.9
colors_age <- viridis(3, begin = 0.0, end = .8, option= "plasma", direction = -1) 

RRL_plot <- plot_MATresponse(RRL_MAT_preds, "RRL", "RA proxy (R/(R+L))", colors_MAP)
R_plot <- plot_MATresponse(R_MAT_preds, "R", "R (Mg ha-1 yr-1)", colors_MAP)
L_plot <- plot_MATresponse(L_MAT_preds, "L", "L (Mg ha-1 yr-1)", colors_MAP)

RRL_FA_plot <- plot_FA_responses(RRL_FA_data, "RRL", "RA proxy (R/(R+L)", colors_age)
R_FA_plot <- plot_FA_responses(R_FA_data, "R", "R (Mg ha-1 yr-1)", colors_age)
L_FA_plot <- plot_FA_responses(L_FA_data, "L", "L (Mg ha-1 yr-1)", colors_age)

# Generate separate legends for MAP and Forest Age
# Function to extract legend from a ggplot object
get_legend <- function(plot) {
  # Create a gtable from the ggplot object
  tmp <- ggplot_gtable(ggplot_build(plot))
  
  # Find the index of the legend in the gtable
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  
  # If there's no legend, return NULL
  if(length(leg) == 0) return(NULL)
  
  # Extract and return the legend
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Get legends from one plot of each type
map_legend <- get_legend(RRL_plot)
fa_legend <- get_legend(RRL_FA_plot)





