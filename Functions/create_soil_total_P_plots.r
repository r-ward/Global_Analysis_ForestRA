# create_soil_total_P_plots.r
library(ggpubr) # arrange plots 


# Sites with measurments of soil total P
soil_P_complete <- df %>%
  filter(!is.na(Site_soil_total_P))
  
# Sanity check
nrow(soil_P_complete)

# Function to create a plot
create_soilP_plot <- function(data, x_var, y_var, y_title, x_label, y_label, y_units = "") {
  
  n <- nrow(data)
  # Fit linear model
  lm_fit <- lm(paste(y_var, "~", x_var), data = data)
  r2 <- round(summary(lm_fit)$r.squared, 3)
  p_value <- round(summary(lm_fit)$coefficients[2, 4], 3)
  
  # Format p-value for display
  p_value_text <- if(p_value < 0.001) "p < 0.001" else paste("p =", p_value)
  
  # Add units to y_label if provided
  y_label_with_units <- if(y_units != "") paste0(y_label, " (", y_units, ")") else y_label
  
  ggplot(data, aes_string(x = x_var, y = y_var)) +
    geom_point(size = 2, color = "black") +
    geom_smooth(method = "lm", color = "black") +
    labs(
      x = x_label, 
      y = y_label_with_units, 
      title = paste0(y_title, " vs Total Phosphorus")
    ) +
    annotate("text", x = Inf, y = Inf, 
             label = paste0("R² = ", r2, "\n", p_value_text),
             hjust = 1.1, vjust = 1.1) +
    theme_bw()# +

    #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
}

# # Create the three plots
# plot1 <- create_soilP_plot(soil_P_complete, "Site_soil_total_P", "RRL", "R/(R+L)","Total P (mg/kg)", "R/(R+L)")
# plot2 <- create_soilP_plot(soil_P_complete, "Site_soil_total_P", "Repro_flux_Mghayr", "R (reproductive flux)", "Total P (mg/kg)", "R", "Mg/ha·yr")
# plot3 <- create_soilP_plot(soil_P_complete, "Site_soil_total_P", "Leaf_flux_Mghayr", "L (leaf flux)","Total P (mg/kg)", "L", "Mg/ha·yr")

# # arrange in a grid 
# TotalP_plot <- ggarrange(plot1, plot2, plot3, ncol = 3, nrow = 1)

# # save
# ggsave("Output/FigureS12_TotalP_plot.jpeg", TotalP_plot, width = 15, height = 6, dpi = 300, bg = "white")
