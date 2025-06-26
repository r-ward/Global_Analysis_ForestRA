# create_bc_transform_plots.r
library(ggplot2)
library(gridExtra)

# This function will generate plots that show the raw and Box-Cox transformed data

create_boxcox_comparison <- function(data, var_name, var_label, var_x_label, bc_var_name, lambda_name) {
  
  # Get the raw and transformed data
  raw_data <- data[[var_name]]
  transformed_data <- data[[bc_var_name]]
  lambda_value <- data[[lambda_name]][1]  # lambda is same for all rows
  
  # Format lambda to show 4 decimal places
  lambda_text <- sprintf("%.4f", lambda_value)
  
  # Create histogram of raw data
  raw_hist <- ggplot(data.frame(x = raw_data), aes(x = x)) +
    geom_histogram(bins = 30, fill = "lightblue", color = "black") +
    theme_bw() +
    labs(title = paste("Raw", var_label, "\n"),
         x = var_x_label,
         y = "Count")
  
  # Create histogram of transformed data with both lambda value printed
  transformed_hist <- ggplot(data.frame(x = transformed_data), aes(x = x)) +
    geom_histogram(bins = 30, fill = "lightblue", color = "black") +
    theme_bw() +
    labs(title = paste0("Box-Cox Transformed ", var_label, 
                       "\n λ = ", lambda_text),
                       # sw_text),
         x = paste("Transformed", var_x_label),
         y = "Count")
  
  # Arrange plots side by side
  grid.arrange(raw_hist, transformed_hist, ncol = 2)
}

