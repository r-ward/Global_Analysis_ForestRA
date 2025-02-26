#create_indvar_v_resid_plots.r

library(ggplot2)
library(gridExtra)

plot_all_residuals <- function(model, data) {
  # Get residuals
  residuals <- resid(model)
  
  # Get independent variable names from model formula
  # Remove response variable, random effects, and squared terms
  formula_terms <- labels(terms(model))
  # Remove interaction terms (those containing ":")
  main_terms <- formula_terms[!grepl(":", formula_terms)]
  # Remove squared terms (those containing "^2" or "2")
  main_terms <- main_terms[!grepl("2$", main_terms)]
  
  # Create list to store plots
  plots <- list()
  
  # Create a plot for each predictor
  for(var in main_terms) {
    # Create data frame for plotting
    plot_data <- data.frame(
      x = data[[var]],
      Residuals = residuals
    )
    names(plot_data)[1] <- "Predictor"
    
    # Create plot
    p <- ggplot(plot_data, aes(x = Predictor, y = Residuals)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "loess", se = TRUE, color = "red", span = 1.5) +
      geom_hline(yintercept = 0, linetype = "dashed") +
     # custom_theme +
      labs(title = paste("Residuals vs", var),
           x = var,
           y = "Residuals")
    
    plots[[var]] <- p
  }
  
  # Calculate grid dimensions
  n_plots <- length(plots)
  n_cols <- min(3, n_plots)  # Maximum 3 plots per row
  n_rows <- ceiling(n_plots / n_cols)
  
  # Arrange plots in a grid
  do.call(grid.arrange, c(plots, ncol = n_cols)) 
                                    
}

