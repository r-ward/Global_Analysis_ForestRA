

create_correlation_plot <- function(d_mod, save_plot = FALSE) {
  # Load required libraries
  require(corrplot)
  require(dplyr)
  
  # Select and prepare variables for correlation
  M <- cor(d_mod %>%
    dplyr::select(c(MAT, MAP, Soil_N, Soil_CEC, Soil_texture_index, Soil_pH)) %>%
    # replace "_" with " " in column names
    setNames(gsub("_", " ", colnames(.))))
  
  # Get test results for the correlation matrix
  results_M <- cor.mtest(M, conf.level = 0.95)
  
  # Create the correlation plot
  corr_plot <- corrplot(M,
    p.mat = results_M$p,
    method = 'circle',
    type = 'lower',
    insig = 'blank',
    addCoef.col = 'black',
    number.cex = 1.2,
    order = 'AOE',
    diag = FALSE,
    tl.col = "black",
    tl.cex = 1.2)
  
  # Save plot if save_plot is TRUE
  if(save_plot) {
    jpeg("Output/FigureS5_corr_plot_fig.jpeg", 
         width = 7, 
         height = 7, 
         units = "in", 
         res = 300)
    
    corrplot(M,
      p.mat = results_M$p,
      method = 'circle',
      type = 'lower',
      insig = 'blank',
      addCoef.col = 'black',
      number.cex = 1.2,
      order = 'AOE',
      diag = FALSE,
      tl.col = "black",
      tl.cex = 1.2)
    
    dev.off()
  }
  
  # Return the correlation matrix and test results invisibly
  invisible(list(correlation = M, tests = results_M))
}
