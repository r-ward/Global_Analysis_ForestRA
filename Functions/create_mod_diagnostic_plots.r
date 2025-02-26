
library(performance) # Plot VIF, plot normality of residuals
library(broom.mixed) # Augment function to get a tidy version of model data
library(cowplot)     # For plot_grid() to arrange plots

################### Model diagnostics ####################

# Function to make the gridded plots for each model
get_diagnostic_plots <- function(mod, color) {

    mod_out <- plot(check_model(mod, panel = FALSE))
    mod_model <- broom.mixed::augment(mod)
    mod_model[".stdresid"] <- resid(mod, type = "pearson")

    # Set color
    mod_color = color

    # Density plot the residuals normality, reference:mod_out[[4]]
    plot_1 <- ggplot(mod_model, aes(.stdresid)) +
        geom_density(fill = mod_color, alpha = 0.5) +
        theme_bw() +
        xlab("Residuals")+
        ylab("Density") +
        ggtitle("Normality of residuals") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) # center and bold title text
    
    # Plot the residuals vs fitted values
    plot_2 <- ggplot(mod_model, aes(.fitted, .resid)) + 
        geom_point(color = mod_color, shape = 21, size = 2) +
        geom_hline(yintercept = 0) +
        theme_bw() +
        xlab("Fitted values") +
        ylab("Residuals") +
        ggtitle("Residuals vs. fitted values") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))    # center and bold title text 
        

    # Normal qq plot
    plot_3 <- ggplot(mod_model, aes(sample = .stdresid)) +
        geom_qq(alpha = .6, color = mod_color) +
        geom_qq_line() +
        theme_bw() +
        xlab("Theoretical quantiles") +
        ylab("Sample quantiles") +
        ggtitle("Normal Q-Q plot") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))   # center & bold

    # Collinearity plot
    plot_4 <- mod_out[[2]]+ 
        theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
        # Remove subtitle
        theme(plot.subtitle = element_text(size = 0)) +
        # Change title text to "Collinearity plot"
        ggtitle("Collinearity of predictors") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
        theme(legend.position = "right") +
        theme(axis.text.x = element_text(size = 8))

    # Arrange in a grid
    plots_in_grid <- plot_grid(plot_1, NULL, plot_2, 
                         plot_3, NULL, plot_4, 
                         ncol = 3, 
                         rel_widths = c(1, -.2, 1, 1, -.2, 1),
                         align = 'hv', axis = 'tblr')

    return(plots_in_grid) 


}


