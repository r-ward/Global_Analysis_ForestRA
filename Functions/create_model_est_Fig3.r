# create_model_est_Fig3.r 

library(gridExtra)


################### model fixed effects figures ####################

# Get the model data
get_mod_est_data <- function(mod) {

    mod_est_data <- get_model_data(mod,
        type = "est", 
            ) %>% 
        # Rename the terms to be more readable
        mutate(term_fig = case_when( 
            term == "MAT2" ~ "MAT^2",
            term == "MAP2" ~ "MAP^2",
            term == "Soil_pH" ~ "Soil pH",
            term == "Soil_N" ~ "Soil N",
            term == "Soil_CEC" ~ "Soil CEC",
            term == "Soil_texture_index" ~ "Soil texture index",
            term == "Forest_ageold" ~ "Forest age (old)",
            term == "Forest_ageyoung" ~ "Forest age (young)",
            term == "Forest_agemid" ~ "Forest age (mid)",
            term == "MAT:MAP" ~ "MAT x MAP",
            term == "MAP:Soil_texture_index" ~ "MAP x \nSoil texture index",
            term == "Soil_texture_index:MAP" ~ "Soil texture index x \nMAP",
            TRUE ~ term
        ))
    return(mod_est_data) 

}


# Function to plot the effect sizes
plot_EF_fig <- function(mod_est_data, response_var, xlims, order, red_color, blue_color){
    
    # 'mod_est_data' is the output of get_mod_est_data
    # 'response_var' is the name of the response variable, e.g., "R/(R+L)"
    # 'xlims' is a vectors of length 2 defining the limits of the x axis
    # 'order' is a vector of variables defining the order of the terms on the y axis

    EF_fig <- mod_est_data %>% 
        ggplot(aes(x = estimate, 
                   y = term_fig, # term names
                   color = estimate < 0)) + # color based on sign of estimate
        # Draw a point at the esimtate value
        geom_point(size = 6) + 
        # Draw error bars
        geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), 
                       height = 0.1, 
                       linewidth = 2) +
        # Option to change the color of blue and red lines 
        scale_color_manual(values = c("TRUE" = red_color, "FALSE" = blue_color)) +
        # Draw line at x = 0
        geom_vline(xintercept = 0, linetype = "dashed") +
        # Add stars by conf.high according to p value
        geom_text(aes(x = conf.high, label = p.stars), 
                 hjust = 0, 
                 nudge_x = 0.01, 
                 size = 14) +
        theme_bw() +
        # Increase axis text size
        theme(
            title = element_text(size = 22),
            axis.title.x = element_text(size = 20),
            axis.text.x = element_text(size = 20),
            axis.title.y = element_text(size = 20),
            axis.text.y = element_text(size = 20)
            ) +
        labs(y = "Predictor", 
             x = "Coefficient estimate", 
            title = paste(response_var)) + 
        # Remove legend of color, shape
        theme(legend.position = "none") +
        # Reorder y axis based on the reverse order of the term_fig
        scale_y_discrete(limits = rev(order)) + 
        xlim(xlims)

    return(EF_fig)

}

