# GlobalForestRA_SI.r

# This script runs the code for the SI figures and tables
# The main analysis must be run first to create the data and model objects used in the SI

source("GlobalForestRA_analysis.r")

############################ Tables ##################################
# Tables S1-S3: Summary stats for response and independent variables
    source("Functions/create_variable_desc_tables.r")
    # write to csv
    # write.csv(resp_variable_summary, "Output/TableS1_Resp_var_summary.csv", row.names = FALSE)
    # write.csv(ind_var_summary, "Output/TableS2_Ind_var_summary.csv", row.names = FALSE)
    # write.csv(biome_age_summary, "Output/TableS3_Biome_age_summary.csv", row.names = FALSE)

# Tables S4: Full model summary table
    tab_model(mod_full_RRL, mod_full_R, mod_full_L, 
            dv.labels = c("RA proxy (R/(R+L))", "Reproductive litterfall (R)", "Leaf litterfall (L)"),
            show.aic = T) #,
            # Save
            # file = "Output/TableS4_FullMod_table.doc")

# # Table 1: Final model summary table
# This table is in the main manuscript 
#     tab_model(mod_RRL, mod_R, mod_L, 
#             dv.labels = c("RA proxy (R/(R+L))", "Reproductive litterfall (R)", "Leaf litterfall (L)"),
#             show.aic = T) #,
#             # Save
#             # file = "Output/Table1_FinalMod_table.doc")

# Table S5: Summary table from sensitivity analysis using sampling_duration as weight
    
    # test weighting schemes
    mod_sqDur_RRL <- lme(RRL ~ MAT + MAT2 + MAP + MAP2 + MAT:MAP + Soil_pH + Soil_N + Soil_texture_index + Soil_texture_index*MAP + Forest_age, random = ~1|site, weights = varFixed(~sqrt_Duration), data = d_mod)
    mod_Dur_RRL <- lme(RRL ~ MAT + MAT2 + MAP + MAP2 + MAT:MAP + Soil_pH + Soil_N  + Soil_texture_index + Soil_texture_index*MAP + Forest_age, random = ~1|site, weights = varFixed(~Sampling_duration), data = d_mod)
    mod_noweight_RRL <- lme(RRL ~ MAT + MAT2 + MAP + MAP2 + MAT:MAP + Soil_pH + Soil_N + Soil_texture_index + Soil_texture_index*MAP + Forest_age, random = ~1|site, data = d_mod)

    # create summary table
    tab_model(mod_sqDur_RRL, mod_Dur_RRL, mod_noweight_RRL, 
            dv.labels = c("sqrt(Duration)", "Duration", "no weight"),
            show.aic = T) #,
            # Save
            # file = "Output/TableS5_Mod_weighting_scheme_test.doc")

# Table S6: Candidate Forest Age schemes 
    # Table created in word, see Table S7 code for mid ranges used

# Table S7: Sensitivity of model fit to Forest Age scheme
    source("Functions/create_forestage_mods.r")
    mod_FA1 <- fit_forest_age_model("scheme1")
    mod_FA2 <- fit_forest_age_model("scheme2")
    mod_FA3 <- fit_forest_age_model("scheme3")
    mod_FA4 <- fit_forest_age_model("scheme4")
    mod_FA5 <- fit_forest_age_model("scheme5")
    tab_model(mod_FA1, mod_FA2, mod_FA3, mod_FA4, mod_FA5, 
            dv.labels = c("Scheme 1","Scheme 2", "Scheme 3","Scheme 4","Scheme 5"),
            show.aic = T) #,
            # Save 
            # file = "Output/TableS7_Forest_age_sensitivity.doc")

# Table S8-9: ANOVA and Tukey's HSD biome; 
    source("Functions/create_word_tables.r")
    # Create Word documents for both analyses
    create_word_tables(df, 
                    group_var = "Biome",
                    doc_path = "Output/TableS8_S9_biome_statistics_tables.docx")

# Table S10-11: ANOVA and Tukey's HSD for biome-plant type
    source("Functions/create_word_tables.r")
    create_word_tables(df_biome, 
                    group_var = "biome_leaftype",
                    doc_path = "Output/TableS10_S11pft_statistics_tables.docx")

# Tables S12-S13: ANOVA and Tukey's HSD for forest age-groups within biomes
    source("Functions/create_word_tables.r")
    create_word_tables(df %>% filter(Biome == "Boreal"), 
                       group_var = "Forest_age_group",
                       doc_path = "Output/TableS12_S13_boreal_age_biome_statistics.docx")
    create_word_tables(df %>% filter(Biome == "Temperate"), 
                       group_var = "Forest_age_group",
                       doc_path = "Output/TableS12_S13_temperate_age_biome_statistics.docx")
    create_word_tables(df %>% filter(Biome == "Tropical"), 
                       group_var = "Forest_age_group",
                       doc_path = "Output/TableS12_S13_tropical_age_biome_statistics.docx")
    
    
# Table S14: Full model site soil 
    source("Functions/create_sitesoil_mods.r")

    # Fit models with site soil data
    full_RRL_site <- lme(RRL ~ MAT + MAT2 + MAP + MAP2 + MAT*MAP + Soil_pH + Soil_N + Soil_texture_index + Soil_CEC +Soil_texture_index*MAP + Forest_age , random = ~1|site, weights = varFixed(~sqrt_Duration), data = df_mod_soil)
    full_R_site <- lme(R ~ MAT + MAT2 + MAP + MAP2 + MAT*MAP + Soil_pH + Soil_N + Soil_texture_index + Soil_CEC + Soil_texture_index*MAP + Forest_age, random = ~1|site, weights = varFixed(~sqrt_Duration), data = df_mod_soil)
    L_site <- lme(L ~ MAT + MAT2 + MAP + MAP2 + MAT*MAP + Soil_pH + Soil_N + Soil_texture_index + Soil_CEC + Soil_texture_index*MAP + Forest_age, random = ~1|site, weights = varFixed(~sqrt_Duration), data = df_mod_soil)
    L_site_noMAT2 <- lme(L ~ MAT + MAP + MAP2 + MAT*MAP + Soil_pH + Soil_N + Soil_texture_index + Soil_CEC + Soil_texture_index*MAP + Forest_age, random = ~1|site, weights = varFixed(~sqrt_Duration), data = df_mod_soil)

    # Create the table
    tab_model(full_RRL_site, full_R_site, L_site, 
            dv.labels = c("R/(R+L); field sampled soil", "R; field sampled soil", "L; field sampled soil"),
            show.aic = TRUE) #,
            # Save
           # file = "Output/TableS14_SiteSoil_mod_table.doc")


################### Figures 

# Fig S1: Correlation between log(R/NPP) and log (R/(R+L))

    source(Functions/create_NPP_plots_Fig1.r)
    log_RNPP_plot <- get_logRNPP_plot(RNPP_sites)
    # ggsave(filename = "Output/FigureS1_logRNPP_plot.jpeg", log_RNPP_plot, width = 6, height = 4)

# Fig S2: Correlation between denominators, log(R+L) and log(NPP)
    logdenom_plot <- get_denom_corr_plot(RNPP_sites)
    # ggsave(filename = "Output/FigureS2_logdenom_plot.jpeg", log_denom_plot, width = 6, height = 4)

# Fig S3: Histogram of sampling duration 
    hist_plot <- ggplot(df, aes(x = Sampling_duration)) + geom_histogram(binwidth = 1) + theme_minimal() +xlab ("Sampling duration (years)")
    #ggsave(filename = "Output/FigureS3_Sampling_duration_hist.jpeg", hist_plot, width = 6, height = 4)

# Fig S4: Box-cox transform plots
    source("Functions/create_bc_transform_plots.r")

    # For RRL, R, L variables
    rrl_bc_plots <- create_boxcox_comparison(df, "RRL", "RA proxy (R/(R+L))", "bc_RRL", "lambda")
    r_bc_plots <- create_boxcox_comparison(df, "Repro_flux_Mghayr", "Reproductive litterfall (R)", "bc_R", "lambda_r")
    l_bc_plots <- create_boxcox_comparison(df, "Leaf_flux_Mghayr", "Leaf litterfall (L)", "bc_L", "lambda_l")

    # Arrange plots
    bc_plots <- grid.arrange(rrl_bc_plots, r_bc_plots, l_bc_plots, nrow = 3)

    # Save plots
    # ggsave(plot = bc_plots, filename = "Output/FigureS4_boxcox_transformation_plots.jpeg", width = 8, height = 10)

# Fig S5: Correlation plot 
    source("Functions/create_corrplot.r")
    create_correlation_plot(d_mod, save_plot = FALSE)

# Fig S6-S8: Model diagnostic plots
    source("Functions/create_mod_diagnostic_plots.r")
    # Generage the diagnostic plots for each model - these take a minute to run. 
    # Expect this error: "Converting missing values (`NA`) into regular values currently not
    # possible for variables of class `NULL`."
    RRL_resid_grid <- get_diagnostic_plots(mod_RRL, "blue")
    R_resid_grid <- get_diagnostic_plots(mod_R, "red")
    L_resid_grid <- get_diagnostic_plots(mod_L, "#0ab00aed")

    # save the plots as a .jpeg to using_Ms
    # ggsave(RRL_resid_grid, filename = "Output/FigureS6_RRL_residuals.jpeg", width = 8, height = 7, units = "in", dpi = 300)
    # ggsave(R_resid_grid, filename = "Output/FigureS7_R_residuals.jpeg", width = 8, height = 7, units = "in", dpi = 300)
    # ggsave(L_resid_grid, filename = "Output/FigureS8L_residuals.jpeg", width = 8, height = 7, units = "in", dpi = 300)

# Fig S9-S11: Model diagnostics - plot individual predictors vs residuals 
    source("Functions/create_indvar_v_resid_plots.r")

    # Plot residuals vs independent variables
    RRL_resid_v_indp_vars <- plot_all_residuals(mod_RRL, d_mod)
    R_resid_v_indp_vars <- plot_all_residuals(mod_R, d_mod)
    L_resid_v_indp_vars <- plot_all_residuals(mod_L, d_mod)

    # Save the plots
    # ggsave(RRL_resid_v_indp_vars, filename = "Output/FigureS9_RRL_resid_v_indp_vars.jpeg", width = 8, height = 7, units = "in", dpi = 300)
    # ggsave(R_resid_v_indp_vars, filename = "Output/FigureS10_R_resid_v_indp_vars.jpeg", width = 8, height = 7, units = "in", dpi = 300)
    # ggsave(L_resid_v_indp_vars, filename = "Output/FigureS11_L_resid_v_indp_vars.jpeg", width = 8, height = 7, units = "in", dpi = 300)

# Fig S12: Correlations b/w response variables and soil P
    source("Functions/create_soil_total_P_plots.r")
    # Create each plots
    plot1 <- create_soilP_plot(soil_P_complete, "Site_soil_total_P", "RRL", "RA proxy (R/(R+L))","Total P (mg/kg)", "R/(R+L)")
    plot2 <- create_soilP_plot(soil_P_complete, "Site_soil_total_P", "Repro_flux_Mghayr", "Reproductive litterfall (R)", "Total P (mg/kg)", "R", "Mg/ha·yr")
    plot3 <- create_soilP_plot(soil_P_complete, "Site_soil_total_P", "Leaf_flux_Mghayr", "Leaf litterfall (L)","Total P (mg/kg)", "L", "Mg/ha·yr")

    # arrange in a grid 
    TotalP_plot <- ggarrange(plot1, plot2, plot3, ncol = 3, nrow = 1)

    # save
    #ggsave("Output/FigureS12_TotalP_plot.jpeg", TotalP_plot, width = 15, height = 6, dpi = 300, bg = "white")
