# create_sitesoil_mods.r
library(datawizard)
library(nlme)
library(MASS)
library(sjPlot)


# For soil properties measured on site (N, pH, CEC, Sand, Clay, Silt)
df_sitesoil <- df %>%
  filter(!is.na(Site_soil_N),
         !is.na(Site_soil_pH),
         !is.na(Site_soil_CEC),
         !is.na(Site_soil_sand),
         !is.na(Site_soil_clay),
         !is.na(Site_soil_silt))

# Sanity check
nrow(df_sitesoil)

########### Box-Cox transform
# R/(R+L)
b <- boxcox(lm(df_sitesoil$RRL ~ 1)) 
lambda <- b$x[which.max(b$y)] # 0.5454545
# R (reproductive flux) 
br <- boxcox(lm(df_sitesoil$Repro_flux_Mghayr ~ 1)) 
lambda_r <- br$x[which.max(br$y)] # 0.2222222
# L (leaf flux)
bl <- boxcox(lm(df_sitesoil$Leaf_flux_Mghayr ~ 1)) 
lambda_l <- bl$x[which.max(bl$y)] # 0.5858586

# Add lambda values to the dataframe
df_sitesoil$lambda <- lambda
df_sitesoil$lambda_r <- lambda_r
df_sitesoil$lambda_l <- lambda_l

# Calculate the box-cox transformed values and save in dataframe
df_sitesoil$bc_RRL <- ((df_sitesoil$RRL ^ lambda) - 1) / lambda
df_sitesoil$bc_R <- ((df_sitesoil$Repro_flux_Mghayr ^ lambda_r) - 1) / lambda_r
df_sitesoil$bc_L <- ((df_sitesoil$Leaf_flux_Mghayr ^ lambda_l) - 1) / lambda_l

############ Model fitting ############
d_mod_soil <- df_sitesoil %>%
  # Select variables
  dplyr::select(ID, Site, Subsite, 
                bc_RRL, bc_R, bc_L,
                MAT_C, MAP_mm,
                Site_soil_pH, Site_soil_N, Site_soil_CEC, Site_soil_texture_index,
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
               "Site_soil_pH", "Site_soil_N", "Site_soil_CEC", "Site_soil_texture_index")
  ) %>%
  # Factor conversion and duration calculation
  mutate(Forest_age = factor(Forest_age, levels = c("young", "mid", "old")),
         sqrt_Duration = sqrt(Sampling_duration))


# Set reference level for Forest_age
#  relevel, note there are no "young" forests in this dataset
d_mod_soil$Forest_age <- relevel(d_mod_soil$Forest_age, ref = "mid")  

# Fit models - in main SI script

# full_RRL_site <- lme(RRL ~ MAT + MAT2 + MAP + MAP2 + MAT*MAP + Site_soil_pH + Site_soil_N + Site_soil_texture_index + Site_soil_CEC + Site_soil_texture_index*MAP + Forest_age , random = ~1|site, weights = varFixed(~sqrt_Duration), data = d_mod_soil)
# full_R_site <- lme(R ~ MAT + MAT2 + MAP + MAP2 + MAT*MAP + Site_soil_pH + Site_soil_N + Site_soil_texture_index + Site_soil_CEC + Site_soil_texture_index*MAP + Forest_age, random = ~1|site, weights = varFixed(~sqrt_Duration), data = d_mod_soil)
# L_site <- lme(L ~ MAT + MAT2 + MAP + MAP2 + MAT*MAP + Site_soil_pH + Site_soil_N + Site_soil_texture_index + Site_soil_CEC + Site_soil_texture_index*MAP + Forest_age, random = ~1|site, weights = varFixed(~sqrt_Duration), data = d_mod_soil)
# L_site_noMAT2 <- lme(L ~ MAT + MAP + MAP2 + MAT*MAP + Site_soil_pH + Site_soil_N + Site_soil_texture_index + Site_soil_CEC + Site_soil_texture_index*MAP + Forest_age, random = ~1|site, weights = varFixed(~sqrt_Duration), data = d_mod_soil)

# Save modelsummary table 
# tab_model(full_RRL_site, full_R_site, L_site, 
#           dv.labels = c("R/(R+L); field sampled soil", "R; field sampled soil", "L; field sampled soil"),
#           show.aic = TRUE,
#           # save
#           file = "Output/TableS13_SiteSoil_mod_table.doc")
