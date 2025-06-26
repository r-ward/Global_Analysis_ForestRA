#create_forestage_mods.r

# Create a standardized age group classifications based on successional stage and forest age
df_age <- df %>%
  mutate(Successional_group = case_when(
    Successional_stage %in% c("old-growth", "intact", "primary") ~ "old",
    Successional_stage %in% c("mid-successional", "secondary", "late-successional") ~ "mid",
    Successional_stage %in% c("early-successional") ~ "young",
    TRUE ~ NA_character_
  )) %>%
  mutate(Successional_group = factor(Successional_group, levels = c("young", "mid", "old")))

  # Define mid ranges for each biome by scheme
  mid_ranges <- list(
    `1` = list(
      tropical = c(50, 100),
      temperate = c(50, 100),
      boreal = c(50, 100)
    ),
    `2` = list(
      tropical = c(20, 60),
      temperate = c(50, 100),
      boreal = c(50, 100)
    ),
    `3` = list(
      tropical = c(20, 60),
      temperate = c(40, 80),
      boreal = c(50, 100)
    ),
    `4` = list(
      tropical = c(20, 60),
      temperate = c(40, 100),
      boreal = c(50, 120)
    ),
    `5` = list(
      tropical = c(30, 60),
      temperate = c(40, 80),
      boreal = c(50, 100)
    )
  )


  # Function to classify ages for a specific scheme
  classify_scheme <- function(forest_age, biome, successional_group, scheme_ranges) {
    case_when(
      # If Forest_age is NA, use the successional stage classification
      is.na(forest_age) ~ successional_group,
      
      # Otherwise use Forest_age based classifications by biome
      biome == "Tropical" & forest_age < scheme_ranges$tropical[1] ~ "young",
      biome == "Tropical" & forest_age >= scheme_ranges$tropical[1] & forest_age < scheme_ranges$tropical[2] ~ "mid",
      biome == "Tropical" & forest_age >= scheme_ranges$tropical[2] ~ "old",
      
      biome == "Temperate" & forest_age < scheme_ranges$temperate[1] ~ "young",
      biome == "Temperate" & forest_age >= scheme_ranges$temperate[1] & forest_age < scheme_ranges$temperate[2] ~ "mid",
      biome == "Temperate" & forest_age >= scheme_ranges$temperate[2] ~ "old",
      
      biome == "Boreal" & forest_age < scheme_ranges$boreal[1] ~ "young",
      biome == "Boreal" & forest_age >= scheme_ranges$boreal[1] & forest_age < scheme_ranges$boreal[2] ~ "mid",
      biome == "Boreal" & forest_age >= scheme_ranges$boreal[2] ~ "old",
      
      TRUE ~ NA_character_
    )
  }
  
  # Add columns for each age classification scheme
  df_age <- df_age %>%
    mutate(
      scheme1 = classify_scheme(Forest_age, Biome, Successional_group, mid_ranges$`1`),
      scheme2 = classify_scheme(Forest_age, Biome, Successional_group, mid_ranges$`2`),
      scheme3 = classify_scheme(Forest_age, Biome, Successional_group, mid_ranges$`3`),
      scheme4 = classify_scheme(Forest_age, Biome, Successional_group, mid_ranges$`4`),
      scheme5 = classify_scheme(Forest_age, Biome, Successional_group, mid_ranges$`5`)
    ) %>%
    mutate(across(starts_with("scheme"), ~factor(., levels = c("young", "mid", "old"))))


# Function to fit model with each Forest_age classification scheme
fit_forest_age_model <- function(df, forestage_col) {
  df %>% 
    mutate(Forest_age = factor(!!sym(forestage_col))) %>%  # Convert to factor
    mutate(Forest_age = relevel(Forest_age, ref = "young")) %>%  # Set reference level
    lme(RRL ~ MAT + MAT2 + MAP + MAP2 + MAT*MAP + Soil_pH + Soil_N + Soil_CEC + 
        Soil_texture_index:MAP + Forest_age, 
        random = ~1|site, 
        weights = varFixed(~sqrt_Duration),
        data = .)
}

# Modify d_mod, dataframe to fit the model (centered/scaled variables) to add forest age schemes
d_mod_age <- d_mod 
d_mod_age$scheme1 <- df_age$scheme1
d_mod_age$scheme2 <- df_age$scheme2
d_mod_age$scheme3 <- df_age$scheme3
d_mod_age$scheme4 <- df_age$scheme4
d_mod_age$scheme5 <- df_age$scheme5

# # Fit all models
 # mod_FA1 <- fit_forest_age_model(d_mod_age, "scheme1")
 # mod_FA2 <- fit_forest_age_model(d_mod_age, "scheme2")
 # mod_FA3 <- fit_forest_age_model(d_mod_age, "scheme3")
 # mod_FA4 <- fit_forest_age_model(d_mod_age, "scheme4")
 # mod_FA5 <- fit_forest_age_model(d_mod_age, "scheme5")

# # save model summary table
# tab_model(mod_FA1, mod_FA2, mod_FA3, mod_FA4, mod_FA5, 
#          dv.labels = c("Scheme 1","Scheme 2", "Scheme 3","Scheme 4","Scheme 5"),
#          show.ci = FALSE, show.se = TRUE,
#          digits = 3,
#          show.aic = T,
#          file = "Output/TableS8_Forest_age_sensitivity.doc")
