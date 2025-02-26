# create tables for SI describing response + independent variables  

################################ Tables ################################
# make summary tables of quantitative variables

# response variables 
resp_variable_summary <- df %>% 
    dplyr::select(Repro_flux_Mghayr, Leaf_flux_Mghayr, RRL) %>% 
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
    group_by(variable) %>% 
    summarise(min = min(value), 
            max = max(value), 
            mean = mean(value), 
            sd = sd(value))
    

# indep variables 
ind_var_summary <- df %>% 
    dplyr::select(MAT_C, MAP_mm, Soil_N, Soil_pH, Soil_CEC, Soil_sand_pct, Soil_clay_pct, Soil_silt_pct, Soil_texture_index) %>% 
    rename(MAT = MAT_C, 
           MAP = MAP_mm, 
           N = Soil_N,
           pH = Soil_pH,
           CEC = Soil_CEC,
           sand = Soil_sand_pct,
           clay = Soil_clay_pct,
           silt = Soil_silt_pct,
           texture = Soil_texture_index) %>% 
  #  group_by(biome) %>%
    summarise(across(everything(), list(
    min = ~min(., na.rm = TRUE),
    max = ~max(., na.rm = TRUE),
   # range = ~range(., na.rm = TRUE),
    mean = ~mean(., na.rm = TRUE),
    sd = ~sd(., na.rm = TRUE),
    n = ~sum(!is.na(.))
    ))) %>% 
    # pivot all except biome longer
    pivot_longer(cols = everything(), names_to = c("variable", "statistic"), names_sep = "_") %>%
    # pivot statistic wider
    pivot_wider(names_from = statistic, values_from = value)


biome_age_summary <- df %>% 
    dplyr::select(Biome, Forest_age, Successional_stage) %>% 
    filter(!is.na(Forest_age) & !is.na(Successional_stage)) %>%
    group_by(Biome, Successional_stage) %>% 
    summarise(min = min (Forest_age, na.rm = T), 
            max = max(Forest_age, na.rm = T), 
            n = n())



