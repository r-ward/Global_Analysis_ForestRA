# create_Whittaker_biome_plot.r

# Load libraries
library(plotbiomes) # Whittaker biome plot
library(sf) # Spatial data

# Fig 1b

# Create poly object loaded with plotbiomes library
Whittaker_sf <- st_as_sf(Whittaker_biomes_poly) #convert to sf object

# Create sf point object from data
point_sf <- tibble("MAT" = df$MAT_C, "MAP" = df$MAP_mm) %>%
  mutate(across(c(MAT,MAP), ~as.numeric(.x))) %>% # make sure numeric
  mutate(MAP = MAP / 10) %>% # MAP needs to be in cm 
  st_as_sf(coords = c("MAT", "MAP"))

# Get Whittaker biome
W_biome <- st_join(point_sf, Whittaker_sf, join = st_within) %>% 
  dplyr::select(biome)

Whittaker_df <- df %>% 
  cbind(W_biome) %>% 
  dplyr::select(-c(geometry)) %>% # remove extra info
  rename(Whittaker_biome = biome)

head(Whittaker_df)


# Create base Whittaker biome plot
base <- ggplot() +
  # Add biome polygons
  geom_polygon(data = Whittaker_biomes,
               aes(x    = temp_c,
                   y    = precp_cm,
                   fill = biome),
               # Adjust polygon borders
               colour = "gray98",
               size   = 1, 
               alpha  = .97) + 
  xlab("Mean annual temperature (°C)") + 
  ylab("Mean annual precipitation (cm)") +
  theme_bw() +
  scale_fill_manual(name = "Whittaker biomes",
                    breaks = names(Ricklefs_colors),
                    labels = names(Ricklefs_colors),
                    values = Ricklefs_colors) +
  guides(fill = guide_legend(order = 1))


get_whittakerbiome_plot <- function (df){
  
  fig <- base + 
    geom_point(data = df %>% 
                 filter(MAP_mm < 5000) %>% 
                 distinct(RRL, .keep_all = T),
               aes(x = MAT_C, 
                   y = (MAP_mm / 10), #mm --> cm
                   size = RRL), 
               shape = 21, 
               color = "gray95",
               fill = "black", 
               stroke = .56, 
               alpha = .30, 
               position = "jitter") +
    theme_bw() +
  #   geom_jitter(width = .3) +
    scale_radius(name = "R/(R+L)", breaks = c(.01, .05, .1, .5, .9), range = c(2, 8)) +
    
    guides(shape = guide_legend(override.aes = list(shape = c(1, 1),
                                                    size = c(2, 2),
                                                    stroke = c(1, 1),
                                                    color = c("black", "black"),
                                                    alpha= c(.4, .8))),
          size = guide_legend(override.aes = list(color = "gray80"))) +
    theme(aspect.ratio = 1, 
          plot.title = element_text(hjust = 0.5, size = title_size),
          strip.text.x = element_text(size = axis_size),
          #legend.title = element_blank (),
          axis.title.x = element_text (size = axis_size), # change the axis title
          axis.title.y = element_text (size = axis_size),
          axis.title.y.right = element_text (size = axis_size, color = "black"),
          axis.text.x = element_text (size = axis_size, colour = "black"),
          axis.text.y = element_text (size = axis_size, colour = "black"))

  
  return(fig)
  
  
}