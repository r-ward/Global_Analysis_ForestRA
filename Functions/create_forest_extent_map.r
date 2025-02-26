# create_forest_extent_map.r

# Load libraries
library(raster) # load raster land cover data
library(rnaturalearth) # load country boundaries
library(sp)# load and work with spatial data

# I use MODIS land cover classes, which can be downloaded from the LP DAAC
# (https://lpdaac.usgs.gov) as "land_cover_classification_5000.tif"

# Load raster of land cover data
lc <- raster("~/Desktop/Field_data/GeoSpatial/land_cover_classification_5000.tif")

# Format raster data and transform into spatial points
names(lc) <- "lc"
lc_points <- rasterToPoints(lc)
x_ <- lc_points[,1] 
y_ <- lc_points[,2] 
lc_ <- lc_points[,3]

# Save points in tibble
lc_df <- tibble("x" = x_,  "y" = y_, "lc" = lc_ )

# Filter for tree cover land classes
lc_df_tree <- lc_df %>% 
  filter(lc %in% c(1:5,8)) # remove all non-tree land classes


# Create base map
    # Land 
    land <- ne_download(
    scale = 50,
    type = "land",
    category = "physical",
    returnclass = "sf")
    # Country borders
    countries <- ne_countries(
    returnclass = "sf",
    scale = 50)

world_base <- ggplot() +
  geom_sf(data = land,
          color = "grey50",
          fill = "aliceblue", 
          lwd = 1) +
  geom_sf(data = countries,
          color = "white",
          fill = "#dfdfdf",
          linewidth = 0.2)  +
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue",
                                        linewidth = 0.5, linetype = "solid")) 


# Create map of forest extent and plot data points 
forest_sitemap <- 
  world_base +
  geom_tile(data = lc_df_tree,
            aes(x = x,
                y = y),
            col = "darkolivegreen4") +
  
  # Overlay the Country borders
  geom_sf(data = countries,
          color = "white",
          fill = NA,
          size = 0.05) +
    # Add lat, lon of data points
  geom_point(data = df, 
             aes(x = Longitude, 
                 y = Latitude),
             shape = 21,
             fill = "white",
             color = "grey20", 
             # alpha = .75,
             size = 1.8, 
             stroke = .4) +
             xlab("") +
             ylab("")


# No forest extent in background
forest_sitemap_alt <- 
  world_base +
  # Overlay the Country borders
  geom_sf(data = countries,
          color = "white",
          fill = NA,
          size = 0.05) +
    # Add lat, lon of data points
  geom_point(data = df, 
             aes(x = Longitude, 
                 y = Latitude),
             shape = 21,
             fill = "white",
             color = "grey20", 
             # alpha = .75,
             size = 1.2, 
             stroke = .4) +
             xlab("") +
             ylab("") 
