# create_forest_extent_map.r

# Load libraries
library(raster) # load raster land cover data
library(rnaturalearth) # load country boundaries
library(sp) # load and work with spatial data

# To plot forest extent we use MODIS land cover classes, which can be downloaded 
# from Google Earth Engine using the JavaScript snippet pasted at the end of this file.

# Load raster of land cover data
lc <- raster("Data/land_cover_classification_5000.tif")

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

# // Google Earth Engine Script:
# // Load 2012 MODIS land cover and select the IGBP classification.
# var cover = ee.Image('MODIS/051/MCD12Q1/2012_01_01')
# .select('Land_Cover_Type_1');
# 
# // Define a palette for the 18 distinct land cover classes.
# var igbpPalette = [
#   'aec3d4', // water
#   '152106', '225129', '369b47', '30eb5b', '387242', // forest
#   '6a2325', 'c3aa69', 'b76031', 'd9903d', '91af40',  // shrub, grass
#   '111149', // wetlands
#   'cdb33b', // croplands
#   'cc0013', // urban
#   '33280d', // crop mosaic
#   'd7cdcc', // snow and ice
#   'f7e084', // barren
#   '6f6f6f'  // tundra
# ];
# 
# // Specify the min and max labels and the color palette matching the labels.
# Map.addLayer(cover,
#              {min: 0, max: 17, palette: igbpPalette},
#              'IGBP classification');
# 
# // Export the land cover classification as a GeoTIFF file for the whole world.
# Export.image.toDrive({
#   image: cover, //reprojected,
#   description: 'land_cover_classification',
#   scale: 5000, // set the desired scale in meters
#   crs: 'EPSG:4326' // EPSG code for WGS84
# });


