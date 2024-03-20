#################################
## Name: Avery Swickard ###
## Date: 3/15/2024 ###
## Lab 7 Deliverable ###
################################

##Question 1: An elevation map of an area of your choosing. 
## Data has been provided in the code. Save an image of it and be sure to turn it in.

#Load in data
abq <- st_read("/Users/averyswickard/Desktop/426 Labs/Lab 7/data/abq.shp")
tracts <- read.csv("/Users/averyswickard/Desktop/426 Labs/Lab 7/data/census_data.csv")
abq_union <- st_union(st_geometry(abq))

#Change from long to wide format
tracts<-tracts %>% 
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  ) %>% dplyr::select(c("GEOID", "NAME", contains('estimate'))) #just keep important variables
tracts$joinName<-gsub(",.*$", "", tracts$NAME)
abq<-abq %>% 
  left_join(tracts, by = c("NAMELSAD" = "joinName"))

#Load additional packages
library(elevatr)
library(progress)

# Provided elevation data from Lab 7 
my_sf_grid <- st_sf(geometry = grd) #create my_sf_grid

eleCoords<-data.frame(x = st_coordinates(my_sf_grid)[,'X'],
                      y = st_coordinates(my_sf_grid)[,'Y'])
abq_elevation <- get_elev_point(eleCoords, prj = 4326, src = "epqs") 

# Remove rows with missing elevation values
abq_elevation <- abq_elevation[complete.cases(abq_elevation$elevation), ]

#First we  need to create a grid in which we want to assign
#interpolation values to. 
bbox <- st_bbox(abq_elevation) #This creates a bounding box around our study area
grd <- st_make_grid(st_as_sfc(bbox), cellsize = 0.0035) # This creates a grid of points
my_sf_grid <- st_sf(geometry = grd)

#Now we need to find the centroid of our polygons. 
centroids <- st_centroid(abq_elevation)

#Complete Inverse Distance Weighted Interpolation
idw_result <- gstat::idw(formula = elevation ~ 1,  # Variable to interpolate
                         locations = centroids,        # Known points
                         newdata = my_sf_grid,         # Points or grid to predict
                         idp = 2,                  
                         nmax = 30,                 
                         maxdist = 1000)             
idw_sf <- st_as_sf(idw_result, coords = c("x", "y"), crs = 4326)

# Reproject abq to match the CRS of idw_sf
abq_reprojected <- st_transform(abq, st_crs(idw_sf))

# Now clip the data to Albuquerque 
clipped_idw <- st_intersection(idw_sf, abq_reprojected)


# Define breaks and labels

breaks_to_labels <- function(breaks) {
  labels <- character(length(breaks))
  for (i in seq_along(labels)) {
    if (i == length(labels)) {
      labels[i] <- paste0("> ", breaks[i], "m")
    } else {
      labels[i] <- paste0(breaks[i], "m - ", breaks[i + 1], "m")
    }
  }
  return(labels)
}

breaks <- c(1500, 1650, 1850, 2050)
elevation_labels <- c("1500 m","1650 m","1850 m","2050 m")

  
# Plot the results
ggplot() + 
  geom_sf(data = clipped_idw, aes(fill = var1.pred), color = NA, size = 0.3) +
  geom_sf(data = abq, fill = NA, color = "black", size = 0.4) +
  scale_fill_viridis_c(option = "rocket", direction = -1, 
                       name = "Elevation (in meters)",
                       breaks = breaks,
                       labels = elevation_labels) +
  labs(title = "Relief Map of Albuquerque", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5) # Center the title
  )

#Save to JPEG
pdf("Relief_Map.pdf")
ggplot() + 
  geom_sf(data = clipped_idw, aes(fill = var1.pred), color = NA, size = 0.3) +
  geom_sf(data = abq, fill = NA, color = "black", size = 0.4) +
  scale_fill_viridis_c(option = "rocket", direction = -1, 
                       name = "Elevation",
                       breaks = breaks,
                       labels = elevation_labels) +
  labs(title = "Relief Map of Albuquerque", x = NULL, y = NULL) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "right",
    plot.title = element_text(hjust = 0.5) # Center the title
  )
dev.off()


###Additional Questions: 

##Question 1: Describe the differences between Inverse Distance Weighted and Kriging approaches to creating 
## smooth continuous surface.

#Answer: Both IDW and Kriging estimate unknown values using the sum of the weighted neighboring observations. 
# However, Kriging is more complex because it models distance between known points to be interpolated 
# but also distances between known points. Also, Kriging will estimate the magnitude & standard error at 
# unknown locations. Whereas, IDW only weighs neighbors depending on their distance from X (more specifically, 
# they are weighted by inverse distance) and does not do anything additional like model distance between points 
# and returns a standard error of estimate like kriging does. Therefore, Kriging allows for more complex models 
# and assess error/uncertainty. 

##Question 2: Describe how contour lines can enhance or retract the quality of a map. 

#Answer: Contour lines can enhance the quality of the map by visualizing a 3D surface more effectively
# because the color scheme in between in each contour line of light and dark colors can be associated with low and 
# high values of a surface. Additionally, the space between contour lines suggest how steep the slope or hill is. 
# If contour lines are close together, that indicates steep slopes and vice versa. However, the use of contour lines 
# can retract the quality of a map by making the surface look like a stepped surface rather than a smooth one because 
# the color scheme is being applied between contour lines. Also, the use of contour lines can cause misleading 
# representation if incorrectly spaced or labeled. If incorrect, the reader will falsely assume information about
# elevation, slope, and terrain features.

