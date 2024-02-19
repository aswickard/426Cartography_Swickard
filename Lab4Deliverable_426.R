####################################
## Name: Avery Swickard ###
## Date: 2/15/2024 ###
## Lab 4 Deliverable ###

##Question 1: Take the world map object and subset to Germany and produce a jpeg image. 
# Submit your code and the Germany jpeg image. 

#Load World Map Data
world <- st_as_sf(maps::map('world', plot = FALSE, fill = TRUE))

#Subset of Germany
germany <- st_crop(europe, xmin = 5, xmax = 15, ymin = 47, ymax = 55)
ggplot(data = germany) +
  geom_sf() +
  labs(title = "Map of Germany") +
  theme_minimal()

#Produce a jpeg image
ggsave("germany_map.jpeg", width = 10, height = 7, dpi = 300)


##Question 2: Change the resolution of the volcano dataset to a factor of 30 and find the ‘max’ cell stat. 

#Load Volcano Data
data(volcano)
volcano_raster <- raster(volcano)

#Plot the Resolution Raster of the Volcano dataset to a factor of 30 
volcano_lowres <- aggregate(volcano_raster, fact = 30)
plot(volcano_lowres, main = "Lower Resolution Raster (Factor = 30)")

#Find the "Max" Cell Stat
lowres_max <- cellStats(volcano_lowres, stat = max)
cat("Max Cell Stat (Lower Resolution - Factor 30):", lowres_max, "\n")

##Question 3: Please smooth the North Carolina dataset using the ‘ksmooth’ and ‘spline’ approaches and 
# plot them to the screen on a 2x2 grid. 

#Load North Carolina Shapefile
nc <- st_read(system.file("shape/nc.shp", package="sf"))
plot(st_geometry(nc), main="Original North Carolina Shapefile")

#Conduct ksmooth and spline 
smoothed_ksmooth_nc <- smooth(nc, method = 'ksmooth')
smoothed_spline_nc <- smooth(nc, method = 'spline')

# Plot in a 2x2 grid
par(mfrow = c(2, 2))

plot(st_geometry(nc), main="Original NC Shapefile")
plot(st_geometry(smoothed_ksmooth_nc), main="Smoothed NC - ksmooth")
plot(st_geometry(nc), main="Original NC Shapefile")
plot(st_geometry(smoothed_spline_nc), main="Smoothed NC - spline")


##Question 4: 4.	Plot your own convex and concave polygon using your own point dataset 
# (you might have to browse the internet for a set of spatial points data).

#Generate Some Random Points
set.seed(123)  
n_points <- 75  
points <- matrix(runif(10 * n_points, min = -6, max = 8), ncol = 2)
colnames(points) <- c("x", "y")
points_sf <- st_as_sf(data.frame(points), coords = c("x", "y"), crs = 4326)

# Create the convex hull as a polygon
convex_hull_polygon <- st_convex_hull(st_combine(points_sf))

# Create the concave hull as a polygon
concave_hull_polygon <- st_sf(geometry = concaveman(points_sf))

# Plot the results
plot(st_geometry(points_sf), col = 'blue', pch = 20, cex = 0.5, main = "Random Points")
plot(st_geometry(convex_hull_polygon), col = 'green', border = 'black', lwd = 2, main = "Random Points (Convex)")
plot(st_geometry(concave_hull_polygon), col = 'green', border = 'black', lwd = 2, main = "Random Points (Concave)")

###Additional Questions

##Question 1: When would you want to change the scale of your data?
#You would want to change the scale of your data when you want to change how your data is represented or analyzed.
#Additionally, When you are changing the area that you want to be studied, then you need to change the scale. Finally, if you want to
#visualize a smaller or larger area of your data, then you need to change the scale. 

##Question 2: What are some pros and cons of changing your raster resolution to have less resolution?
# Some cons of changing your raster resolution to have less resolution is that you loose detail in the area of study. 
# Also, you can no longer distinguish subtle differences in your data. Some pros of changing your raster resolution
# to have lower resolution is when you are displaying a vast area. There is no need for very fine resolution data if your
# map is very small-scale. Another pro of having lower resolution is if don't want your audience to distinguish the finer 
# details of your data, for reasons such as privacy. 

##Question 3: When would you want to generalize your polygon data (think countries, states, counties, etc)
# You would want to generalize your polygon data when you need to reduce the complexity of the geometry while 
# preserving its essential characteristics. Generalization is useful for large scale spatial analyses or visualizations 
# where a lot of detail is unnecessary and may lower readability/clarity for the audience. Lastly, generalization helps to simplify 
# boundaries, reduce file sizes, and improve processing efficiency without sacrificing the overall representation of the data. 

##Question 4: How is simplifying line data different from smoothing the line data?
# Simplifying line data involves reducing the complexity of the geometry by removing redundant 
# vertices or simplifying the shape while maintaining the overall topology. To simplify line data you establish tolerance,
# connect endpoints, and retain points that fall farthest from the line. On the other hand, smoothing the line data reduce 
# jaggedness or irregularities in the line to make it more visually appealing. It is done by enhancing properties of the line 
# without necessarily reducing the number of vertices.

##Question 5: When we are aggregating point to polygons in what situation do you want to use a concave approach? 
## A convex approach?
# You want to use a concave approach when the point data is irregular or clustered. This approach ensures that 
# the internal structure of the polygon is preserved and provides a more accurate representation of point distribution.
# On the other hand, a convex approach is used when you want the spatial distribution of points to be more uniformly 
# spread or you want simplicity, rather than capturing the intricate spatial patterns. 




