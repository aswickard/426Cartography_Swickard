####################################
## Name: Avery Swickard ###
## Date: 2/15/2024 ###
## Lab 3 Deliverable ###

## Question 1: Create your own mixed geometry type with point, line, and polygon geometry and apply a buffer

# Create point, line, and polygon geometries
point <- st_point(c(0, 0))
line <- st_linestring(matrix(c(1, 1, 2, 2, 3, 3), ncol = 2, byrow = TRUE))
polygon <- st_polygon(list(matrix(c(0, 0, 0, 1, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)))

# Combine them into a single sf object
mixed_geom <- st_sfc(point, line, polygon)

# Apply a buffer and plot
buffered_geom <- st_buffer(mixed_geom, 0.3) #Used 0.3 so that the object is still distinguishable. 
plot(buffered_geom)


## Question 2: Create two polygons and perform an union that combines both of them 

# Make Two Separate Squares
square1 <- st_polygon(list(matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)))
square2 <- st_polygon(list(matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2, byrow = TRUE)))

# Create union to combine both and plot
union <- st_union(square1, square2)
plot(union)

##Question 3: Create a multi-layer raster object and produce a jpg output

#Create 3 raster layers 
raster1 = rast(nrows = 8, ncols = 6, # Specifies the raster's dimensions: 8 rows and 6 columns.
                  xmin = -1, xmax = 1, ymin = -1, ymax = 1,
                  vals = 1:48) # Assigns values from 1 to 36 to each cell in the raster.

raster2 = rast(nrows = 8, ncols = 6, # Specifies the raster's dimensions: 8 rows and 6 columns.
                   xmin = -1, xmax = 1, ymin = -1, ymax = 1, 
                   vals = 49:96) # Assigns values from 49 to 96 to each cell in the raster.

raster3 = rast(nrows = 8, ncols = 6, # Specifies the raster's dimensions: 8 rows and 6 columns.
                   xmin = -1, xmax = 1, ymin = -1, ymax = 1, 
                   vals = 97:144) # Assigns values from 97 to 144 to each cell in the raster.

#Use the 3 raster layers to make a multi-raster layer 
multi_layer_raster <- c(raster1, raster2, raster3)

# Print the multi-layer raster object
print(multi_layer_raster)

# Create the file name for the JPEG
output_file <- "multi_layer_raster.jpg"

# Plot the raster and save as JPEG
jpeg(output_file)
plotRGB(multi_layer_raster)
dev.off()

# Print message indicating successful creation of the JPEG file
cat("JPEG file created:", output_file, "\n")

##Question 4: Please write code to represent trees in a different way

#I represented tree density as proportional symbols and added a sequential color scheme so as the density of the trees rises
# the symbol will get darker. 

set.seed(1234)  
trees_data <- data.frame(
  id = 1:10,  # Tree identifiers
  x = runif(10, -80.90, -23.57),  # Random x-coordinates for trees 
  y = runif(10, -64.44, -45.45),  # Random y-coordinates for trees 
  density = c(21, 35, 44, 25, 11, 20, 30, 15, 18, 22) # Density values
)

#Plotting the Proportional Symbols of the Trees with a sequential color scheme 
ggplot(data = trees_data, aes(x = x, y = y, size = density, color = density)) +
  geom_point(alpha = 0.9) +
  scale_size_continuous(range = c(3, 10)) +
  scale_color_gradient(low = "lightgreen", high = "darkgreen") +
  labs(title = "Proportional Symbols of Tree Density")

##Question 5: Please provide a Choropleth map of a variable in the Marriage Dataset that uses a sequential color scheme.  

#Input both datasets
marriages <- read.csv("D:/Classes/Geo425/users/swickar3/Lab03_425/MI_Mariage_rates_2022.csv")
mi_counties <- st_read("D:/Classes/Geo425/users/swickar3/Lab03_425/mi_counties/mi_counties/mi_counties/Counties_(v17a).shp")

# Clean up county names
mi_counties$NAME <- gsub("\\.", "", mi_counties$NAME)

# Join the marriage data with the spatial data of Michigan counties
# and convert certain columns to numeric for analysis
mi_counties <- mi_counties %>% 
  left_join(marriages, by = c('NAME' = 'County')) %>% 
  mutate_at(c("Marriage.Number", "Marriage.Rate", "Divorce.Number", 
              "Divorce.Rate", "Population"), as.numeric)

#Create desired palette 
mp5 <- brewer.pal(5, "Blues")  # 5-class Blue palette

#Create Choropleth map on Divorce Rates using a sequential color scheme 
choroLayer(x = mi_counties, var = "Divorce.Rate",
           method = "quantile", nclass = 5,
           col = mp5,
           border = "grey40",
           legend.pos = "topright",
           legend.title.txt = "2022 Marriage \n Divorce Rate s: \nQuantile, 5 Colors")
           


###Additional Questions for Lab03

## Question 1: What is the difference between the sf library and the terra library 
#  Answer: The sf library handles vector data and the terra library handles raster data. 

## Question 2: There are some common spatial data manipulation functions 
## (GIS shapefile manipulation functions) in the sf library. What are they and what do they do?
# Answer: class(dataset) - shows the classes in the shapefile
#         names(dataset) - names the columns in the shapefile
#         plot(dataset) - Basic plot of the dataset
#         St_read & read_sf - converting dataset into different formats
#         St_union - merging different geometries into a single geometry
#         plot(dataset[3:6]), plot(dataset["column"]) - Plots the specific columns stated.

## Question 3: How would you create a new sf object representing a line or a polygon, given a set of coordinates? 
#  Answer:I would create a new sf object representing a line by using the function “st_linestring,” input the 
#  coordinates, specify two columns, and row-wise. Ex. st_linestring(matrix(c(1, 1, 2, 2, 3, 3), ncol = 2, byrow = TRUE))
#   I would create a new sf object representing a polygon by using the function “st_polygon,” listing the coordinates, 
#   specify two columns, and row-wise. Ex. polygon <- st_polygon(list(matrix(c(0, 0, 0, 1, 1, 1, 0, 1, 0, 0), ncol = 2, byrow = TRUE))). 

## Question 4: How do you filter spatial data based on a specific attribute, such as area or population? 
#  Answer: You want to filter the spatial data to only include a specific attribute by taking the dataset 
#  (ex. United States), then specify column, and the specific feature you want to display from the specified column. 
#   For example, if I want to filter the United States dataset to only show the area of Michigan it would look like 
#   Population = United States[UnitedStates$area == “Michigan”] This will select the rows where this condition is true 
#   to filter the spatial data.

## Question 5: Suppose you have a dataset of weather stations and temperature readings. How would you visually 
## represent areas of high and low temperatures on a map using R? 
#  Answer: I would visually represent the areas of high and low temperatures on a map using R by plotting the 
#  dataset of weather stations, then plot the temperature readings at each weather station, then use a gradient 
#  color scheme from blue to red to represent the temperature at each point. 





























