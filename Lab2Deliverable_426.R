##################################################
# Name: Avery Swickard
# Date: 2/8/2024
# Lab 2 Code Deliverable 
##################################################

# Loading Lab CSV into R Script
data<-read.csv("D:/Classes/Geo425/users/swickar3/Lab 2 Data.csv")

### Question 1: Three measures of centrality for the poverty rate field in the data.
mean_value <- mean(data$poverty, na.rm = TRUE)
median_value <- median(data$poverty, na.rm = TRUE)
variance_value <- var(data$poverty, na.rm = TRUE)

#Now, when you run the central tendencies, it will print nicely.
cat("Poverty Mean:",mean(data$poverty, na.rm = TRUE), "\n")
cat("Poverty Median:",median(data$poverty, na.rm = TRUE), "\n")
cat("Poverty Variance Value:",var(data$poverty, na.rm = TRUE), "\n")

### Question 2: Construct a boxplot of poverty rate with a title and the x-axis labeled. 
boxplot(data$poverty, main="Boxplot of Poverty Rate", xlab="Poverty")

###Question 3: Is the distribution of poverty rate skewed? How so? 

#In order to see if the poverty rate is skewed, I put the data into a histogram. 
hist(data$poverty, main="Histogram of Poverty Rate", xlab="Poverty Rate")

#After looking at this histogram, the poverty rate data is skewed as shown by the data distribution on the graph.
# Also, the box plot from Question 2 indicates that the poverty rate data has outliers. 

#In order to see the variability of the data more clearly, the code below will add more bins to the histogram.
#With a higher number of bins, you can see more variability in the distribution and it shows 
# more clearly where and how the data is skewed.

num_bins <- ceiling(sqrt(length(data$poverty)))
breaks <- seq(from = min(data$poverty, na.rm = TRUE), to = max(data$poverty, na.rm = TRUE), length.out = num_bins + 1)
hist(data$poverty, breaks = breaks, main = "Histogram of Poverty Rate", xlab = "Poverty Rate", ylab = "Frequency")

# Now you can see the density of the data points and at what exact values it is skewed. 
# Therefore, interpretation is easier to visualize. 

### Question 4: Examples of the three spatial dimensions (Point,Line,and Polygon). 

## First spatial dimension: Point Representation
points <- st_as_sf(data.frame(id = 1:4, x = c(-92.63, -92.64, -92.65, -92.66), y = c(29.30, 29.31, 29.32, 29.33)), coords = c("x", "y"), crs = 4326)

## Second spatial dimension: Line Representation
line_data <- data.frame(id = c(1, 1, 1, 1), x = c(-92.63, -92.64, -92.65, -92.66), y = c(29.30, 29.31, 29.32, 29.33))
line_sf <- st_as_sf(line_data, coords = c("x", "y"), crs = 4326)
line_sf_line <- line_sf %>% group_by(id) %>% summarise(geometry = st_combine(geometry)) %>% st_cast("LINESTRING")

## Third spatial dimension:Polygon Representation
# Question 5: I added three more points to create a seven point polygon
coords <- matrix(c(-92.63, 29.30, -92.64, 29.30, -92.65, 29.31, -92.65, 29.32, -92.64, 29.33, -92.62,29.33,-92.62, 29.32, -92.63, 29.30), byrow = TRUE, ncol = 2)
poly <- st_polygon(list(coords))
polygon_sf <- st_sf(geometry = st_sfc(poly), crs = 4326)


# Plotting points, lines, and polygons to visualize spatial features.
plot(st_geometry(points), col = 'black', pch = 19)
plot(st_geometry(line_sf_line), col = 'red')
plot(polygon_sf, col = 'green')


##This is the completion of my Lab 2 R Script Deliverable. 