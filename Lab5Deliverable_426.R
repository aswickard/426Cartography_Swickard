####################################
## Name: Avery Swickard ###
## Date: 2/20/2024 ###
## Lab 5 Deliverable ###

##Question 1: I made a pretty bad choropleth map using a rose colored palette. 
## Correct the coloring of my map using any of the approaches identified in the lab 
## but keep the color as close as possible to the misty rose. Hint - enhance the coloring 
## so that there is more or less.  (Your code should include code for this export) saturation. 
## Export your map as a jpeg. 

#I changed the colors of misty rose and changed the first color to seashell to be more enhanced.
my_color_names<-c("seashell", "mistyrose2", "mistyrose3", "mistyrose4")
my_colors <- as.hexmode( c(256^(2:0) %*% col2rgb(my_color_names)) )
my_colors<-paste0("#", my_colors)
plot(1:length(my_color_names), rep(1, length(my_color_names)), 
     bg = my_colors, pch = 22, cex = 4)

#Plot new palette onto choropleth map
mf_map(mi_counties, 
       var = "Marriage.Number",
       type = "choro",
       pal = my_colors,
       nbreaks = length(my_colors)) 

#Export into a JPEG
jpeg("updated_map_Q1.jpg")
mf_map(mi_counties, 
       var = "Marriage.Number",
       type = "choro",
       pal = my_colors,
       nbreaks = length(my_colors)) 
dev.off()  

##Question 2: I created a 5 class ordinal map based on the total number of marriages by county. 
## Please create an ordinal map that uses 4 classes and a different set of colors. Make sure you 
## use the appropriate color scheme.

new_cols <- mf_get_pal(n =4, pal = "Blues 2", rev = TRUE) #I changed "n" to 4 and the color palette to blue.
mf_map(mi_counties, 
       var = "Marriage.Number",
       type = "choro",
       breaks = "quantile",
       pal = new_cols, #A sequential color scheme is appropriate for the ordinal map
       nbreaks = 4)


##Question 3: Reproject the Michigan Shapefile data to be more appropriate projection for Michigan

mi_counties <- st_read("D:/Classes/Geo425/users/swickar3/Lab 5-20240220T223418Z-001/Lab 5/mi_counties/Counties_(v17a).shp")

#NAD83 (ESPG Code: 4269) is more appropriate for a Michigan projection. 
mi_counties <- st_transform(mi_counties, 4269) 
plot(st_geometry(mi_counties), main = "Michigan with NAD83 Projection")


##Question 4:Filter to Lithuania. Compare a WGS 1984 projection to an equal area projection that is 
## appropriate for that area. Have a side-by-side comparison image in a jpeg and submit the jpeg. 
## (Your code should include code for this export).

world <- st_read("D:/Classes/Geo425/users/swickar3/Lab 5-20240220T223418Z-001/Lab 5/ne_110m_admin_0_countries")

#Filter to Lithuania
Lithuania<-world %>% filter(NAME == "Lithuania")
plot(st_geometry(Lithuania))

#Two different projections
Lithuania_wgs1984<-st_transform(Lithuania, 4326) #These four digits represent WGS 1984 projection. 

Lithuania_equalarea<-st_transform(Lithuania, 5070) #These four digits represent Equal Area Projection. 

#Plot in side-by-side comparison
par(mfrow = c(1, 2))
plot(st_geometry(Lithuania_wgs1984), main = "WGS 1984 Projection")
plot(st_geometry(Lithuania_equalarea), main = "Equal Area Projection")

#Plot in JPEG
jpeg("Lithuania_projection_comparison.jpg", width = 1000, height = 500)
par(mfrow = c(1, 2))
plot(st_geometry(Lithuania_wgs1984), main = "WGS 1984 Projection")
plot(st_geometry(Lithuania_equalarea), main = "Equal Area Projection")
dev.off()

##Question 5: Take the Michigan Shapefile and reproject it to be suited for Michigan 
## (your choice which type equal area, equal distance, etc.). Create a choropleth map for relative risk. 

#Load counties dataset
mi_counties <- st_read("D:/Classes/Geo425/users/swickar3/Lab 5-20240220T223418Z-001/Lab 5/mi_counties/Counties_(v17a).shp")
mi_counties$NAME <- gsub("\\.", "", mi_counties$NAME)

#Load marriage dataset
marriages <- read.csv("D:/Classes/Geo425/users/swickar3/Lab03_425/MI_Mariage_rates_2022.csv")

# Join the marriage data with the spatial data of Michigan counties
# and convert certain columns to numeric
mi_counties <- mi_counties %>% 
  left_join(marriages, by = c('NAME' = 'County')) %>% 
  mutate_at(c("Marriage.Number", "Marriage.Rate", "Divorce.Number", 
              "Divorce.Rate", "Population"), as.numeric)

# Creates relative Risk for marriage in each county 
state_rate = sum(mi_counties$Marriage.Number) / sum(mi_counties$Population) # Calculate state-wide average marriage rate
mi_counties$marriageExp <- state_rate * mi_counties$Population # Calculate expected marriage rate for each county based on state average
mi_counties$relRisk <- ifelse(mi_counties$marriageExp > 0, 100 * (mi_counties$Marriage.Number / mi_counties$marriageExp), 0)

# Summary of relative risk
summary(mi_counties$relRisk)

#Create Equal-Area Projection
mi_counties <-st_transform(mi_counties, 5070) 

# Plot choropleth map with the color brewer approach and custom breaks
mp7 <- brewer.pal(7, "BuPu")
breaks <- div_breaks(mi_counties$relRisk, 7, 150) # Calculate breaks for choropleth map

# Plot choropleth map of Relative Risk 
mf_map(mi_counties, 
       var = "relRisk",
       type = "choro",
       pal = mp7,
       breaks = breaks)



###Additional Questions

##Question 1:Describe the difference between hue, saturation, and lightness (HSL) and the use cases for 
## when to change the values of HSL for different cartography maps.

#Answer: Hue is the attribute of the color and it is just how we describe colors (Red, Green, Blue). Saturation is the 
# intensity of the color. If the color has high saturation, then it is in it's truest form and if the saturation is low
# it has more grey added to it. Lightness is the tone of the color. If the color is closer to white, than it is lighter.
# You would want to use hue when your map is qualitative (nominal or ordinal data). Therefore, if the map has categories
# then you would use hue. If your map is qualitative (interval or ratio data), you want to use saturation or lightness.
# Saturation is useful when you want to differentiate between features and create a visual hierarchy. Lightness is useful
# when representing spatial patterns and indicating intensity/severity. 


##Question 2: What are the RGB values of Red, Blue, and Green?

#Answer: The RGB value for Red is (Red: 255, Green: 0, Blue: 0). The RBG value for blue is (Red: 0, Green: 0, Blue: 255).
# The RBG value for green is (Red: 0, Green: 255, Blue: 0)


#Question 3: What are some pros and cons of EPSG and ESRI based approaches to projections?

#Answer: Some pros of the EPSG projections are that it is used by many by many GIS packages and libraries, making it widely adopted. 
# Also, EPSG codes cover a lot of projections for a wide range of geographic regions. Some cons of the EPSG projection is selecting
# the appropriate code, which can be difficult for beginners. Also, the codes do not fully encompass all projections, so some 
# specialized projections are left out. On the other hand, some pros of the ESRI projections are that most of the GIS industry
# uses ESRI software, making the projection integrate smoothly. Also, users can create custom projections, which is not available in EPSG codes. 
# Some cons include that ESRI projections may not be compatible in other GIS software. Also, it can be difficult for beginners to 
# learn ESRI projections due to the complexity of it's features. 

##Question 4: What is an EPSG value appropriate for the state of Michigan?

#Answer: An ESPG value appropriate for the state of Michigan is 4269 (North American Datum 1983).  






