####################################
## Name: Avery Swickard ###
## Date: 3/8/2024 ###
## Lab 6 Deliverable ###


##Question 1: Please create a proportional symbol map that looks different than the one supplied in the code.
# I used the ggplot package to create the proportional symbol map

ggplot() +
  geom_sf(data = abq, fill = "ivory", color = "black") +  # Base map with ivory fill and black borders
  geom_point(data = fb_shp_coords, aes(x = X, y = Y, size = checkins), 
             shape = 21, color = "purple4", fill = "plum1", alpha = 0.5, stroke = 1.3) +  #Circles with shape outline, fill, and transparency
  scale_size_continuous(range = c(2, 13), 
                        breaks = c(800, 3000, 8000),  #I changed up the range argument and size of breaks
                        labels = c("800", "3k", "8k")) +
  guides(size = guide_legend(override.aes = list(fill = "plum1", color = "black", alpha = 0.5, stroke = 1.3))) + #I updated the legend to match what is on the map
  theme_minimal() +
  labs(size = "Amount of Check-ins", title = "Proportional Symbols Map of Facebook Check-ins in Albuquerque") + #Changed wording
  theme(legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 1))

#Saved as a JPEG      
jpeg("Q1Map.jpg")
ggplot() +
  geom_sf(data = abq, fill = "ivory", color = "black") +  # Base map with ivory fill and black borders
  geom_point(data = fb_shp_coords, aes(x = X, y = Y, size = checkins), 
             shape = 21, color = "purple4", fill = "plum1", alpha = 0.5, stroke = 1.3) +  #Circles with shape outline, fill, and transparency
  scale_size_continuous(range = c(2, 13), 
                        breaks = c(800, 3000, 8000),  #I changed up the range argument and size of breaks
                        labels = c("800", "3k", "8k")) +
  guides(size = guide_legend(override.aes = list(fill = "plum1", color = "black", alpha = 0.5, stroke = 1.3))) + #I updated the legend to match what is on the map
  theme_minimal() +
  labs(size = "Amount of Check-ins", title = "Proportional Symbols Map of Facebook Check-ins in Albuquerque") + #Changed wording
  theme(legend.position = "right", #The legend is positioned on the right 
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()  

##Question 2: Please create a dot density map that looks different than the one supplied in the code. 
# I used the ggplot package to create the dot density map

ggplot() +
  geom_sf(data = abq, fill = "seashell", color = "gray10") +
  geom_point(data = fb_shp_coords, aes(x = X, y = Y, size = checkins), shape = 22, color = "gray10", fill = "cornflowerblue", alpha = 0.8, stroke = 1.0) +
  scale_size_continuous(range = c(2, 8), # I changed the shape to a square in the line above 
                        breaks = c(1000, 5000, 7000),  # Adjusted breaks
                        labels = c("1k", "5k", "7k")) +  # Adjusted labels
  guides(size = guide_legend(override.aes = list(fill = "cornflowerblue", color = "gray10", alpha = 0.8, stroke = 1.0), # I thought cornflowerblue looked more like the FB icon. 
                             title = "Amount of Check-ins", 
                             nrow = 1, byrow = TRUE, direction = "vertical", #Changed the position of the legend
                             label.position = "top")) +
  theme_minimal() +
  labs(size = "Amount Check-ins", title = "Facebook Check-ins in Albuquerque") +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))

#Save as a JPEG
jpeg("Q2Map.jpg")
ggplot() +
  geom_sf(data = abq, fill = "seashell", color = "gray10") +
  geom_point(data = fb_shp_coords, aes(x = X, y = Y, size = checkins), shape = 22, color = "gray10", fill = "cornflowerblue", alpha = 0.8, stroke = 1.0) +
  scale_size_continuous(range = c(2, 8), # I changed the shape to a square in the line above 
                        breaks = c(1000, 5000, 7000),  # Adjusted breaks
                        labels = c("1k", "5k", "7k")) +  # Adjusted labels
  guides(size = guide_legend(override.aes = list(fill = "cornflowerblue", color = "gray10", alpha = 0.8, stroke = 1.0), # I thought cornflowerblue looked more like the FB icon. 
                             title = "Amount of Check-ins", 
                             nrow = 1, byrow = TRUE, direction = "vertical", #Changed the position of the legend
                             label.position = "top")) +
  theme_minimal() +
  labs(size = "Amount Check-ins", title = "Facebook Check-ins in Albuquerque") +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()  

##Question 3: Please create a choropleth map using the ‘mf_map’ function 
## - try to make this map as visually appealing and appropriate as possible

# Create a color palette
my_color_names<-c("cadetblue1", "cadetblue3", "cadetblue4", "darkslategray")
my_colors <- as.hexmode( c(256^(2:0) %*% col2rgb(my_color_names)) )
my_colors<-paste0("#", my_colors)
plot(1:length(my_color_names), rep(1, length(my_color_names)), 
     bg = my_colors, pch = 22, cex = 4)

# Create the choropleth map using mf_map
mf_map(x = abq,
       type = "choro", 
       var = "estimate_DP05_0066",  # Variable for mapping
       nbreaks = 4,  # Specify custom breaks
       pal = my_colors,
       leg_pos = "topleft",
       leg_title = "Facebook Check-Ins in Albuquerque")

#Export to JPEG
jpeg("Q3Map.jpg")
mf_map(x = abq,
       type = "choro", 
       var = "estimate_DP05_0066",  # Variable for mapping
       nbreaks = 4,  # Specify custom breaks
       pal = my_colors, #Use color palette
       leg_pos = "topleft", #Position legend
       leg_title = "Facebook Check-Ins in Albuquerque") #Title
dev.off()

##Question 4: 4.	Please create a choropleth map using the ggplot2 library. Some code has been supplied to help start the process. 
## It will be your job to finish/augment the code to produce the choropleth map.

ggplot(data = abq) +
  geom_sf(aes(fill = estimate_DP05_0066), color = "white") +  
  scale_fill_viridis_c(option = "plasma", direction = -1, 
                       breaks = c(2000, 4000, 6000), 
                       labels = c("2k", "4k", "6k")) +  # Setting custom breaks and labels for the legend
  labs(title = "Amount of Facebook Check-Ins in Albuquerque", fill = "Check-In Amount") +  # Adding titles and labels 
  theme_minimal() +  # Using a minimal theme
  theme(legend.position = "bottom",    # Position the legend at the bottom
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        plot.title = element_text(hjust = 0.5))  # Center the title

#Save as a JPEG
jpeg("Q4Map.jpg")
ggplot(data = abq) +
  geom_sf(aes(fill = estimate_DP05_0066), color = "white") +  
  scale_fill_viridis_c(option = "plasma", direction = -1, 
                       breaks = c(2000, 4000, 6000), 
                       labels = c("2k", "4k", "6k")) +  # Setting custom breaks and labels for the legend
  labs(title = "Amount of Facebook Check-Ins in Albuquerque", fill = "Check-In Amount") +  # Adding titles and labels 
  theme_minimal() +  # Using a minimal theme
  theme(legend.position = "bottom",    # Position the legend at the bottom
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        plot.title = element_text(hjust = 0.5))  # Center the title
dev.off()  


###Additional Questions

##Question 1: Describe when you would use a choropleth map versus a proportional symbol map versus a dot density map.
# You would use a choropleth map if the data has enumeration units, can be standardized, and is uniformly distributed. 
# Also, if your data is in classes than a choropleth map is useful. You would use a proportional symbol map when you want
# to represent true point data. Also, if you have numerical/ordinal data and you want to shows the broader data in the 
# enumeration unit without showing precise location. This map is very versatile and great for comparative analysis. 
# Lastly, you would use a dot density map if you want to show the location of where the phenomena occurred. Each data 
# point has the same size and represents the same unit value, so it works well for showing data that isn't uniformly 
# distributed across an enumeration unit. 

##Question 2: In your own words, describe what the ggplot2 library does in R and why it might be more useful than 
## the 'mf_map' function in mapsf library. 
# The ggplot2 library you can create a wide variety of maps where you can modify color, shapes, labels, etc. to tailor 
# the map to your needs. There are many different aesthetics you can change about the map with the ggplot2 package in R. 
# It handles data manipulation and visualization and works well with other R packages. It is more useful than the 'mf_map' 
# function in the mapsf library because it is more flexible and the syntax is easier for users to learn. While the 'mf_map' 
# function is made specifically for thematic mapping, ggplot2 has more customization options to create a more specialized 
# map as well as different plots. Therefore, the ggplot2 package in R is more versatile and makes map customization easier. 






