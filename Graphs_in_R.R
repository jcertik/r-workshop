##### Packages ######

# You only have to install the packages once
install.packages("dplyr")
install.packages("ggplot2")
install.packages("Hmisc")
intsall.packages("gifski")
install.packages("gganimate")
install.packages("plotly")
install.packages("htmlwidgets")
install.packages("rgl")
install.packages("igraph")
install.packages("network3D")
install.packages("leaflet")
install.packages("shiny")
install.packages("shinydashboard")
install.packages("shinydashboardPlus")
install.packages("leafpop")


# You have to load the packages every time you open RStudio
library(dplyr)
library(ggplot2)
library(Hmisc)
library(gganimate)
library(gifski)
library(plotly)
library(htmlwidgets)
library(rgl)
library(igraph)
library(networkD3)
library(leaflet)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(leafpop)


### Setting a Working Directory

setwd("C:/Users/jcertik/Downloads/Studies/Software Workshop/R/Rworkshopmarch")



##### Basic Graphs #####


# Copy in your file path
footballCakeData <- read.csv("C:/Users/jcertik/Downloads/Studies/Software Workshop/R/Rworkshopmarch/Football Cake Data Year.csv") 

footballCakeData$Country <- as.factor(footballCakeData$Country)


# line graph showing the frequency to which other countries searched for "football"
lineGraph <- ggplot(footballCakeData, aes(x = Year, y = Football, color = Country)) +
  geom_line()
lineGraph

# line graph showing the average frequency
lineGraph <- ggplot(footballCakeData, aes(x = Year, y = Football)) +
  stat_summary(fun.data = mean_sdl, geom = "line")
lineGraph

# adding more details and personalise the graph
lineGraph <- ggplot(footballCakeData) +
  stat_summary(aes(x = Year, y = Football, color = "forestgreen"), fun = mean, geom = "line", linewidth = 1.5) +
  stat_summary(aes(x = Year, y = Cake, color = "chocolate1"), fun = mean, geom = "line", linewidth = 1.5) +
  theme_classic() +
  labs(title = "Frequency of football and cake searches on Google",
       x = "Year",
       y = "Frequency") +
  theme(
    plot.title = element_text(size=14, face="bold", hjust = 0.5),
    axis.title.x = element_text(size=12, face = "bold"),
    axis.title.y = element_text(size=12, face = "bold")) +
  scale_x_continuous(breaks = c(2004, 2008, 2012, 2016, 2020, 2023)) +
  scale_color_identity(name = "Word",
                       breaks = c("forestgreen", "chocolate1"),
                       labels = c("Football", "Cake"),
                       guide = "legend")
lineGraph

# save the graph
ggsave("Football Cake Graph.jpg", width = 1800, height = 1800, units = "px")



##### Animated Graphs #####


lineAnimated <- lineGraph + transition_reveal(Year)

#Try this first
lineAnimated

#If you get the error message "file_renderer failed to copy frames to the destination directory"
animate(lineAnimated, renderer = gifski_renderer())
1
# Save as .gif
anim_save("Football Cake Reveal.gif")

# Create a graph for a single state or time point
footballBar <- ggplot(footballCakeData, aes(x = Country, y = Football, fill = Country)) + 
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip() +
  # Animate by plotting multiple states and time points
  labs(title = 'Year: {frame_time}', x = 'Country', y = '"Football" Google Searches') +
  transition_time(Year)

# Try this first
footballBar

#If the gif doesn't render use this instead
animate(footballBar, renderer = gifski_renderer())

# Save at gif:
anim_save("Football by Year.gif")


### Create the same graph for "Cake" searches

cakeBar <- ggplot(footballCakeData, aes(x = Country, y = Cake, fill = Country)) + 
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(legend.position = "none") +
  coord_flip() +
  labs(title = 'Year: {frame_time}', x = '"Cake" Google Searches', y = 'Country') +
  transition_time(Year)

# Try this first
cakeBar

#If the gif doesn't render use this instead
animate(cakeBar, renderer = gifski_renderer())

# Save gif
anim_save("Cake by Year.gif")



###### Interactive Graphs ######


### Make the line graph interactive

interactiveLine <- ggplotly(lineGraph)
interactiveLine

saveWidget(widget = interactiveLine, file = "Interactive Line.html")

#amended script
library(plotly)
interactiveLine <- ggplotly(lineGraph)
htmlwidgets::saveWidget(interactiveLine, "interactiveLine.html", selfcontained = TRUE)
browseURL("interactiveLine.html")

# Create an average bar graph that is interactive
footballAverageBar <- ggplot(footballCakeData) + 
  stat_summary(aes(x = Country, y = Football, fill = Country), fun = mean, geom = "bar") +
  theme_bw() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = 'Country', y = '"Football" Google Searches')

ggplotly(footballAverageBar)

#amended script
library(plotly)
footballAverageBar <- ggplotly(footballAverageBar)
htmlwidgets::saveWidget(footballAverageBar, "footballAverageBar.html", selfcontained = TRUE)
browseURL("footballAverageBar.html")

##### Have a go creating the interactive bar graph for the average "Cake" searches by country

cakeAverageBar <- ggplot(footballCakeData) + 
  stat_summary(aes(x = Country, y = Cake, fill = Country), fun = mean, geom = "bar") +
  theme_bw() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = '"Cake" Google Searches', y = 'Country')

ggplotly(cakeAverageBar)

#amended script
library(plotly)
cakeAverageBar <- ggplotly(cakeAverageBar)
htmlwidgets::saveWidget(cakeAverageBar, "cakeAverageBar.html", selfcontained = TRUE)
browseURL("cakeAverageBar.html")

###### BREAK


##### 3D Graphs #####

# Copy in your file path
monopolyData <- read.csv("C:/Users/jcertik/Downloads/Studies/Software Workshop/R/Rworkshopmarch/Monopoly.csv")
monopolyData$Colour <- as.factor(monopolyData$Colour)

monopolyData$Fill <- "White"
monopolyData[monopolyData$Colour %in% c('Blue'),]$Fill <- "royalblue4"
monopolyData[monopolyData$Colour %in% c('Green'),]$Fill <- "springgreen4"
monopolyData[monopolyData$Colour %in% c('Light Blue'),]$Fill <- "lightskyblue"
monopolyData[monopolyData$Colour %in% c('Orange'),]$Fill <- "darkorange1"
monopolyData[monopolyData$Colour %in% c('Pink'),]$Fill <- "deeppink"
monopolyData[monopolyData$Colour %in% c('Brown'),]$Fill <- "chocolate4"
monopolyData[monopolyData$Colour %in% c('Red'),]$Fill <- "firebrick1"
monopolyData[monopolyData$Colour %in% c('Yellow'),]$Fill <- "yellow"
monopolyData[monopolyData$Colour %in% c('Train'),]$Fill <- "gray30"
monopolyData[monopolyData$Colour %in% c('Utilities'),]$Fill <- "gray80"

library(rgl)

monopolyGraph <- plot3d(
  x = monopolyData$Price, y = monopolyData$Max_Rent, z = monopolyData$Probability, 
  type = 's', col = monopolyData$Fill, size = 2,
  xlab = "Price", ylab = "Max Rent", zlab = "Probability Percentage"
)
monopolyGraph

library(htmlwidgets)

saveWidget(rglwidget(), file = "3D Monopoly.html", selfcontained = TRUE)


##### Networks #####

# Simple Network

# Copy in your file path
loveActuallyData <- read.csv("C:/Users/jcertik/Downloads/Studies/Software Workshop/R/Rworkshopmarch/Love Actually.csv")

library(networkD3)

network <- simpleNetwork(loveActuallyData, height="1000px", width="1000px", zoom = TRUE, linkColour = loveActuallyData$Colour)
network

saveNetwork(network, file = "Love Actually Network.html")


##### Interactive Maps #####

install.packages("networkD3")
library(leaflet)
library(dplyr)

# Basic Map

outsideData <- read.csv("C:/Users/jcertik/Downloads/Studies/Software Workshop/R/Rworkshopmarch/Outside data.csv")

leaflet(outsideData) %>% # using the package and the data
  addTiles() %>% #add the basic map tile
  addCircles(lng = ~Longitude, lat = ~Latitude, label = ~PopUp) %>% # add circles with a pop up label
  setView(-5,55, 5) # set the start position of the map

#amended script
library(htmlwidgets)

myMap <- leaflet(outsideData) %>%
  addTiles() %>%
  addCircles(lng = ~Longitude, lat = ~Latitude, label = ~PopUp) %>%
  setView(lng = -5, lat = 55, zoom = 5)

saveWidget(myMap, "outsideMap.html", selfcontained = TRUE)
browseURL("outsideMap.html")


# Add to the map
pal <- colorFactor(c( "lightgreen", "darkgreen", "yellow", "orange",  "lightblue", "darkblue"), domain = c("mountain", "mountainTD", "paddle", "paddleTD","swim", "swimTD"))

myMap <- leaflet(outsideData) %>% 
  #add a different map tile
  addProviderTiles("Esri.WorldImagery") %>% 
  # change the colours of the circles and edit the labels
  addCircles(lng = ~Longitude, lat = ~Latitude, fillColor = ~pal(Type), radius = 10000, color = "black", fillOpacity = 0.6, label = ~PopUp, weight = 1.5, labelOptions = labelOptions(textsize = "14px")) %>%  
  addScaleBar(
    position = "bottomright",
    # add a scale bar
    options = scaleBarOptions(imperial = FALSE)) %>% 
  setView(-5,55, 5) 

saveWidget(myMap, "outsideMap.html", selfcontained = TRUE)
browseURL("outsideMap.html")

