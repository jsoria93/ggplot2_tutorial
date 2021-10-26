### if you need to install anything
# install.packages("", repo = "https://cloud.r-project.org/")

### These are all the packages we must call up
library(ggplot2) # for plotting
library(ggpubr) # for showing multiple plots simultaneously
library(gganimate) # for animating our figures
library(scales) # for changing numbers from 6e6 to 6,000,000
library(rgdal) # for loading in spatial data
library(dplyr) # for working with data

setwd("G:/My Drive/Desktop Laptop Shared Files/ggplot2 seminar/data_files/processed_data")
covid_bystate = read.csv("covid_bystate.csv")
covid_bystate_bymonth = read.csv("covid_bystate_bymonth.csv")
covid_usa = read.csv("covid_usa.csv")

### cases vs percent of state population with at least a bachelor's degree
scatter_edu = 



cases_over_time = 


box_edu = 


### the data summarization for showing the first way
### in this way, we are defining the height of the bars
names(covid_bystate)
covid_bystate_summarized = 

covid_bystate_summarized

bar_edu = 


bar_edu

### equivalent to the top


histogram_plot = 


density_plot = 


density_edu = 


ggarrange(box_edu, bar_edu,
          density_edu, cases_over_time,
          ncol = 2, nrow = 2)

### set working directory to the folder containing shapefile and other supporting files
setwd("G:/My Drive/Desktop Laptop Shared Files/ggplot2 seminar/data_files/states_shp")
# dsn is for the "destination" folder
# by putting "." we are saying we want to look at the current working directory
states_spdf = readOGR(dsn = ".", layer = "states")

class(states_spdf)

### see the slots in states_spdf
slotNames(states_spdf)

### accessing the slots
states_spdf_data = states_spdf@data
head(states_spdf_data)

### while we're at it, let's add those row.names as a column to be used later
### this is how we match the correct columns when we use the merge() function
states_spdf@data$id = row.names(states_spdf@data)
# head(states_spdf@data)

states_spdf@data = merge(x = states_spdf@data, by.x = "STATE_FIPS", all.x = T,
                         y = covid_bystate, by.y = "state_fips", all.y = F)

names(states_spdf@data)
head(states_spdf@data)

simple_map = 


### fortify is a function in ggplot2 which transforms the information in the
### SpatialPolygonsDataFrame object into a simple data frame
states_fortified = 

### notice how this new data frame does not contain much information
### it only contains information on how to draw the polygons
### we will now merge the data from 'states_spdf' with the new data frame

fortified_map = 


### merging of data
### here we use the reference column "id" which we created earlier
### to match the correct rows
states_map = merge(...)

head(states_map)

### we can repeat the steps earlier with our new object to map out the USA
### the only piece of code that needs to change is the data which we are feeding it
ggplot(data = states_map) + 
geom_polygon(aes(x = long, y = lat, group = group), fill='white', color = "black") + 
coord_fixed(1.25)

### we can repeat the steps earlier with our new object to map out the USA
### the only piece of code that needs to change is the data which we are feeding it
map_covid = 


names(states_map)

### taking care of the outlier
states_map$percent_bachelors[which(states_map$percent_bachelors > 50)] = 44

map_edu = 

ggarrange(box_edu, map_edu, cases_over_time, map_covid, ncol = 2, nrow = 2)

setwd("G:/My Drive/Desktop Laptop Shared Files/ggplot2 seminar/data_files/processed_data")
states_map_bymonth = read.csv("states_map_bymonth.csv")

map_covid_bymonth = 


setwd("C:/Users/Jason/Desktop")

### this is how we animate it, it basically takes a bunch of plots and puts them together into a gif
### we must have the package 'gifski' to

