### if you need to install anything
# install.packages("", repo = "https://cloud.r-project.org/")

### These are all the packages we must call up
library(ggplot2) # for plotting
library(ggpubr) # for showing multiple plots simultaneously
library(gganimate) # for animating our figures
library(scales) # for changing numbers from 6e6 to 6,000,000
library(rgdal) # for loading in spatial data
library(dplyr) # for working with data

setwd("G:/My Drive/Desktop Laptop Shared Files/ggplot2 seminar/data_files/raw_data")
acs_data = read.csv("acs_us_states.csv")

head(acs_data)

tail(acs_data)

summary(acs_data)

### the ACS data are already in a good structure
### so we don't have to do anything fancy to it like we did with the COVID data
acs_data = (acs_data %>% 
            mutate(# finding percent minority
                   percent_minority = 100 * (pop_total - pop_white) / pop_total,
                   # dummy variable indicating if state is in top half of minority pop
                   top_half_minority = as.integer(median(percent_minority) < percent_minority),
                   # finding percent of population with at least a bachelor's degree
                   percent_bachelors = 100 * (edu_bachelors + edu_masters + edu_profschool + edu_phd) / edu_total,
                   # dummy variable indicating if state is in top half of edu pop
                   top_half_edu = as.integer(median(percent_bachelors) < percent_bachelors),
                   # finding percent of population without healthcare
                   percent_healthcare = 100 * (pop_total - pop_wo_healthcare) / pop_total,
                   # dummy variable indicating if state is in top half of healthcare
                   top_half_healthcare = as.integer(median(percent_healthcare) < percent_healthcare))
            )

# check to see if it worked properly
head(acs_data)

### loading in the COVID dataset
### setwd changes the default folder that R will use to look for files
### alternatively, you can you just put the entire destination + file name into read.csv()
setwd("G:/My Drive/Desktop Laptop Shared Files/ggplot2 seminar/data_files/raw_data")
covid_data = read.csv("covid_us_states.csv")

### what does the top few rows of the data look like?
head(covid_data)

### what does the bottom few rows of the data look like?
tail(covid_data)

### quick summary statistics of the data
summary(covid_data)

### before working with all the data,
### lets add a few columns that will make the df easier to work with
# but first lets convert the date column which is of class factor to a string
covid_data$date = as.character(covid_data$date) # as.character() converts the input into a string

# extracting the year
covid_data$year = as.integer(substr(covid_data$date, 1, 4)) # as.integer() converts the input into an integer
                                                            # substr() extracts the characters between
                                                            # the beginning and ending position index

# extracting the month
covid_data$month = as.integer(substr(covid_data$date, 6, 7))

head(covid_data)

### now with year and month, we can summarize the data properly
### because the number of cases and deaths in this data are cumulative,
### let's find the max case and deaths within each month
### we can use the last day of the month to find the number,
### but max() streamlines the process
covid_state_month = (covid_data %>%
                     group_by(state, year, month) %>%
                     summarize(fips = fips[1], # keeps the fips code for merging later
                               months_after_jan20 = (((year == 2021)*12) + month)[1],
                               cases_total = max(cases),
                               deaths_total = max(deaths))
                    )

# a quick check to see what we just produced
head(covid_state_month)

### lets remove the rows/states from the COVID dataset
### that aren't in the ACS data
### this should take out American Samoa, Virgin Islands, etc
covid_state_month = covid_state_month[which(covid_state_month$state %in% unique(acs_data$state)), ]

### because there are multiple states which do not appear in the first few months of 2020
### we will append the missing months with values of 0 for cases and deaths
### unfortunately, I am lazy and did this in a for loop rather than something more elegant
states_list = unique(covid_state_month$state)
for(i in 1:length(states_list)) {
    state_of_interest = states_list[i]
    temp_df = covid_state_month[which((covid_state_month$state == state_of_interest) &
                                      (covid_state_month$year == 2020)), ]
    first_month = min(temp_df$month)
    fips_code = temp_df$fips[1]
    
    if(first_month != 1) {
        ### finding number of missing months
        num_missing_months = first_month - 1
        
        ### creating vectors that will be used to define the
        ### new dataframe we are appending to the original
        state_name = rep(state_of_interest, num_missing_months)
        years = rep(2020, num_missing_months)
        months = 1:num_missing_months
        state_fips = rep(fips_code, num_missing_months)
        months_after = 1:num_missing_months
        cases = rep(0, num_missing_months)
        deaths = rep(0, num_missing_months)
        
        new_df = data.frame(state = state_name,
                            year = years,
                            month = months,
                            fips = state_fips,
                            months_after_jan20 = months_after,
                            cases_total = cases,
                            deaths_total = deaths)
        
        ### i did bind_rows because rbind did something weird
        ### since the dplyr created data frame covid_state_month
        ### isn't just of class data.frame, so it was returning
        ### something wonky and ugly
        covid_state_month = bind_rows(covid_state_month, new_df)
    }
}

### our appended rows will appear at the bottom of the new data frame
### check it to see that they look correct
### you can sort them later
tail(covid_state_month)

### rather than cases and deaths by month, we will look at the
### most recent total counts for COVID for each state
covid_state_total = (covid_state_month %>%
                     group_by(state) %>%
                     summarize(state_fips = fips[1],
                               cases = max(cases_total),
                               deaths = max(deaths_total))
                     )

head(covid_state_total)

### while we're at it lets look at numbers aggregated to the entire U.S.
covid_usa_summarized = (covid_state_month %>%
                        group_by(year, month) %>%
                        summarize(months_after_jan20 = (((year == 2021)*12) + month)[1],
                                  cases_total_nation = sum(cases_total),
                                  deaths_total_nation = sum(deaths_total)))

covid_usa_summarized

### we will write this into a new csv for use later
setwd("G:/My Drive/Desktop Laptop Shared Files/ggplot2 seminar/data_files/processed_data")
write.csv(covid_usa_summarized, "covid_usa.csv", row.names = F)

### here is where we combine all the ACS and cumulative covid data
### we assign the covid data frame to the x input for the merge() function
### by.x is stating that we want to use the "state_fips" column to match with the acs data
### all.x is stating that we want to maintain all the rows in the covid data
### if there is a row that does not match with the other data frame,
### the missing values will appear as NA
### we are merging with the ACS data which we assign to y
### we remove columns 'state_acronym' and 'state' from the dataset so it's not
### duplicated in the merged dataset
### if a row in the ACS data cannot be matched with one from the COVID data,
### we do not include that row in the merged dataset by saying 'all.y = F'
covid_state = merge(x = covid_state_total,
                    by.x = "state_fips", all.x = T,
                    y = acs_data[, -which(names(acs_data) %in% c("state_acronym", "state"))],
                    by.y = "state_fips", all.y = F)

### this is to normalize the data into something more comparable
### each state has different populations,
### so comparing the number of cases and deaths is not very useful
### therefore, cases and deaths are normalized to a per 100,000 basis
covid_state$cases_per_100000 = 100000 * (covid_state$cases / covid_state$pop_total)
covid_state$deaths_per_100000 = 100000 * (covid_state$deaths / covid_state$pop_total)

head(covid_state)

### writing the data into a new csv
setwd("G:/My Drive/Desktop Laptop Shared Files/ggplot2 seminar/data_files/processed_data")
write.csv(covid_state, "covid_bystate.csv", row.names = F)

### we can do the exact same that we did for above with the monthly counts
covid_state_test = merge(x = covid_state_month,
                         by.x = "fips", all.x = T,
                         y = acs_data[, -which(names(acs_data) %in% c("state"))],
                         by.y = "state_fips", all.y = F)

covid_state_test$cases_per_100000 = 100000 * (covid_state_test$cases / covid_state_test$pop_total)
covid_state_test$deaths_per_100000 = 100000 * (covid_state_test$deaths / covid_state_test$pop_total)

covid_state_test = covid_state_test[with(covid_state_test, order(state, months_after_jan20)), ]
head(covid_state_test)
names(covid_state_test)

important_columns = c("fips", "state", "year", "months_after_jan20", "cases_total", "deaths_total",
                      "pop_total", "cases_per_100000", "deaths_per_100000")
covid_state_test = covid_state_test[, which(names(covid_state_test) %in% important_columns)]

setwd("G:/My Drive/Desktop Laptop Shared Files/ggplot2 seminar/data_files/processed_data")
write.csv(covid_state_test, "covid_bystate_bymonth.csv", row.names = F)

setwd("G:/My Drive/Desktop Laptop Shared Files/ggplot2 seminar/data_files/processed_data")
covid_bystate = read.csv("covid_bystate.csv")
covid_bystate_bymonth = read.csv("covid_bystate_bymonth.csv")
covid_usa = read.csv("covid_usa.csv")

### cases vs percent of state population with at least a bachelor's degree
scatter_edu = 
ggplot() +
geom_point(data = covid_bystate, aes(x = percent_bachelors, y = cases_per_100000, color = factor(top_half_edu))) +
labs(title = "Comparing State COVID Cases with Education Level",
     y = "COVID-19 Cases per 100,000",
     x = "Percent of Population with at least a Bachelors Degree",
     color = "Education Rank") +
scale_color_manual(labels = c("Bottom Half", "Top Half"), values = c("0" = "black", "1" = "red")) +
theme_classic()

scatter_edu

cases_over_time = 
ggplot(data = covid_usa) +
geom_line(aes(x = months_after_jan20, y = cases_total_nation, color = "Cumulative Cases")) + 
scale_color_manual(name = "", values = c("Cumulative Cases" = "blue")) +
theme_classic() +
theme(legend.position = "top") +
scale_y_continuous(labels = scales::comma,
                   "Total Cases") +
scale_x_continuous("Month",
                   breaks = c(1, 7, 13, 19),
                   labels = c("Jan 20", "Jul 20", "Jan 21", "Jul 21")) + 
ggtitle("Cumulative COVID\nCases and Deaths")
                   

cases_over_time

box_edu = 
ggplot() +
geom_boxplot(data = covid_bystate, aes(y = cases_per_100000, x = factor(top_half_edu))) +
labs(title = "Distribution of COVID Cases\nComparing by Education Level",
     x = NULL,
     y = "COVID-19 Cases per 100,000") +
scale_x_discrete(labels = c("Fewer Bachelor\'s\nDegrees", "More Bachelor\'s\nDegrees")) +
geom_jitter(data = covid_bystate, aes(y = cases_per_100000, x = factor(top_half_edu)),
            position = position_jitter(width = 0.2)) +
theme_bw()

box_edu

### the data summarization for showing the first way
### in this way, we are defining the height of the bars
names(covid_bystate)
covid_bystate_summarized = (covid_bystate %>%
                            group_by(top_half_edu) %>%
                            summarize(average_cases_per_100000 = mean(cases_per_100000),
                                      sd_cases_per_100000 = sd(cases_per_100000)))

covid_bystate_summarized

bar_edu = 
ggplot() +
geom_col(data = covid_bystate_summarized,
         aes(x = factor(top_half_edu), y = average_cases_per_100000)) +
geom_errorbar(data =  covid_bystate_summarized,
              aes(x = factor(top_half_edu),
                  ymin = average_cases_per_100000 - (2*sd_cases_per_100000),
                  ymax = average_cases_per_100000 + (2*sd_cases_per_100000)),
              width = 0.2) +
scale_x_discrete(labels = c("Fewer Bachelor\'s\nDegrees", "More Bachelor\'s\nDegrees")) + 
labs(title = "Average Cases per 100,000 people",
     subtitle = "Comparing states by education",
     x = NULL,
     y = "COVID-19 Cases per 100,000 people") +
theme_classic()

bar_edu

### equivalent to the top
ggplot() +
geom_bar(data = covid_bystate_summarized,
         aes(x = factor(top_half_edu), y = average_cases_per_100000),
         stat = "identity") +
geom_errorbar(data =  covid_bystate_summarized,
              aes(x = factor(top_half_edu),
                  ymin = average_cases_per_100000 - (2*sd_cases_per_100000),
                  ymax = average_cases_per_100000 + (2*sd_cases_per_100000)),
              width = 0.2) +
scale_x_discrete(labels = c("Fewer Bachelor\'s\nDegrees", "More Bachelor\'s\nDegrees")) + 
labs(title = "Average Cases per 100,000 people",
     subtitle = "Comparing states by education",
     x = NULL,
     y = "COVID-19 Cases per 100,000 people") +
theme_classic()

histogram_plot = 
ggplot() +
geom_histogram(data = covid_bystate,
               aes(x = cases_per_100000),
               binwidth = 2500) + # changing binwidth is key
labs(title = "Frequency of COVID rates per 100,000 people",
     subtitle = "Count of states",
     x = "Cases per 100,000 people",
     y = "Count") +
theme_classic()

histogram_plot

density_plot = 
ggplot() +
geom_density(data = covid_bystate, aes(cases_per_100000), bw = 1000) +
labs(title = "Distribution of COVID rates per 100,000 people",
     x = "Cases per 100,000 people",
     y = "Density") +
theme_classic()

density_plot

density_edu = 
ggplot() +
geom_density(data = covid_bystate, aes(cases_per_100000, color = factor(top_half_edu)), bw = 1000) +
labs(title = "Distribution of COVID\nrates per 100,000 people",
     x = "Cases per 100,000 people",
     y = "Density") +
theme_classic() +
scale_colour_manual(values = c("0" = "green", "1" = "blue"),
                    labels = c("Fewer", "More")) +
labs(color = "Share of Population\nwith a Bachelor's Degree  ") +
theme(legend.position = "top",
      legend.text=element_text(size=8),
      legend.title=element_text(size=8))

density_edu

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
tail(states_spdf@data)

ggplot() + 
geom_polygon(data = states_spdf,
             aes(x = long, y = lat, group = group),
             fill='white', color = "black") + 
coord_fixed(1.25)

### fortify is a function in ggplot2 which transforms the information in the
### SpatialPolygonsDataFrame object into a simple data frame
states_fortified = fortify(states_spdf)
head(states_fortified)

### notice how this new data frame does not contain much information
### it only contains information on how to draw the polygons
### we will now merge the data from 'states_spdf' with the new data frame

ggplot() + 
geom_polygon(data = states_fortified, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
coord_fixed(1.25)

### merging of data
### here we use the reference column "id" which we created earlier
### to match the correct rows
states_map = merge(x = states_fortified, by.x = "id",
                   y = states_spdf@data, by.y = "id")

head(states_map)

### we can repeat the steps earlier with our new object to map out the USA
### the only piece of code that needs to change is the data which we are feeding it
ggplot(data = states_map) + 
geom_polygon(aes(x = long, y = lat, group = group), fill='white', color = "black") + 
coord_fixed(1.25)

### we can repeat the steps earlier with our new object to map out the USA
### the only piece of code that needs to change is the data which we are feeding it
map_covid = 
ggplot() + 
geom_polygon(data = states_map,
             aes(x = long, y = lat, group = group, fill = cases_per_100000),
             color = "black") + 
coord_fixed(1.25) +
theme_void() +
scale_fill_distiller(palette = "Spectral") +
labs(title = "COVID Cases (normalized)\nin the United States",
     fill = "Cases\nper\n100,000")


map_covid

names(states_map)

states_map$percent_bachelors[which(states_map$percent_bachelors > 50)] = 44

map_edu = (ggplot() + 
           geom_polygon(data = states_map,
                        aes(x = long, y = lat, group = group, fill = percent_bachelors),
                        color = "black") + 
           coord_fixed(1.25) +
           theme_void() +
           scale_fill_distiller(palette = "Spectral", trans = "reverse") +
           labs(title = "Percent of Population\nwith at least a Bachelor's Degree",
                fill = "Percent")
           )

map_edu

ggarrange(box_edu, map_edu, cases_over_time, map_covid, ncol = 2, nrow = 2)

states_fortified_temp = merge(states_fortified,
                              states_spdf@data[, c("id", "STATE_FIPS")],
                              by = "id")

states_map_bymonth = merge(x = covid_bystate_bymonth, by.x = "fips",
                           y = states_fortified_temp, by.y = "STATE_FIPS")

states_map_bymonth = states_map_bymonth[with(states_map_bymonth, order(months_after_jan20, id, order)), ]

setwd("G:/My Drive/Desktop Laptop Shared Files/ggplot2 seminar/data_files/processed_data")
write.csv(states_map_bymonth, "states_map_bymonth.csv", row.names = F)

setwd("G:/My Drive/Desktop Laptop Shared Files/ggplot2 seminar/data_files/processed_data")
states_map_bymonth = read.csv("states_map_bymonth.csv")

map_covid_bymonth = 
ggplot() + 
geom_polygon(data = states_map_bymonth, # we had to change the data source
          aes(x = long, y = lat, group = group, fill = cases_per_100000),
          color = "black") + 
coord_fixed(1.25) +
theme_void() +
scale_fill_distiller(palette = "Spectral") +
labs(title = "COVID Cases (normalized)\nin the United States",
     fill = "Cases\nper\n100,000") +
transition_states(months_after_jan20) # our few lines of new code! it's all we need

setwd("C:/Users/Jason/Desktop")

### this is how we animate it, it basically takes a bunch of plots and puts them together into a gif
### we must have the package 'gifski' to
animate(map_covid_bymonth, end_pause = 20, width = 600, height = 600, renderer = gifski_renderer())
anim_save("the_gif.gif")
