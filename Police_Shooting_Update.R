library(RCurl)
library(lubridate)
library(ggmap)



# Read the .csv data file from the Washington Post
# github repository, converting blank
# cells(unavailable or not recorded data) to NA

dataPoliceShooting <-read.csv(text=getURL("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv"), header=T, na.strings = c("", " "))



# Read the .csv data file from my GitHub
# repository, that has data saved from
# prior analysis, from Jan 2015 to July 2016

dataPoliceShootingSaved <-read.csv(text = getURL("https://raw.githubusercontent.com/arvindbetrabet/Police_Shooting_Analysis/master/data/fatal-police-shootings-data-2.csv"), header = T)


# The id column in the Washington Post repository
# is unique and incremental, although the data
# on the shooting has been updated with information
# about current and past incidents, the id has been
# incremented accordingly. Eventhough past incidents have been
# inserted based on date, the id is incremented

# To find the new data inserted or appended
# find the highest id in the csv file saved
# in my github repo

maxID <- max(dataPoliceShootingSaved$id) #1740

# Using the maxID, subset the data from
# Washington Post repo to find the updated
# records

dataPoliceShootingNew <- subset(dataPoliceShooting, id > maxID)


##############################
# Get Month and Day of the
# Week from the date 
# information
##############################

dataPoliceShootingNew$month <- month(dataPoliceShootingNew$date, label=TRUE, abbr = FALSE)

dataPoliceShootingNew$dayOfWeek <- wday(dataPoliceShootingNew$date, label=TRUE, abbr = FALSE)

dataPoliceShootingNew$year <- year(dataPoliceShootingNew$date)


############################################################
# Get the full state name from the two letter abbreviation,
# the region (Northeast, South, North Central, West) that 
# each state belongs to.
# state divisions (New England, Middle Atlantic, South Atlantic, 
# East South Central, West South Central, East North Central, 
# West North Central, Mountain, and Pacific)
############################################################

data(state) # load the state information data

# state data doesn't include DC, so adding it

state.abb <- c(state.abb[1:8], "DC", state.abb[9:50])
state.name <- c(state.name[1:8], "District of Columbia", state.name[9:50])

state.division <- c(state.division[1:8], "3", state.division[9:50])

state.region <- c(state.region[1:8], "2", state.region[9:50])



dataPoliceShootingNew$StateName <- state.name[match(dataPoliceShootingNew$state, state.abb)]


dataPoliceShootingNew$Division <- state.division[match(dataPoliceShootingNew$StateName, state.name)]

# Re-factorizing the Division information to 9 levels
dataPoliceShootingNew$Division <- factor(dataPoliceShootingNew$Division,
                                         levels = c(1,2,3,4,5,6,7,8,9),
                                         labels = c("New England", "Middle Atlantic", "South Atlantic", "East South Central", "West South Central", "East North Central", "West North Central", "Mountain", "Pacific"))



dataPoliceShootingNew$Region <- state.region[match(dataPoliceShootingNew$StateName, state.name)]

# Re-factorizing the Region information to 4 levels
dataPoliceShootingNew$Region <- factor(dataPoliceShootingNew$Region,
                                       levels = c(1,2,3,4),
                                       labels = c("Northeast", "South", "North Central", "West"))


############################################################
# Get the Lat and Long of the city.
# gecode calls the Google API  to provide the Lat and Long
# using the city and the full state name instead of
# the two letter improves the accuracy of the Google 
# query (reduces ambiguity)
# NOTE: geocode calls to the Google API is limited to
# 2500 queries in a day
############################################################

dataPoliceShootingNew$CityState <- paste(trimws(as.character(dataPoliceShootingNew$city)), ",", trimws(dataPoliceShootingNew$StateName))


shootingCoordinates <- geocode(dataPoliceShootingNew$CityState, output = "latlona")

dataPoliceShootingNew$cityLon <- shootingCoordinates$lon
dataPoliceShootingNew$cityLat <- shootingCoordinates$lat


myMergedData <- rbind(dataPoliceShootingSaved, dataPoliceShootingNew)

# Ordered my merged data by the date of the incident
myMergedData <- myMergedData[order(myMergedData$date), ]


# Write to a file, suppress row names
# Use the file for visualization 
write.csv(myMergedData, "data/fatal-police-shootings-data-update-10252016.csv", row.names=FALSE)
