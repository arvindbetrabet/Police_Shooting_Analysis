########################
# load libraries
########################

library(RCurl)
library(lubridate)
library(ggmap)



# Read the .csv data file from the Washington Post
# github repository

dataPoliceShooting <-read.csv(text=getURL("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv"), header=T)

summary(dataPoliceShooting)

# id                      name              date              manner_of_death          armed    
# Min.   :   3.0   TK TK          :  13   2015-07-07:   8   beaten          :   1   gun         :875  
# 1st Qu.: 504.0   Eric Harris    :   2   2015-12-14:   8   shot            :1443   knife       :224  
# Median : 915.0   Mario Martinez :   2   2016-01-27:   8   shot and Tasered:  97   unarmed     :128  
# Mean   : 908.9   Michael Johnson:   2   2015-02-28:   7                           vehicle     : 90  
# 3rd Qu.:1327.0   Aaron  Marchese:   1   2015-03-11:   7                           toy weapon  : 57  
# Max.   :1740.0   Aaron Rutledge :   1   2015-03-27:   7                           undetermined:x <- x[-length(x)]x <- x[-length(x)] 55  
# (Other)        :1520   (Other)   :1496                           (Other)     :112  

# age        gender   race              city          state     signs_of_mental_illness
# Min.   : 6.00   F:  67    : 92   Los Angeles :  21   CA     :261   False:1156             
# 1st Qu.:27.00   M:1474   A: 22   Houston     :  20   TX     :144   True : 385             
# Median :34.00            B:393   Chicago     :  15   FL     : 95                          
# Mean   :36.38            H:254   Phoenix     :  15   AZ     : 69                          
# 3rd Qu.:45.00            N: 18   Indianapolis:  13   CO     : 47                          
# Max.   :86.00            O: 21   Las Vegas   :  13   GA     : 45                          
# NA's   :35               W:741   (Other)     :1444   (Other):880                          

#        threat_level           flee      body_camera 
#  attack      :1051              :  16   False:1400  
#  other       : 419   Car        : 231   True : 141  x <- x[-length(x)]
#  undetermined:  71   Foot       : 179               
#                      Not fleeing:1073               
#                      Other      :  42  
 


# Read the .csv data file from the Washington Post
# github repository, converting blank
# cells(unavailable or not recorded data) to NA

dataPoliceShooting <-read.csv(text=getURL("https://raw.githubusercontent.com/washingtonpost/data-police-shootings/master/fatal-police-shootings-data.csv"), header=T, na.strings = c("", " "))

summary(dataPoliceShooting)

# id                      name              date              manner_of_death        armed          age        gender     race    
# Min.   :   3.0   TK TK          :  13   2015-07-07:   8   beaten          :   1   gun       :875   Min.   : 6.00   F:  67   A   : 22  
# 1st Qu.: 504.0   Eric Harris    :   2   2015-12-14:   8   shot            :1443   knife     :224   1st Qu.:27.00   M:1474   B   :393  
# Median : 915.0   Mario Martinez :   2   2016-01-27:   8   shot and Tasered:  97   unarmed   :128   Median :34.00            H   :254  
# Mean   : 908.9   Michael Johnson:   2   2015-02-28:   7                           vehicle   : 90   Mean   :36.38            N   : 18  
# 3rd Qu.:1327.0   Aaron  Marchese:   1   2015-03-11:   7                           toy weapon: 57   3rd Qu.:45.00            O   : 21  
# Max.   :1740.0   Aaron Rutledge :   1   2015-03-27:   7                           (Other)   :166   Max.   :86.00            W   :741  
# (Other) :1520   (Other)   :1496                                                   NA's      :  1   NA's   :35               NA's: 92  

#            city          state     signs_of_mental_illness       threat_level           flee      body_camera 
#  Los Angeles :  21   CA     :261   False:1156              attack      :1051   Car        : 231   False:1400  
#  Houston     :  20   TX     :144   True : 385              other       : 419   Foot       : 179   True : 141  
#  Chicago     :  15   FL     : 95                           undetermined:  71   Not fleeing:1073               
#  Phoenix     :  15   AZ     : 69                                               Other      :  42               
#  Indianapolis:  13   CO     : 47                                               NA's       :  16               
# Las Vegas   :  13   GA     : 45                                                                              
# (Other)     :1444   (Other):880  


##############################
# Get Month and Day of the
# Week from the date 
# information
##############################

dataPoliceShooting$month <- month(dataPoliceShooting$date, label=TRUE, abbr = FALSE)

dataPoliceShooting$dayOfWeek <- wday(dataPoliceShooting$date, label=TRUE, abbr = FALSE)

dataPoliceShooting$year <- year(dataPoliceShooting$date)

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



dataPoliceShooting$StateName <- state.name[match(dataPoliceShooting$state, state.abb)]


dataPoliceShooting$Division <- state.division[match(dataPoliceShooting$StateName, state.name)]

# Re-factorizing the Division information to 9 levels
dataPoliceShooting$Division <- factor(dataPoliceShooting$Division,
                                      levels = c(1,2,3,4,5,6,7,8,9),
                                      labels = c("New England", "Middle Atlantic", "South Atlantic", "East South Central", "West South Central", "East North Central", "West North Central", "Mountain", "Pacific"))



dataPoliceShooting$Region <- state.region[match(dataPoliceShooting$StateName, state.name)]

# Re-factorizing the Region information to 4 levels
dataPoliceShooting$Region <- factor(dataPoliceShooting$Region,
                                      levels = c(1,2,3,4),
                                      labels = c("Northeast", "South", "North Central", "West"))


summary(dataPoliceShooting)

# id                      name              date              manner_of_death        armed          age       
# Min.   :   3.0   TK TK          :  13   2015-07-07:   8   beaten          :   1   gun       :875   Min.   : 6.00  
# 1st Qu.: 504.0   Eric Harris    :   2   2015-12-14:   8   shot            :1443   knife     :224   1st Qu.:27.00  
# Median : 915.0   Mario Martinez :   2   2016-01-27:   8   shot and Tasered:  97   unarmed   :128   Median :34.00  
# Mean   : 908.9   Michael Johnson:   2   2015-02-28:   7                           vehicle   : 90   Mean   :36.38  
# 3rd Qu.:1327.0   Aaron  Marchese:   1   2015-03-11:   7                           toy weapon: 57   3rd Qu.:45.00  
# Max.   :1740.0   Aaron Rutledge :   1   2015-03-27:   7                           (Other)   :166   Max.   :86.00  
#                  (Other)        :1520   (Other)   :1496                           NA's      :  1   NA's   :35     

# gender     race               city          state     signs_of_mental_illness       threat_level           flee     
# F:  67   A   : 22   Los Angeles :  21   CA     :261   False:1156              attack      :1051   Car        : 231  
# M:1474   B   :393   Houston     :  20   TX     :144   True : 385              other       : 419   Foot       : 179  
#          H   :254   Chicago     :  15   FL     : 95                           undetermined:  71   Not fleeing:1073  
#          N   : 18   Phoenix     :  15   AZ     : 69                                               Other      :  42  
#          O   : 21   Indianapolis:  13   CO     : 47                                               NA's       :  16  
#          W   :741   Las Vegas   :  13   GA     : 45                                                                 
#          NA's: 92   (Other)     :1444   (Other):880                                                                 

# body_camera   StateName                       Division             Region   
# False:1400   Length:1541        Pacific           :330   Northeast    :114  
# True : 141   Class :character   South Atlantic    :286   South        :630  
#              Mode  :character   West South Central:240   North Central:250  
#                                 Mountain          :217   West         :547  
#                                 East North Central:154                      
#                                 East South Central:104                      
#                                 (Other)           :210                  



############################################################
# Get the Lat and Long of the city.
# gecode calls the Google API  to provide the Lat and Long
# using the city and the full state name instead of
# the two letter improves the accuracy of the Google 
# query (reduces ambiguity)
# NOTE: geocode calls to the Google API is limited to
# 2500 queries in a day
############################################################

dataPoliceShooting$CityState <- paste(trimws(as.character(dataPoliceShooting$city)), ",", trimws(dataPoliceShooting$StateName))


shootingCoordinates <- geocode(dataPoliceShooting$CityState, output = "latlona")

dataPoliceShooting$cityLon <- shootingCoordinates$lon
dataPoliceShooting$cityLat <- shootingCoordinates$lat

# Write to a file, suppress row names
# Use the file for visualization 
write.csv(dataPoliceShooting, "data/fatal-police-shootings-data-2.csv", row.names=FALSE)
