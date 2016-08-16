##############################
# Libraries
##############################

library(ggplot2)
library(RColorBrewer)
library(viridis)
library(ColorPalette)

library(plotly)
library(dplyr)


# Read csv
dataPoliceShooting <- read.csv(file = "data/fatal-police-shootings-data-2.csv", header = TRUE)


filterData2015_1 <- filter(dataPoliceShooting, dataPoliceShooting$year == 2015)

filterData2016_1 <- filter(dataPoliceShooting, dataPoliceShooting$year == 2016)


myFont<- list(
  family = "Courier New, monospace",
  size = 18,
  color = "#7f7f7f"
)
x <- list(
  title = "Months",
  titlefont = myFont
)

xMonths <- list(
  title = "Months",
  titlefont = myFont
)

y <- list(
  title = "Number of Victims",
  titlefont = myFont
)

yVictims <- list(
  title = "Number of Victims",
  titlefont = myFont
)

#############################
# Set Margins and Padding
#############################
m = list(
  l = 100,
  r = 50,
  b = 150,
  t = 100,
  pad = 0
)

############################################################
# Plot Number of Victims by Month
############################################################


byMonth <- group_by(dataPoliceShooting,OrderedMonths(month))
( sumMonth <- summarize(byMonth,count=n()) )

xMonths <- list(
  title = "Months",
  titlefont = myFont
)

yVictims <- list(
  title = "Number of Victims",
  titlefont = myFont
)

# Reordered the factor levels
OrderedMonths <- function(x){
  
  return <- factor(x, levels=c("January","February","March", "April","May","June","July","August","September", "October","November","December"),ordered=TRUE)
  
}

pByMonth <- plot_ly(
  x = sumMonth$`OrderedMonths(month)`,
  y = sumMonth$count,
  type = "bar",
  marker = list(color = brewer.pal(12, "Paired"))) %>%
  layout(xaxis = xMonths, yaxis = yVictims, title = "Victims by Month", margin = m)

pByMonth

# 2015

filterData2015 <- filter(dataPoliceShooting, dataPoliceShooting$year == 2015)

by2015 <- group_by(filterData2015, OrderedMonths(month))
( sum2015 <- summarize(by2015, count=n()) )

pBy2015 <- plot_ly(
  x = sum2015$`OrderedMonths(month)`,
  y = sum2015$count,
  type = "bar",
  name = "2015",
  marker = list(color = brewer.pal(12, "Paired"))) %>%
  layout(xaxis = xMonths, yaxis = yVictims, title = "Victims by Month", margin = m)

pBy2015

# 2016
filterData2016 <- filter(dataPoliceShooting, dataPoliceShooting$year == 2016)

by2016 <- group_by(filterData2016, OrderedMonths(month))
( sum2016 <- summarize(by2016, count=n()) )


# Combining two plots side-by-side for comparision
# Year-by-Year
pBoth <- add_trace(
  pBy2015,
  x = sum2016$`OrderedMonths(month)`,
  y = sum2016$count,
  name = "2016",
  type = "bar")

pBoth



############################################################
# Plot Number of Victims by Day of the Week
############################################################

OrderedDayOfWeek <- function(x){
  
  return <- factor(x, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), ordered = TRUE)
  
}


byDOW <- group_by(dataPoliceShooting,OrderedDayOfWeek(dayOfWeek))
( sumDOW <- summarize(byDOW,count=n()) )


xDOW <- list(
  title = "Day of the Week",
  titlefont = myFont
)


pDOW <- plot_ly(
  x = sumDOW$`OrderedDayOfWeek(dayOfWeek)`,
  y = sumDOW$count,
  type = "bar",
  marker = list(color = brewer.pal(12, "Paired"))) %>%
  layout(xaxis = xDOW, yaxis = yVictims, title = "Victims by Day of the Week", margin = m)

pDOW



############################################################
# Plot Number of Victims by Race
############################################################

myRaceFactor <- as.character(dataPoliceShooting$race)
myRaceFactor[is.na(myRaceFactor)] <- " NA"
myRaceFactor <- factor(myRaceFactor)


# Re-factorizing the Race information to 7 levels
dataPoliceShooting$RaceFactor <- factor(myRaceFactor,
                                        levels = c("A","B","H","N", " NA", "O","W"),
                                        labels = c("Asian", "Black", "Hispanic", "Not Known", "Not Available", "Other", "White"))


byRace <- group_by(dataPoliceShooting, RaceFactor)
(sumRace <- arrange(summarize(byRace, count=n()), count))

xRace <- list(
  title = "Race",
  titlefont = myFont
)

pRace <- plot_ly(
  x = sumRace$RaceFactor,
  y = sumRace$count,
  type = "bar",
  marker = list(color = brewer.pal(12, "Paired"))) %>%
  layout(xaxis = xRace, yaxis = yVictims, title = "Victims by Race", margin = m)

pRace



############################################################
# Plot Number of Victims by 4 Regions of USA
# "West", "North Central", "South", "Northeast"
############################################################



byRegion <- group_by(dataPoliceShooting, Region)
(sumRegion <- arrange(summarize(byRegion, count=n()), count))

xRegion <- list(
  title = "Regions in US",
  titlefont = myFont
)

pRegion <- plot_ly(
  x = sumRegion$Region,
  y = sumRegion$count,
  type = "bar",
  marker = list(color = brewer.pal(12, "Paired"))) %>%
  layout(xaxis = xRegion, yaxis = yVictims, title = "Victims by Region", margin = m)

pRegion



############################################################
# Plot Number of Victims by 9 Divisions of USA
# "Pacific", "West North Central", "Mountain", 
# "West South Central", "Middle Atlantic", "East North Central", 
# "South Atlantic", "East South Central", "New England"
############################################################


byDivision <- group_by(dataPoliceShooting, Division)
(sumDivision <- arrange(summarize(byDivision, count=n()), count))

xDivision <- list(
  title = "Divisions in US",
  titlefont = myFont
)

pDivision <- plot_ly(
  x = sumDivision$Division,
  y = sumDivision$count,
  type = "bar",
  marker = list(color = brewer.pal(12, "Paired"))) %>%
  layout(xaxis = xDivision, yaxis = yVictims, title = "Victims by Division", margin = m)

pDivision


############################################################
# Plot Number of Victims by their Action
############################################################

myActionFactor <- as.character(dataPoliceShooting$flee)
myActionFactor[is.na(myActionFactor)] <- " NA"
myActionFactor <- factor(myActionFactor)

# plot(myActionFactor)

# Re-factorizing the Race information to 5 levels
dataPoliceShooting$ActionFactor <- factor(myActionFactor,
                                        levels = c("Car","Foot","Not fleeing","Other", " NA"),
                                        labels = c("Car", "Foot", "Not Fleeing", "Other", "Not Available"))



byAction <- group_by(dataPoliceShooting, ActionFactor)
(sumAction <- arrange(summarize(byAction, count=n()), count))

xAction <- list(
  title = "Action",
  titlefont = myFont
)

pAction <- plot_ly(
  x = sumAction$ActionFactor,
  y = sumAction$count,
  type = "bar",
  marker = list(color = brewer.pal(12, "Paired"))) %>%
  layout(xaxis = xAction, yaxis = yVictims, title = "Victim's Action at the time of the Incident", margin = m)

pAction




############################################################
# Plot Number of Victims by Age
############################################################

byAge <- group_by(dataPoliceShooting,age)
( sumAge <- summarize(byAge,count=n()) )


xAge <- list(
  title = "Age of Victims",
  titlefont = myFont
)

pAge <- plot_ly(
  x = sumAge$age,
  y = sumAge$count,
  type = "bar",
  name = "Age",
  marker = list(color = colorRampPalette(brewer.pal(8,"Dark2"))(100))) %>%
  layout(xaxis = xAge, yaxis = yVictims, title = "Victims by Age", margin = m)

pAge

# Box Plot of Age of the Victims

yAgeBox <- list(
  title = "Age Distribution",
  titlefont = myFont
)

pAgeBox <- plot_ly(
  y = dataPoliceShooting$age, 
  type = "box", 
  name = "Age", 
  boxmean = TRUE) %>%
  layout(yaxis = yAgeBox, title = "Age Distribution of Victims")

pAgeBox


############################################################
# Plot Number of Victims by Manner of Death
############################################################

# Re-factorizing the Manner of Death information to 3 levels
dataPoliceShooting$DMFactor <- factor(dataPoliceShooting$manner_of_death,
                                          levels = c("beaten","shot","shot and Tasered"),
                                          labels = c("Beaten", "Shot", "Shot and Tasered"))


byDeathManner <- group_by(dataPoliceShooting, DMFactor)
( sumDeathManner <- arrange(summarize(byDeathManner,count=n()), count ))

# sumDM_Arranged <- arrange(sumDeathManner, desc(count))
# 
# sumDM_Arranged

xDeathManner <- list(
  title = "Manner of Death",
  titlefont = myFont
)

pDeathManner <- plot_ly(
  x = sumDeathManner$DMFactor,
  y = sumDeathManner$count,
  type = "bar",
  marker = list(color = brewer.pal(12, "Paired"))) %>%
  layout(xaxis = xDeathManner, yaxis = yVictims, title = "Victim's Manner of Death", margin = m)

pDeathManner

# pDeathManner2 <- plot_ly(
#   x = sumDM_Arranged$DMFactor,
#   y = sumDM_Arranged$count,
#   type = "bar",
#   marker = list(color = brewer.pal(12, "Paired"))) %>%
#   layout(xaxis = xDeathManner, yaxis = yVictims, title = "Victim's Manner of Death", margin = m)
# 
# pDeathManner2


############################################################
# Plot Number of Victims by Weapons in Hand - Top 5
############################################################

byWeaponsInHand <- group_by(dataPoliceShooting, armed)
WIHTop5 <- arrange(( sumWeaponsInHand <- summarize(byWeaponsInHand,count=n()) ) %>% top_n(5), count)


xWeaponsInHand <- list(
  title = "Weapon(s) in Hand",
  titlefont = myFont,
  tickangle = -45
)

pWeaponsInHand <- plot_ly(
  x = WIHTop5$armed,
  y = WIHTop5$count,
  type = "bar",
  marker = list(color = brewer.pal(12, "Paired"))) %>%
  layout(xaxis = xWeaponsInHand, yaxis = yVictims, title = "Top 5 Victim's Choice of Weapon(s)", margin = m)

pWeaponsInHand




############################################################
# Plot Number of Victims by State - Top 15
############################################################

byState <- group_by(dataPoliceShooting, StateName)
StateTop15 <- arrange(( sumState <- summarize(byState,count=n()) ) %>% top_n(15), count)

xState <- list(
  title = "State",
  titlefont = myFont,
  tickangle = -45
)

pState <- plot_ly(
  x = StateTop15$StateName,
  y = StateTop15$count,
  type = "bar",
  marker = list(color = colorRampPalette(brewer.pal(8,"Dark2"))(50))) %>%
  layout(xaxis = xState, yaxis = yVictims, title = "Victim's by State - Top 15", margin = m)

pState


############################################################
# Plot Number of Victims by City - Top 15
############################################################

byCity <- group_by(dataPoliceShooting, city)
CityTop15 <- arrange(( sumCity <- summarize(byCity,count=n()) ) %>% top_n(15), count)


xCity <- list(
  title = "City",
  titlefont = myFont,
  tickangle = -45
)

pCity <- plot_ly(
  x = CityTop15$city,
  y = CityTop15$count,
  type = "bar",
  marker = list(color = colorRampPalette(brewer.pal(12,"Paired"))(50))) %>%
  layout(xaxis = xCity, yaxis = yVictims, title = "Victim's by City - Top 15", margin = m)

pCity


############################################################
# Plot Number of Victims by Perceived Threat
############################################################

# Re-factorizing the Threat Level information to 3 levels
dataPoliceShooting$TLFactor <- factor(dataPoliceShooting$threat_level,
                                      levels = c("attack","other","undetermined"),
                                      labels = c("Attack", "Other", "Undetermined"))

byThreatLevel <- group_by(dataPoliceShooting, TLFactor)
(sumThreatLevel <- summarize(byThreatLevel, count=n()))

xThreatLevel <- list(
  title = "Perceived Threat Level",
  titlefont = myFont
)

pThreatLevel <- plot_ly(
  x = sumThreatLevel$TLFactor,
  y = sumThreatLevel$count,
  type = "bar",
  marker = list(color = brewer.pal(12, "Paired"))) %>%
  layout(xaxis = xThreatLevel, yaxis = yVictims, title = "Victims by Perceived Threat Level", margin = m)

pThreatLevel
