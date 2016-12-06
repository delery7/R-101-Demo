# FIRST - WHAT IS GOING ON IN THIS IDE? SOURCE, CONSOLE, ENVIRONMENT, GENERAL

# LOAD DATA
# FIRST STEP - INSTALL A PACKAGE THAT ALLOWS MY ODBC CONNECTION, THEN LOAD THE PACKAGES LIBRARY
# SOME CONNECTIONS ARE BASIC INSTALL - LIKE LOADING A CSV, OTHERS REQUIRE PACKAGES

install.packages("RODBC")
install.packages("dplyr")
install.packages("tidyr")
library(RODBC)
library(dplyr)
library(tidyr)

# LETS REVIEW THIS LINE OF CODE
# OBJECT NAME ON THE LEFT, THESE CAN BE SINGLE VALUE VARIABLES, VECTORS, MATRIX, TABLES, ETC
# ARROW INDICATES WHAT YOU ARE PUTTING INTO THIS OBJECT

connection <- odbcDriverConnect(connection="Driver=SQL Server;server=ALLNB377\\SQLSRV16WITHR;database=SQLSaturdayDEMO;trusted_connection=yes;")

# TABLE NAME ON LEFT, USING RODBC FUNCTION TO QUERY MY SQL SERVER TABLE
# THIS IS SIMILAR TO LOADING A TEMP TABLE IN MANAGEMENT STUDIO (MS), THIS TABLE IS IN MEMORY, CAN NOW BE USED AT WILL
# ALL IN MEMORY UNTIL YOU WRITE IT SOMEWHERE ELSE

table <- sqlQuery(connection,"select * from AirlineData16Demo")

# THINK "SELECT TOP 10 *"
head(table, n=10)
#LOAD THE TABLE TO A WINDOW TO EXPLORE, FILTER, AND SORT - CAN BE DONE VIA THE UI AS WELL
View(table)

# NO SQL SERVER, NO PROBLEM, LOAD CSV'S FROM YOUR COMPUTER
# USE THE TOOLS MENU TO GENERATE THIS CODE

ImportedCSVFile1 <- read.csv("C:/Users/delery/OneDrive for Business/Data for SQL Sat/On_Time_On_Time_Performance_2016_2.csv", stringsAsFactors=FALSE)
ImportedCSVFile2 <- read.csv("C:/Users/delery/OneDrive for Business/Data for SQL Sat/On_Time_On_Time_Performance_2016_1.csv", stringsAsFactors=FALSE)
ImportedCSVFile3 <- read.csv("C:/Users/delery/OneDrive for Business/Data for SQL Sat/On_Time_On_Time_Performance_2015_12.csv", stringsAsFactors=FALSE)
ImportedCSVFile4 <- read.csv("C:/Users/delery/OneDrive for Business/Data for SQL Sat/On_Time_On_Time_Performance_2015_11.csv", stringsAsFactors=FALSE)

# APPEND THE FILES TO ONE FILE
FullImportedCSVFile <- rbind(ImportedCSVFile1, ImportedCSVFile2)
FullImportedCSVFile <- rbind(FullImportedCSVFile, ImportedCSVFile3)
FullImportedCSVFile <- rbind(FullImportedCSVFile, ImportedCSVFile4)
# rm(ImportedCSVFile) - CODE USED TO DROP THE TABLE, KEPT FOR EXAMPLE

#SEE THE DATA IN A WINDOW
View(FullImportedCSVFile)

# SEE THE COLUMN NAMES IN THE CONSOLE
names(FullImportedCSVFile)

#Create a subset of the raw data for analysis
FlightData1 <- (select(FullImportedCSVFile,FlightDate, Carrier,Origin, OriginCityName, OriginState, DestAirportID, Dest, DestCityName, DepTime, DepDelay, DepartureDelayGroups, DepTimeBlk, ArrTime, ArrDelay, ArrivalDelayGroups, ArrTimeBlk, Cancelled, Diverted,ActualElapsedTime, AirTime, Distance, DistanceGroup))
# SEPERATE THE CITY NAME FROM THE STATE IN THE COLUMN
FlightData2 <- separate(FlightData1, DestCityName, c("DestCity","DestState"), sep = ",")
# SEPERATE THE CITY NAME FROM THE STATE IN THE COLUMN
FlightData3 <- separate(FlightData2, OriginCityName, c("OrigCity","OrigState"), sep=",")
#PULL IN FIELDS OF INTEREST 
FlightData4 <- select(FlightData3, FlightDate, Carrier, Origin, OrigCity, OrigState, DestAirportID, Dest, DestCity, DestState, DepTime, DepDelay, DepartureDelayGroups, DepTimeBlk, ArrTime, ArrDelay, ArrivalDelayGroups, ArrTimeBlk, Cancelled, Diverted, ActualElapsedTime, AirTime, Distance, DistanceGroup)
#CALCULATE A COLUMN
FlightData5 <- mutate(FlightData4, TotalNetDelay = DepDelay+ArrDelay)
#REMOVE NA DATA - DATA MUST BE CLEAN TO ANALYZE
FlightData6 <- na.omit(FlightData5)

# JUST SCRATCHING THE SURFACE OF DATA MUNGING/WRANGLING POSSIBILITIES
# ANOTHER EXAMPLE OF POSSIBILITY I FOUND OF INTEREST AT THE END OF THIS SCRIPT

# Descriptive Statistics

# CODE EXPLANATION: FUNCTION(TABLENAME$FIELDNAME)

min(FlightData6$TotalNetDelay)
max(FlightData6$TotalNetDelay)
range(FlightData6$TotalNetDelay)
diff(range(FlightData6$TotalNetDelay))
mean(FlightData6$TotalNetDelay)
quantile(FlightData6$TotalNetDelay)
quantile(FlightData6$TotalNetDelay, 0.95)
IQR(FlightData6$TotalNetDelay)
var(FlightData6$TotalNetDelay)
sd(FlightData6$TotalNetDelay)

# WANT TO RUN DEEPER DESCRIPTIVE ANALYTICS?
# FOR EXAMPLE SKEWNESS OF A MEASURE, MEASURE OF ASYMMETRY AROUND THE MEAN
skewness(FlightData6$TotalNetDelay)

# THIS FUNCTION IS NOT INCLUDED IN THE BASE INSTALL, NEED A PACKAGE
# THE PACKAGE LIBRARY IS VERY LARGE AND LIKELY HAS ANYTHING YOU WOULD WANT TO DO
# CHECK OUT THE PACKAGES LIST AND HELP IN WHAT I CALL THE GENERAL CONSOLE

install.packages("moments")
library(moments)

skewness(FlightData6$TotalNetDelay)
kurtosis(FlightData6$TotalNetDelay)

# FOR MOST, WE ARE GETTING TO INTO THE WEEDS OF STATISTICS, WHAT IS SKEWNESS AND KURTOSIS? 
# UNLESS YOU ARE DIVING INTO DEEPER STATISTICS, IT DOESN'T MATTER, MOST WANT TO KNOW WHAT IT MEANS TO THEM, NOT IN STATISTICAL THEORY

# MOVE TO EXPLORATORY STATS - VISUALS

# PLOTS A DENSITY PLOT OF MY MEASURE
# PROBABILITY OF A POINT FALLING SOMEWHERE IN THE CHART
# SHAPE OF MY MEASURE
plot(density(FlightData6$TotalNetDelay))

# OUTLIERS ARE MAKING THE CHART HARD TO READ, THEREFORE, SETTING THE CHART LIMITS TO IGNORE THE OUTLIERS FROM THE VISUAL
# THIS DOES NOT REMOVE THE OUTLIERS FROM THE ANALYSIS
plot(density(FlightData6$TotalNetDelay), xlim=c(-100,400))

# BASIC VISUAL EXAMPLES

# I WANT TO SLICE AND DICE BY THE CARRIER NAME, NOT ID

# PULL IN THE DIM TABLE
CarriersNameDim <- read.csv("C:/Users/delery/Desktop/SQL Saturday Dev files/L_UNIQUE_CARRIERS (DIM TABLE).csv", stringsAsFactors=FALSE)
# RENAME THE COLUMN FOR THE JOIN
colnames(CarriersNameDim)[1] <- "Carrier"
# JOIN THE TALBES
FlightData7 <- merge(FlightData6,CarriersNameDim, by=c("Carrier"))
# TAKE A LOOK
View(FlightData7)

# LOAD A PACKAGE LIBRARY TO USE THE RENAME FUNCTION
library(plyr)
# RENAME THE FIELD
FlightData8 <- rename(FlightData7,c("Description"="CarrierName"))
# REMOVE ROWS WITH MISSING DATA
FlightData8 <- na.omit(FlightData8)

# THE FOREVER LOVED PIE CHART 
pie(table(FlightData8$CarrierName), clockwise = TRUE)

# LOOK AT THAT WITH SOME DESCRIPTIVE STATS PUT INTO THE VISUAL
boxplot(
  x = FlightData8$TotalNetDelay, 
  xlab = "NetDelay (minutes)", 
  horizontal = TRUE)

# THE OUTLIERS MAKE THAT CHART BASICALLY UNUSABLE
# TWO APPROACHES - Remove from the data, hide from the view
# THE EXAMPLE BELOW IS USING R FUNCTION TO IGNORE OUTLIERS INSTEAD OF JUST HIDING THEM FROM THE VISUAL - IMPACTS ANALYSIS
# THIS IS BARELY SCRATCHING THE SURFACE OF WHAT IS POSSIBLE IN R, BUT I HOPE THIS GETS YOU THINKING OF DIFFERENT APPROACHES TO PROBLEMS

boxplot(
  x = FlightData8$TotalNetDelay, 
  xlab = "NetDelay (minutes)", 
  horizontal = TRUE, axis=FALSE,outline=FALSE)

# HISTOGRAM EXAMPLE - SIMILAR TO DENSITY BUT NOW LOOKING AT COUNTS
hist(FlightData8$TotalNetDelay)

# SHRINK MY BIN SIZES SO GET THE SHAPE
hist(
  x = FlightData8$TotalNetDelay,
  breaks = 100)

# MAYBE THIS STILL ISN'T WHAT YOU WERE LOOKING FOR, WANT A BETTER LOOKING VISUAL, GO GET A PACKAGE

install.packages("ggplot2")
library(ggplot2)

qplot(data = FlightData8, x = FlightData8$TotalNetDelay) + xlim(c(-100, 500))


# SCATTER PLOT - EXAMPLE OF BIVARIATE ANALYSIS
qplot(x = FlightData8$DestState, y = FlightData8$TotalNetDelay,  ylim=c(-50,2000))

# AVERAGE DELAY BY STATE
ggplot(FlightData8, aes(x=FlightData8$DestState, y=FlightData8$TotalNetDelay))+ stat_summary(fun.y="mean",geom="bar")+ labs(list(title="Avg Delay by State",x = "State",y="Avg Delay"))

# MINING FOR RELATIONSHIPS - LOOKING FOR CORRELATION AND COVERIANCE
cor(FlightData8$AirTime,FlightData8$TotalNetDelay,method ="pearson")
cov(FlightData8$AirTime,FlightData8$TotalNetDelay)

# GET BASIC DESCRIPTIVE STATISTICS FOR EACH COLUMN QUICKLY
summary(FlightData8)

# ONE OF MY FAVORITE FINDS - SCATTERPLOT MATRIX
# plot(table) - TAKES TOO LONG ON MY MACHINE, PRIME CANDIDATE TO DEMO SQL SERVER BENEFITS FOR A FUTURE DEMO
# X AND Y AXIS ARE DATA FROM THE TABLE - DIAGNAL IS EACH COLUMN IN THE TABLE
# USE THIS AS A FASTER WAY TO LOOK FOR RELATIONSHIPS AMONG THE DATA IN YOUR TABLE
# SEE A LINEAR OR CLUSTER TREND, MOVE ON TO PREDICTIVE ANALYSIS
# plot(FlightData8) 

# I COULD NOT FIGURE OUT THE ERRORS IN THE ABOVE PLOT ATTEMPT
# PULL IN A DATA SET FROM MY SQL SOURCE
FlightData9 <- select(table, Date, Carrier, Origin, OriginState, Dest, DestCityName, DestState, DepEarlyOrDelayMin, TotalNetDelay, TotalDelay_onlydelay, DelayInd, AirTime, Distance)
plot(FlightData9[1:5])
plot(FlightData9[5:10])
plot(FlightData9[11:16])

# CALL THE HELP MENU
?plot
?par

#  END OF BASIC MINING DEMO  #

# REGRESSION EXAMPLE - R has demo data and examples - go to datasets Package and click on it #

plot(
  x = FlightData9$Distance, 
  y = FlightData9$AirTime)

# Get correlation coefficient
cor(
  x = FlightData9$Distance, 
  y = FlightData9$AirTime)

# Create a linear regression model
x <- FlightData9$Distance

y <- FlightData9$AirTime

modelRegression <- lm(y ~ x)

# Draw linear regression model on plot
lines(
  x = FlightData9$Distance,
  y = modelRegression$fitted, 
  col = "blue",
  lwd = 5)

# Summarize the model
summary(modelRegression)

# Predict new unknown values from the model
predict(
  object = modelRegression, 
  newdata = data.frame(x = c(100, 1000, 3000 )))

# END OF REGRESSION EXAMPLE ####


# Data tranformation example - raw data was in different formats, k for thousands, M for millions, or nothing for whole dollars.  Converted to standard numerical value for analysis
# TAKEN FROM PLURALSIGHT
# Remove $, and "k" and "m", then converts each to millions of dollars
# Convert factor to character string, replace $,k,M, convert to numeric, scale to millions - leverages grepl function
# Need full script from Pluralsight to run

# DATA MUNGING - COMMON FOR STORED PROC
convertBoxOffice <- function(boxOffice)
{
  stringBoxOffice <- as.character(boxOffice)
  
  replacedBoxOffice <- gsub("[$|k|M]", "", stringBoxOffice)
  
  numericBoxOffice <- as.numeric(replacedBoxOffice)
  
  if (grepl("M", boxOffice)) {
    numericBoxOffice 
  } else if (grepl("k", boxOffice)){
    numericBoxOffice * 0.001 
  } else { 
    numericBoxOffice * 0.000001
  }
}



