install.packages('ISLR')
install.packages('neuralnet')
library(neuralnet)
library(scales)
library(stats)


setwd("C:/Users/marcu/Downloads")

data <- read.csv('airline_dataset.csv', stringsAsFactors=T, na.strings=NA)

head(data)


# (10 pts) Dataset description: origin, data points, variables 
# str(), summary(), mean(), sd(), class().

str(data$Flight.Distance)
summary(data$Flight.Distance)
class(data$Flight.Distance)
mean(data$Flight.Distance)
sd(data$Flight.Distance)
hist(data$Flight.Distance)

str(data$Seat.comfort)
summary(data$Seat.comfort)
class(data$Seat.comfort)
mean(data$Seat.comfort)
sd(data$Seat.comfort)
hist(data$Seat.comfort)

str(data$Inflight.entertainment)
summary(data$Inflight.entertainment)
class(data$Inflight.entertainment)
mean(data$Inflight.entertainment)
sd(data$Inflight.entertainment)
hist(data$Inflight.entertainment)
table(data$Inflight.entertainment)


str(data$Inflight.wifi.service)
summary(data$Inflight.wifi.service)
class(data$Inflight.wifi.service)
mean(data$Inflight.wifi.service)
sd(data$Inflight.wifi.service)
hist(data$Inflight.wifi.service)
table(data$Inflight.wifi.service)


str(data$Ease.of.Online.booking)
summary(data$Ease.of.Online.booking)
class(data$Ease.of.Online.booking)
mean(data$Ease.of.Online.booking)
sd(data$Ease.of.Online.booking)
hist(data$Ease.of.Online.booking)
table(data$Ease.of.Online.booking)

table(data$Type.of.Travel)



# (20 pts) Data preprocessing activities and results
# complete.cases(), na.omit, md.pattern(), aggr(), cor(), histogram hist(), boxplot(), bar chart  barplot().

head(data)
 
# Remove NA values
# How many NA values are in each column?
colSums(is.na(data))
# Arrival.Delay.in.Minutes has 363

# Remove NA values from Arrival.Delay.in.Minutes
data <- data[ !is.na(data$Arrival.Delay.in.Minutes), ]

# Remove the ID column
data <- data[, -1] 

# data$Gender
# Set Male to 1 and Female to 0
data$Gender <- ifelse(
  data$Gender == "Male",
  1,       # value when Male
  0        # value when Female
)
# Change the attribute name Gender to reflect the binary outcome
names(data)[names(data) == "Gender"] <- "Gender.is.Male"

# data$Customer.Type
# Set Loyal Customer to 1 and disloyal Customer to 0
data$Customer.Type <- ifelse(
  data$Customer.Type == "Loyal Customer",
  1,       # value when Loyal Customer
  0        # value when disloyal Customer
)
# Change the attribute name Customer.Type to reflect the binary outcome
names(data)[names(data) == "Customer.Type"] <- "Customer.Type.is.Loyal"

# Scale age from 0-1
data$Age <- rescale(data$Age)   # to [0,1]  


# data$Type.of.Travel
# Set Business travel to 1 and Personal Travel to 0
data$Type.of.Travel <- ifelse(
  data$Type.of.Travel == "Business travel",
  1,       # value when Business travel
  0        # value when Personal Travel
)
# Change the attribute name Type.of.Travel to reflect the binary outcome
names(data)[names(data) == "Type.of.Travel"] <- "Travel.is.for.Business"

# data$Class 
# Set Business to 1, Eco to 0.5, and Eco Plus to 0
map <- c("Business" = 1, "Eco" = 0.5, "Eco Plus" = 0)
data$Class <- map[as.character(data$Class)]

# Scale Flight.Distance from 0-1
data$Flight.Distance <- rescale(data$Flight.Distance)

# Review the survey data before preprocessing activities
hist(data$Inflight.wifi.service)


# Remove 0 values from the survey results.     
data <- data[data$Inflight.wifi.service != 0, ]
data <- data[data$Departure.Arrival.time.convenient != 0, ]
data <- data[data$Ease.of.Online.booking != 0, ]
data <- data[data$Gate.location != 0, ]
data <- data[data$Food.and.drink != 0, ]
data <- data[data$Online.boarding != 0, ]
data <- data[data$Seat.comfort != 0, ]
data <- data[data$Inflight.entertainment != 0, ]
data <- data[data$On.board.service != 0, ]
data <- data[data$Leg.room.service != 0, ]
data <- data[data$Baggage.handling != 0, ]
data <- data[data$Checkin.service != 0, ]
data <- data[data$Inflight.service != 0, ]
data <- data[data$Cleanliness != 0, ]

# Scale the survey results 0-1
head(data)
data$Inflight.wifi.service <- rescale(data$Inflight.wifi.service, to = c(0,1), from = c(1,5))
data$Departure.Arrival.time.convenient <- rescale(data$Departure.Arrival.time.convenient, to = c(0,1), from = c(1,5))
data$Ease.of.Online.booking <- rescale(data$Ease.of.Online.booking, to = c(0,1), from = c(1,5))
data$Gate.location <- rescale(data$Gate.location, to = c(0,1), from = c(1,5))
data$Food.and.drink <- rescale(data$Food.and.drink, to = c(0,1), from = c(1,5))
data$Online.boarding <- rescale(data$Online.boarding, to = c(0,1), from = c(1,5))
data$Seat.comfort <- rescale(data$Seat.comfort, to = c(0,1), from = c(1,5))
data$Inflight.entertainment <- rescale(data$Inflight.entertainment, to = c(0,1), from = c(1,5))
data$On.board.service <- rescale(data$On.board.service, to = c(0,1), from = c(1,5))
data$Leg.room.service <- rescale(data$Leg.room.service, to = c(0,1), from = c(1,5))
data$Baggage.handling <- rescale(data$Baggage.handling, to = c(0,1), from = c(1,5))
data$Checkin.service <- rescale(data$Checkin.service, to = c(0,1), from = c(1,5))
data$Inflight.service <- rescale(data$Inflight.service, to = c(0,1), from = c(1,5))
data$Cleanliness <- rescale(data$Cleanliness, to = c(0,1), from = c(1,5))

# Scale the Departure.Delay.in.Minutes 0-1
data$Departure.Delay.in.Minutes <- rescale(data$Departure.Delay.in.Minutes)

# Scale the Arrival.Delay.in.Minutes 0-1
data$Arrival.Delay.in.Minutes <- rescale(data$Arrival.Delay.in.Minutes)

# Set neutral or dissatisfied to 0 and satisfied to 1
data$satisfaction <- ifelse(
  data$satisfaction == "neutral or dissatisfied",
  0,       # value when neutral or dissatisfied
  1        # value when satisfied
)
# Change the attribute name satisfaction to reflect the binary outcome
names(data)[names(data) == "satisfaction"] <- "Customer.is.Satisfied"

# Number of records remaining after preprocessing activities
nrow(data)
# [1] 119204


# (20 pts) Intended algorithms to be used and rationale
# neuralnetwork, and why?

# (25 pts) Implementation and Evaluation

# 80/20 SPLIT
# STEP 1: calculate 80% * rows and round to nearest integer
sample_size <- floor(0.8 * nrow(data))

# STEP 2: get the data frame indices of our random sample
training_index <- sample(nrow(data), size = sample_size)

# STEP 3a: get the rows by the index from our sample
train <- data[training_index,]

# STEP 3b: get the rows not in the index
test <- data[-training_index,]

# we need to create a formula to insert into the machine learning model.
features <- names(data)[ names(data) != "Customer.is.Satisfied" ]
f <- paste(features, collapse=' + ')
f <- paste('Customer.is.Satisfied ~', f)

rm(nn)
# Good practice: number of hidden nodes is 1/2(input nodes + output nodes)
nn <- neuralnet(f, data = train, 
                hidden = 11, 
                linear.output = FALSE,
                threshold = 0.06,
                stepmax = 1e5)

# Compute Predictions off Test Set
predicted <- compute(nn, test[1:22])

# Check out net.result
head(predicted$net.result)

