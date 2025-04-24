install.packages('ISLR')
install.packages('neuralnet')
install.packages("tidyverse")
library(dplyr)
library(neuralnet)
library(scales)
library(stats)


setwd("C:/Users/marcu/Downloads")

data <- read.csv('airline_dataset.csv', stringsAsFactors=T, na.strings=NA)

head(data)


# (10 pts) Dataset description: origin, data points, variables 
# str(), summary(), mean(), sd(), class().



summary(data$Age)
summary(data$Flight.Distance)
summary(data$Inflight.wifi.service)
summary(data$Departure.Arrival.time.convenient)
summary(data$Ease.of.Online.booking)
summary(data$Gate.location)
summary(data$Food.and.drink)
summary(data$Seat.comfort)
summary(data$Inflight.entertainment)
summary(data$On.board.service)
summary(data$Leg.room.service)
summary(data$Baggage.handling)
summary(data$Checkin.service)
summary(data$Inflight.service)
summary(data$Cleanliness)
summary(data$Departure.Delay.in.Minutes)
summary(data$Arrival.Delay.in.Minutes)

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
hist(data$Gender.is.Male)

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
hist(data$Age)

# data$Type.of.Travel
# Set Business travel to 1 and Personal Travel to 0
data$Type.of.Travel <- ifelse(
  data$Type.of.Travel == "Business travel",
  1,       # value when Business travel
  0        # value when Personal Travel
)
# Change the attribute name Type.of.Travel to reflect the binary outcome
names(data)[names(data) == "Type.of.Travel"] <- "Travel.is.for.Business"
table(data$Travel.is.for.Business)

# data$Class 
# Set Business to 1, Eco to 0.5, and Eco Plus to 0
map <- c("Business" = 1, "Eco" = 0.5, "Eco Plus" = 0)
data$Class <- map[as.character(data$Class)]
table(data$Class)

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

hist(data$Inflight.wifi.service)
hist(data$Ease.of.Online.booking)
hist(data$Seat.comfort)
hist(data$Inflight.entertainment)


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

# Review Departure.Delay.in.Minutes before preprocessing
hist(data$Departure.Delay.in.Minutes)

# Create a new feature qualitatively assigning how long of a delay
breaks   <- c(-Inf,  5,   30, Inf)
labels   <- c("OnTime", "ShortDelay", "LongDelay")

data$Departure.Qualitative_Delay_Status <- cut(
  data$Departure.Delay.in.Minutes,
  breaks = breaks,
  labels = labels,
  right  = TRUE    # intervals are (a, b]
)


head(data)

data <- data[data$Departure.Delay.in.Minutes <= 120, ]

# Apply natural log10 of (delay + 1)
# to compress the tail and spread out the low‐delay region.
data$Departure.Delay.in.Minutes <- log10(data$Departure.Delay.in.Minutes + 1)
hist(data$Departure.Delay.in.Minutes)



# complete.cases(), na.omit, md.pattern(), aggr(), cor(), histogram hist(), boxplot(), bar chart  barplot().
head(data)

hist(data$Arrival.Delay.in.Minutes)

# Create a new feature qualitatively assigning how long of a delay
new_breaks <- c(-Inf,   0,    5,   30,  Inf)
new_labels <- c("Early", "OnTime", "ShortDelay", "LongDelay")

data$ArrivalDelayCategory <- cut(
  data$Arrival.Delay.in.Minutes,
  breaks = new_breaks,
  labels = new_labels,
  right  = TRUE      # intervals are (a, b]
)

# Only consider range 0-120
data <- data[data$Arrival.Delay.in.Minutes <= 120, ]


data$Arrival.Delay.in.Minutes <- log10(data$Arrival.Delay.in.Minutes + 1)

# Set neutral or dissatisfied to 0 and satisfied to 1
data$satisfaction <- ifelse(
  data$satisfaction == "neutral or dissatisfied",
  0,       # value when neutral or dissatisfied
  1        # value when satisfied
)
# Change the attribute name satisfaction to reflect the binary outcome
names(data)[names(data) == "satisfaction"] <- "Customer.is.Satisfied"

table(data$Customer.Type.is.Loyal)


head(data)



data <- data %>%
  select(-Departure.Qualitative_Delay_Status)


# We only want to know about loyalty customers
data <- data[data$Customer.Type.is.Loyal == 1, ]

# vector of the exact column names you want
keep_cols <- c(
  "Inflight.entertainment",
  "Ease.of.Online.booking",
  "Travel.is.for.Business",
  "Class",
  "Seat.comfort",
  "Inflight.wifi.service",
  "Customer.is.Satisfied"
)

preprocessed_data <- data[, keep_cols]

set.seed(41)
preprocessed_data <- sample_n(preprocessed_data, 10000)


# Number of records remaining after preprocessing activities
nrow(preprocessed_data)
# [1] 10000
head(preprocessed_data)

# (20 pts) Intended algorithms to be used and rationale
# neuralnetwork, and why?

# (25 pts) Implementation and Evaluation

# 80/20 SPLIT
# STEP 1: calculate 80% * rows and round to nearest integer
sample_size <- floor(0.8 * nrow(preprocessed_data))

# STEP 2: get the data frame indices of our random sample
training_index <- sample(nrow(preprocessed_data), size = sample_size)

# STEP 3a: get the rows by the index from our sample
train <- preprocessed_data[training_index,]

# STEP 3b: get the rows not in the index
test <- preprocessed_data[-training_index,]
head(test)

# we need to create a formula to insert into the machine learning model.
#features <- names(preprocessed_data)[ names(data) != "Customer.is.Satisfied" ]
#f <- paste(features, collapse=' + ')
#f <- paste('Customer.is.Satisfied ~', f)
f <- as.formula(
  "Customer.is.Satisfied ~ Inflight.entertainment +
                          Ease.of.Online.booking +
                          Travel.is.for.Business +
                          Class +
                          Seat.comfort +
                          Inflight.wifi.service")


rm(nn)

# Good practice: number of hidden nodes is 1/2(input nodes + output nodes)
nn <- neuralnet(f, data=train, hidden=4, linear.output=FALSE, stepmax=1e6)

# Compute Predictions off Test Set
predicted <- neuralnet::compute(nn, test[1:6])

# Check out net.result
head(predicted$net.result)

predicted$net.result <- sapply(predicted$net.result, round, digits = 0)

evaluation <- data.frame(test$Customer.is.Satisfied, predicted$net.result)
colnames(evaluation) <- c("actual", "predict")
table(evaluation)

evaluation$correct <- ifelse(evaluation$actual == evaluation$predict, 1, 0)
sum(evaluation$correct)/nrow(evaluation)

# assuming actual and predict are 0/1
TP <- sum(evaluation$predict == 1 & evaluation$actual == 1)
TN <- sum(evaluation$predict == 0 & evaluation$actual == 0)
FP <- sum(evaluation$predict == 1 & evaluation$actual == 0)
FN <- sum(evaluation$predict == 0 & evaluation$actual == 1)

sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)

sensitivity
specificity













# 75/25 SPLIT
preprocessed_data <- data[, keep_cols]

preprocessed_data <- sample_n(preprocessed_data, 10000)

# STEP 1: calculate 75% * rows and round to nearest integer
sample_size2 <- floor(0.60 * nrow(preprocessed_data))

# STEP 2: get the data frame indices of our random sample
training_index2 <- sample(nrow(preprocessed_data), size = sample_size2)

# STEP 3a: get the rows by the index from our sample
train2 <- preprocessed_data[training_index2,]

# STEP 3b: get the rows not in the index
test2 <- preprocessed_data[-training_index2,]
head(test2)

# we need to create a formula to insert into the machine learning model.
#features <- names(preprocessed_data)[ names(data) != "Customer.is.Satisfied" ]
#f <- paste(features, collapse=' + ')
#f <- paste('Customer.is.Satisfied ~', f)
f <- as.formula(
  "Customer.is.Satisfied ~ Inflight.entertainment +
                          Ease.of.Online.booking +
                          Travel.is.for.Business +
                          Class +
                          Seat.comfort +
                          Inflight.wifi.service")


# Good practice: number of hidden nodes is 1/2(input nodes + output nodes)
nn2 <- neuralnet(f, data=train2, hidden=3, linear.output=FALSE, stepmax=5e6)

# Compute Predictions off Test Set
predicted2 <- neuralnet::compute(nn2, test2[1:6])

# Check out net.result
head(predicted2$net.result)

predicted2$net.result <- sapply(predicted2$net.result, round, digits = 0)

evaluation2 <- data.frame(test$Customer.is.Satisfied, predicted$net.result)
colnames(evaluation2) <- c("actual", "predict")
table(evaluation2)

evaluation2$correct <- ifelse(evaluation2$actual == evaluation2$predict, 1, 0)
sum(evaluation2$correct)/nrow(evaluation2)

# assuming actual and predict are 0/1
TP2 <- sum(evaluation2$predict == 1 & evaluation2$actual == 1)
TN2 <- sum(evaluation2$predict == 0 & evaluation2$actual == 0)
FP2 <- sum(evaluation2$predict == 1 & evaluation2$actual == 0)
FN2 <- sum(evaluation2$predict == 0 & evaluation2$actual == 1)

sensitivity2 <- TP2 / (TP2 + FN2)
specificity2 <- TN2 / (TN2 + FP2)

sensitivity2
specificity2









# ----– 1.  SETTINGS ----------------------------------------------------------
train_grid <- seq(0.60, 0.80, 0.10) # 50% … 90%
n_iter     <- 5                    # repetitions per split

# ----– 2.  DATA & FORMULA ----------------------------------------------------
#  • preprocessed_data must already exist
#  • Target must be coded 0/1 (numeric) – convert if it isn’t.
#    preprocessed_data$Customer.is.Satisfied <- as.numeric(preprocessed_data$Customer.is.Satisfied) 

predictors <- c(                    # keep in one place
  "Inflight.entertainment",
  "Ease.of.Online.booking",
  "Travel.is.for.Business",
  "Class",
  "Seat.comfort",
  "Inflight.wifi.service"
)

f <- as.formula(
  paste("Customer.is.Satisfied ~", paste(predictors, collapse = " + "))
)

# ----– 3.  LOOP & COLLECT METRICS -------------------------------------------
results <- data.frame()             # will hold the summary for each split

for (train_pct in train_grid) {
  
  acc  <- sens <- spec <- numeric(n_iter)   # vectors for this split
  
  for (i in seq_len(n_iter)) {
    ## 3a. split --------------------------------------------------------------
    idx   <- sample(seq_len(nrow(preprocessed_data)),
                    size = floor(train_pct * nrow(preprocessed_data)))
    train <- preprocessed_data[idx, ]
    test  <- preprocessed_data[-idx, ]
    
    ## 3b. train --------------------------------------------------------------
    nn <- neuralnet(
      f,
      data          = train,
      hidden        = 3,          
      linear.output = FALSE,
      stepmax       = 5e6
    )
    
    ## 3c. predict ------------------------------------------------------------
    prob <- neuralnet::compute(nn, test[ , predictors])$net.result
    pred <- ifelse(prob > 0.5, 1, 0)         # threshold can be tuned
    
    ## 3d. metrics ------------------------------------------------------------
    cm <- table(Actual = test$Customer.is.Satisfied, Predicted = pred)
    # guard for rare cases with no positives/negatives
    tn <- ifelse("0" %in% rownames(cm) && "0" %in% colnames(cm), cm["0","0"], 0)
    tp <- ifelse("1" %in% rownames(cm) && "1" %in% colnames(cm), cm["1","1"], 0)
    fp <- ifelse("0" %in% rownames(cm) && "1" %in% colnames(cm), cm["0","1"], 0)
    fn <- ifelse("1" %in% rownames(cm) && "0" %in% colnames(cm), cm["1","0"], 0)
    
    acc [i] <- (tp + tn) / sum(cm) * 100
    sens[i] <- ifelse(tp + fn == 0, NA, tp / (tp + fn))
    spec[i] <- ifelse(tn + fp == 0, NA, tn / (tn + fp))
  }
  
  ## 3e. aggregate ------------------------------------------------------------
  results <- rbind(
    results,
    data.frame(
      Training_Percent = train_pct * 100,
      Accuracy         = mean(acc , na.rm = TRUE),
      Sensitivity      = mean(sens, na.rm = TRUE),
      Specificity      = mean(spec, na.rm = TRUE)
    )
  )
}

# ----– 4.  REVIEW ------------------------------------------------------------
print(results)



