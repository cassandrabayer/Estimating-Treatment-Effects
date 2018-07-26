# Author: Cassandra Bayer
# Date:
# Objective: CPL Technical Assessment 

# Load data  --------------------------------------------------------------
# Inital loading and basic cleaning
case <- as.data.table(read.csv("case.csv", stringsAsFactors = F))
demo <- as.data.table(read.csv("demo.csv",stringsAsFactors = F))
priorArrests <- as.data.table(read.csv("prior_arrests.csv", stringsAsFactors = F))

# Pre-processing/cleaning -------------------------------------------------------------------------------
## Get names and data types for all in environment
sapply(case, class)
sapply(demo, class)
sapply(priorArrests, class)

## Update data types
case[ , c("arrest_date", "dispos_date") := lapply(.SD, as.Date), .SDcols = c("arrest_date", "dispos_date")]
demo[, bdate := as.Date(bdate)]
priorArrests[, arrest_date := as.Date(arrest_date)]


# Merges/binds ------------------------------------------------------------------------------------------
## Merge 1: Get a comprehensive list of all arrests by person
caseSmall <- case[, .(person_id, arrest_date)]
totalArrests <- unique(rbind(caseSmall, priorArrests))
totalArrests <- totalArrests[order(person_id, arrest_date)]

## Merge 2: demo/case data together
setkey(case, person_id)
setkey(demo, person_id)
full <- demo[case]
full <- full[order(person_id, arrest_date)]

## Look for completeness of data before proceeding
sapply(full,function(x) sum(is.na(x)))

# Question 1 --------------------------------------------------------------------------------------------
# The main outcome of interest is whether the defendant was arrested again prior to his/her disposition 
# (trial or court appearance) date. Create a variable called ‘re_arrest’ that equals one if the defendant was 
# arrested prior to trial and zero otherwise. E.g., imagine person 1 was arrested three times.

## are there any arrest dates by person that fall between the arrest date and the disp date?
## For each person
## 
full <- full[order(person_id, arrest_date)]
full[, totalArrests := as.integer(uniqueN(arrest_date)), by = person_id]
full <- full[, re_arrest := 0]
full[totalArrests > 1 & 
       (arrest_date < shift(arrest_date, 1) & shift(arrest_date, 1) < dispos_date), 
     re_arrest := 1, by = person_id]

## Check my work
re_arrests <- full[re_arrest==1, unique(person_id)]
full[person_id %in% re_arrests]

# Question 2 --------------------------------------------------------------------------------------------
# Create a variable called prior_arrests that equals the number of arrests prior to the current arrest. 
# Assume that all of the individual’s arrests prior to the study period are contained in prior.csv. 
# For example, if person z has five arrests in prior.csv and two in full.csv, her first arrest in full.csv should 
# have prior_arrests = 5 and the second should have prior_arrests = 6.
# If someone is not included in prior_arrests.csv, assume they had zero arrests at the start of the study period.

## I need to find out if anyone in the prior data set is not in the full-- they have no previous arrests
totalArrests[, index := 1]
totalArrests[, prior_arrests := cumsum(index)-1, by = person_id]
totalArrests[, index := NULL]
setkey(totalArrests, person_id, arrest_date)
setkey(full, person_id, arrest_date)
full <- totalArrests[full]
full[, .N, by = prior_arrests]

## cheeck my work


# Question 3 --------------------------------------------------------------------------------------------
# Create an ‘age’ variable that equals the defendant’s age at the time of each arrest.
full[, age := arrest_date - bdate, by = person_id]
full[, age := as.integer(age)]
full[, age := round((age/365),0)]


# Post Processing ---------------------------------------------------------------------------------------
## Get a general sense of distribution
## I want to know if there is an even distribution amongst those who have received the treatment
full[, .N, by = race]
full[, .N, by = gender]
full[, .N, by = age][order(age)]

# Let's get a quick plot
toPlot <- full[, .(arrestYear = year(arrest_date),
                   race = as.factor(race),
                   gender = as.factor(gender),
                   disposYear = year(dispos_date),
                   treat, totalArrests, re_arrest, age)]
toPlot %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram() + theme_minimal()

corData <- cor(toPlot[,.(arrestYear,disposYear,treat,re_arrest,totalArrests,age)])

# Correlation matrix
corrplot(corData, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


# Write the data
write.csv(x = full, "bayer_transformed data.csv")
