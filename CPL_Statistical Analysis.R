# Help determine whether the program should be continued/expanded by estimating its effect on re- arrests prior to disposition. 

# Pre-processing ----------------------------------------------------------------------------------------
# Let's drop any variables that are date based and id based
full <- full[, .SD, .SDcols = !grepl(x = names(full),pattern =  "id|date")]
trainData<- floor(0.75 * nrow(full))
set.seed(8675309)
trainData <- sample(seq_len(nrow(full)), size = trainData)
train <-full[trainData]
test <- full[-trainData]
nrow(train) + nrow(test) == nrow(full) # gut check

train[,`:=`(race = as.factor(race),
            gender = as.factor(gender))]
# logit regression
## 1) Simple model to look at relationship between treatment and re_arrest
cor(x = train$treat, y = train$re_arrest)

lm <- glm(formula = re_arrest ~ treat,
          data = train,
          family = binomial(link = 'logit'))

summary(lm)

# 2) Full model with all vars --------------------------------------------------------------------------
lmFull <- glm(formula = re_arrest ~ .,
              data = train,
              family = binomial(link = 'logit'))

summary(lmFull)

# Step-wise ---------------------------------------------------------------------------------------------
## model that returns the lowest deviance in the AIC
lmStep <- stepAIC(lmFull, direction = "both", 
                  trace = FALSE)
summary(lmStep)


# Interaction Variables ---------------------------------------------------------------------------------
full2 <- full
full2[, `:=`(totalArrests2 = totalArrests^2,
             ageTotalArrests = age*totalArrests,
             agePriorArrests = age*prior_arrests)]

train2 <- full[1:(.75*nrow(full))]
test2 <- full[(nrow(train)+1):nrow(full)]
nrow(train2) + nrow(test2) == nrow(full)

lmFull2 <- glm(formula = re_arrest ~ .,
               data = train2,
               family = binomial(link = 'logit'))
summary(lmFull2)

lmStep2 <- stepAIC(lmFull2, direction = "both", 
                   trace = FALSE)
summary(lmStep2)

# Model Assessment --------------------------------------------------------------------------------------
# Assess the deviance between our model and the null model
anova(lmStep, test = "Chisq")
anova(lmStep2, test = "Chisq")
pR2(lmStep)
pR2(lmStep2)

# #1 Validation  ---------------------------------------------------------------------------------------
## Get probabilities for each obs that obs re_arrest = 1
lmStepProbs <- predict(lmStep,
                       newdata=test,
                       type='response')

lmStepProbs2 <- predict(lmStep2,
                        newdata=test,
                        type='response')

## Convert probs > .5 to 1 and lower than .5 to 0
lmStepResults <- ifelse(lmStepProbs > 0.5,1,0)
lmStepResults2 <- ifelse(lmStepProbs2 > 0.5,1,0)

# Get our average
misClasificError <- mean(lmStepResults != test$re_arrest)
misClasificError2 <- mean(lmStepResults2 != test2$re_arrest)
accuracyMessage <- print(paste0('The accuracy for model w/o interaction vars is ',1-misClasificError,
                                'and the accuracy for model w/ interaction vars is ', 1-misClasificError2, 
                                if(misClasificError < misClasificError2){'. Model 1 is more accurate.'}
                                else {'. Model 2 is more accurate.'}))

# Area under the ROC Curve ------------------------------------------------------------------------------
prediction <- prediction(lmStepProbs, test$re_arrest)
performance <- performance(prediction, measure = "tpr", x.measure = "fpr")
plot(performance)

auc <- performance(prediction, measure = "auc")
auc <- auc@y.values[[1]]
auc

## Write up resources
# https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
# prior arrests

# What if we changed the data? --------------------------------------------------------------------------
# Propensity score matching ----------------------------------------------------------------------------
pScores <- matchit(re_arrest ~ age + prior_arrests + totalArrests + race, 
                   data = full,
                   method = "nearest", 
                   distance = "logit")

matchedFull <- dagta.table(match.data(pScores, distance = "pscore"))
hist(matchedFull$pscore)
summary(matchedFull$pscore)

t.test(matchedFull[treat==1]$re_arrest,matchedFull[treat==0]$re_arrest,paired = T)
# What is the greatest predictor of rearrests?

# what is the program doing to change behavior?

# how does the program disparately affect people of different ages/gender?

# has the prgram improved over time or degenerated over time? Did it disparately affect people

# How long is the program? 

# When is is given to people?