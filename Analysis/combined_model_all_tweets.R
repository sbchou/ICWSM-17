library(lme4)
library(lmerTest)
library(MASS)
library(AER)
library(stargazer)
library(pscl)

stories <- read.table('FOR_CORR/all_fields_no_title.csv', header=TRUE, sep = ",", quote = "'")
nrow(stories)
head(stories)

# remove NaN for some reason the python code breaks the CSV
stories <- stories[complete.cases(stories),]
nrow(stories)
# SHOULD BE 2651


### DON'T FORGET TO CONVERT TO FACTORS!###
stories$num_tweets = as.numeric((stories$num_tweets))
stories$wc = as.numeric((stories$wc))
stories$emotionality = as.numeric((stories$emotionality))
stories$positivity = as.numeric((stories$positivity)) 


### Model as Negative Binomial ###
model.nb.combined = glm.nb(num_tweets ~ wc + emotionality + positivity, data=stories, control=glm.control(maxit=100))
summary(model.nb.combined)
stargazer(model.nb.combined, type="text")

# Get psuedo R^2
require(pscl)
pR2(model.nb.combined)

### Take Log of Word Count, because of the Power Law ###
model.nb.log = glm.nb(num_tweets ~ log(wc) + emotionality + positivity, data=stories, control=glm.control(maxit=100))
summary(model.nb.log)
stargazer(model.nb.log, type="text")

# Get psuedo R^2 
pR2(model.nb.log)



