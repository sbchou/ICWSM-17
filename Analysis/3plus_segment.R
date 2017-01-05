### Analyze Tweet Volume as a Count Distribution
### Which it is.
library(lme4)
library(lmerTest)
library(MASS)
library(AER)

stories <- read.table('FOR_CORR/GROUPBY_FOLLOWING/following_gt_3.csv', header=TRUE, sep = ",", quote = "'")
nrow(stories)
head(stories)

### DON'T FORGET TO CONVERT TO FACTORS!###
stories$num_tweets = as.numeric((stories$num_tweets))
stories$wc = as.numeric((stories$wc))
stories$emotionality = as.numeric((stories$emotionality))
stories$positivity = as.numeric((stories$positivity)) 

# remove NaN for some reason the python code breaks the CSV
stories <- stories[complete.cases(stories),]
nrow(stories) # should be 2,581

### Take Log of Word Count, because of the Power Law ###
model.nb.log = glm.nb(num_tweets ~ log(wc) + emotionality + positivity, data=stories, control=glm.control(maxit=100))
summary(model.nb.log)
stargazer(model.nb.log,type="text")

# Get psuedo R^2 
pR2(model.nb.log) 

#stargazer(model.nb.log)
