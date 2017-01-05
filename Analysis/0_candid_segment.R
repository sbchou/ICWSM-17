### Analyze Tweet Volume as a Count Distribution
### Which it is.
library(lme4)
library(lmerTest)
library(MASS)
library(AER)

stories <- read.table('FOR_CORR/GROUPBY_FOLLOWING/following_0.csv', header=TRUE, sep = ",", quote = "'")
nrow(stories)
head(stories)

### DON'T FORGET TO CONVERT TO FACTORS!###
stories$num_tweets = as.numeric((stories$num_tweets))
stories$wc = as.numeric((stories$wc))
stories$emotionality = as.numeric((stories$emotionality))
stories$positivity = as.numeric((stories$positivity)) 

# remove NaN for some reason the python code breaks the CSV
stories <- stories[complete.cases(stories),]
nrow(stories) # should be 2640-- this makes sense (remember story is unit of analysis) 

### Take Log of Word Count, because of the Power Law ###
model.nb.log = glm.nb(num_tweets ~ log(wc) + emotionality + positivity, data=stories, control=glm.control(maxit=100))
summary(model.nb.log)
stargazer(model.nb.log)

# Get psuedo R^2 
pR2(model.nb.log) 
