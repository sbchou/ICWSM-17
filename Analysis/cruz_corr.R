### Analyze Tweet Volume as a Count Distribution
### Which it is.
library(lme4)
library(lmerTest)
library(MASS)
library(AER)

# LOAD DATUMS
stories <- read.table('FOR_CORR/cruz_w_emot.csv', header=TRUE, sep = ",", quote = "'") 

# Convert to factors 
stories$num_tweets = as.numeric((stories$num_tweets))
stories$wc = as.numeric((stories$wc))
stories$emotionality = as.numeric((stories$emotionality))
stories$positivity = as.numeric((stories$positivity)) 
# remove NaN for some reason the python code breaks the CSV
stories <- stories[complete.cases(stories),]
nrow(stories)
# should be 1495

### Take Log of Word Count, because of the Power Law ###
model.nb.log = glm.nb(num_tweets ~ log(wc) + emotionality + positivity, data=stories, control=glm.control(maxit=100))
summary(model.nb.log)
stargazer(model.nb.log, type="text")

# Get psuedo R^2 
pR2(model.nb.log)
