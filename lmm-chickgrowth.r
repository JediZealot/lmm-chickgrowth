

##############################################################################
# Linear Mixed Effects Models
##############################################################################


###############################################################################
###############################################################################


rm(list = ls())

library(languageR)
library(lme4)
library(lattice)
library(Matrix)
library(ggplot2)
library(dplyr)


###############################################################################
### 1. Linear mixed model for chicken growth 
###############################################################################

## a) We will first look at the dataset ChickWeight, which is already 
##    loaded in base R. Check out the help page of the data set to understand 
##    how the data was collected and look at the summary.
?ChickWeight

## b) Let's plot the data. 
##    1. Group the data by Diet and Time. Use the function summarySE() 
##       from Rmisc library to get the mean and se of weight. 
##       Assign the resulting dataset to aggData.
install.packages("Rmisc")
library(Rmisc)
aggData<- summarySE(data=ChickWeight, measurevar="weight", groupvars = c("Diet","Time"))

##    2. Create a line plot of aggData, plotting weight on the y axis, time on the x-axis and color by Diet. 
##       Also add errorbars (mean+/-1.96*se)
ggplot(aggData, aes(x=Time,y=weight, color = Diet))+ geom_line() +
  geom_errorbar(aes(ymin = weight - 1.96 * se, ymax = weight + 1.96 * se), width = 0.2)+
  labs(x = "Time (days)", y = "Weight (g)", color = "Diet")
## c) The above plot ignored that the data comes from a fixed set of chicks. Let's look at individual growth
##    by coloring by Chick instead of by Diet, but faceting by Diet (side by side). You have to use ChickWeight 
##    instead of aggData here! Also you don't need error bars, because you are not plotting the mean, but the
##    actual data
ggplot(ChickWeight, aes(x = Time, y = weight, color = Chick)) +
  geom_line() +
  facet_wrap(~ Diet, ncol = 2) + 
  labs(x = "Time (days)", y = "Weight (g)", color = "Chick")

## d) What do you observe, looking at c? (2-5 sentences!)

    # We can see the different weight growth trajectories of the Chicks with 
    # different diets. We observe that some chicks had steady weight gain while some
    # gained weight initially but stopped after some time and weight became constant.
    # We can also see that in diet 4, we get a less varying weight gain trajectory than
    # other diets.

## e) We want to investigate whether the type of diet has an effect on the chick's growth, i.e. we are
##    looking for an interaction between time after birth and the diet type. Before running the model,
##    specify:

##  1) What fixed effect(s) do you enter into the model?

    # (weight~time*diet)

##  2) what random effect(s) should be included to account for the repeated measures structure of the data?
    
    # (1 | Chick)

##  3) In addition to random intercept(s), which random slope(s) should you add to get a maximal model?
    
    # (1 + time| Chick)

## f) Run the model you specified in e) using lmer() and assign it to chickmod
model <- weight ~ Time * Diet + (1 + Time | Chick)
chickmod <- lmer(model, data = ChickWeight)


#########################################################################
##  The remainder of this sheet is optional!
#########################################################################

## g*) Rerun the model leaving out the interaction between Time and Diet and assign it to chicknull

model_null <- weight ~ Time + Diet + (1 + Time | Chick) 
chicknull <- lmer(model_null, data=ChickWeight) 

## h*) compare the two models using the anova() function, which performs a likelihood ratio test

anova(chickmod, chicknull)

## i*) Report the p-value (from h) and the conclusion with respect to the research hypothesis

  # the p-value is 0.0012 which is lower than 0.05 so the interaction between 
  # time after birth and diet is statistically significant, which means it has an
  # effect on the rate of weight gain. We can reject null hypothesis that the interaction
  # has no significant effect on the rate of weight gain.

## j*) The following code creates a plot of all chick specific intercepts and slopes. What do you see?
print(dotplot(ranef(chickmod,condVar=TRUE),  scales = list(x = list(relation = 'free')))[["Chick"]])

