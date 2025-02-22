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

## We will first look at the dataset ChickWeight, which is already 
##    loaded in base R. Check out the help page of the data set to understand 
##    how the data was collected and look at the summary.
?ChickWeight

## Let's plot the data. 
##     Group the data by Diet and Time. Use the function summarySE() 
##       from Rmisc library to get the mean and se of weight. 
##       Assign the resulting dataset to aggData.
install.packages("Rmisc")
library(Rmisc)
aggData<- summarySE(data=ChickWeight, measurevar="weight", groupvars = c("Diet","Time"))

##     Create a line plot of aggData, plotting weight on the y axis, time on the x-axis and color by Diet. 
##       Also add errorbars (mean+/-1.96*se)
ggplot(aggData, aes(x=Time,y=weight, color = Diet))+ geom_line() +
  geom_errorbar(aes(ymin = weight - 1.96 * se, ymax = weight + 1.96 * se), width = 0.2)+
  labs(x = "Time (days)", y = "Weight (g)", color = "Diet")
## The above plot ignored that the data comes from a fixed set of chicks. Let's look at individual growth
##    by coloring by Chick instead of by Diet, but faceting by Diet (side by side). You have to use ChickWeight 
##    instead of aggData here! Also you don't need error bars, because you are not plotting the mean, but the
##    actual data
ggplot(ChickWeight, aes(x = Time, y = weight, color = Chick)) +
  geom_line() +
  facet_wrap(~ Diet, ncol = 2) + 
  labs(x = "Time (days)", y = "Weight (g)", color = "Chick")

## d) What do you observe, looking at c? 

    # We can see the different weight growth trajectories of the Chicks with 
    # different diets. We observe that some chicks had steady weight gain while some
    # gained weight initially but stopped after some time and weight became constant.
    # We can also see that in diet 4, we get a less varying weight gain trajectory than
    # other diets.



##  Run the model using lmer() and assign it to chickmod
model <- weight ~ Time * Diet + (1 + Time | Chick)
chickmod <- lmer(model, data = ChickWeight)



##  Rerun the model leaving out the interaction between Time and Diet and assign it to chicknull

model_null <- weight ~ Time + Diet + (1 + Time | Chick) 
chicknull <- lmer(model_null, data=ChickWeight) 

## compare the two models using the anova() function, which performs a likelihood ratio test

anova(chickmod, chicknull)



  # the p-value is 0.0012 which is lower than 0.05 so the interaction between 
  # time after birth and diet is statistically significant, which means it has an
  # effect on the rate of weight gain. We can reject null hypothesis that the interaction
  # has no significant effect on the rate of weight gain.

##The following code creates a plot of all chick specific intercepts and slopes.
print(dotplot(ranef(chickmod,condVar=TRUE),  scales = list(x = list(relation = 'free')))[["Chick"]])

