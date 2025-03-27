## Homework 4 Submission ## 
#Fitting GLMs



## Question 1 a: Fit a glm assessing evidence for the following hypothesis: Seedling density is increased beneath trees experiencing mistletoe infection. Describe the rationale you used in selecting a glm structure and probability distribution. Calculate model fit using MAE.

#loading data
library(readr)
mistletoes <- read_csv("mistletoes.csv")
View(mistletoes)

#Installing packages 

#install.packages(c("pROC", "MLmetrics", "marginaleffects", " modelr", "MASS", "performance"))

#Loading packages 

library(pROC)
library(MLmetrics)
library(marginaleffects) # Optional!
library(ggplot2) # Optional!
library(modelr) #
library(MASS) 
library(performance) 

 
##Hypothesis: testing the effect of mistletoe infection on Seedling density

#fitting the model 

model_mistletoe.poisson <- glm(Seedlings ~ Treatment, data= mistletoes, family="poisson"(link="log"))


summary(model_mistletoe.poisson) #Veiwing th emodel

coef(model_mistletoe.poisson)


 # Assess the fitness of the model

performance_mae(model_mistletoe.poisson) # The model performs really poorly
 

#checking dispersion 
mean(mistletoes$Seedlings)
var(mistletoes$Seedlings)

check_overdispersion(model_mistletoe.poisson)
#the data is Overdispersed therefore i cannot continue with Poisson 
#Negeative binomial is the best alternaitve


##Fiting negative binomial 

mistletoe.negativebinomial <-glm.nb(Seedlings ~ Treatment, data=mistletoes)
#Note to me: i am not including the interaction (of different varibales) because it is not included on the hypothesis

summary(mistletoe.negativebinomial)

## Compare results to our poisson fit:
summary(mistletoe.negativebinomial)


#Calculate model fit using MAE
performance_mae(mistletoe.negativebinomial)
#MAE = 145.841 therefore the prediction is off by 146



## 1b)  Use visual (e.g. a marginal effects plot) and written (e.g. effect sizes on scale of response) approaches to interpret the results of your model. 
#Based on your fitted model results and model fit,write 3-4 sentences to discuss the following biological conclusions:  
 
#Does mistletoe infection alter seedling density? How much does seedling recruitment differ beneath parasitized and unparasitized trees? Explain which elements of your glm results informed your conclusions and annotate the steps you needed to take to interpret your parameters. 


# Save the coefficients as vectors/object in the environment

intercept_mistletoe <- summary(mistletoe.negativebinomial)$coefficients["(Intercept)", "Estimate"] # getting tehn coefficient that represents the seedling density in parasitized trees.

slope_unparasitized <- summary(mistletoe.negativebinomial)$coefficients["Treatmentunparasitized", "Estimate"] #  getting slope that represents the seedling density in unparasitized trees relative to the intercept (i.e parasitized trees)

## Using the link function exp() to help me make understand the coefficients

# Species density of Parasitized trees

exp(intercept_mistletoe+slope_unparasitized*0) 
# = 308

exp(intercept_mistletoe) #also gives the answer same as above


# Species density of unparasitized trees

exp(intercept_mistletoe+slope_unparasitized*1) 
# = 13

#Biological intercept:#Does mistletoe infection alter seedling density?

# Yes, mistletoe infection alter seedling density. This is because, the Seedling density in parasitized plants (308) is significantly greater than unparasitized plants (13). The low p-value  <0.05 is statistically significant  

# How much does seedling recruitment differ beneath parasitized and unparasitized trees?

# I can calculate this by finding the difference between those two seedling densities

round((exp(intercept_mistletoe)) - (exp(intercept_mistletoe+slope_unparasitized*1)))
# = 295

#Answer:Seedling recruitment beneath parasitized and unparasitized treatment trees differ by 295 seedlings. Parasitised plants having more seedlings than unparasitized plants. 

## ASW: wonderful!! Nice work!
 
#Explain which elements of your glm results informed your conclusions and annotate the steps you needed to take to interpret your parameters. 

#Answer: The coefficient (intercept, slope, exp()) of the model informed the conclusion 


#visualizing the plot 
plot_predictions(mistletoe.negativebinomial, condition="Treatment")

 


## 1c) During the course of this study, 2012 was an atypically rainy year, compared to 2011. Fit an additional glm that quantifies how the effect of mistletoe differs between the two years in this study. Write ~2 new sentences that summarize the results of the new model and their biological implications

#Note to me: here we now have an interaction term therefore we can include the (year)

#FITTING THE MOSDEL WITH THE interaction teraction


mistletoes$Year <- as.factor(mistletoes$Year) #turning the data (year) into  factor so that i can get 2 different years 

mistletoe.negativebinomial.yr <-glm.nb(Seedlings ~ Treatment + Treatment*Year, data= mistletoes)

summary(mistletoe.negativebinomial.yr)
coef(mistletoe.negativebinomial.yr)

######
#Save the coefficient for different years 

intercept_mistletoe_2 <- summary(mistletoe.negativebinomial.yr)$coefficients["(Intercept)", "Estimate"] # This is the coefficient that represents the seedling density of parasitized trees in 2011

slope_unparasitized_2 <- summary(mistletoe.negativebinomial.yr)$coefficients["Treatmentunparasitized", "Estimate"] #the coefficient that represents the seedling density of unparasitized trees in 2011 relative to the intercept (i.e parasitized trees in 2011). The negative value means that there is a decrease of seedling density of parasitized and unparaticied trees in 2011

slope_parasitized_2012 <- summary(mistletoe.negativebinomial.yr)$coefficients["Year2012", "Estimate"] # the coefficient that represents the seedling density of parasitized trees in 2012 relative to the intercept (i.e parasitized trees in 2011). The positive value means that there is a slight increase in the seedling density in the more rainy season. but there eis no statistical significance because pvalue >0.05

slope_unparasitized_2012 <- summary(mistletoe.negativebinomial.yr)$coefficients["Treatmentunparasitized:Year2012", "Estimate"] # This is the coefficient that represents the seedling density of unparasitized trees in 2012 relative to the intercept (i.e parasitized trees in 2011). The positive value means that there is a slight increase in the seedling density.


#biological explanation cont


# new formula with interaction variables = exp(b0 + b1X1 + b2X2 + b3*(X1*X2))

# using Seedling density of parasitized trees in 2011 (keeping 2012 and unparatisized treatment in 2011 at constant)

exp(intercept_mistletoe_2 + slope_unparasitized_2*0 + slope_parasitized_2012*0 + slope_unparasitized_2012*0)
#218
# There is 218 Seedling density in parasitized plot in 2011

######getting  Seedling density of unparasitized trees in year 2011 ####

exp(intercept_mistletoe_2 + slope_unparasitized_2*1 + slope_parasitized_2012*0 + slope_unparasitized_2012*0)
#=5
# There are 5 Seedling density in unparasitized plot in 2011


# Seedling density of parasitized trees in the year 2012 (keeping 2011 at zero as orignially absorbed in the model intercept)

exp(intercept_mistletoe_2 + slope_unparasitized_2*0 + slope_parasitized_2012*1 + slope_unparasitized_2012*0)
#=398
# therefore there is 398 Seedling density in parasitized plot in 2012


# Seedling density of unparasitized trees in the year 2012 

exp(intercept_mistletoe_2 + slope_unparasitized_2*1 + slope_parasitized_2012*1 + slope_unparasitized_2012*1)
#=21
# There is 21 Seedling density in unparasitized plot in 2012. therefore there is an increase from 5 seedlings that were present in the same plots in 2011. Biologically this means that rainfall has a slightly positive effect on the number of seedlings.

## ASW: great work here, Tshia! This is awesome!

#Write ~2 new sentences that summarize the results of the new model and their biological implications:

#Since in the year 2012 there was more rainfall. the results shows a positve coefficient value for both pararitized and unparasitixed trees in 2012 when the were was more rainfall compared to 2011.

## ASW: Right, but what does the interaction indicate? The positive interaction suggests that the difference between parasitized and unparasitized trees is slightly reduced.



#visualizing the plot 
 

plot_predictions(mistletoe.negativebinomial.yr, condition=c("Treatment", "Year"))

## ASW: nice work! 27/30

##########################################################################


## Question 2:

## 2a)Fit a glm (using a probability distribution of yourchoice) that reflects the following research question (including thinning as your only predictor and mortality as your response): Do forest thinning treatments reduce the probability of tree mortality? Interpret the results of the glm by writing 2-4 sentences about the biological significance of the effect of thinning, including descriptions of the sizes of the effect on the scale of the response variable, evidence for/against the hypothesis, and a metric of model fit.


#Loading data
library(readr)
treemortality <- read_csv("treemortality.csv")
View(treemortality)


#here i am fitting a generalized linear model (GLM) using a binomial distribution because the response variable (mortality) is binary (0 = survived, 1 = died).


glm_model <- glm(mortality ~ thinning, data = treemortality, family = binomial)

# Summary of the model
summary(glm_model)

# Extract the coefficients for interpretation
exp(coef(glm_model))  ## ASW: this is on the odds ratio scale, so it's a little bit of a different interpretation. 


#Explanation

#The results suggests that thinning is an effective way to help trees survive wildfires. Thinning significantly reduces tree mortality in wildfires (p < 0.001). Trees in thinned areas are 84% less likely to die compared to trees in unthinned areas. 

## ASW: odds are 84% lower in the thinned group, compared to the unthinned group. We will talk about odds interpretation in class!



## 2b)  The researchers explicitly considered the potential for confounding relationships related to tree size in their design and randomized their post-fire sampling by tree size. Given this information, do the researchers need to incorporate tree size into their glm to accurately estimate the effect of thinning? Why or why not?

#####Answer:
#No - because when they returned to re-sample they re-sampled 1000 trees in a randomized fashion using tree size, to ensure that small and large stems were recorded equally commonly across thinned and unthinned forests.Therefore the researchers do not need to incorporate tree size in the glm model because they already randomized their sampling by tree size, eliminating its confounding effect

## 2c) 


glm_updated_2a <- glm(mortality ~ thinning + roaddist + slope, family = binomial, data = treemortality)

summary(glm_updated_2a)


#Modifying model interpretation from 2a

#In the previous model (from Model 2a): Thinning had a strong negative effect on mortality (Estimate = -1.8559), suggesting that thinning reduced tree mortality in wildfires.

#second updated model (from 2c): included road-distance and slope as confounders):

###The effect of thinning weakened (reduced)(Estimate = -0.9163), though it remains statistically significant (p < 0.001).
#from the updated model, slope and road distance both significantly increase tree mortality (positive estimates: roaddist=0.54, slope= 0.82). This suggests that trees on steeper slopes and those far from roads have a higher mortality rater (high chance of dying in the wildfire)


## ASW: Great! What does that change in effect size look like on the probability scale? And why did it change so much? The key thing here is that slope and distance from roads are biasing the effect of thinning in the first model, making it appear more effective than it is because of the fact that thinning treatments are more likely to occur in locations where fire severity is already lower (closer to roads, on shallower slopes). The predicted effect of thinning in the first model is a decrease in mortality from 73% to 29%, but in the second model, this effect decreases (Mortality decreases from 54% to 29%)

## 17/20


## ASW: nice job! 44/50

