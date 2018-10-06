#  Introduction
## ══════════════

#   • Learning objectives:
##     • Learn the R formula interface
##     • Specify factor contrasts to test specific hypotheses
##     • Perform model comparisons
##     • Run and interpret variety of regression models in R

## Set working directory
## ─────────────────────────

##   It is often helpful to start your R session by setting your working
##   directory so you don't have to type the full path names to your data
##   and other files

# set the working directory
# setwd("~/Desktop/Rstatistics")
# setwd("C:/Users/dataclass/Desktop/Rstatistics")

##   You might also start by listing the files in your working directory

getwd() # where am I?
list.files("dataSets") # files in the dataSets folder

## Load the states data
## ────────────────────────

# read the states data
states.data <- readRDS("dataSets/states.rds") 
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#look at last few labels
tail(states.info, 8)

## Linear regression
## ═══════════════════

## Examine the data before fitting models
## ──────────────────────────────────────────

##   Start by examining the data to check for problems.

# summary of expense and csat columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)
# correlation between expense and csat
cor(sts.ex.sat)

## Plot the data before fitting models
## ───────────────────────────────────────

##   Plot the data to look for multivariate outliers, non-linear
##   relationships etc.

# scatter plot of expense vs csat
plot(sts.ex.sat)

## Linear regression example
## ─────────────────────────────

##   • Linear regression models can be fit with the `lm()' function
##   • For example, we can use `lm' to predict SAT scores based on
##     per-pupal expenditures:

# Fit our regression model
sat.mod <- lm(csat ~ expense, # regression formula
              data=states.data) # data set
# Summarize and print the results
summary(sat.mod) # show regression coefficients table

## Why is the association between expense and SAT scores /negative/?
## ─────────────────────────────────────────────────────────────────────

##   Many people find it surprising that the per-capita expenditure on
##   students is negatively related to SAT scores. The beauty of multiple
##   regression is that we can try to pull these apart. What would the
##   association between expense and SAT scores be if there were no
##   difference among the states in the percentage of students taking the
##   SAT?

summary(lm(csat ~ expense + percent, data = states.data))

## The lm class and methods
## ────────────────────────────

##   OK, we fit our model. Now what?
##   • Examine the model object:

class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]

##   • Use function methods to get more information about the fit

confint(sat.mod)
# hist(residuals(sat.mod))

## Linear Regression Assumptions
## ─────────────────────────────────

##   • Ordinary least squares regression relies on several assumptions,
##     including that the residuals are normally distributed and
##     homoscedastic, the errors are independent and the relationships are
##     linear.

##   • Investigate these assumptions visually by plotting your model:

par(mar = c(4, 4, 2, 2), mfrow = c(1, 2)) #optional
plot(sat.mod, which = c(1, 2)) # "which" argument optional

## Comparing models
## ────────────────────

##   Do congressional voting patterns predict SAT scores over and above
##   expense? Fit two models and compare them:

# fit another model, adding house and senate as predictors
sat.voting.mod <-  lm(csat ~ expense + house + senate,
                      data = na.omit(states.data))
sat.mod <- update(sat.mod, data=na.omit(states.data))
# compare using the anova() function
anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

## Exercise: least squares regression
## ────────────────────────────────────────

##   Use the /states.rds/ data set. Fit a model predicting energy consumed
##   per capita (energy) from the percentage of residents living in
##   metropolitan areas (metro). Be sure to
##   1. Examine/plot the data before fitting the model
##   2. Print and interpret the model `summary'
##   3. `plot' the model to look for deviations from modeling assumptions

#subsetting the data
nrg.metro <- subset(states.data, select = c("energy", "metro"))
#examining
summary(nrg.metro)
##50 states plus District of Columbia (DC) makes 51 observations.

nrg.metro.no <- na.omit(nrg.metro)
##for metro, DC has NA because it is itself a metro area

summary(nrg.metro.no)
plot(nrg.metro.no)  
#Seems to be a linear with 4 outliers
cor(nrg.metro.no)
##Correlation between energy and metro is negative 0.3397.

mod.nrg.met <- lm(energy ~ metro, data = nrg.metro.no)

summary(mod.nrg.met)
## regression equation:  energy = 501.0292 - 2.2871(metro)
##is not a good model with an R-squared of only 0.1154. 
##Metro is not strongly significant at the 0.05 level and the standard error is large relatively.

hist(residuals(mod.nrg.met))
##The residuals are not normally distributed violating the assumptions for OLS but clearly metro
## does not have a lot of predictive value for per capita energy consumption due to its R-squared.


##   Select one or more additional predictors to add to your model and
##   repeat steps 1-3. Is this model significantly better than the model
##   with /metro/ as the only predictor?
head(states.info, 13)
#subsetting the data for second model
nrg.metgrn <- subset(states.data, select = c("energy", "metro", "green"))
#examining
summary(nrg.metgrn)
nrg.metgrn.no <- na.omit(nrg.metgrn)
summary(nrg.metgrn.no)
#The plot() generated visually told me very little.
#Need to load ggplot for more in depth look at the two dependent variables.

cor(nrg.metgrn.no)
##Correliation of energy and green is 0.7706 given metro's negative correlaion of 0.3117.

mod.nrg.metgrn <- lm(energy ~ metro + green, data = nrg.metgrn.no)
summary(mod.nrg.metgrn)
## Energy(btus per capita) = 203.8 + .0328(metro) + 5.4821(green)
##metro variable has dropped to not significant and green variable is highly signifcant. 
##With an Multi-R-squared of 0.5939 the model is better than the metro R-sq = 0.1154 