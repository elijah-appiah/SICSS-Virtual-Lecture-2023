##########################################################################
#                                                                        #
#             SOCIAL SCIENCE DATA MODELING WITH R                        #
#                                                                        #
##########################################################################

# PACKAGES

install.packages("tidyverse")        # packages for working with data
library(tidyverse)

install.packages("summarytools")     # descriptive statistics
library(summarytools)

install.packages("visdat")           # visualize missing values
library(visdat)

install.packages("dlookr")           # visualize and impute missing values
library(dlookr)

install.packages("missRanger")       # impute missing values
library(missRanger)

install.packages("mfx")
library(mfx)


#######################   LINEAR REGRESSION ANALYSIS   ###########################

# DATASET:  CAR RESALE DATA - 2023 FROM KAGGLE

# Source: https://www.kaggle.com/datasets/rahulmenon1758/car-resale-prices

# This dataset contains Car resale prices all over the cities from India, 
# updated as of August 2023. This dataset contains information in 
# raw/unclean format, to provide Hands-on experience of working with 
# real-life data.

# Key Features:
#   full_name: Name of the car along with model
#   resale_price: Resale price of the car
#   registered_year: Year the car was registered
#   engine_capacity: Engine Displacement of car (cc)
#   insurance: Type of insurance made available for the car (if any)
#   transmission_type: Transmission type of the car
#   kms_driven: Total kilometers the car was driven for
#   owner_type: Number of owners who previously owned the car
#   fuel_type: Type of fuel the car uses
#   max_power: Maximum power of the car (bhp)
#   seats: Number of seats the car has
#   mileage: Mileage of the car
#   body_type: Body configuration of the car
#   city: City in India the car is sold in

resale <- read.csv(file.choose())   # base R

# Note: The file.choose() function used as an argument in read.csv()
#       allows you to interactively navigate and locate the data file
#       to import into R


## Check the structure of the data frame

str(resale)


## DATA CLEANING

## Remove the first column as it is not necessary.

resale <- resale %>% dplyr::select(-X)


# The "insurance" column has some values among the categories.
# We need to recode those values ("", "1", "2") to "Not Available".

resale %>% reframe(unique_values = unique(insurance))

resale <- resale %>% 
  mutate(insurance = ifelse(insurance %in% c("", "1", "2"), "Not Available", insurance))


# We need to clean some columns and make them numeric after removing some
# characters attached to those numbers. Again, the columns that are recognized
# as text but can serve as categorical variables need to be converted to 
# factor data type. Factor data types are recognized by R as categorical in nature.
# All of that is done in the code below.


resale <- resale %>% 
  mutate(resale_price =  as.numeric(str_remove_all(resale_price, "[₹Lakh]")),
         registered_year = as.numeric(registered_year),
         kms_driven = as.numeric(str_remove_all(kms_driven, "[,Kms]")),
         max_power = as.numeric(str_remove_all(max_power, "[bhp]")),
         mileage = as.numeric(str_remove_all(mileage, "[kmpl]")),
         insurance = as.factor(insurance),
         transmission_type = as.factor(transmission_type),
         owner_type = as.factor(owner_type),
         fuel_type = as.factor(fuel_type),
         body_type = as.factor(body_type),
         city = as.factor(city),
         engine_capacity = as.numeric(str_remove_all(engine_capacity, "[cc]")))
         
# Note: Some columns are character data types as recognized in R.
# But we need to have them in our model as categorical variables and that
# mean we have convert them from character to "factor" data types.
# The factor() function is used to create factors (categorical variables), and
# so the as.factor() function makes it much easier.


## Are there missing values?

plot_na_pareto(resale)    # "dlookr" package
vis_miss(resale)          # "visdat" package



# Remove the "registered_year" column because it has more than
# 70% missing data, and after that, drop all missing values.

resale <- resale %>% dplyr::select(-registered_year)

resale <- resale %>% drop_na()



#..........   Simple Linear Regression

# Dependent variable: resale_price
# Independent variable: max_power

s_model <- lm(resale_price ~ max_power, data = resale)
summary(s_model)



#..........   Multiple Linear Regression
# Dependent variable: resale_price
# Independent variables: engine_capacity
#                        transmission_type
#                        kms_driven
#                        max_power
#                        seats
#                        mileage


m_model <- lm(resale_price ~ 
                engine_capacity + 
                transmission_type + 
                kms_driven + 
                max_power + 
                seats + 
                mileage, data = resale)

summary(m_model)

# p-value and significance level (1% [0.01], 5% [0.05], 10% [0.1])
# If the p-value is less than the significance level, reject the null
# hypothesis and conclude that the regression coefficient is significant.

# This is an example p-value from the regression results: 2e-16

format(2e-16, scientific = FALSE)  #gives the value "0.0000000000000002"

# For example, since the p-value for engine_capacity is 2e-16 and is less than
# 0.05 significance level, we reject the null hypothesis and conclude that 
# engine_capacity is statistically significant. This means that 
# engine_capacity is a significant predictor of the car's resale price.


# Let's introduce a package here called "performance"
# This package is used to diagnose regression models of some of the problems
# such as multicollinearity, heteroscedasticity, and autocorrelation.

install.packages("performance")
library(performance)

# Diagnose Multicollinearity
check_collinearity(m_model)

# Note: Multicollinearity is good since it has low and moderate correlation.
# If it was high, then we might have to drop that variable with high collinearity.

# Diagnose Heteroscedasticity
check_heteroscedasticity(m_model)

# Noet: Heteroscedasticity was detected so it has to be remedied but not here yet. Sorry!

# Diagnose Autocorrelation
check_autocorrelation(m_model)

# Noet: Autocorrelation was detected so it has to be remedied but not here yet. Sorry!


#######################   GENERALIZED LINEAR MODELS   #######################


# DATASET:   EMPLOYEE DATASET FROM KAGGLE

# This dataset contains information about employees in a company, 
# including their educational backgrounds, work history, demographics, 
# and employment-related factors. It has been anonymized to protect 
# privacy while still providing valuable insights into the workforce.

# Source:
#   https://www.kaggle.com/datasets/tawfikelmetwally/employee-dataset
# 
# Education: 
#   The educational qualifications of employees, including degree, 
#   institution, and field of study.
# 
# Joining Year: 
#   The year each employee joined the company, indicating their length of service.
# 
# City: 
#   The location or city where each employee is based or works.
# 
# Payment Tier: 
#   Categorization of employees into different salary tiers.
# 
# Age: 
#   The age of each employee, providing demographic insights.
# 
# Gender: 
#   Gender identity of employees, promoting diversity analysis.
# 
# Ever Benched: 
#   Indicates if an employee has ever been temporarily without assigned work.
# 
# Experience in Current Domain: 
#   The number of years of experience employees have in their current field.
# 
# Leave or Not: 
#   a target column



# The dataset is an excel file so install the "readxl" package

install.packages("readxl")
library(readxl)

employee <- read_excel(file.choose())

str(employee)

# Dependent Variable: LeaverOrNot
# Independent Variables: Education
#                        City
#                        Payment Tier
#                        Age
#                        Gender
#                        EverBenched
#                        ExperienceInCurrentDomain
                         

# Let's make "Education", "City", "Gender", "EverBenched" categorical by
# converting them to factor types.
# The Linear Probability Model requires the dependent variable to be numeric
# rather than categorical so let's leave it at that.
# Later on, before we run the logistic and probit regression models, then the
# dependent variable should be made categorical.

employee <- employee %>% 
  mutate(Education = as.factor(Education),
         City = as.factor(City),
         Gender = as.factor(Gender),
         EverBenched = as.factor(EverBenched))


### LINEAR PROBABILITY MODEL

lpm <- lm(LeaveOrNot ~ 
            Education +
            City +
            PaymentTier +
            Age +
            Gender +
            EverBenched +
            ExperienceInCurrentDomain,
          data = employee)

summary(lpm)


### LOGISTIC REGRESSION MODEL

employee <- employee %>% mutate(LeaveOrNot = as.factor(LeaveOrNot))

logit <- glm(LeaveOrNot ~ 
            Education +
            City +
            PaymentTier +
            Age +
            Gender +
            EverBenched +
            ExperienceInCurrentDomain,
           family = binomial(link = "logit"),
          data = employee)

summary(logit)


# Calculate the "marginal effects" (probabilities) of the Logit Model

logitmfx(LeaveOrNot ~ 
           Education +
           City +
           PaymentTier +
           Age +
           Gender +
           EverBenched +
           ExperienceInCurrentDomain,
         data = employee)
         

### PROBIT REGRESSION MODEL

probit <- glm(LeaveOrNot ~ 
               Education +
               City +
               PaymentTier +
               Age +
               Gender +
               EverBenched +
               ExperienceInCurrentDomain,
             family = binomial(link = "probit"),
             data = employee)

summary(probit)

# Calculate the "marginal effects" (probabilities) of the Probit Model

probitmfx(LeaveOrNot ~ 
           Education +
           City +
           PaymentTier +
           Age +
           Gender +
           EverBenched +
           ExperienceInCurrentDomain,
         data = employee)


### The code below is a function I've created to assess the logit and probit
#   models. You notice from the results that model diagnostics such as 
#   R-squared, overall model p-value for significance, etc... were not present.
#   Therefore, just run the function below, and simply input the model object
#   as an argument.
# Note: You don't need to understand the code now. Just run it and see what happens.

assess_glm <- function(model, sig.level = 0.05){
  n = dim(model$data)[1]
  chi = model$null.deviance - model$deviance
  df = model$df.null - model$df.residual
  p = 1 - pchisq(q = chi, df = df)
  hosmer_r2 = chi / model$null.deviance
  cox_r2 = 1 - exp((model$deviance - model$null.deviance) / n)
  nagelkerke_r2 = cox_r2 / (1 - exp(-((model$null.deviance) / n)))
  AIC = model$deviance + (2 * 14)
  BIC = model$deviance + (2 * 14) * log(n)
  
  cat(
    "The model follows a chi-square distribution with a chi-square \n",
    "statistic of ",
    round(chi,2), " and ", df, " degrees of freedom. \n\n",
    "The p-value associated with the chi-square distribution is found \n",
    "to be ", p, " which is statistically",
    ifelse(p < sig.level, " significant.", " not significant."),"\n\n",
    "The following are the R-squared values:\n\n",
    "Hosmer and Lemeshow R-squared: ", hosmer_r2,",\n",
    "Cox and Snell's R-squared: ", cox_r2,",\n",
    "Nagelkerke R-squared: ", nagelkerke_r2,".\n\n",
    "The information criteria for the model are given as:\n\n",
    "Akaike Information Criterion (AIC): ", AIC,",\n",
    "Bayesian Information Criterion (BIC): ", BIC, ".", sep = ""
  )
}


## Model Diagnostics for Logit Model

assess_glm(model = logit)


## Model Diagnostics for Probit Model

assess_glm(model = probit)



