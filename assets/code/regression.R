library(tidyverse)

## Regression Example
set.seed(1)

tb <- tibble(
  x = rnorm(10000),
  u = rnorm(10000),
  y = 5.5*x + 12*u
) 

reg <- lm(y ~ x, data = tb)

summary(reg)

betas <- reg$coefficients

tb <- tb %>% 
  mutate(
    yhat1 = predict(reg),
    yhat2 = betas[1] + betas[2]*x, 
    uhat1 = residuals(reg),
    uhat2 = y - yhat2
  )

summary(tb)


tb %>% 
  ggplot(aes(x=x, y=y)) + 
  geom_point(size = 0.05, color = "black", alpha = 0.5) +
  geom_abline(intercept = betas[1],slope=betas[2], color = "black")

tb %>% 
  ggplot(aes(x=x, y=y)) + 
  geom_point(size = 0.05, color = "black", alpha = 0.5) +
  geom_smooth(method = lm, color = "black")

## Residuals are not correlated with predicted values

tb %>% 
  ggplot(aes(x=yhat1, y=uhat1)) +
  geom_point(size = 0.05, color = "black", alpha = 0.5) +
  geom_smooth(method = lm, color = "black")



## Simulation to check unbiasedness

# Set simulation parameters
n_simulations <- 1000      # Number of simulations to run
n_observations <- 1000    # Number of observations per simulation
true_intercept <- 3        # True intercept in the model
true_slope <- 2            # True slope in the model
x_sd <- 9                  # Standard deviation for x
error_sd <- 36             # Standard deviation for error term

# Create an empty list to store simulation results
estimated_intercepts <- numeric(n_simulations)
estimated_slopes <- numeric(n_simulations)

# Run the simulations
for (sim in 1:n_simulations) {
  # Generate the data
  sim_data <- tibble(
    x = rnorm(n_observations, mean = 0, sd = x_sd),
    u = rnorm(n_observations, mean = 0, sd = error_sd),
    y = true_intercept + true_slope * x + u
    )
  
  # Fit the linear model
  model <- lm(y ~ x, data = sim_data)
  
  # Store the model in the results list
  estimated_intercepts[sim] <- coef(model)[1]
  estimated_slopes[sim] <- coef(model)[2]
  
}

# Create a data frame for plotting
coef_df <- tibble(
  estimated_slopes = estimated_slopes,
  estimated_intercepts = estimated_intercepts
)

# Plot the distribution
ggplot(coef_df, aes(x = estimated_slopes)) +
  geom_histogram(bins = 50, fill = "blue", alpha = 0.7) +
  geom_vline(xintercept = true_slope, color = "red", linetype = "dashed") +
  labs(
    title = "Distribution of Estimated Slope Coefficients",
    subtitle = paste("True Slope =", true_slope),
    x = "Estimated Slope",
    y = "Frequency"
  ) +
  theme_minimal()


#####################################
# Multiple regression
#####################################

library(causaldata)
library(estimatr)

data <- causaldata::cps_mixtape

summary(data)

lm(re74~educ, data=data) %>% summary()

lm(re74~educ+black+hisp+age+marr, data=data) %>% summary()

lm_robust(re74~educ+black+hisp+age+marr, data=data) %>% summary()

lm_robust(re74~educ+I(educ^2)+black+hisp+age+marr, data=data) %>% summary()

lm_robust(re74~educ+I(educ^2)+I(educ^3)+black+hisp+age+marr, data=data) %>% summary()

# Pruebas de hipotesis

library(car) # para linearHypothesis
library(gmodels) # para estimable

reg <- lm_robust(re74~educ+I(educ^2)+I(educ^3)+black+hisp+age+marr, data=data)
linearHypothesis(reg, "black - hisp =0")

linearHypothesis(reg, c("educ=0","I(educ^2)=0","I(educ^3)=0") )


reg <- lm(re74~educ+black+hisp+age+marr, data=data)
estimable(reg,c("(Intercept)"=1, 
                "educ"=12, 
                "black"=0,
                "hisp"=1,
                "age"=40,
                "marr"=0))


###
## Working with interactions, polynomials, logs


lm_robust(re74~educ+black+hisp+age+marr, data=data) %>% 
summary()


lm_robust(re74~educ+black+hisp+age+marr + marr*black + marr*hisp, data=data) %>% 
  summary()


lm_robust(re74~black+hisp+age+marr+educ+educ*black+educ*hisp , data=data) %>% 
  summary()


lm_robust(re74~black+hisp+age+marr+educ+educ*age , data=data) %>% summary()


### Probit, Logit

lm_robust(marr ~ black + hisp + age + educ, data = data) %>% 
  summary()

logit <- glm(marr ~ black + hisp + age + educ, data = data, 
             family = binomial(link="logit"))
summary(logit)


probit <- glm(marr ~ black + hisp + age + educ, data = data, 
             family = binomial(link="probit"))
summary(probit)


library(margins)

margins_logit <- margins(logit)  # Adjust for variables of interest
summary(margins_logit)

margins_probit <- margins(probit)  # Adjust for variables of interest
summary(margins_probit)

data$predictlogit<-predict(logit, type = "response")
data$predictprobit<-predict(probit, type = "response")


ggplot(data) + geom_point(aes(x=predictlogit,y=predictprobit),alpha=0.2) + 
  geom_abline(intercept=0,slope=1, color="blue")


lpm <- lm(marr ~ age,data=data)
logitmodel <- glm(marr ~ age,data=data, family=binomial(link="logit"))

data$lpm_yhat <- predict(lpm,type = "response")
data$logit_yhat <- predict(logitmodel,type = "response")

ggplot(data)+geom_point(aes(x=age,y=lpm_yhat),color="blue")+
  #geom_point(aes(x=age,y=predictlogit),color="green") + 
  geom_point(aes(x=age,y=logit_yhat),color="red") 
