library(tidyverse)
library(haven)
library(estimatr)

filepath <- "https://github.com/scunning1975/mixtape/raw/master/lmb-data.dta"

lmb_data <- read_dta(filepath) 

lmb_data <- lmb_data |>  
  mutate(demvoteshare_c = demvoteshare - 0.5) |> 
  filter(!is.na(score),
         !is.na(demvoteshare_c)) |> 
  select(score, demvoteshare_c, id)

lmb_data <- lmb_data %>% 
  mutate(demwin = ifelse(demvoteshare_c > 0, 1, 0))


lm_1 <- lm_robust(score ~ demvoteshare_c, data = lmb_data, clusters = id)
summary(lm_1)


#aggregating the data
binned_lmb_data <- lmb_data |> 
  mutate(dvote_bins = cut(demvoteshare_c, 
                          breaks = (-50:50)*0.01)) |> 
  group_by(dvote_bins) |> 
  reframe(demvoteshare_c = mean(demvoteshare_c),
          score = mean(score))
  


(base_graph <- ggplot() +
    geom_point(aes(x=demvoteshare_c,y=score),data=binned_lmb_data) + 
    geom_vline(xintercept = 0,linetype="dashed"))



linear_fit <- geom_smooth(aes(x=demvoteshare_c,
                              y=score, 
                              group = demwin),
                          method = "lm",
              data = lmb_data)

ggplot()+linear_fit

base_graph +linear_fit


lm_2 <- lm_robust(score ~ demwin*demvoteshare_c, 
                  data = lmb_data, 
                  clusters = id)
summary(lm_2)




lmb_data <- lmb_data |> 
  mutate(demvoteshare_sq = demvoteshare_c^2)

lm_3 <- lm_robust(score ~ demwin*demvoteshare_c + demwin*demvoteshare_sq, 
                  data = lmb_data, clusters = id)

summary(lm_3)

quadratic_fit <- geom_smooth(aes(x=demvoteshare_c, 
                                 y=score, 
                                 group = demwin), 
                             method = "lm",
                             formula = y ~ x + I(x^2),
                             data = lmb_data) 


ggplot()+quadratic_fit

base_graph+quadratic_fit

poly_fit <- geom_smooth(aes(x=demvoteshare_c, 
                                 y=score, 
                                 group = demwin), 
                             method = "lm",
                             formula = y ~ poly(x,5),
                             data = lmb_data) 

base_graph+poly_fit



loess_fit <- geom_smooth(aes(x=demvoteshare_c, 
                                         y=score, 
                                         group = demwin), 
                                     method = "loess",
                                     data = lmb_data) 

base_graph+loess_fit


# Changing the bandwidth
bw <- 0.10
lmb_restricted <- lmb_data |>  filter(abs(demvoteshare_c)<bw)

loess_fit <- poly_fit <- geom_smooth(aes(x=demvoteshare_c, 
                                         y=score, 
                                         group = demwin), 
                                     method = "loess",
                                     data = lmb_restricted) 
base_graph+loess_fit

# using weights

lmb_data <- lmb_data |> 
  mutate(weights = 1-2*abs(demvoteshare_c))

lmb_data |> ggplot() + geom_line(aes(x=demvoteshare_c,y=weights))

linear_fit_weight <- geom_smooth(aes(x=demvoteshare_c,
                              y=score, 
                              weight = weights,
                              group = demwin),
                          method = "lm",
                          data = lmb_data,
                          )

base_graph +linear_fit_weight



loess_fit <-  geom_smooth(aes(x=demvoteshare_c, 
                                         y=score, 
                                         weight = weights,
                                         group = demwin), 
                                     method = "loess",
                                     data = lmb_data) 

base_graph+loess_fit


# Using RD Robust

library(rdrobust)

rdr <- rdrobust(y = lmb_data$score,
                x = lmb_data$demvoteshare_c)
summary(rdr)

rdplot(lmb_data$score,lmb_data$demvoteshare_c)

# McCrary Test
library(rddensity)

density <- rddensity(lmb_data$demvoteshare_c)

summary(density)

rdplotdensity(density, lmb_data$demvoteshare_c)
