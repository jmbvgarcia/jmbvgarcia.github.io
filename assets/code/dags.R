library(tidyverse)
library(stargazer)

tb <- tibble(
  female = ifelse(runif(10000)>=0.5,1,0),
  ability = rnorm(10000),
  discrimination = female,
  occupation = 1 + 2*ability + 0*female - 2*discrimination + rnorm(10000),
  wage = 1 - 1*discrimination + 1*occupation + 2*ability + rnorm(10000) 
)

lm_1 <- lm(wage ~ female, tb)
lm_2 <- lm(wage ~ female + occupation, tb)
lm_3 <- lm(wage ~ female + occupation + ability, tb)

stargazer(lm_1,lm_2,lm_3, type = "text", 
          column.labels = c("Biased Unconditional", 
                            "Biased",
                            "Unbiased Conditional"))





library(viridisLite)

tb <- tibble(
  talent = rnorm(10000),
  work_ethic = rnorm(10000),
  success = talent + work_ethic + 0.5*rnorm(10000)
)

tb %>% ggplot()+geom_point(aes(x=talent,y=work_ethic,color=success)) +
  scale_color_viridis_c()


tb %>% ggplot()+geom_point(aes(x=talent,y=work_ethic,color=success)) +
  scale_color_viridis_b(n.breaks=6)

lm1 <- lm(talent~work_ethic, data=tb) 

lm2 <- lm(talent~work_ethic + success, data=tb) 

stargazer(lm1,lm2)
