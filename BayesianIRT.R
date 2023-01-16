#READ PAPER: https://www.researchgate.net/publication/367180753_Intersectionality_in_the_Autism_Quotient_A_Bayesian_Explanatory_Item_Response_Theory_Analysis

library(bayesplot)
library(rjags)
library(rethinking)
library(dplyr)
library(tidyr)
library(tidyverse)
library(tidybayes)
library(ggplot2)
library(MCMCvis)
library(brms)
install.packages("bayestestR")
library(bayestestR)
install.packages("see")
library(see)
#Code for Final
#Name: Palmy Chomchat Silarat
#Data can be downloaded directly from: https://osf.io/qn2es/

#data preparation
data <- read.csv("~/Desktop/Academia/datasets/aqage.csv")
dat <- data %>% filter(diagnosed_asd == ("ASD-")) %>% drop_na()
dat$gender <- ifelse(dat$gender == "Female", "Non-male", "Male")
dat$race01 <- ifelse(dat$race01 == "Not-White", "Non-white", "White")

dat$gender <-factor(dat$gender)
dat$race01 <-factor(dat$race01)
dat$X <- factor(dat$X)
str(dat)
# dat <- dat %>% mutate(gender = (case_when(
#   gender == "Female" ~ 1,
#   gender == "Male" ~ 0
# ))) %>% mutate(race01 = (case_when(
#   race01 == "White" ~ 0,
#   race01 == "Not-White" ~ 1
# )))
# dat$gender <- as.integer((dat$gender))
# dat$race01 <- as.integer((dat$race01))


##Prepare data for analysis

datdif <- dat[5:54]
names <- c()
for (i in 1:50) {
  if (i < 10) {
    Item <- paste("Item",gsub(" ", "", paste("0", i )))
  } else {
    Item <- paste("Item", i)
  }
  names[length(names) + 1] <- Item
}
names(datdif) <- names

datdif$gender <- dat$gender
datdif$race <- dat$race01
datdif$id <- dat$X

#Reshape to long format
datdif <- datdif %>% pivot_longer(cols = 1:50)
colnames(datdif) <- c("gender", "race", "person", "item", "y")
#datdif$y <- factor(datdif$y, order = TRUE, levels = c(0, 1))
datdif$y <-  factor(datdif$y)
datdif$item <- factor(datdif$item)

str(datdif)

#model 1 person explanatory
formula1 = y ~ 1 + (1 | item) + (1 | person) + race + gender #coef of gender and race wont vary across person
prior1 <- set_prior("inv_gamma(1,1)", class = "sd", group = "item") +
  set_prior("inv_gamma(1,1)", class = "sd", group = "person")

fit_mod1 <- brm(
  formula = formula1, 
  data = datdif,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior1,
  iter = 5000,
  chains = 3,
  warmup = 1000
)


summary(fit_mod1)
write.csv(rhat(fit_mod1),"~/Desktop/Academia/RI/mod/rhat_mod1.csv", row.names = TRUE)
write.csv(get_variables(fit_mod1),"~/Desktop/Academia/RI/mod/var_mod1.csv", row.names = TRUE)

pp_check(fit_mod1, ndraws = 100)

post1 <- posterior_samples(fit_mod1)
write.csv(post1,"~/Desktop/Academia/RI/mod/posterior1.csv", row.names = TRUE)

post1 %>% 
  ggplot(aes(x = b_Intercept, y = 0)) +
  stat_halfeye() +
  scale_y_continuous(NULL, breaks = NULL)

post1 %>% 
  ggplot(aes(x = b_gender, y = 0)) +
  stat_halfeye() +
  scale_y_continuous(NULL, breaks = NULL)

plot(conditional_effects(fit_mod1, "gender"))
plot(conditional_effects(fit_mod1, "race"))
plot(conditional_effects(fit_mod1, "gender:race"))


table_coef_mod1 <- fit_mod1 %>%
  gather_draws(b_Intercept, b_genderNonMmale, b_raceWhite) %>%
  median_qi()
write.csv(table_coef_mod1,"~/Desktop/Academia/RI/mod/table_coef_mod1.csv", row.names = TRUE)

fit_mod1 %>%
  spread_draws(r_item[,Intercept]) %>%
  median_qi()

#posterior predictive test
mod1_pdirect <- as.data.frame(p_direction(fit_mod1))
mod1_pdirect <- mod1_pdirect[,1:2]
mod1_pdirect <- mod1_pdirect %>% mutate(p_value_equivalent = pd_to_p(pd))
write.csv(mod1_pdirect,"~/Desktop/Academia/RI/mod/mod1_pdirect.csv", row.names = TRUE)

meep <- 0.29
boop <- exp(meep)
if(meep >= 0) {
  bip = boop - 1
} else {
  bip = 1 - boop
}
boop
bip

?bayes_factor
bayes_factor(fit_mod1, fit_mod3)
pp_check(fit_mod1)
waic(fit_mod1)

plot(fit_mod1)
plot(fit_mod1, variable = "b_gender", combo = c("dens", "trace"))
get_variables(fit_mod1)
mod1df <- as.data.frame(mod1[[1]])


fit_mod1 %>% 
  spread_draws(b_gender, b_race) %>%
  median_qi(b_gender, b_race)

# However, although the direction of the effect
#is quite clear, its magnitude tends to be hard to interprete as it the regression coefficients are
#on the logit scale. To ease interpretation, we can transform and plot them on the original
#probability scale 

conditional_effects(fit_mod1, "gender", categorical = TRUE)

conditional_effects(fit_mod1, c("person", "gender:0"))


prior_summary(fit_mod1)


conditional_effects(fit_mod11, "gender", categorical = TRUE)
post1 <- posterior_samples(fit_mod11)
stanplot(fit_mod11, pars = c("^r_"))
pp_check(fit_mod11)


write.csv(mod11rhat,"~/Desktop/Academia/RI/mod/rhat_mod1.csv", row.names = TRUE)
write.csv(get_variables(fit_mod11),"~/Desktop/Academia/RI/mod/varmod1.csv", row.names = TRUE)


#model 2 for gender and race being a function on theta

formula2 = bf(y ~ 0 + item + (1 | person),
              person ~ race + gender,
              nl = TRUE)

?get_prior
          
#coef of gender and race wont vary across person
prior2 <- get_prior(formula = formula2, family = c("bernoulli", "logit"), data=datdif)

prior2$prior[1] <- "normal(0,10)"
#prior2 <- set_prior("inv_gamma(1,1)", class = "sd", group = "item") +
#  set_prior("normal(0,1)", class = "b", group = "person")

fit_mod2 <- brm(
  formula = formula2, 
  data = datdif,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior2,
  iter = 5000,
  chains = 3,
  warmup = 1000
)

#model 3 - using items that involve social interaction
datdif$socint <- ifelse(datdif$item %in% c(
  "Item 10",
  "Item 11",
  "Item 20",
  "Item 26",
  "Item 27",
  "Item 31",
  "Item 35",
  "Item 36",
  "Item 38",
  "Item 40",
  "Item 44",
  "Item 45",
  "Item 47",
  "Item 50"
), 1, 0)
datdif$socint <- factor(datdif$socint)
str(datdif)

formula3 = y ~ 1 + gender + race + socint + gender:socint + race:socint + (1 | item) + (1 | person) #coef of gender and race wont vary across person
prior3 <- set_prior("inv_gamma(1,1)", class = "sd", group = "item") +
  set_prior("inv_gamma(1,1)", class = "sd", group = "person")

fit_mod3 <- brm(
  formula = formula3, 
  data = datdif,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior3,
  iter = 5000,
  chains = 3,
  warmup = 1000
)

#https://discourse.mc-stan.org/t/change-aesthetics-conditional-effects/13500/2
bayesplot_theme_set(theme_default() + theme(text=element_text(family="Arial")))
plot(fit_mod3) + thm

post3 <- posterior_samples(fit_mod3)
write.csv(post3,"~/Desktop/Academia/RI/mod/posterior3.csv", row.names = TRUE)
conditional_effects(fit_mod3, "gender")
conditional_effects(fit_mod3, "race")
conditional_effects(fit_mod3, "socint:race")
 
table_coef_mod3 <- fit_mod3 %>%
  gather_draws(b_Intercept, b_genderNonMmale, b_raceWhite, b_socint1) %>%
  median_qi()
write.csv(table_coef_mod3,"~/Desktop/Academia/RI/mod/table_coef_mod3.csv", row.names = TRUE)

#https://easystats.github.io/blog/posts/bayestestr_pd/
plot(p_direction(fit_mod3))
write.csv(p_direction(fit_mod3),"~/Desktop/Academia/RI/mod/mod3_pdirection.csv", row.names = TRUE)
pd_to_p(0.9998)
waic(fit_mod3)
summary(fit_mod3)

w <- loo_compare(fit_mod1, fit_mod3, criterion = "waic")

mod3_pdirect <- as.data.frame(p_direction(fit_mod3))
mod3_pdirect <- mod3_pdirect[,1:2]
mod3_pdirect <- mod3_pdirect %>% mutate(p_value_equivalent = pd_to_p(pd))
write.csv(mod3_pdirect,"~/Desktop/Academia/RI/mod/mod3_pdirect.csv", row.names = TRUE)

#posterior predictive check
y <- datdif$y
yrep <- posterior_predict(fit_mod3, draws = 500)

pp_check(fit_mod3, type = "loo_pit")


#model 4
#model 3 - using items that involve social interaction
datdif$collectivist <- ifelse(datdif$item %in% c(
  "Item 01",
  "Item 10",
  "Item 11",
  "Item 15",
  "Item 17",
  "Item 22",
  "Item 38",
  "Item 44",
  "Item 47"
), 1, 0)
datdif$collectivist <- factor(datdif$collectivist)
str(datdif)

formula4 = y ~ 1 + gender + race + collectivist + gender:collectivist + race:collectivist + (1 | item) + (1 | person) #coef of gender and race wont vary across person
prior4 <- set_prior("inv_gamma(1,1)", class = "sd", group = "item") +
  set_prior("inv_gamma(1,1)", class = "sd", group = "person")

fit_mod4 <- brm(
  formula = formula4, 
  data = datdif,
  family = brmsfamily("bernoulli", "logit"),
  prior = prior4,
  iter = 5000,
  chains = 3,
  warmup = 1000
)
plot(fit_mod4)
table(dat$race01)
bayesplot_theme_set(theme_default() + theme(text=element_text(family="Arial")))
plot(fit_mod4) + thm

table_coef_mod4 <- fit_mod4 %>%
  gather_draws(b_Intercept, b_genderNonMmale, b_raceWhite, b_collectivist1) %>%
  median_qi()
write.csv(table_coef_mod4,"~/Desktop/Academia/RI/mod/table_coef_mod4.csv", row.names = TRUE)

conditional_effects(fit_mod4, "gender")
conditional_effects(fit_mod4, "race")
conditional_effects(fit_mod4, "collectivist:race")
conditional_effects(fit_mod4, "collectivist:gender")


mod4_pdirect <- as.data.frame(p_direction(fit_mod4))
mod4_pdirect <- mod4_pdirect[,1:2]
mod4_pdirect <- mod4_pdirect %>% mutate(p_value_equivalent = pd_to_p(pd))
write.csv(mod4_pdirect,"~/Desktop/Academia/RI/mod/mod4_pdirect.csv", row.names = TRUE)
