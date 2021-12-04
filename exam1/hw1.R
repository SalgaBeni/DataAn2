# Clear memory and import packages
rm(list = ls())

library(tidyverse)
library(modelsummary)
library(fixest)
library(grid)
library(lspline)
library(cowplot)
library(boot)
library(estimatr)
library(huxtable)
library(stargazer)

theme_set(theme_bw(base_size=16))

# Load data:
cps_data <- read_csv("https://osf.io/4ay9x/download")

#filter for teachers
cps_data <- cps_data %>%
  filter(cps_data$occ2012 == 2310)

#generate variables(female, earnings / hour, log earnings/hour)
cps_data <- cps_data %>% mutate(female=as.numeric(sex==2)) %>%
                         mutate(wageh=earnwke/uhours) %>% 
                         mutate(lnwage=log(wageh))

cps_data$gen <- as.numeric(cps_data$sex)
cps_data$gen[cps_data$gen==1] <- "Male"
cps_data$gen[cps_data$gen==2] <- "Female"
cps_data$gen <- as.character(cps_data$gen)

# Summary statistics on Edu level and log of earnings per hour
P95 <- function(x){ quantile(x,.95,na.rm=T)}
datasummary( grade92 + lnwage + female ~ Mean + SD + Min + Max + Median + P95 + N , data = cps_data )


#number of male and female and different edu levels
male <- cps_data %>%
  filter(cps_data$sex == 1)

female <- cps_data %>%
  filter(cps_data$sex == 2)

hours <- cps_data %>%
  filter(cps_data$uhours == 40)

hours41 <- cps_data %>%
  filter(cps_data$uhours >= 41)

hours50 <- cps_data %>%
  filter(cps_data$uhours >= 50)

#Relationship between Edu level and log of earnings per hour
p1 <- ggplot( data = cps_data , aes( x = grade92, y = lnwage )) +
  geom_point( color = 'red', size = 2, shape = 16, ) + 
  geom_smooth( method = 'loess', formula = y ~ x) +
  labs(x = "Edu level",y = "Log of earnings per hour")
p1

# p2: Relationship between log of earnings per hour and gender
p2 <- ggplot( data = cps_data , aes( x = female, y = lnwage )) +
  geom_point( color = 'red', size = 2, shape = 16, ) + 
  geom_smooth( method = 'loess', formula = y ~ x) +
  labs(x = "Gender",y = "Log of earnings per hour") +
  scale_x_continuous(limits = c(0,1) , breaks = seq(0, 1, by = 0.5))
p2

#Regressions
reg0 <- lm_robust( lnwage ~ female, data = cps_data , se_type = "HC1")
reg0

reg1 <- lm_robust( lnwage ~ grade92 , data = cps_data , se_type = "HC1")
reg1

reg2 <- lm_robust( lnwage ~ female + grade92 , data = cps_data , se_type = "HC1")
reg2

reg3 <- lm_robust( grade92 ~ female, data=cps_data, se_type = "HC1")
reg3

#Regression table
ht<-huxreg(reg0,reg2, reg3,
           statistics = c(N = "nobs", R2 = "r.squared")) 
ht

#
p3 <- ggplot(data = cps_data, aes (x = grade92, y = 2*(..count..)/sum(..count..))) +
  geom_histogram(binwidth = 4, color = "red", fill = "red", size = 0.25, alpha = 0.8,  
                 boundary=0, closed='left',  show.legend=F, na.rm =TRUE) +
  labs(x = "edu level", y = "Percent") +
  facet_wrap(~ifelse(female, "Female", "Male"))+
  labs(x = "Edu level",y = "Percent")
p3

#Earnings per hour distribution
p4<- ggplot(data = cps_data, aes(x=wageh, color=gen))+
  geom_density() +
  scale_x_continuous(expand = c(0.01, 0.01), limits = c(0, 80), breaks = seq(0, 80, by = 20)) +
  labs(title="Wage distribution", x="log Wage", y="Percent")
p4

#Relationship between log of earnings per hour and Edu level by gender
p5 <- ggplot(data = cps_data, aes(x = grade92, y = lnwage, col=gen)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_grid(~gen) +
  labs(x = "Edu level",y = "ln(earnings per hour)")
p5
