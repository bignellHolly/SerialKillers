head(killersandmotives)
library(dplyr)
library(ggplot2)
install.packages("hrbrthemes")
library(hrbrthemes)

createsample(23)
mysample

clean_sample <- mysample[complete.cases(mysample), ]

percentage_deleted <- 100 - ((72 / 82) * 100)
percentage_deleted 


clean_sample

condition <- clean_sample$AgeFirstKill != 99999

condition

clean_sample <- clean_sample[condition,]
clean_sample

del_2 <- 100 - ((63/72)*100)
del_2

per_overall <- 100 - ((63/82)*100)
per_overall

clean_sample$YearFirstKill <- clean_sample$YearBorn + clean_sample$AgeFirstKill

clean_sample

condition_2 <- clean_sample$YearFirstKill > 1900
condition_2

clean_sample <- clean_sample[condition_2, ]
clean_sample

del_3 <- 100 - ((62/63)*100)
del_3

clean_sample$CareerDuration <- clean_sample$AgeLastKill - clean_sample$AgeFirstKill

motives <- clean_sample$Motive
motives

#Data Exploration 
afk_mean <- mean(clean_sample$AgeFirstKill)
afk_mean 

alk_mean <- mean(clean_sample$AgeLastKill)
alk_mean 

cd_mean <- mean(clean_sample$CareerDuration)
cd_mean

afk_sd <- sd(clean_sample$AgeFirstKill)
afk_sd

alk_sd <- sd(clean_sample$AgeLastKill)
alk_sd

cd_sd <- sd(clean_sample$CareerDuration)
cd_sd


#Unique variables for 'motive' 
motives <- unique(clean_sample$Motive)
motives


  
  
install.packages("moments")
library(moments)

afk_skew <- skewness(clean_sample$AgeFirstKill)
afk_skew
#postive skew 

alk_skew <- skewness(clean_sample$AgeLastKill)
alk_skew
#positive skew 

cd_skew <- skewness(clean_sample$CareerDuration)
cd_skew
#positive skew 
#distribution is right skewed - biased towards higher values 


#Normal distribution on agefirstkill + density curve 

 
afk_x <- seq(from = min(clean_sample$AgeFirstKill), to = max(clean_sample$AgeFirstKill), by = 1.0)
afk_y <- dnorm(afk_x, mean= afk_mean, sd = afk_sd)
afk_lambda <- 1/afk_mean
afk_y_exp <- dexp(afk_x, rate = afk_lambda, log = FALSE)


df_afk <- data.frame( clean_sample$AgeFirstKill)
density_afk <- data.frame(afk_x, afk_y, afk_y_exp)

afk_hist <- ggplot(df_afk, aes(x=clean_sample.AgeFirstKill)) +
  geom_histogram(binwidth=8, aes(y=..density..), alpha = 0.8) + 
  geom_line(data=density_afk, aes(x=afk_x, y=afk_y, colour= "Normal"), size = 1) + 
  geom_line(data=density_afk, aes(x=afk_x, y=afk_y_exp, colour="Exponential"), size = 1) + 
  ggtitle("Distribution Modelling of Age First Kill") + 
  labs(
    colour = "Distribution"
  ) + 
  xlab("Age First Kill") + 
  ylab("Density") 
  


afk_hist

afk_xlog <- data.frame(log(clean_sample$AgeFirstKill))
names(afk_xlog) = "x_log"

afk_hist_log <- ggplot(afk_xlog, aes(x=x_log)) +
  geom_histogram(binwidth=0.2, aes(y=..density..), alpha = 0.8) 
  

  
afk_hist_log
#Distribution model of AgeLastKill + density curve 

alk_x <- seq(from = min(clean_sample$AgeLastKill), to = max(clean_sample$AgeLastKill))   
alk_y <- dnorm(alk_x, mean=alk_mean, sd = alk_sd)
alk_lambda <- 1/alk_mean
alk_y_exp <- dexp(alk_x, rate=alk_lambda, log= FALSE) 

df_alk <- data.frame(clean_sample$AgeLastKill)
density_alk <- data.frame(alk_x, alk_y, alk_y_exp)

alk_hist <- ggplot(df_alk, aes(x=clean_sample.AgeLastKill))+
  geom_histogram(binwidth = 7.5, aes(y=..density..), alpha=0.8) + 
  geom_line(data= density_alk, aes(x=alk_x, y=alk_y, colour="Normal"), size = 1) + 
  geom_line(data=density_alk, aes(x=alk_x, y=alk_y_exp, colour="Exponential"), size=1) + 
  ggtitle("Distribution Modelling for Age Last Kill") + 
  labs( 
    colour = "Distribution"
    ) + 
  xlab("Age Last Kill") + 
  ylab("Density")

alk_hist

#Distribution model Career duration 
cd_x <- seq(from = min(clean_sample$CareerDuration), to = max(clean_sample$CareerDuration))
cd_y <- dnorm(cd_x, mean = cd_mean, sd = cd_sd)
cd_lambda <- 1/cd_mean
cd_y_exp <- dexp(cd_x, rate = cd_lambda)

df_cd <- data.frame(clean_sample$CareerDuration)
density_cd <- data.frame(cd_x, cd_y, cd_y_exp)

hist_cd <- ggplot(df_cd, aes(x=clean_sample.CareerDuration)) + 
  geom_histogram(binwidth=8, aes(y=..density..), alpha=0.8) + 
  geom_line(data=density_cd, aes(x=cd_x, y=cd_y, colour="Normal"), size = 1) + 
  geom_line(data=density_cd, aes(x=cd_x, y=cd_y_exp, colour="Exponential"), size=1)+ 
  ggtitle("Distribution Modelling of Career Duration") + 
  labs(colour = "Distribution") + 
  xlab("Career Duration") + 
  ylab("Density")

hist_cd

#z-test of Age First Kill using the CLT 
# Previous research suggested a population mean of 27 and a variance of 74 

#Null hypothesis 
#H0 = X = x
#H1 = X != x 
# (within 5% siginificance)

pop_mean <- 27 
pop_var <- 74
sample_size <- 62

sig <- qnorm(p = 0.025, mean = 0, sd = 1)
sig

z <- (afk_mean - pop_mean)/(sqrt(pop_var)/sqrt(62))
z

# The z value: 3.56 is greater than the significance level 1.96 therefore we can reject the null hypothesis 
# The difference between the population mean and the sample mean is statistically significant. 


#One sample t-test
results <- t.test(clean_sample$AgeFirstKill, mu = 27)
results
#p-value = 0.0002642 

#Conclusions 
#The results here indicate that we can again reject the null hypothesis. 
#agrees with the z test. 




#Selecting motives
mental_illness <- clean_sample %>% 
  filter(Motive =="Mental illness (including paranoia, visionary or Munchausen's syndrome)")

afk_mental_illness_mean <- mean(mental_illness$AgeFirstKill)
afk_mental_illness_cd <- sd(mental_illness$AgeFirstKill)

escape <- clean_sample %>% 
  filter(Motive =="Escape or avoid arrest")

afk_escape_mean <- mean(escape$AgeFirstKill)
afk_escape_sd <- sd(escape$AgeFirstKill)


angel_of_death <- clean_sample %>% 
  filter(Motive == "Angel of Death")

afk_angel_mean <- mean(angel_of_death$AgeFirstKill)
afk_angel_sd <- sd(angel_of_death$AgeFirstKill)

#Komolgorov-smifnov test 
#testing if each sample is of a normal distribution 

#mental illness 
mental_illness_afk <- mental_illness$AgeFirstKill
mental_illness_results <- ks.test(x = mental_illness$AgeFirstKill, y = "pnorm", mean = afk_mental_illness_mean, sd = afk_mental_illness_cd)
mental_illness_results

#escape 
escape_results <-ks.test(x = escape$AgeFirstKill, y = "pnorm", mean = afk_escape_mean, sd = afk_escape_sd)
escape_results

#angel of death
angel_results <- ks.test( x = angel_of_death$AgeFirstKill, y = "pnorm", mean = afk_angel_mean, sd = afk_angel_sd)
angel_results 

#t-test to see if the average age of first kill for each motive is 27 years 

tres_mental <- t.test(mental_illness$AgeFirstKill, mu = 27)
tres_mental

tres_escape <- t.test(escape$AgeFirstKill, mu = 27)
tres_escape

tres_angel <- t.test(angel_of_death$AgeFirstKill, mu = 27)
tres_angel

#mental illness is the only motive where the true mean is equal to 27 
#escape and angel of death reject null - there mean is statistically siginficantly different 
#why ? incarceration usually requires time to be caught, and angel of death requires someone to be qualified and a healthcare professional 


mental_vs_escape <- t.test(mental_illness$AgeFirstKill, escape$AgeFirstKill, mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 0.95)
mental_vs_escape

mental_vs_angel <- t.test(mental_illness$AgeFirstKill, angel_of_death$AgeFirstKill, mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 0.95)
mental_vs_angel

angel_vs_escape <- t.test(angel_of_death$AgeFirstKill, escape$AgeFirstKill, mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 0.95)
angel_vs_escape


#plotting confidence intervals 

analysis <- c("Mental Illness vs Escape", 
              "Mental Illness vs Angel of Death", 
              "Angel of Death vs Escape")

estimate <- c(-5.75, -5.16, -0.58)
upper <- c(-1.42, -0.06, 4.25)
lower <- c(-10.08, -10.26, -5.41)
pval <- c("p = 0.01055", "p = 0.04758", "p = 0.8075")
labels <- c(5, 5, 5)

x_scale <- seq(-12,10,2)
x_scale


df <- data.frame(analysis, estimate, lower, upper, labels)
install.packages("tidyverse")

list <- c(1,2,3)
intervals <- c()
 
for(i in list) {
   intervals[i] <- paste0("(", lower[i], ",", upper[i], ")")
}
intervals

my_chart <- ggplot(data=df, aes(x=estimate, y=analysis, xmin=lower, xmax=upper)) + 
  geom_pointrange(shape = 18, size = 1, fill = "#21D418", color="#0A8B03", alpha=0.8) +
  geom_text(label = pval, nudge_y = 0.2, nudge_x = 0.2, angle = 35, size = 4, color="black", alpha=1) +
  geom_vline(xintercept= 0, lty = 2, alpha=0.8) +
  theme(axis.text.y = element_text(
    angle = 35, 
    size=10,
    color='black'
  ))  + 
  ylab("Tests") + 
  xlab("Estimate") + 
  scale_x_continuous(breaks = x_scale) +
  xlim(-12, 12) +
  geom_label(label= intervals,
             x=9,
             y= c(3, 2, 1), 
             size = 4) + 
  ggtitle("Estimated Difference in means, with 95% confidence interval")
 
  
my_chart


