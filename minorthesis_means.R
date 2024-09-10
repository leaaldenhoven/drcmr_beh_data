
############### Group comparison between diagnoses in age #####################

##      START-UP       ################################
## Packages ##

#For info on packages
#p_help(psych, web = F)
library(datasets)  # Load base packages manually
pacman::p_load(pacman, apaTables, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny, 
               stringr, tidyr, psych, tidyverse, car) 

## Import data ##
set <- import("C:/Users/leano/OneDrive/Documents/DRCMR/R/minorthesis.csv")
head(set)

## Reduce data to diagnosis stroke or MS ##
#(remove diagnosis = 2 & 3)
set1 = filter(set, diagnosis < 2)
dia1 <- table(set1$diagnosis)
barplot(dia1,
        main = "Diagnosis",
        col = "cyan")


##      DESCRIPTIVES   ################################
par(mfrow=c(1,1))
boxplot(age ~ diagnosis,
        data = set1,
        main = "Age Distribution by Diagnosis",
        xlab = "Diagnosis",
        ylab = "Age",
        col = "steelblue",
        border = "black")


##      ASSUMPTIONS    ###############################
## Assumption of normality of age ##
#Comparison with normal data
normal_data <- rnorm(200)

#Checking normality with histograms & normality curves
describe(set1$age)
par(mfrow = c(3, 1))
hist(set$age [set$diagnosis == 0],
     breaks = 10,
     main = "CV patients",
     freq = FALSE,
     xlab = "Age",
     col = "lightpink2")
curve(dnorm(x, mean = mean(set$age), sd = sd(set$age)),
      col = "lightpink4",
      lwd = 2,
      add = TRUE)
hist(set$age [set$diagnosis == 1],
     breaks = 10,
     main = "MS patients",
     freq = FALSE,
     xlab = "Age",
     col = "indianred1")
curve(dnorm(x, mean = mean(set$age), sd = sd(set$age)),
      col = "indianred4",
      lwd = 2,
      add = TRUE)
hist(normal_data,
     breaks = 10,
     main = "Norm Comparison",
     freq = FALSE,
     col = "gray")
curve(dnorm(x, mean = mean(normal_data), sd = sd(normal_data)),
      col = "gray4",
      lwd = 2,
      add = TRUE)

#Checking normality with Q-Q plot
par(mfrow=c(1,3)) 
qqnorm(set$age [set$diagnosis == 0],
       main = "Q-Q Plot CVS",
       col="thistle4")
qqline(set$age [set$diagnosis == 0],
       col="thistle4")
qqnorm(set$age [set$diagnosis == 1],
       main = "Q-Q Plot MS",
       col="thistle4")
qqline(set$age [set$diagnosis == 1],
       col="thistle4")
qqnorm(normal_data,
       main = "Q-Q Normal",
       col = "gray4")
qqline(normal_data,
       col = "gray4")

#Checking normality with Shapiro-Wilk Test
  #-> all nonsignificant -> normally distributed
shapiro.test(set1$age)
shapiro.test(set$age [set$diagnosis == 0])
shapiro.test(set$age [set$diagnosis == 1])
shapiro.test(normal_data)

## Assumption of homoscedacity of age across diagnostic groups ##
#Checking homoscedacity manually
describe(set$age [set$diagnosis == 0])
describe(set$age [set$diagnosis == 1])
  #SD are 11.26 and 8.23
  #Variances are 126.79 and 67.73 (just about, using rule of thumb)
  #homoscedacity is not given
  #Solutions -> perform Welch's test (non-parametric version)

#Checking homoscedacity with Levene's Test 
 #Enter as: leveneTest(response variable ~ group variable, data = data)
 #Result: Levene's test is nonsignificant (P=0,21) -> groups are equal
set1$diagnosis <- as.character(set1$diagnosis)
leveneTest(age ~ diagnosis, set1)

#Student's T-test
  #Observing boxplots we see that the groups overlapbut the cores are separate, test needed
  #Refer back to Variances
t.test_knownvar <- function(x, y, V1, V2, m0 = 0, alpha = 0.05, alternative = "two.sided"){
  M1 <- mean(x)
  M2 <- mean(y)
  n1 <- length(x)
  n2 <- length(y)
  sigma1 <- sqrt(V1)
  sigma2 <- sqrt(V2)
  S <- sqrt((V1 / n1) + (V2 / n2))
  statistic <- (M1 - M2 - m0) / S
  p <- if (alternative == "two.sided") {
    2 * pnorm(abs(statistic), lower.tail = FALSE)
  } else if (alternative == "less") {
    pnorm(statistic, lower.tail = TRUE)
  } else {
    pnorm(statistic, lower.tail = FALSE)
  }
  LCL <- (M1 - M2 - S * qnorm(1 - alpha / 2))
  UCL <- (M1 - M2 + S * qnorm(1 - alpha / 2))
  value <- list(mean1 = M1, mean2 = M2, m0 = m0, sigma1 = sigma1, sigma2 = sigma2, S = S, statistic = statistic, p.value = p, LCL = LCL, UCL = UCL, alternative = alternative)
  # print(sprintf("P-value = %g",p))
  # print(sprintf("Lower %.2f%% Confidence Limit = %g",
  #               alpha, LCL))
  # print(sprintf("Upper %.2f%% Confidence Limit = %g",
  #               alpha, UCL))
  return(value)
}

test <- t.test_knownvar(set$age [set$diagnosis == 0],
                        set$age [set$diagnosis == 1],
                        V1 = 126.79, V2 = 67.73
)
test
test$p.value

#Results:
# Mean1 = 67.2
# Mean2 = 51.08
# SD1 = 11.26
# SD2 = 8.23
# T = 5.24
# p = 0,0000001532312 (1.532312e-07)
# LCL = 10.10
# UCL = 22.14


##Put results in a table & save it
#(now with ANOVA command but also works
#can also use as template to put in T-test values
table(test)
view(test)

setwd("~/DRCMR/R")
apa.aov.table(lm(age~diagnosis,set1),filename = "ShowingBebeR.doc",table.number = 3)


## CLEAN UP #################################################

# Clear environment
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons
detach("package:datasets", unload = TRUE)  # For base

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)