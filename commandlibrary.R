##############################################
####                                      ####
####                                      ####
####                                      ####
####                                      ####
####           COMMAND COLLECTION         ####
####                                      ####
####                                      ####
####                                      ####
####                                      ####
##############################################

##############################################
####                                      ####
####                                      ####
####               START-UP               ####
####                                      ####
####                                      ####
##############################################

##############################################
####        LOAD PACKAGES & DATA          ####
##############################################


getwd()
setwd('C:/Users/leano/OneDrive/Documents/DRCMR/R')

df <- read.csv('C:/Users/leano/OneDrive/Documents/DRCMR/R/tap_performance.CSV', sep = ";", header=TRUE)
df <- import("C:/Users/leano/OneDrive/Documents/DRCMR/R/tap_performance/minorthesis_tap.csv")

view(df)
head(df)
names(df)
summary(df)
describe()

##############################################
####        RECODE CAT VARIABLES          ####
##############################################


df$gender <- factor(df$gender, levels = c("0","1"), labels = c("male","female")) 


##############################################
####             FILTER DATA              ####
##############################################


diaex <- filter(df, df$diagnosis != 'Other')
diaex$diagnosis <- factor(diagnosis)


##############################################
####      CATEGORIZE QUANT VARIABLES      ####
##############################################


val_repl1 <- c(0:15)                                                            #specify values to be replaced
val_repl2 <- c(16:84)
val_repl3 <- c(85:100)
val_repl4 <- 9999
col_repl <- c("al", "da", "flex", "go", "scan_time", "scan_miss")                #specify columns
df[col_repl] <- sapply(df[col_repl],
                          function(x) replace(x, x %in% val_repl1, "low"))
df[col_repl] <- sapply(df[col_repl],
                          function(x) replace(x, x %in% val_repl2, "average"))
df[col_repl] <- sapply(df[col_repl],
                          function(x) replace(x, x %in% val_repl3, "high"))
df[col_repl] <- sapply(df[col_repl],
                          function(x) replace(x, x %in% val_repl4, NA))

##############################################
####                                      ####
####                                      ####
####         DATA VISUALIZATION           ####
####                                      ####
####                                      ####
##############################################

##############################################
####            PLOTTING BASICS           ####  
##############################################

#Multiple plots
plot_grid(rt_plot,max_plot,slope_plot,deslope_plot,auc_plot,acc_plot,labels='AUTO')
par(mfrow = c(1,1))     #Dividing screen for multiple plots
ggplot(df, aes(x=age,fill=diagnosis)) + geom_density() + xlim(0,100) + facet_grid(diagnosis~ .) #Multifaceted plot

plot(df$gender)         #automatic plotting of cat variables
plot(df$da)             #automatic plotting of quant variables


##############################################
####             BARPLOTS                 ####  
##############################################

al <- table(df$al)
barplot(al,
        main = "Alertness",
        col = "lavender")
 #or

ggplot(df, aes(x=gender,fill=education)) + geom_bar()

##############################################
####             HISTROGRAMS              ####  
##############################################

hist(df$age,
     xlim = c(30,90),
     breaks = 10,
     main = "Age Distribution",
     xlab = "Age",
     col = "red")

 #or

ggplot(df, aes(x=age,fill=diagnosis))
ggplot(df, aes(x=age,fill=diagnosis)) + geom_bar()
ggplot(df, aes(x=age,fill=diagnosis)) + geom_bar() + xlim(30,90)
ggplot(df, aes(x=age,fill=diagnosis)) + geom_bar(width=2) + xlim(30,90)

##############################################
####               BOXPLOTS               ####  
##############################################

boxplot(age ~ diagnosis,
        data = df,
        main = "Age Distribution by Diagnosis",
        xlab = "Diagnosis",
        ylab = "Age",
        col = "steelblue",
        border = "black")

 #or

library(tidyverse)

ggplot(diaex, aes(x=diagnosis,y=age)) + geom_boxplot()
ggplot(diaex, aes(x=diagnosis,y=age,fill=gender)) + geom_boxplot()
ggplot(diaex, aes(x=diagnosis,y=age,fill=gender)) + geom_boxplot() + coord_flip()

##############################################
####            SCATTERPLOT               ####  
##############################################

plot(df$quantitative_variable)                         #Single variables

ggplot(df, aes(x=age, y=sea_adq)) + geom_point()  #Two variables
# color by diagnosis (categorical)
ggplot(df, aes(x=age, y=sea_adq,color=diagnosis)) + geom_point()
ggplot(df, aes(x=age, y=sea_adq,color=diagnosis)) + geom_point() + xlim(25,100) + ylim(0,100)
# color by age (numerical)
ggplot(df, aes(x=age, y=sea_adq,color=age)) + geom_point() + xlim(-500,500) + ylim(-500,500)
ggplot(df, aes(x=ref.dist,y=query.dist,color=age)) + geom_point() + xlim(-500,500) + ylim(-500,500) + scale_colour_gradient(limits=c(0,500))

##############################################
####            VIOLIN PLOT               ####  
##############################################

ggplot(df, aes(x=diagnosis,y=age)) + geom_violin()
ggplot(df, aes(x=diagnosis,y=age,fill=gender)) + geom_violin() + ylim(30,90) + guides(fill=FALSE)
ggplot(df, aes(x=diagnosis,y=age,fill=gender)) + geom_violin(adjust=0.2) + ylim(30,90) + guides(fill=FALSE)
 # default adjust is 1, lower means finer resolution

ggplot(df, aes(x=diagnosis,y=age,fill=diagnosis)) + geom_violin() +
  scale_y_log10() # You can log-scale any numerical axis on any plot

##############################################
####           DENSITY PLOT               ####  
##############################################

ggplot(df, aes(x=age,fill=diagnosis)) + geom_density() + xlim(0,100)
# similar to this histogram:
ggplot(df, aes(x=age,fill=diagnosis)) + geom_bar(binwidth=5) + xlim(0,100)

ggplot(df, aes(x=age,fill=diagnosis)) + geom_density(position="stack") + xlim(0,100)
ggplot(df, aes(x=age,fill=diagnosis)) + geom_density(alpha=0.5) + xlim(0,100)

##############################################
####              DOT PLOT                ####  
##############################################


ggplot(df, aes(x=age,fill=diagnosis)) + geom_dotplot()
# a dot plot makes more sense with fewer observations where each individual item matters,
# so let's grab the largest events only
large_data <- df[df$age>50,  ] # [rows,columns]
ggplot(large_data, aes(x=age,fill=diagnosis)) + geom_dotplot(method="histodot")
# careful, they don't stack automatically, so some of the dots are behind others
ggplot(large_data, aes(x=age,fill=diagnosis)) + geom_dotplot(method="histodot",stackgroups=TRUE)

##############################################
####              LINE PLOT               ####  
##############################################

# Time-course data for line plot
filename <- "Lesson-06/time_course_data.txt"
time_course <- read.csv(filename, sep=",", quote='', stringsAsFactors=TRUE,header=TRUE)
time_course

# line plot:
ggplot(time_course, aes(x=seconds,y=value,colour=sample)) + geom_line()
ggplot(time_course, aes(x=seconds,y=value,colour=sample)) + geom_line(age=3)


##############################################
####              PIE CHART               ####  
##############################################


# Pie charts
type_counts = summary(df$type)
type_counts

pie(type_counts)
pie(type_counts,col=brewer.pal(length(type_counts),"Set1"))


##############################################
####             VENN DIAGRAM             ####  
##############################################


# Gene lists for Venn Diagram
listA <- read.csv("Lesson-06/genes_list_A.txt",header=FALSE)
A <- listA$V1
A

listB <- read.csv("Lesson-06/genes_list_B.txt",header=FALSE)
B <- listB$V1
B

listC <- read.csv("Lesson-06/genes_list_C.txt",header=FALSE)
C <- listC$V1
C

listD <- read.csv("Lesson-06/genes_list_D.txt",header=FALSE)
D <- listD$V1
D

length(A)
length(B)
length(C)
length(D)

# install package VennDiagram
library(VennDiagram)

# This function only works by saving directly to a file

venn.diagram(list("list C"=C, "list D"=D), fill = c("yellow","cyan"), cex = 1.5, filename="Lesson-06/Venn_diagram_genes_2.png")

venn.diagram(list(A = A, C = C, D = D), fill = c("yellow","red","cyan"), cex = 1.5,filename="Lesson-06/Venn_diagram_genes_3.png")

venn.diagram(list(A = A, B = B, C = C, D = D), fill = c("yellow","red","cyan","forestgreen"), cex = 1.5,filename="Lesson-06/Venn_diagram_genes_4.png")


##############################################
####        OTHER VISUALIZATIONS          ####  
##############################################

# For fun:
# Any plot can be made in polar coordinates:
# line plot
ggplot(time_course, aes(x=seconds,y=value,colour=sample)) + geom_line(age=3) + coord_polar()

# violin plot
ggplot(df, aes(x=diagnosis,y=age,fill=diagnosis)) + geom_violin(adjust=0.5) + ylim(0,1000) + coord_polar()
# bar plot
ggplot(df, aes(x=age,fill=diagnosis)) + geom_bar(binwidth=5) + xlim(0,500) + coord_polar()



##############################################
####                                      ####
####                                      ####
####        STATISTICAL TESTING           ####
####                                      ####
####                                      ####
##############################################

##############################################
####        ASSUMPTION: NORMALITY         ####  
##############################################

hist(df$age [df$diagnosis == "CV"],
     breaks = 10,
     main = "CV patients",
     freq = FALSE,
     xlab = "Age",
     col = "lightpink2")
curve(dnorm(x, mean = mean(df$age), sd = sd(df$age)),
      col = "lightpink4",
      lwd = 2,
      add = TRUE)

qqnorm(df$age [df$diagnosis == "CV"],
       main = "Q-Q Plot CVS",
       col="thistle4")
qqline(df$age [df$diagnosis == "CV"],
       col="thistle4")

shapiro.test(df$age [df$diagnosis == "CV"])

##############################################
####      ASSUMPTION: HOMOSCEDACITY       ####  
##############################################

describe(df$age [df$diagnosis == "CV"])  ####  ~> checking manually
leveneTest(age ~ diagnosis, df)          ####  ~> Checking homoscedacity with Levene's Test 

##############################################
####         STUDENT'S T-TEST             ####  
##############################################

#can also be used for Welch's Test

#x: Outcome variable data for the first group
#y: Outcome variable data for the second group
#alternative: The alternative hypothesis for the test. Default is two.sided.

t.test(x, y, alternative = c(“two.sided”, “less”, “greater”))

  #or

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

test <- t.test_knownvar(df$age [df$diagnosis == 0],
                        df$age [df$diagnosis == 1],
                        V1 = 126.79, V2 = 67.73
)
test
test$p.value

##############################################
####               ANOVA                  ####
##############################################

modelRT <- lmer(RT_init ~ FaceGender*FaceEmotion*RewardPromise + (1 | SubjectNr),data=dfCorrect)


##############################################
####            SAVE OUTPUT               ####
##############################################

table(test)
view(test)

dfwd("~/DRCMR/R")
apa.aov.table(lm(age~diagnosis,df),filename = "ShowingBebeR.doc",table.number = 3)

##############################################
####               CLEAN-UP               ####
##############################################

detach("package:datasets", unload = TRUE) #packages
rm(list = ls())                           #Environment
dev.off()                                 #plots
cat("\014")                               #console (ctrl+L)s




