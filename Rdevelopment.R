rm(list = ls())
library(gdata)
library(lme4)
library(ggplot2)
library(lmerTest)
library(cowplot)
library(readxl)

dat =read_excel('C:/Users/leano/OneDrive/Documents/DRCMR/ADAPT Behavioural Study Vytautas/behavioral_output_final.xlsx')

#Determine factor (cat) variables
dat$SubjectNr = factor(dat$SubjectNr)
dat$Gender = factor(dat$Gender)
dat$Run = factor(dat$Run)
dat$FaceGender = factor(dat$FaceGender)
dat$FaceEmotion = factor(dat$FaceEmotion)
dat$RewardPromise = factor(dat$RewardPromise)

#Include only cases with correct response, no missing data, RT between 0.2 and 1.0 seconds
dfCorrect = dat[dat$Correct==1 & is.na(dat$RT_init)==FALSE & dat$RT_init>.2 & dat$RT_init<1,]


## Plot participants
rt_plot <- ggplot(data = dfCorrect, mapping = aes(x = SubjectNr, y = RT_init)) +
  geom_boxplot()
max_plot<-ggplot(data = dfCorrect, mapping = aes(x = SubjectNr, y = MaxGrip)) +
  geom_boxplot()
slope_plot<-ggplot(data = dfCorrect, mapping = aes(x = SubjectNr, y = Slope)) +
  geom_boxplot()
deslope_plot<-ggplot(data = dfCorrect, mapping = aes(x = SubjectNr, y = DeSlope)) +
  geom_boxplot()
auc_plot<-ggplot(data = dfCorrect, mapping = aes(x = SubjectNr, y = AUC)) +
  geom_boxplot()
acc_plot<-ggplot(data = dfCorrect, mapping = aes(x = SubjectNr, y = Acceleration)) +
  geom_boxplot()
plot_grid(rt_plot,max_plot,slope_plot,deslope_plot,auc_plot,acc_plot,labels='AUTO')
################################################################################
init_plot <- ggplot(data = dfCorrect, mapping = aes(x = SubjectNr, y = RT_init)) +
  geom_boxplot()
riset_plot <- ggplot(data = dfCorrect, mapping = aes(x = SubjectNr, y = RiseTime)) +
  geom_boxplot()
fallt_plot <- ggplot(data = dfCorrect, mapping = aes(x = SubjectNr, y = FallTime)) +
  geom_boxplot()
set_plot <- ggplot(data = dfCorrect, mapping = aes(x = SubjectNr, y = SettlingTime)) +
  geom_boxplot()
GripDur_plot <- ggplot(data = dfCorrect, mapping = aes(x = SubjectNr, y = GripDuration)) +
  geom_boxplot()
maxtime_plot <- ggplot(data = dfCorrect, mapping = aes(x = SubjectNr, y = MaxTime)) +
  geom_boxplot()
plot_grid(init_plot,riset_plot,fallt_plot,set_plot,GripDur_plot,maxtime_plot,labels='AUTO')
################################################################################
#Reward promise
rt_plot2 <- ggplot(data = dfCorrect, mapping = aes(x = RewardPromise, y = RT_init)) +
  geom_boxplot()
max_plot2<-ggplot(data = dfCorrect, mapping = aes(x = RewardPromise, y = MaxGrip)) +
  geom_boxplot()
slope_plot2<-ggplot(data = dfCorrect, mapping = aes(x = RewardPromise, y = Slope)) +
  geom_boxplot()
deslope_plot2<-ggplot(data = dfCorrect, mapping = aes(x = RewardPromise, y = DeSlope)) +
  geom_boxplot()
auc_plot2<-ggplot(data = dfCorrect, mapping = aes(x = RewardPromise, y = AUC)) +
  geom_boxplot()
acc_plot2<-ggplot(data = dfCorrect, mapping = aes(x = RewardPromise, y = Acceleration)) +
  geom_boxplot()
plot_grid(rt_plot2,max_plot2,slope_plot2,deslope_plot2,auc_plot2,acc_plot2,labels='AUTO')
################################################################################
#Emotion of the face
rt_plot3 <- ggplot(data = dfCorrect, mapping = aes(x = FaceEmotion, y = RT_init)) +
  geom_boxplot()
max_plot3<-ggplot(data = dfCorrect, mapping = aes(x = FaceEmotion, y = MaxGrip)) +
  geom_boxplot()
slope_plot3<-ggplot(data = dfCorrect, mapping = aes(x = FaceEmotion, y = Slope)) +
  geom_boxplot()
deslope_plot3<-ggplot(data = dfCorrect, mapping = aes(x = FaceEmotion, y = DeSlope)) +
  geom_boxplot()
auc_plot3<-ggplot(data = dfCorrect, mapping = aes(x = FaceEmotion, y = AUC)) +
  geom_boxplot()
acc_plot3<-ggplot(data = dfCorrect, mapping = aes(x = FaceEmotion, y = Acceleration)) +
  geom_boxplot()
plot_grid(rt_plot3,max_plot3,slope_plot3,deslope_plot3,auc_plot3,acc_plot3,labels='AUTO')
################################################################################


#Linear models
modelRT <- lmer(RT_init ~ FaceGender*FaceEmotion*RewardPromise + (1 | SubjectNr),data=dfCorrect)
anova(modelRT)
modelMax <- lmer(MaxGrip ~ FaceGender*FaceEmotion*RewardPromise + (1 | SubjectNr),data=dfCorrect)
anova(modelMax)                                                                                   #Main effect of rewardPromise on maxgrip
modelSlope <- lmer(Slope ~ FaceGender*FaceEmotion*RewardPromise + (1 | SubjectNr),data=dfCorrect)
anova(modelSlope)
modeldeslope <- lmer(DeSlope ~ FaceGender*FaceEmotion*RewardPromise + (1 | SubjectNr),data=dfCorrect)
anova(modeldeslope)
modelAUC <- lmer(AUC ~ FaceGender*FaceEmotion*RewardPromise + (1 | SubjectNr),data=dfCorrect)
anova(modelAUC)
modelACC <- lmer(Acceleration ~ FaceGender*FaceEmotion*RewardPromise + (1 | SubjectNr),data=dfCorrect)
anova(modelACC)
#modelVelocity <- lmer(Velocity ~ FaceGender*FaceEmotion*RewardPromise + (1 | SubjectNr),data=dfCorrect)
#anova(modelVelocity)

modelRT_init <- lmer(RT_init ~ FaceGender*FaceEmotion*RewardPromise + (1 | SubjectNr),data=dfCorrect)
anova(modelRT_init)

modelSettlingTime <- lmer(SettlingTime ~ FaceGender*FaceEmotion*RewardPromise + (1 | SubjectNr),data=dfCorrect)
anova(modelSettlingTime)

modelGripDur <- lmer(GripDuration~ FaceGender*FaceEmotion*RewardPromise + (1 | SubjectNr),data=dfCorrect)
anova(modelGripDur)

modelMaxTime<- lmer(MaxTime~ FaceGender*FaceEmotion*RewardPromise + (1 | SubjectNr),data=dfCorrect)
anova(modelMaxTime)

ggplot(data = dfCorrect, mapping = aes(x = FaceEmotion,fill = FaceGender, y = RT_init)) +
  geom_boxplot() 



#Acceleration
gend<-ggplot(data = dfCorrect, mapping = aes(x = RewardPromise,fill = FaceGender, y = Acceleration)) +
  geom_boxplot() 
emot <- ggplot(data = dfCorrect, mapping = aes(x = RewardPromise,fill = FaceEmotion, y = Acceleration)) +
  geom_boxplot() 
plot_grid(gend,emot,labels='AUTO')

ggplot(data = dfCorrect, mapping = aes(x = RewardPromise, y = RT_init)) +
  geom_boxplot() 
ggplot(data = dfCorrect, mapping = aes(x = RewardPromise, y = MaxTime)) +
  geom_boxplot() 
ggplot(data = dfCorrect, mapping = aes(x = RewardPromise, y = AUC)) +
  geom_boxplot() 
ggplot(data = dfCorrect, mapping = aes(x = SubjectNr,fill = RewardPromise, y = RT_init)) + xlab("Subject Nr.") + ylab("Grip Initiation")+labs(fill="Reward promise")+
  geom_boxplot() 
#predictions = predict(model1)
#dfCorrect$modelprediction = predictions

#plot(log10(dfCorrect$RT))
#lines(dfCorrect$modelprediction,col='red')



#predictionsmodelAUC = predict(modelAUC)
#dfCorrect$modelpredictionmodelAUC = predictionsmodelAUC

plot(dfCorrect$AUC)
lines(dfCorrect$modelpredictionmodelAUC,col='red')

ggplot(data = dfCorrect, mapping = aes(x = RewardPromise, fill=RewardPromise,y = Acceleration)) +
  geom_boxplot() 

modelACC <- lmer(Acceleration ~ FaceGender*FaceEmotion*RewardPromise + (1 | SubjectNr),data=dfCorrect)
anova(modelACC)


ggplot(dfCorrect,aes(x=RewardPromise,y=RT_init,group=SubjectNr,color=SubjectNr))+geom_line(size=.1)
ggplot(dfCorrect,aes(x=RewardPromise,y=MaxGrip,group=SubjectNr,color=SubjectNr))+geom_line(size=.1)
ggplot(dfCorrect,aes(x=RewardPromise,y=MaxTime,group=SubjectNr,color=SubjectNr))+geom_line(size=.1)
ggplot(dfCorrect,aes(x=RewardPromise,y=Slope,group=SubjectNr,color=SubjectNr))+geom_line(size=.1)
ggplot(dfCorrect,aes(x=RewardPromise,y=AUC,group=SubjectNr,color=SubjectNr))+geom_line(size=.1)
ggplot(dfCorrect,aes(x=RewardPromise,y=Acceleration,group=SubjectNr,color=SubjectNr))+geom_line(size=.1)

# ggplot(dfCorrect,aes(x=FaceEmotion,y=RT_init,group=SubjectNr,color=SubjectNr))+geom_line(size=.75)
# ggplot(dfCorrect,aes(x=FaceEmotion,y=MaxGrip,group=SubjectNr,color=SubjectNr))+geom_line(size=.75)
# ggplot(dfCorrect,aes(x=FaceEmotion,y=MaxTime,group=SubjectNr,color=SubjectNr))+geom_line(size=.75)
# ggplot(dfCorrect,aes(x=FaceEmotion,y=Slope,group=SubjectNr,color=SubjectNr))+geom_line(size=.75)
# ggplot(dfCorrect,aes(x=FaceEmotion,y=AUC,group=SubjectNr,color=SubjectNr))+geom_line(size=.75)
# ggplot(dfCorrect,aes(x=FaceEmotion,y=Acceleration,group=SubjectNr,color=SubjectNr))+geom_line(size=.1)
