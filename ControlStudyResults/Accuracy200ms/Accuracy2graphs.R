library(lme4)
library(ggplot2)
library(scales)
library(dplyr)
library(boot)
library(gridExtra)
library(Hmisc)

cbPalette <- c("#E69F00", "#56B4E9","#B5CB8B")
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

setwd("C:/Users/r02al13/Documents/GitHub/HemianopiaTraining")

# read in acc data:
print("Processing Acc data")
dat <- read.csv("data/accuracyPeripheralVision.txt", sep="\t")
print("Processing Acc data")
dat <- read.csv("AccuracyControl.txt", sep="\t")
names(dat) = c("subj","session", "trialNum","Difficulty", "targPresent", "targSide","response", "acc")
dat$targPresent = as.factor(dat$targPresent)
levels(dat$targPresent) = c("absent", "present")
dat$subj = as.factor(dat$subj)
dat$session = as.factor(dat$session)
dat$Difficulty= as.factor(dat$Difficulty)
levels(dat$Difficulty) = c("heterogeneous", "homogeneous")
dat$session= as.factor(dat$session)
levels(dat$session) = c("Monday", "Friday")
dat$subj = factor(dat$subj)
# save!!!
saveRDS(dat,file="AccData200ms.Rda")
#saveRDS(dat,file="../data/AccData200ms.Rda")
accdat  = (dat
	%>% group_by(subj, Difficulty, targPresent, session)
	%>% summarise(
		accuracy = mean(acc),
		nTrials = length(acc)))

bci95 =	binconf(accdat$accuracy*accdat$nTrials, accdat$nTrials)
accdat$lower = bci95[,2]
accdat$upper = bci95[,3]
rm(bci95)


accdat2 = (accdat
	%>%group_by(Difficulty, targPresent, session)
	%>% summarise(
		meanAccuracy = mean(accuracy),
		std = sd(accuracy),
		nPeople = length(accuracy),
		stder = std/sqrt(nPeople),
		lower = meanAccuracy - 1.96*stder,
		upper = meanAccuracy + 1.96*stder))

pAcc1 = ggplot(accdat2, aes(x=targPresent, y=100*meanAccuracy, fill=Difficulty))+ geom_bar(stat="identity",position="dodge") + theme_minimal()
pAcc1 = pAcc1 + scale_y_continuous(name="Accuracy(%)") + scale_x_discrete(name="Target Position")+scale_fill_manual(name="Difficulty", values=cbPalette) + facet_wrap(~session)
pAcc1 = pAcc1 + geom_errorbar(position=position_dodge(.9), aes(ymin=(lower)*100,ymax=(upper)*100),width=.5)
pAcc1 = pAcc1 + ggtitle("Training")+ theme(plot.title = element_text(hjust = 0.5))

CONTROL EXPERIMENT
cbPalette <- c("#E69F00", "#56B4E9","#B5CB8B")
setwd("C:/Users/r02al13/Documents/GitHub/HemianopiaTraining/ControlStudyResults/Accuracy200ms")

# read in acc data:
print("Processing Acc data")
dat <- read.csv("AccuracyControl.txt", sep="\t")
names(dat) = c("subj","session", "trialNum","Difficulty", "targPresent", "targSide","response", "acc")
dat$targPresent = as.factor(dat$targPresent)
levels(dat$targPresent) = c("absent", "present")
dat$subj = as.factor(dat$subj)
dat$session = as.factor(dat$session)
dat$Difficulty= as.factor(dat$Difficulty)
levels(dat$Difficulty) = c("heterogeneous", "homogeneous")
dat$session= as.factor(dat$session)
levels(dat$session) = c("Monday", "Friday")
dat$subj = factor(dat$subj)

# save!!!
saveRDS(dat,file="AccData200Control.Rda")
#saveRDS(dat,file="../data/AccData200ms.Rda")

accdat  = (dat
	%>% group_by(subj, Difficulty, targPresent, session)
	%>% summarise(
		accuracy = mean(acc),
		nTrials = length(acc)))

bci95 =	binconf(accdat$accuracy*accdat$nTrials, accdat$nTrials)
accdat$lower = bci95[,2]
accdat$upper = bci95[,3]
rm(bci95)


accdat2 = (accdat
	%>%group_by(Difficulty, targPresent, session)
	%>% summarise(
		meanAccuracy = mean(accuracy),
		std = sd(accuracy),
		nPeople = length(accuracy),
		stder = std/sqrt(nPeople),
		lower = meanAccuracy - 1.96*stder,
		upper = meanAccuracy + 1.96*stder))

pAcc2 = ggplot(accdat2, aes(x=targPresent, y=100*meanAccuracy, fill=Difficulty))+ geom_bar(stat="identity",position="dodge") + theme_minimal()
pAcc2 = pAcc2 + scale_y_continuous(name="Accuracy (%)") + scale_x_discrete(name="Target Position")+scale_fill_manual(name="Difficulty", values=cbPalette) + facet_wrap(~session)
pAcc2 = pAcc2 + geom_errorbar(position=position_dodge(.9), aes(ymin=(lower)*100,ymax=(upper)*100),width=.5)
pAcc2 = pAcc2 + ggtitle("Control")+ theme(plot.title = element_text(hjust = 0.5))
legend<- get_legend(pAcc1)
legend<- get_legend(pAcc2)
pAcc2<- pAcc2+ theme(legend.position="none")
pAcc1<- pAcc1+ theme(legend.position="none")
jpeg(filename = "Accuracy2graphs.jpg",width=1400,height=500, pointsize =5, quality = 1000, bg = "white", res = 150, restoreConsole = TRUE)

grid.arrange(pAcc1,pAcc2,legend,ncol=3,widths=c(8,8, 3))
dev.off()

