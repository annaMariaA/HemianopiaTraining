library(lme4)
library(ggplot2)
library(scales)
library(dplyr)
library(boot)
library(gridExtra)
library(Hmisc)

cbPalette <- c("#E69F00", "#56B4E9")
setwd("C:/Users/r02al13/Documents/GitHub/HemianopiaTraining/ControlStudyResults/Accuracy200ms")
# TODO: check subjects to replace!
# setwd("C:/Users/r02al13/Documents/GitHub/HemianopiaTraining")
# here is a list of the subjects we want to exlude from the analysis:
#subjectsToRemove = 3

# read in acc data:
print("Processing Acc data")
dat <- read.csv("AccuracyControl.txt", sep="\t")
#dat <- read.csv("Acc20.txt", sep="\t")
names(dat) = c("subj","session", "trialNum","Difficulty", "targPresent", "targSide","response", "acc")
dat$targPresent = as.factor(dat$targPresent)
levels(dat$targPresent) = c("absent", "present")
# remove some subjects
dat$subj = as.factor(dat$subj)
dat$session = as.factor(dat$session)
dat$Difficulty= as.factor(dat$Difficulty)
levels(dat$Difficulty) = c("heterogeneous", "homogeneous")
dat$session= as.factor(dat$session)
levels(dat$session) = c("Monday", "Friday")


#dat = (dat[!(dat$subj%in% subjectsToRemove),])
dat$subj = factor(dat$subj)

# save!!!
saveRDS(dat,file="AccData200ms.Rda")
#saveRDS(dat,file="../data/AccData200ms.Rda")



#  why is the line below here?
# dat = dat[which(dat$trial>9),]

accdat  = (dat
	%>% group_by(subj, Difficulty, targPresent, session)
	%>% summarise(
		accuracy = mean(acc),
		nTrials = length(acc)))

bci95 =	binconf(accdat$accuracy*accdat$nTrials, accdat$nTrials)
accdat$lower = bci95[,2]
accdat$upper = bci95[,3]
rm(bci95)


pAcc2 = ggplot(accdat, aes(x=subj, y=accuracy, colour=session,ymin=lower,ymax=upper)) 
pAcc2 = pAcc2 + geom_point() 
pAcc2 = pAcc2 + theme_light()
pAcc2 = pAcc2 + scale_y_continuous(name="Accuracy(%)", limits=c(0,1)) 
pAcc2 = pAcc2 + scale_x_discrete(name="Participant")+scale_fill_manual(name="Search Difficulty", values=cbPalette)+ facet_grid(targPresent~Difficulty)
pAcc2 = pAcc2 + geom_errorbar()
pAcc2
ggsave("accuracy200control.jpg",dpi=600, width=12, height=6)
write.csv(accdat, "../data/accDat200ms.txt", row.names=F)


accdat2 = (accdat
	%>%group_by(Difficulty, targPresent, session)
	%>% summarise(
		meanAccuracy = mean(accuracy),
		std = sd(accuracy),
		nPeople = length(accuracy),
		stder = std/sqrt(nPeople),
		lower = meanAccuracy - 1.96*stder,
		upper = meanAccuracy + 1.96*stder))

pAcc3 = ggplot(accdat2, aes(x=session, y=meanAccuracy, colour=Difficulty, ymin=lower,ymax=upper)) 
pAcc3 = pAcc3 + geom_point() 
pAcc3 = pAcc3 + theme_light()
pAcc3 = pAcc3 + scale_y_continuous(name="Accuracy(%)", limits=c(0,1)) 
pAcc3 = pAcc3 + scale_x_discrete(name="Session")+scale_fill_manual(name="Search Difficulty", values=cbPalette)+ facet_grid(.~targPresent)
pAcc3 = pAcc3 + geom_errorbar()
pAcc3
ggsave("../plots/accuracy200msc2.jpg",dpi=600, width=6, height=4)
ggsave("plots/accuracy200msc2.jpg",dpi=600, width=6, height=4)

dat <- read.csv("data/dprime.txt", sep="\t")
names(dat) = c("subj","session","Difficulty", "dprime")
# remove some subjects
dat$subj = as.factor(dat$subj)
dat$session = as.factor(dat$session)
dat$Difficulty= as.factor(dat$Difficulty)
levels(dat$Difficulty) = c("heterogeneous", "homogeneous")
dat$session= as.factor(dat$session)
levels(dat$session) = c("Monday", "Friday")
DIprime=(dat
	%>%group_by(Difficulty, session)
	%>% summarise(
		meanDprime = mean(dprime),
		std = sd(dprime),
		nPeople = length(dprime),
		stder = std/sqrt(nPeople),
		lower = meanDprime - 1.96*stder,
		upper = meanDprime + 1.96*stder))

dP = ggplot(DIprime, aes(x=session, y=meanDprime, colour=Difficulty, ymin=lower,ymax=upper)) 
dP = dP + geom_point() 
dP = dP + theme_light()
dP = dP + scale_y_continuous(name="Mean d'", limits=c(-1,5)) 
dP = dP + scale_x_discrete(name="Session")+scale_fill_manual(name="Search Difficulty", values=cbPalette)
dP = dP + geom_errorbar()
dP
ggsave("plots/Dprime.jpg",dpi=600, width=6, height=4)
