library(lme4)
library(ggplot2)
library(scales)
library(dplyr)
library(boot)
library(gridExtra)
library(Hmisc)

cbPalette <- c("#E69F00", "#56B4E9")

# TODO: check subjects to replace!
# setwd("C:/Users/r02al13/Documents/GitHub/HemianopiaTraining")
# here is a list of the subjects we want to exlude from the analysis:
#subjectsToRemove = 3

# read in acc data:
print("Processing Acc data")
dat <- read.csv("../data/Acc20.txt", sep="\t")
names(dat) = c("subj","session", "trialNum","difficulty", "targPresent", "targSide", "acc")
dat$targPresent = as.factor(dat$targPresent)
levels(dat$targPresent) = c("absent", "present")
# remove some subjects
dat$subj = as.factor(dat$subj)
dat$session = as.factor(dat$session)
dat$difficulty= as.factor(dat$difficulty)
levels(dat$difficulty) = c("hetero", "homo")
dat$session= as.factor(dat$session)
levels(dat$session) = c("Monday", "Friday")


#dat = (dat[!(dat$subj%in% subjectsToRemove),])
dat$subj = factor(dat$subj)

# save!!!
saveRDS(dat,file="../data/AccData200ms.Rda")



#  why is the line below here?
# dat = dat[which(dat$trial>9),]

accdat  = (dat
	%>% group_by(subj, difficulty, targPresent, session)
	%>% summarise(
		accuracy = mean(acc),
		nTrials = length(acc))

bci95 =	binconf(accdat$accuracy*accdat$nTrials, accdat$nTrials)
accdat$lower = bci95[,2]
accdat$upper = bci95[,3]
rm(bci95)


pAcc2 = ggplot(accdat, aes(x=subj, y=accuracy, colour=session,ymin=lower,ymax=upper)) 
pAcc2 = pAcc2 + geom_point() 
pAcc2 = pAcc2 + theme_light()
pAcc2 = pAcc2 + scale_y_continuous(name="Accuracy(%)", limits=c(0,1)) 
pAcc2 = pAcc2 + scale_x_discrete(name="Participant")+scale_fill_manual(name="Search Difficulty", values=cbPalette)+ facet_grid(targPresent~difficulty)
pAcc2 = pAcc2 + geom_errorbar()
pAcc2
ggsave("../plots/accuracy200msc.jpg",dpi=600, width=12, height=6)
write.csv(accdat, "../data/accDat200ms.txt", row.names=F)


