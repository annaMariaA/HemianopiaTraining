library(lme4)
library(ggplot2)
library(scales)
library(bear)
library(boot)
library(gridExtra)

cbPalette <- c("#E69F00", "#56B4E9")

# TODO: check subjects to replace!
setwd("C:/Users/r02al13/Documents/GitHub/HemianopiaTraining")
# here is a list of the subjects we want to exlude from the analysis:


# read in acc data:
print("Processing Acc data")
dat <- read.csv("data/Acc10.txt", sep="\t")
names(dat) = c("subj","session", "trialNum","difficulty", "targPresent", "targSide", "acc")
dat$targPresent = as.factor(dat$targPresent)
levels(dat$targPresent) = c("absent", "present")
# remove some subjects
dat$subj = as.factor(dat$subj)
dat$session = as.factor(dat$session)
dat$difficulty= as.factor(dat$difficulty)
levels(dat$difficulty) = c("hetero", "homo")
#dat = (dat[!(dat$subj%in% subjectsToRemove),])
dat$subj = factor(dat$subj)

# save!!!
saveRDS(dat,file="data/AccData200ms.Rda")
dat = dat[which(dat$trial>10),]
accdat  = aggregate(data=dat, acc ~ subj + difficulty + targPresent + session, FUN="mean")

errorbar <- summarySEwithin(accdat, measurevar="acc", withinvars=c("targPresent","difficulty","session"), idvar="subj")
pAcc2 = ggplot(errorbar, aes(x=targPresent, y=100*acc, fill=difficulty)) + geom_bar(stat="identity", position="dodge") + theme_minimal()
pAcc2 = pAcc2 + scale_y_continuous(name="Accuracy(%)", limits=c(0,110)) + scale_x_discrete(name="Target Present")+scale_fill_manual(name="Search Difficulty", values=cbPalette)+ facet_wrap(~session)
pAcc2 = pAcc2 + geom_errorbar(position=position_dodge(.9), within=.25, aes(ymin=(acc-ci)*100,ymax=(acc+ci)*100),width=.5)

ggsave("plots/accuracy200msc.jpg",dpi=600, width=6, height=3)
write.csv(accdat, "data/accDat200ms.txt", row.names=F)


