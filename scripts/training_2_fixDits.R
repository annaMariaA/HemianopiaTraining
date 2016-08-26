

library(lme4)
library(ggplot2)
library(scales)
library(dplyr)
library(car)

rtdat = readRDS(file="../data/processedRTandAccData.Rda")
fxdat = readRDS(file="../data/processedFixData.Rda")

fxdat$session = as.factor(fxdat$session)


#  check number of trials for each person/block
ntrialsdat = (fxdat 
		%>% group_by(subj, session, difficulty, trialType, targSide) 
		%>% summarise(
			nTrials = length(unique(trialNo))))

# define x=0 to be vertical midline
fxdat$xFix = fxdat$xFix - 512
# code into AOIs (left/centre/right)
centreHalfWidth = 30
fxdat$xAOI = 0
fxdat$xAOI[fxdat$xFix < -centreHalfWidth] = -1
fxdat$xAOI[fxdat$xFix >  centreHalfWidth] =  1

# /fxdat = filter(fxdat, fixNum<51)

# take the subset of target absent + serial/parallel search
fxdat = filter(fxdat, 
	targSide=="absent", 
	trialType=="blank", 
	difficulty=="serial")


aoidat = (fxdat 
		%>% group_by(fixNum, session, difficulty, subj) 
		%>% summarise(
			meanSide=mean(xAOI), 
			nFix=length(xAOI)))


# remove entries with fewer than 10 fixations
aoidat = filter(aoidat, nFix>6, fixNum<11)




plt2 = ggplot(aoidat, aes(y=meanSide, x=as.numeric(session), group=as.factor(subj)))
plt2 = plt2 + geom_path() + facet_grid(difficulty~fixNum)
plt2 = plt2 + geom_smooth(group=1)
plt2 = plt2 + scale_y_continuous(name="prop. of fixations on either side", expand=c(0,0), limits=c(-1,1), breaks=c(-1,-.5,0,.5,1), labels=c("100% left", "75% left", "50% balanced", "75% right", "100% right"))
plt2 = plt2 + scale_x_continuous(breaks=c(1,3,5,7,10))
plt2 = plt2 + theme_light()
ggsave("../plots/fixdists/LeftVrightFixationsSerialMean.pdf", width=10, height=3)
#