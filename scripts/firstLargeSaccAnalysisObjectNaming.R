

library(lme4)
library(ggplot2)
library(scales)
library(dplyr)
library(car)

fxdat = readRDS(file="data/processedObjectData.Rda")


fxdat$session = as.factor(fxdat$session)


# define x=0 to be vertical midline
fxdat$xFix = fxdat$xFix - 512
# code into AOIs (left/centre/right)
centreHalfWidth = 30
fxdat$xAOI = 0
fxdat$xAOI[fxdat$xFix < -centreHalfWidth] = -1
fxdat$xAOI[fxdat$xFix >  centreHalfWidth] =  1

fxdat = filter(fxdat, fixNum<16)


aoidat = (fxdat 
		%>% group_by(fixNum, session, subj) 
		%>% summarise(
			meanSide=mean(xAOI), 
			nFix=length(xAOI)))
# remove entries with fewer than 100 fixations
aoidat = filter(aoidat, nFix>9, fixNum<10)




plt2 = ggplot(aoidat, aes(y=meanSide, x=fixNum, colour=session))
#plt2 = plt2 + facet_wrap(~subj)
plt2 = plt2 + geom_smooth(se=F)
plt2 = plt2 + scale_y_continuous(name="prop. of fixations on either side", expand=c(0,0), limits=c(-1,1), breaks=c(-1,-.5,0,.5,1), labels=c("100% left", "75% left", "50% balanced", "75% right", "100% right"))
plt2 = plt2 + scale_x_continuous(breaks=c(1,3,5, 7, 10))
plt2 = plt2 + theme_light()
ggsave("FixationDistributionObjectNamingAllPps.pdf", width=8, height=6)
ggsave("LeftVrightFixationsParallel.jpg", width=8, height=6)
