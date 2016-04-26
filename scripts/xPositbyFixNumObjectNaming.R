

library(lme4)
library(ggplot2)
library(scales)
library(dplyr)

rtdat = readRDS(file="data/processedRTandAccData.Rda")
fxdat = readRDS(file="data/processedObjectData.Rda")

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

fxdat = filter(fxdat, fixNum<11)
meanXposbyFixNum  = aggregate(data=fxdat, xFix ~ subj + fixNum+ session, FUN="mean")

plt2 = ggplot(meanXposbyFixNum, aes(y=xFix, x=fixNum, colour=session))
#plt2 = plt2 + facet_wrap(subj~difficulty, nrow=2)
plt2 = plt2 + geom_smooth(se=F)
plt2 = plt2 + scale_y_continuous(name="Mean X Position", expand=c(0,0), limits=c(-300,300), breaks=c(-300,0,300))
plt2 = plt2 + scale_x_continuous(breaks=c(1,5,10))
plt2 = plt2 + theme_light()
ggsave("MeanXposByFixNumObjectNaming.pdf", width=8, height=6)
