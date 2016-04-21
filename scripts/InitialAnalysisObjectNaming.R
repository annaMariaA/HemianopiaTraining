library(dplyr)
library(ggplot2)

setwd("C:/Users/r02al13/Documents/GitHub/HemianopiaTraining")

subjectsToRemove = c(11, 13, 14)

saccInfo <- function(trialDat)
{
	# this is a funtion that calculates saccade amplitude and angle for a sequenece of fixations in a  trial
	nFix = max(trialDat$fixNum)
	saccInfo  = trialDat
	saccAmp2 = vector()
	theta = vector()
	for (f in 1:(nFix-1))
	{
		dx = trialDat$xFix[f] - trialDat$xFix[f+1]
		dy = trialDat$yFix[f] - trialDat$yFix[f+1]
		saccAmp2[f] 	= dx^2 + dy^2
		theta[f] 		= atan2(dx, dy)
	}
	saccAmp2[nFix] = NaN
	theta[nFix]    = NaN
	saccInfo$amp   = sqrt(saccAmp2)
	saccInfo$ang   = theta
	return(saccInfo)
}

#
# now read in fixation data
#
print("Processing Fix data...")

dat <- read.csv("data/FixDataObjectNaming2.txt", header=T, sep="\t")
names(dat) = c("subj", "session", "trialNo", "fixNum", "xFix", "yFix", "fixStartTime", "fixEndTime", "hemiSide")

summary(dat)


levels(dat$hemiSide) = c("left","right")
dat$subj = as.factor(dat$subj)
dat = (dat[!(dat$subj%in% subjectsToRemove),])
dat$subj = factor(dat$subj)
# make trial unique (just now there is a trial 1 for each block ,etc)
dat$trial = factor(paste(dat$hemiSide, dat$trialNo))


#
# flip hemiSide = right so we can pretend hemiSide isn't a condition
#
print("...flipping trials for hemi==right")



fixData1to15=dat[which(dat$subj=="1" | dat$subj=="2" | dat$subj=="3" | dat$subj=="4" | dat$subj=="5" | dat$subj=="6" | dat$subj=="7" | dat$subj=="8" | dat$subj=="9" | dat$subj=="10" | dat$subj=="12" | dat$subj=="15"),]
fixData16to20=dat[which(dat$subj=="16" | dat$subj=="17" | dat$subj== "18" | dat$subj== "19" | dat$subj=="20" ),]
fixData16to20$xFix=fixData16to20$xFix -128
fixdat = rbind(fixData1to15,fixData16to20 )


fixdat$xFix=fixdat$xFix -512
fixdat$subj = as.factor(fixdat$subj)
# itemdat$itemX = itemdat$itemX - 512 + 64
  
for (s in levels(fixdat$subj))
{
	subjDat = filter(fixdat, subj==s)
	subjDat$trial = factor(subjDat$trial)
	for (t in levels(subjDat$trial))
	{
		if (subjDat$hemiSide[which(subjDat$trial==t)][1]=="right")
		{
			idx = which(fixdat$subj==s & fixdat$trial==t)
			fixdat$xFix[idx] = - fixdat$xFix[idx]
		}
	}
	rm(subjDat)
}
rm(s,t, idx)

 fixdat$xFix = fixdat$xFix + 512

#
# get saccade info
#
print("...calcualting sacc amp and ang")
fixdat$saccAmp = NaN
fixdat$saccAng = NaN
for (s in levels(fixdat$subj))
{
	subjdat = fixdat[which(fixdat$subj==s),]
	subjdat$trial = factor(subjdat$trial)
	for (t in levels(subjdat$trial))
	{
		if (length(which(fixdat$subj==s & fixdat$trial==t))>0)
		{
			saccDat    = saccInfo(fixdat[which(fixdat$subj==s & fixdat$trial==t),])		
			fixdat$saccAmp[which(fixdat$subj==s & fixdat$trial==t)] = saccDat$amp
			fixdat$saccAng[which(fixdat$subj==s & fixdat$trial==t)] = saccDat$ang	
			rm(saccDat)	
		}
	}
	rm(subjdat)
}
rm(s, t)
fixdat$fixDur = fixdat$fixEndTime - fixdat$fixStartTime
dat = fixdat
fixdat = data.frame(subj=dat$subj,  session=dat$session, trial=dat$trial, hemiType=dat$hemiSide, fixNum=dat$fixNum, xFix=dat$xFix, yFix=dat$yFix, fixDur=dat$fixDur, saccAmp=dat$saccAmp, saccAng=dat$saccAng)


saveRDS(fixdat,file="data/processedObjectData.Rda")
write.table(fixdat, "data/processedObjectData.txt", sep=",")

fxdat=fixdat = readRDS(file="data/processedObjectData.Rda")


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
ggsave("FixationDistributionObjectNamingMean.pdf", width=8, height=6)
ggsave("LeftVrightFixationsParallel.jpg", width=8, height=6)