library(dplyr)
library(ggplot2)

setwd("C:/Users/r02al13/Documents/GitHub/HemianopiaTraining/ControlStudyResults/ObjectNaming")

#subjectsToRemove = c(11, 13, 14)

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

dat <- read.csv("DataObjectsControl.txt", header=T, sep="\t")
names(dat) = c("subj", "session", "trialNo", "fixNum", "xFix", "yFix", "fixStartTime", "fixEndTime", "hemiSide")

summary(dat)


levels(dat$hemiSide) = c("left","right")
dat$subj = as.factor(dat$subj)
#dat = (dat[!(dat$subj%in% subjectsToRemove),])
dat$subj = factor(dat$subj)
# make trial unique (just now there is a trial 1 for each block ,etc)
dat$trial = factor(paste(dat$hemiSide, dat$trialNo))


#
# flip hemiSide = right so we can pretend hemiSide isn't a condition
#
print("...flipping trials for hemi==right")
fixdat=dat

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


saveRDS(fixdat,file="processedObjectData.Rda")
write.table(fixdat, "processedObjectData.txt", sep=",")
