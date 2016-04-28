library(dplyr)

 setwd("C:/Users/r02al13/Documents/GitHub/HemianopiaTraining")
# here is a list of the subjects we want to exlude from the analysis:


# max fixation duration - remove trial if it is exceeded 
maxFixDur = 2000
# read in reaction time and acc data:
# this will allow us to remove fixation data for incorrect trials
print("Processing RT and Acc data")
#dat <- read.csv("../data/RtAcc20.txt", sep="\t")
dat <- read.csv("data/RtAcc20.txt", sep="\t")
names(dat) = c(
	"subjectN", 
	"session", 
	"trialNum", 
	"trialType", 
	"hemiSide",
	"variability", 
	"targPresent", 
	"targSide",
	 "RT", 
	 "acc")
dat$targPresent = as.factor(dat$targPresent)
levels(dat$targPresent) = c("absent", "present")
levels(dat$trialType) = c("left","right", "unmodified")

dat$var = as.factor(dat$var)
levels(dat$var) = c("serial", "parallel")
# remove some subjects
dat$subjectN = as.factor(dat$subjectN)
#dat = (dat[!(dat$subj%in% subjectsToRemove),])
dat$session = as.factor(dat$session)
levels(dat$hemiSide) = c("left", "right")
# make trial unique (just now there is a trial 1 for each block ,etc)
dat$trial = factor(paste(dat$session, dat$trialType, dat$trialNum))
# refdefine targSide relative to hemiSide
levels(dat$targSide) = c("left", "right", "absent")
dat$targSideRel = as.factor(as.character(dat$hemiSide) == as.character(dat$targSide))
levels(dat$targSideRel) = levels(dat$targSideRel) = c("sighted", "blind", "absent")
dat$targSideRel[which(dat$targPresent=="absent")] = "absent"

# make a new, tidier version of dataframe only including the stuff we want!
rtdat = data.frame(subjN=dat$subjectN, session=dat$session, trial=dat$trial, trialType=dat$trialType, targSide=dat$targSideRel, RT=dat$RT, acc=dat$acc, var=dat$var)
# we don't want to be looking at RTs for incorrect trials
rtdat$RT[rtdat$acc==0] = NaN
# save!!!

#saveRDS(rtdat,file="../data/processedRTandAccData.Rda")
saveRDS(rtdat,file="data/processedRTandAccData.Rda")
write.table(rtdat, "data/processedRTandAccData.txt", sep=",")
# remove data for now
rm(dat, rtdat)

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
#dat <- read.csv("../data/Fix10.txt", header=T, sep="\t")
dat <- read.csv("data/FixDataFinal.txt", header=T, sep="\t")
names(dat) = c("subj", "session", "trialNo", "fixNum",	"trialType", "xFix", "yFix", "fixStartTime", "fixEndTime", "hemianopia", "targPresent",	"targSide",	"row", "column", "difficulty", "name")
levels(dat$targPresent) = c("absent", "present")
levels(dat$trialType) = c("left","right", "unmodified")
dat$subj = as.factor(dat$subj)
levels(dat$targSide) = c("left", "right", "absent")
dat$difficulty = as.factor(dat$difficulty)
levels(dat$difficulty) = c("serial", "parallel")
# make trial unique (just now there is a trial 1 for each block ,etc)
dat$trial = factor(paste(dat$session, dat$trialType, dat$trialNo))

# refdefine targSide relative to hemiSide
dat$targSideRel = as.factor(as.character(dat$trialType) == as.character(dat$targSide))
levels(dat$targSideRel) = levels(dat$targSideRel) = c("sighted", "blind", "absent")
dat$targSideRel[which(dat$targSide=="absent")] = "absent"
levels(dat$trialType) = c("blank", "blank", "unmodified")
# calcualte fixation durations
dat$fixDur = dat$fixEndTime - dat$fixStartTime


#we want to filter out all incorrect trials!

print("...removing fixation for incorrect trials and fix.dur exceptions")
#accdat = readRDS(file="../data/processedRTandAccData.Rda")
accdat = readRDS(file="data/processedRTandAccData.Rda")
dat$acc = 0
for (s in levels(dat$subj))
{
	subjDat = filter(dat, subj==s)
 	subjDat$trial = factor(subjDat$trial)
 	for (t in levels(subjDat$trial))
 	{
 		j = which(accdat$trial==t & accdat$subj==s)
 		idx = which(dat$subj==s & dat$trial==t)
     	if (accdat$acc[j]==1 & max(dat$fixDur[idx]) <= maxFixDur )
     	{
     		dat$acc[idx] = 1
     	}
     }
 }
 print(paste("... keeping ", 100*mean(dat$acc), "% of fixations"))
 dat = filter(dat, acc==1)
saveRDS(dat,file="data/processedFixData.Rda")
#saveRDS(dat,file="../data/processedFixData.Rda")
 rm(dat)

#fixdat = readRDS(file="../data/processedFixData.Rda")
fixdat = readRDS(file="data/processedFixData.Rda")

#
# flip hemiSide = right so we can pretend hemiSide isn't a condition
#
print("...flipping trials for hemi==right")


fixdat$xFix=fixdat$xFix -512
fixdat$subj = as.factor(fixdat$subj)
# itemdat$itemX = itemdat$itemX - 512 + 64
  
for (s in levels(fixdat$subj))
{
	subjDat = filter(fixdat, subj==s)
	subjDat$trial = factor(subjDat$trial)
	for (t in levels(subjDat$trial))
	{
		if (subjDat$trialType[which(subjDat$trial==t)][1]=="right")
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

 dat = fixdat
fixdat = data.frame(subj=dat$subj,  session=dat$session, trial=dat$trial, trialType=dat$trialType, targSide=dat$targSideRel, fixNum=dat$fixNum, xFix=dat$xFix, yFix=dat$yFix, fixDur=dat$fixDur, saccAmp=dat$saccAmp, saccAng=dat$saccAng, difficulty=dat$difficulty)

levels(dat$trialType) = c("blank", "blank", "unmodified")

saveRDS(fixdat,file="data/processedFixData.Rda")
write.table(fixdat, "data/processedFixData.txt", sep=",")
#saveRDS(fixdat,file="../data/processedFixData.Rda")
#write.table(fixdat, "../data/processedFixData.txt", sep=",")



