
# TODO: check subjects to replace

setwd("C:/Users/r02al13/Documents/GitHub/HemianopiaTraining")
# here is a list of the subjects we want to exlude from the analysis:

#subjectsToRemove = c(7)#accuracy at 0
# max fixation duration - remove trial if it is exceeded 
maxFixDur = 2000
# read in reaction time and acc data:
# this will allow us to remove fixation data for incorrect trials
print("Processing RT and Acc data")
dat <- read.csv("data/RtAcc5.txt", sep="\t")
names(dat) = c("subjectN", "session", "trialNum", "hemiType", "hemiSide","variability", "targPresent", "targSide", "RT", "acc")
dat$targPresent = as.factor(dat$targPresent)
levels(dat$targPresent) = c("absent", "present")
levels(dat$hemiType) = c("Blank", "Blank", "Unmodified")
levels(dat$hemiSide) = c("left", "right")
dat$var = as.factor(dat$var)
levels(dat$var) = c("serial", "parallel")
# remove some subjects
dat$subjectN = as.factor(dat$subjectN)
#dat = (dat[!(dat$subj%in% subjectsToRemove),])
dat$session = as.factor(dat$session)

# make trial unique (just now there is a trial 1 for each block ,etc)
dat$trial = factor(paste(dat$hemiType, dat$hemiSide, dat$trialNum))
# refdefine targSide relative to hemiSide
levels(dat$targSide) = c("left", "right", "absent")
dat$targSideRel = as.factor(as.character(dat$hemiSide) == as.character(dat$targSide))
levels(dat$targSideRel) = levels(dat$targSideRel) = c("sighted", "blind", "absent")
dat$targSideRel[which(dat$targPresent=="absent")] = "absent"
# make a new, tidier version of dataframe only including the stuff we want!
rtdat = data.frame(subjN=dat$subjectN, trial=dat$trial, hemiType=dat$hemiType, hemiSide=dat$hemiSide, targSide=dat$targSideRel, RT=dat$RT, acc=dat$acc, var=dat$var, session=dat$session)
# we don't want to be looking at RTs for incorrect trials
rtdat$RT[rtdat$acc==0] = NaN
# save!!!
saveRDS(rtdat,file="data/processedRTandAccData.Rda")

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
		dx = trialDat$fixX[f] - trialDat$fixX[f+1]
		dy = trialDat$fixY[f] - trialDat$fixY[f+1]
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
# read in the trial data
#
#print("Processing Trial data")
#itemdat = read.csv("data/allTrialData.txt", sep="\t", header=T)
#names(itemdat) = c("subj", "trialNum", "hemiType", "hemiSide", "targPresent", "targSide", "itemID", "itemX", "itemY")	

#remove unwanted subjects
#itemdat$subj = as.factor(itemdat$subj)
#itemdat = (itemdat[!(itemdat$subj%in% subjectsToRemove),])
#itemdat$subj = factor(itemdat$subj)

#levels(itemdat$hemiType) = c("Blank", "Blank", "Dot", "Dot", "Filter", "Filter", "Unmodified", "Unmodified")
#levels(itemdat$hemiSide) = c("left", "right")
#levels(itemdat$targSide) = c("left", "right", "absent")

# make trial unique (just now there is a trial 1 for each block ,etc)
#itemdat$trial = factor(paste(itemdat$hemiType, itemdat$hemiSide, itemdat$trialNum))



#
# now read in fixation data
#
print("Processing Fix data...")
dat <- read.csv("data/Fix11.txt", header=T, sep="\t",
	colClass = c(
		"subj"="factor", 
		"trialNum"="numeric", 
		"fixNum"="numeric", 
		"hemiType"="factor",   
		"fixX" = "numeric",
		"fixY" = "numeric",
		"fixOn" = "numeric",
		"fixOff" = "numeric",
		"hemiSide" = "factor",
		"targPresent" = "factor",
		"targSide" = "factor",
		"row" = "numeric",
		"column" = "numeric",
		"var"="numeric",
		"subjectN"="factor",
            "session"="factor"))
names(dat) = c("subj", "trialNum", "fixNum", "hemiType", "fixX", "fixY", "fixOn", "fixOff", "hemiSide", "targPresent", "targSide","row","column","var","subjN","session")
levels(dat$targPresent) = c("absent", "present")
levels(dat$hemiType) = c("Blank","Blank", "Unmodified", "Unmodified")
levels(dat$hemiSide) = c("left", "right")
levels(dat$targSide) = c("left", "right", "absent")
dat$var = as.factor(dat$var)
levels(dat$var) = c("serial", "parallel")
# make trial unique (just now there is a trial 1 for each block ,etc)
dat$trial = factor(paste(dat$hemiType, dat$hemiSide, dat$trialNum))

# refdefine targSide relative to hemiSide
dat$targSideRel = as.factor(as.character(dat$hemiSide) == as.character(dat$targSide))
levels(dat$targSideRel) = levels(dat$targSideRel) = c("sighted", "blind", "absent")
dat$targSideRel[which(dat$targPresent=="absent")] = "absent"

# calcualte fixation durations
dat$fixDur = dat$fixOff - dat$fixOn

# remove unwanted participants
#dat = (dat[!(dat$subj%in% subjectsToRemove),])
dat$subj = factor(dat$subj)
dat$subjN = factor(dat$subjN)
#
 #we want to filter out all incorrect trials!

 print("...removing fixation for incorrect trials and fix.dur exceptions")
 accdat = readRDS(file="data/processedRTandAccData.Rda")
 dat$acc = 0
 for (s in levels(dat$subj))
 {
 	subjDat = dat[which(dat$subj==s),]
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
 dat = dat[which(dat$acc==1),]

saveRDS(dat,file="data/processedFixData.Rda")
 rm(dat)

fixdat = readRDS(file="data/processedFixData.Rda")


#
# flip hemiSide = right so we can pretend hemiSide isn't a condition
#
# print("...flipping trials for hemi==right")

 fixdat$fixX = fixdat$fixX - 512
# itemdat$itemX = itemdat$itemX - 512 + 64
  
for (s in levels(fixdat$subj))
{
	subjDat = fixdat[which(fixdat$subj==s),]
	subjDat$trial = factor(subjDat$trial)
	for (t in levels(subjDat$trial))
	{
		if (subjDat$hemiSide[which(subjDat$trial==t)][1]=="right")
		{
			idx = which(fixdat$subj==s & fixdat$trial==t)
			fixdat$fixX[idx] = - fixdat$fixX[idx]
		}
	}
	rm(subjDat)
}
rm(s,t, idx)

 fixdat$fixX = fixdat$fixX + 512
# itemdat$itemX = itemdat$itemX + 512 - 64


# define itemID for each face
# qx = 8*itemdat$itemX/1024
# qy = 6*itemdat$itemY/768
# itemdat$isTarget = (itemdat$itemID == " Target")
# itemdat$itemID = as.factor((qx)*10 + (qy))

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

#
# assigning fixations to regions
#
# print("...assigning fixations to regions")
# nItemsX = 8
# nItemsY = 6
# # quantise fix!
# qx = ceiling(nItemsX*fixdat$fixX/1024)
# qy = ceiling(nItemsY*fixdat$fixY/768)
# qx[qx<1] = NaN
# qy[qy<1] = NaN
# qx[qx>7] = NaN
# qy[qy>5] = NaN

# fixdat$fixRegion = as.factor((qx-1)*10 + (qy-1))

# for (s in levels(fixdat$subj))
# {
# 	subjdat = fixdat[which(fixdat$subj==s),]
# 	subjdat$trial = factor(subjdat$trial)
#  	for (t in levels(subjdat$trial))
# 	{
# 		itemTrialDat = itemdat[itemdat$subj==s & itemdat$trial==t,] 		
# 		trialDat = fixdat[s==fixdat$subj & t==fixdat$trial,]
# 		trialDat = removeregionsthatwerentpresent(itemTrialDat, trialDat)
# 		fixdat[s==fixdat$subj & t==fixdat$trial,] = trialDat
# 	}
# 	rm(subjdat)
# }
 dat = fixdat
fixdat = data.frame(subj=dat$subj,subjN=dat$subjN, trial=dat$trial, hemiType=dat$hemiType, hemiSide=dat$hemiSide, targSide=dat$targSideRel, fixNum=dat$fixNum, fixX=dat$fixX, fixY=dat$fixY, fixDur=dat$fixDur, saccAmp=dat$saccAmp, saccAng=dat$saccAng, var=dat$var, session=dat$session)


saveRDS(fixdat,file="data/processedFixData.Rda")
write.table(fixdat, "data/processedFixData.txt", sep=",")




