	

library(lme4)
library(ggplot2)
library(scales)
library(dplyr)
setwd("C:/Users/r02al13/Documents/GitHub/HemianopiaTraining")

fxdat = readRDS(file="data/processedObjectData.Rda")
#fxdat = readRDS(file="../data/processedObjectData.Rda")

fxdat$session = as.factor(fxdat$session)

# define x=0 to be vertical midline
fxdat$xFix = fxdat$xFix - 512

plt = ggplot(filter(fxdat, fixNum<11), aes(x=xFix, colour=session)) + geom_density()
plt = plt + scale_x_continuous(limits=c(-400,400))
ggsave("../plots/objects/xHistFix.pdf")

plt = ggplot(filter(fxdat, fixNum<11), aes(x=xFix, colour=session)) + geom_density()
plt = plt + facet_wrap(~fixNum, scales="free_y", nrow=5)
plt = plt + scale_x_continuous(limits=c(-400,400))
ggsave("../plots/objects/xHistFixFacet.pdf")


plt = ggplot(filter(fxdat, fixNum<11), aes(x=xFix, colour=session)) + geom_density()
plt = plt + facet_wrap(~fixNum, scales="free_y", nrow=5)
plt = plt + scale_x_continuous(limits=c(-400,400))
ggsave("../plots/objects/xHistFixFacet.pdf")




# code into AOIs (left/centre/right)
centreHalfWidth = 30
fxdat$xAOI = 0
fxdat$xAOI[fxdat$xFix < -centreHalfWidth] = -1
fxdat$xAOI[fxdat$xFix >  centreHalfWidth] =  1

####################################
# plot the mean x position
####################################

xdat = (fxdat 
		%>% group_by(fixNum, session, subj) 
		%>% summarise(
			meanX=mean(xFix), 
			nFix=length(xFix)))
# remove entries with fewer than 10 fixations
xdat = filter(xdat, nFix>=10, fixNum==2)

# plot
plt = ggplot(xdat, aes(y=meanX, x=fixNum, colour=session))
plt = plt + geom_point(position=position_jitter(height=0.01, width=0.1))
plt = plt + geom_smooth(se=F)
plt = plt + scale_y_continuous(name="mean x position of fixation", expand=c(0,0), limits=c(-400,400))
plt = plt + scale_x_continuous(breaks=c(1,4,7,10))
plt = plt + theme_light()
ggsave("../plots/objects/meanXfixPos_agg.pdf", width=8, height=6)



xdatSes1 = filter(xdat, fixNum<=10, session==1)
xdatSes2 = filter(xdat, fixNum<=10, session==2)
xdatSes2$ses1=xdatSes1$meanX
dat <- read.csv("Objejct_Naming_Responses/MeanObjectNamed.txt", sep="\t")
subjectsToRemove=c(12,19)
dat$partNo = as.factor(dat$partNo)
dat = (dat[!(dat$partNo%in% subjectsToRemove),])
xdat$meanObjectNamed=dat$ses2less1

plt = ggplot(xdat, aes(y=meanX, x=meanObjectNamed, colour=subj))
plt = plt + geom_point(position=position_jitter(height=0.01, width=0.1))
plt = plt + geom_smooth(method=lm, se=FALSE)
plt = plt + scale_y_continuous(name="mean x position of fixation", expand=c(0,0), limits=c(-400,400))
plt = plt + scale_x_continuous(breaks=c(-1,0,1,2))
plt = plt + theme_light()+facet_wrap(~fixNum)
ggsave("plots/objects/meanXfixMeanObjectNamed.pdf", width=8, height=6)




####################################
# plot prop. left v right fixations
####################################

aoidat = (fxdat 
		%>% group_by(fixNum, session, subj) 
		%>% summarise(
			meanSide=mean(xAOI), 
			nFix=length(xAOI)))
# remove entries with fewer than 10 fixations
aoidat = filter(aoidat, nFix>9, fixNum<10)


plt2 = ggplot(aoidat, aes(y=meanSide, x=fixNum, colour=session))
plt2 = plt2 + geom_point(position=position_jitter(height=0.01, width=0.1))
plt2 = plt2 + geom_smooth(se=F)
plt2 = plt2 + scale_y_continuous(name="prop. of fixations on either side", expand=c(0,0), limits=c(-1,1), breaks=c(-1,-.5,0,.5,1), labels=c("100% left", "75% left", "50% balanced", "75% right", "100% right"))
plt2 = plt2 + scale_x_continuous(breaks=c(1,3,5, 7, 10))
plt2 = plt2 + theme_light()
ggsave("../plots/objects/FixToEitherSideOfDisplay_agg.pdf", width=8, height=6)

####################################
# plot prop. left v right saccades
####################################

saccDat = data.frame()
for (ii in levels(fxdat$subj))
{
	for (ss in 1:5)
	{	
		sdat = filter(fxdat, subj==ii, session==ss)
		for (jj in levels(sdat$trial))
		{
			trialDat = filter(sdat, trial==jj)
			if (nrow(trialDat)>1)
			{
				xd = trialDat$xFix[2:nrow(trialDat)]-trialDat$xFix[1:nrow(trialDat)-1]
				yd = trialDat$yFix[2:nrow(trialDat)]-trialDat$yFix[1:nrow(trialDat)-1]
				ampDirection = xd >0
				amps = sqrt(xd^2+yd^2)
				amps[ampDirection==FALSE]=-amps[ampDirection==FALSE]
				ang = atan2(xd,yd)
				saccDat = rbind(saccDat, 
					data.frame(subj=ii, session=ss, trial=jj, saccNum = seq(1, nrow(trialDat)-1), amp = amps, ang=ang))}
		}
	}
}

# only take left and right saccades
saccDat$ang = 180*saccDat$ang / pi
saccDat$horizontal = FALSE
saccDat$horizontal[abs(abs(saccDat$ang)-90)<45] = TRUE

saccDat$session = as.factor(saccDat$session)
sxdat = (filter(saccDat, horizontal==TRUE) 
		%>% group_by(saccNum, session, subj) 
		%>% summarise(
			meanAmp=mean(amp), 
			nFix=length(amp)))
# remove entries with fewer than 10 fixations
sxdat = filter(sxdat, nFix>=10, saccNum<=10)

sxdat$session = as.factor(sxdat$session)


plt3 = ggplot(sxdat, aes(y=meanAmp, x=saccNum, colour=session))
plt3 = plt3 + geom_point(position=position_jitter(height=0.01, width=0.1))
plt3 = plt3 + geom_smooth(se=F)
plt3 = plt3 + scale_y_continuous(name="saccadic amplitude", expand=c(0,0), limits=c(-400,400))
plt3 = plt3 + scale_x_continuous(name="saccade number", breaks=c(1,4, 7, 10))
plt3 = plt3 + theme_light()
ggsave("../plots/objects/horiSaccAmplitude_agg.pdf", width=8, height=6)

plt = ggplot(filter(saccDat, saccNum<11), aes(x=amp, colour=session)) + geom_density()
plt = plt + scale_x_continuous(limits=c(-400,400))
ggsave("../plots/objects/xHistSacc.pdf")

plt = ggplot(filter(saccDat, saccNum<11), aes(x=amp, colour=session)) + geom_density()
plt = plt + facet_wrap(~saccNum, scales="free_y", nrow=5)
plt = plt + scale_x_continuous(limits=c(-400,400))
ggsave("../plots/objects/xHistSaccFacet.pdf")

