
library(lme4)
library(ggplot2)
library(scales)
library(dplyr)
library(car)
setwd("C:/Users/r02al13/Documents/GitHub/HemianopiaTraining")
#rtdat = readRDS(file="../data/processedRTandAccData.Rda")
#fxdat = readRDS(file="../data/processedFixData.Rda")

rtdat = readRDS(file="data/processedRTandAccData.Rda")
fxdat = readRDS(file="data/processedFixData.Rda")






fxdat$fixNo = as.numeric(fxdat$fixNum)
fxdat$session = as.factor(fxdat$session)
fxdat = filter(fxdat, fixNum<51)
fxdat$positReal<-fxdat$xFix-512
fxdat$positRealRelative<-fxdat$xFix-512



###########keep this loop for future reference!!!!!!!
fxdat <- within(fxdat,  positRealRelative<-xFix-512 )
fxdat[!(fxdat$hemiside=="l"), "positRealRelative"] <- fxdat$positRealRelative*(-1)








# take the subset of target absent + serial/parallel search
fxdat = filter(fxdat, 
	targSide=="absent", 
	trialType=="blank", 
	difficulty=="parallel")
xdat = (fxdat 
		%>% group_by(fixNum, session, subj) 
		%>% summarise(
			meanX=mean(positReal), 
			nFix=length(positReal)))
#only for first 8 fixations
xdat = filter(xdat, fixNum<=8)
head(xdat)
#write.csv(xdat,"data/meanFixPosition.txt",row.names=F)
#xdat$fixNum = xdat$fixNum-1
plt = ggplot(xdat, aes(y=meanX, x=fixNum, colour=session))
plt = plt + geom_point(position=position_jitter(height=0.01, width=0.1))
plt = plt + geom_smooth( se=F)
plt = plt + scale_y_continuous(name="mean x position of fixation", expand=c(0,0), limits=c(-400,400))
plt = plt + scale_x_continuous(name="fixation number",breaks=c(1,2,3,4,5,6,7,8), expand=c(0,0.01))
plt = plt + theme_light()
ggsave("plots/meanXfixPos_agg.pdf", width=8, height=6)
ggsave("plots/meanXfixPos_agg.jpg", width=8, height=6)

# modelling! exicting! woooo

simpleModel = lmer(data=filter(xdat, fixNum<=5), scale(meanX)~session:fixNum+0+(0+fixNum:session|subj))
ciSM = confint(simpleModel, method="boot")

# aoidat = filter(aoidat, nFix>6, fixNum<6)
# aoidat = (fxdat 
# 		%>% group_by(fixNum, session, difficulty, subj) 
# 		%>% summarise(
# 			meanSide=mean(xAOI), 
# 			nFix=length(xAOI)))

largerModel = lmer(data=filter(fxdat, fixNum<=5), scale(xFix)~session:fixNum+0+(0+fixNum:session|subj))
ciLM = confint(largerModel, method="boot")

slopeDat = data.frame(session=as.factor(c(1,2,3,4,5)), m=as.numeric(fixef(largerModel)))
plt = plt + geom_abline(data=slopeDat, aes(slope=m,colour=session, intercept=0))


slopeDat = data.frame(
	model=c(rep("mean x",5), rep("x",5)),
	session=as.factor(rep(c(1,2,3,4,5),2)), 
	m=as.numeric(c(fixef(simpleModel), fixef(largerModel))),
	lower = c(ciSM[17:21,1],ciLM[17:21,1]), upper=c(ciSM[17:21,2], ciLM[17:21,2]))

plt = ggplot(slopeDat, aes(x=session, y=m, colour=model, ymin=lower, ymax=upper, group=model))
plt = plt + geom_point(position = position_dodge(width = .25)) + geom_errorbar(position = position_dodge(width = 0.25) ) + geom_path(position = position_dodge(width = 0.25))
plt = plt + geom_hline(yintercept=0)
plt = plt + scale_y_continuous(name = "slope (pixels/fixation)")
plt = plt + theme_bw()
ggsave("../plots/horiModel.pdf", width=6, height=6) 








# plt2 = ggplot(aoidat, aes(y=meanSide, x=as.numeric(session), group=as.factor(subj)))
# plt2 = plt2 + geom_path() + facet_grid(difficulty~fixNum)
# plt2 = plt2 + geom_smooth(group=1)
# plt2 = plt2 + scale_y_continuous(name="prop. of fixations on either side", expand=c(0,0), limits=c(-1,1), breaks=c(-1,-.5,0,.5,1), labels=c("100% left", "75% left", "50% balanced", "75% right", "100% right"))
# plt2 = plt2 + scale_x_continuous(breaks=c(1,3,5,7,10))
# plt2 = plt2 + theme_light()
# ggsave("../plots/fixdists/LeftVrightFixationsSerialMean.pdf", width=10, height=3)
# #