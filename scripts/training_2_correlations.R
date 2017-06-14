####mean x position as a function of reaction time
setwd("C:/Users/r02al13/Documents/GitHub/HemianopiaTraining")

fxdat = readRDS(file="data/processedFixData.Rda")


subjectsToRemove = c(15,3)
fxdat$subj = as.factor(fxdat$subj)
fxdat = (fxdat[!(fxdat$subj%in% subjectsToRemove),])
fxdat$subj = factor(fxdat$subj)
fxdat$session = as.factor(fxdat$session)

fxdat = filter(fxdat, 
	targSide=="absent", 
	trialType=="blank", 
	difficulty=="parallel",
      fixNum<6)

fixAgg  = aggregate(data=fxdat,  xFix~ subj + session, FUN="mean")


#only for first session for nowstr(xdat)
#xdat = filter(xdat,session==1)

rtdat = readRDS(file="data/processedRTandAccData.Rda")
rtdat$subj = as.factor(rtdat$subj)
rtdat = (rtdat[!(rtdat$subj%in% subjectsToRemove),])
rtdat$subj = factor(rtdat$subj)
rtdat$session = as.factor(rtdat$session)


rtdat = rtdat[which(rtdat$acc==1&rtdat$var=="parallel"&rtdat$trialType!="unmodified"&rtdat$targSide=="blind"),]
dataAgg  = aggregate(data=rtdat,  RT~ subjN + session, FUN="mean")

mergedData = data.frame(subj=fixDat$subj, session=fixDat$session, RT=fixDat$RT,meanX=fixAgg$xFix)

plt = ggplot(mergedData, aes(x=meanX, y=RT)) 
plt = plt + geom_point(colour="gray") + geom_smooth(method="lm", se=F, colour="black")
plt = plt + theme_bw()+facet_wrap(~session, ncol=5)

plt = plt + scale_x_continuous(name="mean X position")
plt = plt + scale_y_continuous(name="reaction time (seconds)")

plt  
ggsave("plots/Cor.pdf", height=3.2, width=9)