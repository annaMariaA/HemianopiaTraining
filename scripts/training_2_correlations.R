####mean x position as a function of reaction time
setwd("C:/Users/r02al13/Documents/GitHub/HemianopiaTraining")

fxdat = readRDS(file="data/processedFixDataDisplayOn.Rda")
fxdat$subj = factor(fxdat$subj)
fxdat$session = as.factor(fxdat$session)
fxdat$xFix<-fxdat$xFixFlipped


fxdat = filter(fxdat, 
	targSide=="absent", 
	trialType=="blank", 
	difficulty=="parallel",
      fixNum<6)

fixAgg  = aggregate(data=fxdat,  xFix~ subj + session, FUN="mean")


#only for first session for nowstr(xdat)


rtdat = readRDS(file="data/processedRTandAccData.Rda")
rtdat$subj = as.factor(rtdat$subj)
rtdat$subj = factor(rtdat$subj)
rtdat$session = as.factor(rtdat$session)


rtdat = rtdat[which(rtdat$acc==1&rtdat$var=="parallel"&rtdat$trialType!="unmodified"&rtdat$targSide=="blind"),]
dataAgg  = aggregate(data=rtdat,  RT~ subjN + session, FUN="mean")

mergedData = data.frame(subj=fixAgg$subj, session=fixAgg$session, RT=dataAgg$RT,meanX=fixAgg$xFix)

plt = ggplot(mergedData, aes(x=meanX, y=RT)) 
plt = plt + geom_point(colour="gray") + geom_smooth(method="lm", se=F, colour="black")
plt = plt + theme_bw()+facet_wrap(~session, ncol=5)

plt = plt + scale_x_continuous(name="mean X position")
plt = plt + scale_y_continuous(name="reaction time (seconds)")

plt  
ggsave("plots/CorDisplayOn.pdf", height=9, width=9)