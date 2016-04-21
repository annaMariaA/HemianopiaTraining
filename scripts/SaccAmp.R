


library(lme4)
library(ggplot2)
library(scales)
library(dplyr)
library(car)

rtdat = readRDS(file="../data/processedRTandAccData.Rda")
fxdat = readRDS(file="../data/processedFixData.Rda")


fxdat$session = as.factor(fxdat$session)


fxdat = filter(fxdat, targSide == "absent", fixNum<51)

# take the subset of target absent + serial/parallel search
# fxdat = filter(fxdat, targSide=="absent", trialType=="blank", difficulty=="parallel")

medianSaccAmpPerTrial = aggregate(saccAmp ~
	subj + session + trialType + targSide + difficulty + trial, fxdat, "median" )

	# distrution is skewed so take log
	medianSaccAmpPerTrial$saccAmp = log(medianSaccAmpPerTrial$saccAmp)

# mean sacc amp per person
meanSaccAmpPerTrial = aggregate(saccAmp ~
	subj + session + trialType + targSide + difficulty, medianSaccAmpPerTrial, "mean" )

aggDat = (meanSaccAmpPerTrial 
	%>% group_by(session, trialType, targSide, difficulty)
	%>% summarise(
		saccadicAmp = mean(saccAmp),
		stdev = sd(saccAmp),
		stder = stdev/sqrt(length(levels(fxdat$subj))),
		lower = saccadicAmp - 1.96*stder,
		upper = saccadicAmp + 1.96*stder))

plt = ggplot(aggDat, aes(x=session, y=saccadicAmp, ymin=lower, ymax=upper, group=1))
plt = plt + geom_errorbar()
plt = plt + geom_smooth(method=lm)
plt = plt + facet_grid(difficulty ~ trialType)
plt = plt + scale_y_continuous(name = "mean ( median (log (saccadic amp.)))")
plt = plt + theme_light()
plt
ggsave("../plots/saccAmpBySession.pdf")


medianFixDurPerTrial = aggregate(fixDur ~
	subj + session + trialType + targSide + difficulty + trial, fxdat, "median" )

meanFixDurPerTrial = aggregate(fixDur ~
	subj + session + trialType + targSide + difficulty, medianFixDurPerTrial, "mean" )

aggDat = (meanFixDurPerTrial 
	%>% group_by(session, trialType, targSide, difficulty)
	%>% summarise(
		fixationDur = mean(fixDur),
		stdev = sd(fixDur),
		stder = stdev/sqrt(length(levels(fxdat$subj))),
		lower = fixationDur - 1.96*stder,
		upper = fixationDur + 1.96*stder))


plt = ggplot(aggDat, aes(x=session, y=fixationDur, ymin=lower, ymax=upper, group=1))
plt = plt + geom_errorbar()
plt = plt + geom_smooth(method=lm)
plt = plt + facet_grid(difficulty ~ trialType)
plt = plt + scale_y_continuous(name = "mean ( median (log (fix. dur.)))")
plt = plt + theme_light()
plt
ggsave("../plots/fixDurBySession.pdf")