

library(lme4)
library(ggplot2)
library(scales)
library(dplyr)

fxdat = readRDS(file="../data/processedObjectData.Rda")

fxdat$session = as.factor(fxdat$session)

centreHalfWidth = 30

#  check number of trials for each person/block
ntrialsdat = (fxdat 
		%>% group_by(subj, session, trialType, targSide) 
		%>% summarise(
			nTrials = length(unique(trialNo))))

# define x=0 to be vertical midline
fxdat$xFix = fxdat$xFix - 512
# code into AOIs (left/centre/right)
fxdat$xAOI = 0
fxdat$xAOI[fxdat$xFix < -centreHalfWidth] = -1
fxdat$xAOI[fxdat$xFix >  centreHalfWidth] =  1

fxdat = filter(fxdat, fixNum<11)
meanXposbyFixNum  = aggregate(data=fxdat, xFix ~ subj + fixNum+ session, FUN="mean")


agDat = (fxdat 
	%>% group_by(session, fixNum)
	%>% summarise(
		meanX = mean(xFix),
		sdevX = sd(xFix),
		stder = sd(xFix)/sqrt(length(xFix)),
		lower = meanX - 1.96*stder,
		upper = meanX + 1.96*stder))


normpts = data.frame(fixNum=numeric(), session=numeric(), x=numeric(), y=numeric())
for (ii in 1:nrow(agDat))
{
	normpts = rbind(normpts, data.frame(
		fixNum=agDat$fixNum[ii],
		session=agDat$session[ii],
		x=seq(-400,400),
		y= dnorm(seq(-400,400), mean=agDat$meanX[ii], sd=agDat$stder[ii])))	
}

plot(dnorm(seq(-400,400), mean=agDat$meanX[1], sd=agDat$stder[1]))



plt = ggplot(fxdat, aes(x=xFix, fill=session))+ geom_histogram(alpha=0.5)
# plt = plt + stat_function(fun = dnorm, args = list(mean = agDat$meanX, sd=agDat$stder))
plt = plt + facet_grid(fixNum~.)
plt = plt + scale_x_continuous(limits=c(-600,600))
# plt = plt + geom_path(x=normpts$x, y=normpts$y, colour=normpts$session)
ggsave("fixX_A.pdf", width=4, height=12)


plt2 = ggplot(normpts, aes(x=x,y=y, colour=session)) + geom_path() + facet_grid(fixNum~.)
ggsave("fixX_B.pdf", width=4, height=12)

library(MCMCglmm)

m = MCMCglmm(xFix ~ as.factor(fixNum)*session, random=~subj, data=fxdat)