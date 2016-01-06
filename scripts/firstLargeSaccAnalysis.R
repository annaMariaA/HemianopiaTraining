 options(width=125)

library(lme4)
library(ggplot2)
library(scales)
library(dplyr)
library(car)

rtdat = readRDS(file="../data/processedRTandAccData.Rda")
fxdat = readRDS(file="../data/processedFixData.Rda")

# analyse fix-x position for second fixation

pltX = ggplot(filter(fxdat, fixNum==2, targSide=="absent"), aes(x=xFix, fill=difficulty))
pltX = pltX + geom_density(alpha=0.5) + facet_grid(trialType ~ session)
pltX


# model1 = lmer(scale(xFix) ~ session*trialType*difficulty
# 	+ (trialType+difficulty+session|subj), 
# 	filter(fxdat, fixNum==2, targSide=="absent"),
# 	control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun=100000)))