
setwd("C:/Users/Anna/Documents/GitHub/HemianopiaTraining")
library(scales)

#rtdat = readRDS(file="../data/processedRTandAccData.Rda")
rtdat = readRDS(file="data/processedRTandAccData.Rda")
cbPalette <- c("#E69F00", "#56B4E9")

library(lme4)
library(ggplot2)
library(scales)
library(dplyr)
library(boot)
library(binom)
# library(gridExtra)
# library(binom)


get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
levels(rtdat$trialType) = c("blank","blank", "unmodified")

#accdat  = aggregate(data=rtdat, acc ~ subjN + session + trialType + targSide + var, FUN="mean")
#write.csv(accdat, "data/accData.txt", row.names=F)

#rtdat = rtdat[which(rtdat$acc==1),]
#RT  = aggregate(data=rtdat, RT ~ subjN + session + trialType + targSide + var, FUN="median")
#write.csv(RT, "data/rtData.txt", row.names=F)


# let us first look at accuracy for target present and absent
aggAcc = (rtdat
  %>% group_by(subjN, session, trialType, targSide, var) 
    %>% summarise(
     nTrials=length(RT),
     acc=mean(acc),      
     lower = binom.confint(acc*nTrials,nTrials, method='wilson')$lower,
     upper = binom.confint(acc*nTrials,nTrials, method='wilson')$upper))

pd <- position_dodge(width = 0.5)
accplt = ggplot(aggAcc, aes(x=session, y=acc, ymin=lower, ymax=upper, colour=targSide))
accplt = accplt + geom_point(aes(group=subjN:targSide), position = pd) + geom_errorbar(aes(group=subjN:targSide), position = pd, width=0.5) 
accplt = accplt + geom_smooth(aes(group=targSide), method="glm",span=0.3,  se=F)
accplt = accplt + facet_grid(trialType~var)
accplt = accplt + theme_bw() + scale_y_continuous(name="accuracy")
#ggsave("../plots/accuracy.pdf", width=16, height=8)
ggsave("plots/accuracy.pdf", width=9, height=5)
ggsave("plots/accuracy.png", width=9, height=5)

rtdat$session = as.numeric(rtdat$session)

m1 = glmer(data=rtdat, 
	acc ~ session * trialType * targSide * var 
	+ (session+trialType+targSide+var|subjN), 
	family="binomial",
	control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun=100000)))
m2 = update(m1,~.-session:trialType:targSide:var)
m3 = update(m2,~.-session:targSide:var)
m4 = update(m3,~.-session:trialType:targSide)

# > Anova(m4)
# Analysis of Deviance Table (Type II Wald chisquare tests)

# Response: acc
#                          Chisq Df Pr(>Chisq)    
# session                50.9665  1  9.395e-13 ***
# trialType              46.8036  2  6.866e-11 ***
# targSide               32.7537  2  7.720e-08 ***
# var                    42.8988  1  5.765e-11 ***
# session:trialType       0.1983  2  0.9055885    
# session:targSide       21.6161  2  2.024e-05 ***
# trialType:targSide     26.7011  4  2.285e-05 ***
# session:var             5.4585  1  0.0194736 *  
# trialType:var           2.8877  2  0.2360211    
# targSide:var            2.6804  2  0.2617981    
# session:trialType:var   3.2985  2  0.1921973    
# trialType:targSide:var 19.2913  4  0.0006888 ***


# now lets look at RTs... 

# first we need to filter out incorrect trials
rtdat = rtdat[which(rtdat$acc==1),]



# rt for target present and absent
aggRtData = (rtdat
  %>% group_by(subjN, session, trialType, targSide, var) 
    %>% summarise(
     nTrials=length(RT),
     rt=median(RT, na.rm=T),      
     lower = quantile(RT, 0.25, na.rm=T),
     upper = quantile(RT, 0.75, na.rm=T)))


rtplt = ggplot(aggRtData, aes(x=session, y=rt, ymin=lower, ymax=upper, colour=targSide, group=targSide))
rtplt = rtplt + geom_point(aes(group=subjN:targSide), position = pd)+ geom_smooth(method="lm",span=0.3 ) + geom_errorbar(aes(group=subjN:targSide), position = pd, width=0.5) 
rtplt = rtplt + facet_grid(trialType~var) + coord_trans(y="log2")
rtplt = rtplt + theme_bw() + scale_y_continuous(name="median reaction time (seconds)", breaks=c(1,2,4,8,16))#, limits=c(1,16)
#ggsave("../plots/RTserial.pdf", width=16, height=8)
ggsave("../plots/RTserial.pdf", width=12, height=6)
ggsave("../plots/RTserial.jpg", width=12, height=6)



# model reaction times
m1 = lmer(data=rtdat, 
	scale(log(RT)) ~ session*targSide*trialType*var + (session+targSide+trialType+var|subjN), 
	control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun=100000)))

# Analysis of Deviance Table (Type II Wald chisquare tests)

# Response: scale(log(RT))
#                                    Chisq Df Pr(>Chisq)    
# session                          16.5595  1  4.715e-05 ***
# targSide                        220.7999  2  < 2.2e-16 ***
# trialType                        63.2704  1  1.802e-15 ***
# var                              68.9636  1  < 2.2e-16 ***
# session:targSide                 71.0196  2  3.787e-16 ***
# session:trialType                22.4830  1  2.120e-06 ***
# targSide:trialType              637.6169  2  < 2.2e-16 ***
# session:var                       0.6997  1    0.40288    
# targSide:var                   1689.5594  2  < 2.2e-16 ***
# trialType:var                     0.3992  1    0.52748    
# session:targSide:trialType        0.7676  2    0.68128    
# session:targSide:var             74.3003  2  < 2.2e-16 ***
# session:trialType:var            29.7427  1  4.934e-08 ***
# targSide:trialType:var          110.1097  2  < 2.2e-16 ***
# session:targSide:trialType:var    8.0889  2    0.01752 *


# m2 = update(m1, ~.-session:targSide:trialType)
# m3 = update(m2, ~.-session:targSide)
# m4 = update(m3, ~.-session:trialType)
# m5 = update(m4, ~.-session)

m2 = lmer(data=rtdat, 
  scale(log(RT)) ~ targSide*trialType*var + (session+targSide+trialType+var|subjN), 
  control=lmerControl(optimizer="bobyqa", optCtrl = list(maxfun=100000)))



write.csv(rtdat2, "data/RTdata.txt", row.names=F)

rtdat3  = aggregate(data=rtdat, RT ~  subj+ hemiType + var + targSide, FUN="median")
write.csv(rtdat3, "../data/rtData.txt", row.names=F)

legend<- get_legend(pAcc2)
legend<- get_legend(pRT1)
pAcc2<- pAcc2+ theme(legend.position="none")
pRT1<- pRT1+ theme(legend.position="none")
jpeg(filename = "plots/RtAcc.jpg",width=1200,height=500, pointsize =10, quality = 1000, bg = "white", res = 150, restoreConsole = TRUE)

grid.arrange(pRT1,pAcc2,legend,ncol=3,widths=c(12,12, 5))
dev.off()

family="binomial",


