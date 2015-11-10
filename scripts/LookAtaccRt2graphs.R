#setwd("E:/Anna Desktop/SimulatedHemLineSegm")
#setwd("C:/Users/r02al13/Desktop/LineSegmEasyDiff")
rtdat = readRDS(file="../data/processedRTandAccData.Rda")

cbPalette <- c("#E69F00", "#56B4E9")

library(lme4)
library(ggplot2)
library(scales)
library(dplyr)
library(boot)
library(gridExtra)
library(binom)

get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# let us first look at accuracy for target present and absent
aggAcc = (rtdat
  %>% group_by(subjN, session, trialType, targSide) 
    %>% summarise(
     nTrials=length(RT),
     acc=mean(acc),      
     lower = binom.confint(acc*nTrials,nTrials, method='wilson')$lower,
     upper = binom.confint(acc*nTrials,nTrials, method='wilson')$upper))

accplt = ggplot(aggAcc, aes(x=session, y=acc, ymin=lower, ymax=upper, colour=targSide, group=targSide))
accplt = accplt + geom_point() + geom_errorbar() + geom_path()
accplt = accplt + facet_grid(trialType~subjN)
accplt = accplt + theme_bw() + scale_y_continuous(name="accuracy")
ggsave("../plots/accuracy.jpg",dpi=600, width=10, height=5)
write.csv(accdattargPres, "data/accDataTargPres.txt", row.names=F)

# now lets look at RTs... 

# first we need to filter out incorrect trials
rtdat = rtdat[which(rtdat$acc==1),]





library(scales)     

# rt for target present and absent
aggRtData = (rtdat
  %>% group_by(subjN, session, trialType, targSide) 
    %>% summarise(
     nTrials=length(RT),
     rt=median(RT),      
     lower = quantile(RT, 0.25),
     upper = quantile(RT, 0.75)))


rtplt = ggplot(aggRtData, aes(x=session, y=rt, ymin=lower, ymax=upper, colour=targSide, group=targSide))
rtplt = rtplt + geom_point() + geom_errorbar() + geom_smooth(method="lm")
rtplt = rtplt + facet_grid(trialType~subjN, scales="free_y")
rtplt = rtplt + theme_bw() + scale_y_continuous(name="median reaction time (seconds)")
ggsave("../plots/RT.jpg",dpi=600, width=10, height=5)

rtplt = ggplot(rtdat, aes(x=session, y=RT, colour=targSide, group=targSide))
rtplt = rtplt + geom_point()  + geom_smooth(method="lm")
rtplt = rtplt + facet_grid(trialType~subjN, scales="free_y")
rtplt = rtplt + theme_bw() + scale_y_continuous(name="median reaction time (seconds)")
ggsave("../plots/RT2.jpg",dpi=600, width=10, height=5)


library(lme4)
rtdat$session = as.numeric(rtdat$session)
m = lmer(data=rtdat, scale(log(RT)) ~ session*targSide*trialType + (session+targSide+trialType|subjN))


write.csv(rtdat2, "data/RTdata.txt", row.names=F)

rtdat3  = aggregate(data=rtdat, RT ~  subj+ hemiType + var + targSide, FUN="median")
write.csv(rtdat3, "../data/rtData.txt", row.names=F)

legend<- get_legend(pAcc2)
legend<- get_legend(pRT1)
pAcc2<- pAcc2+ theme(legend.position="none")
pRT1<- pRT1+ theme(legend.position="none")
jpeg(filename = "../plots/RtAcc.jpg",width=1200,height=500, pointsize =10, quality = 1000, bg = "white", res = 150, restoreConsole = TRUE)

grid.arrange(pRT1,pAcc2,legend,ncol=3,widths=c(12,12, 5))
dev.off()




