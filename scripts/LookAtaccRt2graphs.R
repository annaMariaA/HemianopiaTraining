#setwd("E:/Anna Desktop/SimulatedHemLineSegm")
#setwd("C:/Users/r02al13/Desktop/LineSegmEasyDiff")
rtdat = readRDS(file="data/processedRTandAccData.Rda")

cbPalette <- c("#E69F00", "#56B4E9")

library(lme4)
library(ggplot2)
library(scales)
library(bear)
library(boot)
library(gridExtra)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
# let us first look at accuracy for target present and absent

#rtdat = rtdat[rtdat$subj=="A",]
rtdat = rtdat[rtdat$hemiType=="Blank",]

accdat2 = aggregate(data=rtdat, acc ~ trial+ session+var +subjN, FUN="mean")
errorbar <- summarySEwithin(accdat2, measurevar="acc", withinvars=c("session", "var", "subjN"), idvar="trial")
pAcc2 = ggplot(errorbar, aes(x=session, y=100*acc, fill=var)) + geom_bar(stat="identity", position="dodge") + theme_minimal()
pAcc2 = pAcc2 + scale_y_continuous(name="Accuracy(%)") + scale_x_discrete(name="Target Position")+scale_fill_manual(name="Search Difficulty", values=cbPalette) + facet_wrap(~subjN)
pAcc2 = pAcc2 + geom_errorbar(position=position_dodge(.9), within=.25, aes(ymin=(acc-ci)*100,ymax=(acc+ci)*100),width=.5)

ggsave("plots/accuracy1.jpg",dpi=600, width=6, height=3)
write.csv(accdattargPres, "data/accDataTargPres.txt", row.names=F)

# now lets look at RTs... 
# first we need to filter out incorrect trials
rtdat = rtdat[which(rtdat$acc==1),]
library(scales)     
# rt for target present and absent
rtdat2  = aggregate(data=rtdat, RT ~ trial+session+var+subjN, FUN="median")
errorbarRT <- summarySEwithin(rtdat2, measurevar="RT", withinvars=c("session","var", "subjN"), idvar="trial")
pRT1 = ggplot(errorbarRT, aes(x=session, y=RT, fill=var)) + geom_bar(stat="identity", position="dodge") + theme_minimal()
pRT1 = pRT1 + scale_y_continuous(name="Reaction Time") + scale_x_discrete(name="Target Position")+scale_fill_manual(name="Search Difficulty", values=cbPalette) + facet_wrap(~subjN)
pRT1 = pRT1 + geom_errorbar(position=position_dodge(.9), within=.25, aes(ymin=RT-ci,ymax=RT+ci),width=.5)

ggsave("plots/RTBlank.jpg",dpi=600, width=6, height=3)
write.csv(rtdat2, "data/RTdata.txt", row.names=F)

rtdat3  = aggregate(data=rtdat, RT ~  subj+ hemiType + var + targSide, FUN="median")
write.csv(rtdat3, "data/rtData.txt", row.names=F)

legend<- get_legend(pAcc2)
legend<- get_legend(pRT1)
pAcc2<- pAcc2+ theme(legend.position="none")
pRT1<- pRT1+ theme(legend.position="none")
jpeg(filename = "plots/RtAcc.jpg",width=1200,height=500, pointsize =10, quality = 1000, bg = "white", res = 150, restoreConsole = TRUE)

grid.arrange(pRT1,pAcc2,legend,ncol=3,widths=c(12,12, 5))
dev.off()




