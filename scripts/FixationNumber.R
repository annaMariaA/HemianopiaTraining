setwd("C:/Users/r02al13/Documents/GitHub/HemianopiaTraining")
fixdat = readRDS(file="data/processedFixData.Rda")
cbPalette <- c("#56B4E9", "#E69F00", "#0072B2", "#D55E00", "#CC79A7")
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

nbins = 16
bw = 360/nbins	
# only look at TA trials

fixdat=fixdat[fixdat$subjN=="A",]

fixdatSighted=fixdat[fixdat$targSide=="sighted",]
numFixSighted = aggregate(data=fixdatSighted, fixNum ~ hemiType+var+session+trial, FUN="median")
numFixSighted = aggregate(data=numFixSighted, fixNum ~ hemiType+var+session, FUN="mean")

fixdatBlind=fixdat[fixdat$targSide=="blind",]
numFixBlind = aggregate(data=fixdatBlind, fixNum ~ hemiType+var+session+trial, FUN="median")
numFixBlind = aggregate(data=numFixBlind, fixNum ~ hemiType+var+session, FUN="mean")

nFix = ggplot(numFixSighted, aes(x=session, y=fixNum, group=var, colour=var, group=supp)) + geom_smooth(method=lm,size=1) + geom_point(position=pd,size=2)
nFix = nFix + scale_y_continuous(name="Numbe of Fixations") + scale_x_discrete(name="Target Position")+scale_color_manual(name="Search Difficulty", values=cbPalette) + facet_wrap(~hemiType)
nFix = nFix +theme_minimal()
ggsave("plots/NumFixSightedA11.jpg",dpi=600, width=6, height=3)


# now lets look at RTs... 
# first we need to filter out incorrect trials
rtdat = rtdat[which(rtdat$acc==1),]
library(scales)     
# rt for target present and absent
rtdat2  = aggregate(data=rtdat, RT ~ trial+session+var+subj, FUN="median")
errorbarRT <- summarySEwithin(rtdat2, measurevar="RT", withinvars=c("session","var", "subj"), idvar="trial")
pRT1 = ggplot(errorbarRT, aes(x=session, y=RT, fill=var)) + geom_bar(stat="identity", position="dodge") + theme_minimal()
pRT1 = pRT1 + scale_y_continuous(name="Reaction Time") + scale_x_discrete(name="Target Position")+scale_fill_manual(name="Search Difficulty", values=cbPalette) + facet_wrap(~subj)
pRT1 = pRT1 + geom_errorbar(position=position_dodge(.9), within=.25, aes(ymin=RT-ci,ymax=RT+ci),width=.5)

ggsave("plots/RTunmod.jpg",dpi=600, width=6, height=3)
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
