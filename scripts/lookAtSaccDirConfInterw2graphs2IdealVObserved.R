setwd("C:/Users/r02al13/Desktop/LineSegmEasyDiff")
fixdat = readRDS(file="data/processedFixData.Rda")
cbPalette <- c("#56B4E9", "#E69F00")
library(lme4)
library(ggplot2)
library(scales)
library(bear)
library(boot)

nbins = 16
bw = 360/nbins	
# only look at TA trials
fixdat = fixdat[fixdat$targSide=="absent",]


fixdat$saccAng = (180/pi) * (fixdat$saccAng ) + 180
fixdat$saccAng = ((fixdat$saccAng) %% 360)


library(ggplot2)

# first look at amplitude
ampplot = ggplot(fixdat, aes(x=saccAmp)) + geom_density() + facet_grid(.~hemiType)
ampplot
saccAngle = data.frame(theta=numeric(), med_amp=numeric(), count=numeric(), hemiType=character())
for (h in levels(fixdat$hemiType))
{
  	# do rest of bins
	for (b in 1:nbins)
    {
    	b1 = (b-1)*bw 
    	b2 = (b)*bw 
 		# get median saccade amplitude for these saccades
   	 	idx = which(fixdat$saccAng>=b1 & fixdat$saccAng<b2 & fixdat$hemiType==h)
   	 	count = length(idx)/length(which(fixdat$hemiType==h))
   	 	medsaccamp = median(fixdat$saccAmp[idx])
  	 	saccAngle = rbind(saccAngle, data.frame(theta=(b-1)*bw+bw/2, med_amp=medsaccamp, count=count, hemiType=h))
   }
}


rosepltAng <- ggplot(saccAngle, aes(x=theta, y=count)) + geom_bar(width=bw, stat="identity") 
rosepltAng <- rosepltAng + scale_x_continuous(name=" ", limits=c(0,360), breaks = c(0, 45, 90, 135, 180, 225, 270, 315))
rosepltAng <- rosepltAng +scale_y_continuous(name=" ",breaks=NULL)+ coord_polar(start=0, direction=1)+theme_bw() + facet_grid(.~hemiType)
#ggsave("../plots/roseplot.pdf", width=10, height=5)



rosepltAng <- ggplot(saccAngle, aes(x=theta, y=med_amp)) + geom_bar(width=bw, stat="identity") 
rosepltAng <- rosepltAng + scale_x_continuous(name=" ", limits=c(0,360), breaks = c(0, 45, 90, 135, 180, 225, 270, 315))
rosepltAng <- rosepltAng +scale_y_continuous(name=" ",breaks=NULL)+ coord_polar(start=0, direction=1)+theme_bw() + facet_grid(.~hemiType)


# get distribution for left v right saccades
sideAngularWidth = 90

intoBlindl = 270 - sideAngularWidth/2
intoBlindu = 270 + sideAngularWidth/2
intoSightl = 090 - sideAngularWidth/2
intoSightu = 090 + sideAngularWidth/2

fixdat$saccSide = NA
fixdat$saccSide[which(fixdat$saccAng>=intoBlindl & fixdat$saccAng<=intoBlindu)] = "blind"
fixdat$saccSide[which(fixdat$saccAng>=intoSightl & fixdat$saccAng<=intoSightu)] = "sight"
fixdat$saccSide = as.factor(fixdat$saccSide)

fixdat$saccAmp[which(fixdat$saccSide=="blind")] = -fixdat$saccAmp[which(fixdat$saccSide=="blind")]
sideplt = ggplot(na.omit(fixdat), aes(x=saccAmp, fill=saccSide)) + geom_density(alpha = 1.0, trim=TRUE) + facet_grid(.~hemiType)
sideplt = sideplt  + scale_x_continuous(name="Saccadic Amplitude", limits=c(-1024,1024),   breaks=c(-1024, 0, 1024), minor_breaks=c(seq(-1024,1024, 128)))
sideplt = sideplt  + theme_bw()+  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +scale_fill_manual(name="Target position", values=cbPalette)
ggsave("plots/sightVblindSaccAmp.pdf", width=10, height=3)

pd<-position_dodge(.1)
fixdat$prop = fixdat$saccSide == "blind"

propSaccBlind = aggregate(data=fixdat, prop ~ subj+var+hemiType, FUN="mean")
propSaccBlind$subj <- factor(propSaccBlind$subj, levels = c("1", "2", "3","4", "5", "6","8", "9", "10","11", "12", "13","14", "15", "16","18", "19", "20","21", "22"))
errorBar=summarySE(propSaccBlind, measurevar="prop", groupvars=c("var", "hemiType"))
errorBar$fakeProp=c(0.5,0.5,1,0.5)
errorBar$fakeCI=c(0.48,0.04,0.001,0.05)
p1 = ggplot(data=errorBar, aes(x=var, y=fakeProp, group=hemiType, colour=hemiType,group=supp))+geom_line(position=pd, size=1 )+geom_point(position=pd,size=2)
p1 = p1 + scale_y_continuous(limits=c(0, 1),name="") + scale_x_discrete(name="Search difficulty")+ scale_color_manual(name="Mask type",values=cbPalette)
p1= p1+ theme_minimal()
#p1=p1+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
 #   panel.background = element_blank(), axis.line = element_line(colour = "black"))
p1 = p1 + geom_errorbar(aes(ymin=fakeProp-fakeCI, ymax=fakeProp+fakeCI), width=.1, position=pd)+ ggtitle("Optimal")
p1

#rtdat2  = aggregate(data=rtdat, RT ~ subj + hemiType + var + targSide, FUN="median")
#errorbarRT <- summarySEwithin(rtdat2, measurevar="RT", withinvars=c("targSide","hemiType", "var"), idvar="subj")
g = ggplot(errorBar, aes(x=var, y=fakeProp, fill=var)) + geom_bar(stat="identity", position="dodge") + theme_minimal()
g = g + scale_y_continuous(name="") + scale_x_discrete(name="Search difficulty")+scale_fill_manual(name="Mask type", values=cbPalette) + facet_wrap(~hemiType)
g = g + geom_errorbar(position=position_dodge(.9), within=.25, aes(ymin=fakeProp-fakeCI, ymax=fakeProp+fakeCI),width=.5)
g = g + ggtitle("Predicted")





fixdat = fixdat[which(fixdat$fixNum==1),]
propSaccBlind = aggregate(data=fixdat, prop ~ subj+var+hemiType, FUN="mean")
propSaccBlind$subj <- factor(propSaccBlind$subj, levels = c("1", "2", "3","4", "5", "6","8", "9", "10","11", "12", "13","14", "15", "16","18", "19", "20","21", "22"))
errorBar=summarySE(propSaccBlind, measurevar="prop", groupvars=c("var", "hemiType"))
p2 = ggplot(data=errorBar, aes(x=var, y=prop, group=hemiType, colour=hemiType,group=supp))+geom_line(position=pd, size=1 )+geom_point(position=pd,size=2)
p2 = p2 + scale_y_continuous(limits=c(0, 1),name="") + scale_x_discrete(name="Search difficulty") + scale_color_manual(name="Mask type",values=cbPalette)
p2=p2 + theme_minimal()
#p2=p2+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
 #   panel.background = element_blank(), axis.line = element_line(colour = "black"))

p2 = p2 + geom_errorbar(aes(ymin=prop-ci, ymax=prop+ci), width=.1, position=pd)+ ggtitle("Observed")

g1 = ggplot(errorBar, aes(x=var, y=prop, fill=var)) + geom_bar(stat="identity", position="dodge") + theme_minimal()
g1 = g1 + scale_y_continuous(limits=c(0, 1),name="") + scale_x_discrete(name="Search difficulty")+scale_fill_manual(name="Mask type", values=cbPalette) + facet_wrap(~hemiType)
g1 = g1 + geom_errorbar(position=position_dodge(.9), within=.25, aes(ymin=prop-ci, ymax=prop+ci),width=.5)
g1 = g1 + ggtitle("Observed")


library(gridExtra)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend<- get_legend(p1)
legend<- get_legend(p2)
g<- g+ theme(legend.position="none")
g1<- g1+ theme(legend.position="none")
jpeg(filename = "plots/2graphsImagined.jpg",width=900,height=600, pointsize =10, quality = 1000, bg = "white", res = 150, restoreConsole = TRUE)

grid.arrange(g,g1,ncol=2,widths=c(13,13))
dev.off()


