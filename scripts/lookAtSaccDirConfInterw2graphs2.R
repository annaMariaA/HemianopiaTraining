setwd("C:/Users/r02al13/Documents/GitHub/HemianopiaTraining")
fixdat = readRDS(file="data/processedFixData.Rda")
cbPalette <- c("#56B4E9", "#E69F00", "#0072B2", "#D55E00", "#CC79A7")
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
#fixdat = fixdat[which(fixdat$fixNum==1),]
fixdat = fixdat[which(fixdat$hemiType=="Blank"),]
propSaccBlind = aggregate(data=fixdat, prop ~ subjN+var+session, FUN="mean")
p1 = ggplot(data=propSaccBlind, aes(x=var, y=prop, group=session, colour=session,group=supp))+geom_line(position=pd, size=1 )+geom_point(position=pd,size=2)
p1 = p1 + scale_y_continuous(limits=c(0.1, 1),name="Proportion of all saccades into blind") + scale_x_discrete(name="Search difficulty")+ scale_color_manual(name="Session",values=cbPalette)+ facet_wrap(~subjN)
p1= p1+ theme_minimal()
ggsave("plots/proportionAllSaccadesIntoBlind.jpg", width=10, height=3)

