
library(lme4)
library(ggplot2)
library(scales)
library(bear)
library(boot)

#setwd("C:/Users/r02al13/Desktop/SimulatedHemianopia")
fixdat=read.table("mergedFix.txt")
#fixdat = readRDS(file="data/processedFixData.Rda")
cbPalette <- c("#56B4E9", "#E69F00", "#B5CB8B")
nbins = 16
bw = 360/nbins	
# only look at TA trials
fixdat = fixdat[fixdat$targSide=="absent",]
pd=position_dodge(0.1)


fixdat$saccAng = (180/pi) * (fixdat$saccAng ) + 180
fixdat$saccAng = ((fixdat$saccAng) %% 360)


library(ggplot2)

# first look at amplitude
ampplot = ggplot(fixdat, aes(x=saccAmp)) + geom_density() + facet_grid(.~hemiType)

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
ggsave("../plots/roseplot.pdf", width=10, height=5)



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
#fixdat$saccAmp=log(fixdat$saccAmp)
data=aggregate(data=fixdat, saccAmp ~ subj+hemiType+saccSide, FUN="mean")
write.csv(data, "NonSuccesiveSaccAmpLog1.txt", row.names=F)

fixdat$saccAmp[which(fixdat$saccSide=="blind")] = -fixdat$saccAmp[which(fixdat$saccSide=="blind")]

sideplt = ggplot(na.omit(fixdat), aes(x=saccAmp, fill=saccSide)) + geom_density(alpha = 1.0, trim=TRUE) + facet_grid(.~hemiType)
sideplt = sideplt  + scale_x_continuous(name="Saccadic Amplitude", limits=c(-768, 768),   breaks=c(-1024,-512, -256, 0, 256, 512, 1024))
sideplt = sideplt  + theme_bw()+ scale_fill_manual(name="Saccades into...", values=cbPalette)
ggsave("sightVblindSaccAmp1.jpg",dpi=600, width=10, height=3)