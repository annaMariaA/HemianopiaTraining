	

library(lme4)
library(ggplot2)
library(scales)
library(dplyr)
library(car)
library(gridExtra)

cbPalette <- c("#E69F00", "#56B4E9","#B5CB8B")
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

setwd("C:/Users/r02al13/Documents/GitHub/HemianopiaTraining/Data")
fxdat = readRDS(file="processedObjectData.Rda")

fxdat$session = as.factor(fxdat$session)

# define x=0 to be vertical midline
fxdat$xFix = fxdat$xFix - 512

# code into AOIs (left/centre/right)
centreHalfWidth = 30
fxdat$xAOI = 0
fxdat$xAOI[fxdat$xFix < -centreHalfWidth] = -1
fxdat$xAOI[fxdat$xFix >  centreHalfWidth] =  1

####################################
# plot the mean x position
####################################

xdat = (fxdat 
		%>% group_by(fixNum, session, subj) 
		%>% summarise(
			meanX=mean(xFix), 
			nFix=length(xFix)))

#do some statistics, save aggregated results
xdat2 = xdat[which(xdat$fixNum>1 & xdat$fixNum<=10),]
xdat2  = aggregate(data=xdat2, meanX ~ subj + session, FUN="mean")
write.csv(xdat2, "aggregatedObjectNamingTraining.txt", row.names=F)

# plot
# remove entries with fewer than 8 fixations
xdat = filter(xdat, fixNum<=8)
plt1 = ggplot(xdat, aes(y=meanX, x=fixNum, colour=session))
plt1 = plt1 + geom_point(position=position_jitter(height=0.01, width=0.1))
plt1 = plt1 + geom_smooth(se=F)
plt1 = plt1 + scale_y_continuous(name="mean x position of fixation", expand=c(0,0), limits=c(-400,400))
plt1 = plt1 + scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8))
plt1 = plt1 + theme_light()+ ggtitle("Training")+ theme(plot.title = element_text(hjust = 0.5))
plt1
ggsave("meanXfixPos_agg.pdf", width=8, height=6)


#CONTROL STUDY
setwd("C:/Users/r02al13/Documents/GitHub/HemianopiaTraining/ControlStudyResults/ObjectNaming")
fxdat = readRDS(file="processedObjectData.Rda")
#fxdat = readRDS(file="../data/processedObjectData.Rda")

fxdat$session = as.factor(fxdat$session)

# define x=0 to be vertical midline
fxdat$xFix = fxdat$xFix - 512

# code into AOIs (left/centre/right)
centreHalfWidth = 30
fxdat$xAOI = 0
fxdat$xAOI[fxdat$xFix < -centreHalfWidth] = -1
fxdat$xAOI[fxdat$xFix >  centreHalfWidth] =  1

####################################
# plot the mean x position
####################################

xdat = (fxdat 
		%>% group_by(fixNum, session, subj) 
		%>% summarise(
			meanX=mean(xFix), 
			nFix=length(xFix)))

write.csv(xdat, "xdat.txt", row.names=F)

#do some statistics, save aggregated results
xdat2 = xdat[which(xdat$fixNum>1 & xdat$fixNum<=10),]
xdat2  = aggregate(data=xdat2, meanX ~ subj + session, FUN="mean")
write.csv(xdat2, "aggregatedObjectNamingControl.txt", row.names=F)

# plot
# remove entries with fewer than 10 fixations
xdat = filter(xdat, fixNum<=8)
plt2 = ggplot(xdat, aes(y=meanX, x=fixNum, colour=session))
plt2 = plt2 + geom_point(position=position_jitter(height=0.01, width=0.1))
plt2 = plt2 + geom_smooth(se=F)
plt2 = plt2 + scale_y_continuous(name="mean x position of fixation", expand=c(0,0), limits=c(-400,400))
plt2 = plt2 + scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8))
plt2 = plt2 + theme_light()+ ggtitle("Control")+ theme(plot.title = element_text(hjust = 0.5))
plt2
ggsave("meanXfixPos_agg.pdf", width=8, height=6)

legend<- get_legend(plt1)
legend<- get_legend(plt2)
plt2<- plt2+ theme(legend.position="none")
plt1<- plt1+ theme(legend.position="none")
jpeg(filename = "MeanFixationPosition2graphsObjects.jpg",width=1400,height=500, pointsize =5, quality = 1000, bg = "white", res = 150, restoreConsole = TRUE)

grid.arrange(plt1,plt2,legend,ncol=3,widths=c(8,8, 3))
dev.off()


