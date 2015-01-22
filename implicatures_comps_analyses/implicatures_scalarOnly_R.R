rm(list=ls())
library(lme4)
library(plotrix)
library(ggplot2)
library(diptest)
library(plyr)
d1 <- read.csv("~/new compy/Stanford/Research/Frank Lab/Conferences/CogSci/CogSci 2015/implicatures_comps_analyses/implicatures_scalarOnly_data.csv")


##########################TEST TRIALS#########################
library(reshape)

d2=melt.data.frame(d1,c("Subj_ID","age","agegroup","list"),c("carrots", "hats", "cookies", "trains", "cats", "purses",  "keys", "shirts", "breads", "horses", "bears", "frogs", "plates", "books", "elephants", "lamps", "bananas", "butterflies", 
"carrots_selectionType", "hats_selectionType", "cookies_selectionType", "trains_selectionType", "cats_selectionType", "purses_selectionType",  "keys_selectionType", "shirts_selectionType", "breads_selectionType", "horses_selectionType", "bears_selectionType", "frogs_selectionType", "plates_selectionType", "books_selectionType", "elephants_selectionType", "lamps_selectionType", "bananas_selectionType", "butterflies_selectionType", 
"carrots_type", "hats_type", "cookies_type", "trains_type", "cats_type", "purses_type",  "keys_type", "shirts_type", "breads_type", "horses_type", "bears_type", "frogs_type", "plates_type", "books_type", "elephants_type", "lamps_type", "bananas_type", "butterflies_type"))



data <- d2[1:918,] 
data$selectionType <- d2$value[919:1836]
data$type <- d2$value[1837:2754]
names(data)[5] <- "item"
names(data)[6] <- "correct"

#data$older <- (data$age>4.5)
data$correct <- data$correct==1


attach(data)
data$trial_type[type == "some"] <- "implicature"
data$trial_type[type != "some"] <- "control"
detach(data)



######## data by selectionType and trial type ##########
agg.data <- aggregate(data$correct, list( data$type, data$trial_type, data$agegroup), FUN=sum)
agg.data.len <- aggregate(data$correct, list(data$type, data$trial_type, data$agegroup), FUN=length)
agg.data$x <- agg.data$x 
agg.data.len$x <- agg.data.len$x 

names(agg.data) <- c( "trial_type", "implicature", "agegroup", "correct")
agg.data$total <- agg.data.len$x
agg.data$prop.corr <- agg.data$correct / agg.data$total

agg.data$q <- 1 - agg.data$prop.corr
agg.data$err <- sqrt((agg.data$prop.corr * agg.data$q) / agg.data$total)

########  plot by age
plot.style <- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_line(colour="black",size=.5), axis.ticks = element_line(size=.5),legend.justification=c(1,0), legend.position=c(0.89,.55),legend.title=element_blank(), axis.title.x = element_text(vjust=-.5), axis.title.y = element_text(angle=90,vjust=0.25))

dodge <- position_dodge(width=0.9) 
limits <- aes(ymax = prop.corr + err, ymin=prop.corr - err) 


qplot(data = agg.data,
	x = trial_type,
	y = prop.corr,
	geom="bar",
	stat="identity",
	fill=agegroup,	
	#main="Preschooler results", 
	ylab="Proportion Correct",
	xlab="Scalar trial type",
	position=dodge,
	ylim=c(0,1))  + geom_abline(intercept=.5,slope=0,lty=2) + geom_errorbar(limits,position=dodge,width=0.25) + theme_bw() + plot.style


########## individual differences

ms <- ddply(data, .(type, trial_type, Subj_ID), summarise, 
	mean = mean(correct),
	agegroup=agegroup[1])
	#older = older[1])	
	#note: am I allowed to substitute agegroup for older

cs <- cast(ms, Subj_ID + agegroup ~ type + trial_type, value="mean")

qplot(none_control, some_implicature, col=agegroup, position=position_jitter(.05), data=cs) + geom_smooth(method="lm")

qplot(none_control, some_implicature, col=agegroup, 
	ylab="Proportion of 'some' trials correct",
	xlab="Proportion of 'none' trials correct",
		position=position_jitter(.02), data=cs) + 
	geom_smooth(method="lm", col="black", lty=1) + 
	geom_smooth(aes(col=agegroup, group=agegroup), 
				se=FALSE, method="lm",lty=3)
library(dplyr)
cor.test(cs$none_control, cs$some_implicature)
cs %>% group_by(agegroup) %>% 
	summarise(r = cor.test(none_control, some_implicature)$estimate,
			  p = cor.test(none_control, some_implicature)$p.value)

dip.test(cs$some_implicature)
dip.test(cs$none_control)
dip.test(cs$all_control)


gl <- glmer(correct ~  type * age    + (type | Subj_ID), data=data, family=binomial)
summary(gl)


gl <- glmer(correct ~  trial_type * age    + (trial_type | Subj_ID), data=data, family=binomial)
summary(gl)




