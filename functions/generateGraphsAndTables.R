### generate graph and table output
#     Copyright (C) 2019  Leonardo Jost
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

source("functions/helpers.R")

##create output directories, if they don't exist (outputs warnings otherwise)
dir.create("figs")
dir.create("figs/MR")
dir.create("figs/phys")
library(ggplot2)
baseSize=15
#H1 crpe
dataset=datasetPhysNoOutlier
ggplot(dataset,aes(y=cRPE,x=relativeResistanceUnscaled, color=condition, linetype=condition)) + 
  geom_smooth() +
  labs(x="Relative Resistance",y="Cognitive RPE") + 
  theme_classic(base_size = baseSize)
ggsave("figs/MR/cRPE.png")
#H1a rt
#dataset to plot
dataset=datasetMRNoOutlier[which(datasetMRNoOutlier$type=="hit"),]
ggplot(dataset,aes(y=reactionTime,x=relativeResistanceUnscaled)) + 
  geom_smooth() +
  labs(x="Relative Resistance",y="Reaction Time (ms)") + 
  theme_classic(base_size = baseSize)
ggsave("figs/MR/rt.png")
#combination with step
dataset=datasetMR[which(datasetMR$type=="hit"),]
ggplot(dataset,aes(y=reactionTime,x=startTimeOfStimulus, color=as.factor(step), linetype=as.factor(step))) + 
  geom_smooth(aes(fill=as.factor(step)),method="lm") +
  labs(x="Time(min)",y="Reaction Time (ms)") + 
  theme_classic(base_size = baseSize)
#H1a acc
dataset=datasetMRNoOutlier
#average acc for relative resistance
library(plyr)
#create dataset summarized by steps for each ID
datasetByStep=ddply(dataset,
                    .(ID,relativeResistanceUnscaled),
                    summarize,
                    hits=sum((type=="hit")),
                    incorrects=sum((type=="incorrect")),
                    acc=hits/(hits+incorrects))
ggplot(datasetByStep,aes(y=acc,x=relativeResistanceUnscaled)) + 
  geom_smooth() +
  labs(x="Relative Resistance",y="Average Accuracy") + 
  theme_classic(base_size = baseSize)
ggsave("figs/MR/acc.png")
#H2 heart rate
dataset=datasetPhysNoOutlier
ggplot(dataset,aes(y=heartRate,x=relativeResistanceUnscaled, color=condition, linetype=condition)) + 
  geom_smooth() +
  labs(x="Relative Resistance",y="Heart Rate (bpm)") + 
  theme_classic(base_size = baseSize)
ggsave("figs/Phys/hr.png")
#H3 rpe
dataset=datasetPhysNoOutlier
ggplot(dataset,aes(y=RPE,x=relativeResistanceUnscaled, color=condition, linetype=condition)) + 
  geom_smooth() +
  labs(x="Relative Resistance",y="Physical RPE") + 
  theme_classic(base_size = baseSize)
ggsave("figs/Phys/rpe.png")
#h4 max heart rate
dataset=datasetPhysMaxPerformance
ggplot(dataset,aes(y=maxHRCondition, x=condition)) + 
  geom_boxplot() +
  stat_summary(fun.y=mean,geom="point") +
  labs(x="Condition",y="Maximal Heart Rate (bpm)") + 
  theme_classic(base_size = baseSize)
ggsave("figs/Phys/hrmax.png")
#h4a max power (relative)
dataset=datasetPhysMaxPerformance
ggplot(dataset,aes(y=maxPowerConditionRelative, x=condition)) + 
  geom_boxplot() +
  stat_summary(fun.y=mean,geom="point") +
  labs(x="Condition",y="Maximal Relative Power (W/kg)") + 
  theme_classic(base_size = baseSize)
ggsave("figs/Phys/pmax.png")
#with gender
ggplot(dataset,aes(y=maxPowerConditionRelative, x=condition, color=Gender)) + 
  geom_boxplot() +
  stat_summary(fun.y=mean,geom="point",position=position_dodge2(width=0.8)) +
  labs(x="Condition",y="Maximal Relative Power (W/kg)") + 
  theme_classic(base_size = baseSize)
ggsave("figs/Phys/pmaxGender.png")
#s2 cadence
dataset=datasetPhysNoOutlier
ggplot(dataset,aes(y=cadence,x=relativeResistanceUnscaled, color=condition, linetype=condition)) + 
  geom_smooth() +
  labs(x="Relative Resistance",y="Cadence (rpm)") + 
  theme_classic(base_size = baseSize)
ggsave("figs/Phys/cadence.png")

#means and standard deviations
#mr
dataset=datasetMRNoOutlier
dataset$relativeResistanceGrouped=ceiling(dataset$relativeResistanceUnscaled*10)/10
library(plyr)
#create dataset summarized by steps for each ID
datasetByStepAndID=ddply(dataset,
                    .(ID,relativeResistanceGrouped),
                    summarize,
                    reactionTime=weighted.mean(reactionTime,type=="hit"),
                    hits=sum((type=="hit")),
                    incorrects=sum((type=="incorrect")),
                    acc=hits/(hits+incorrects))
#average over IDs
datasetByStep=ddply(datasetByStepAndID,
                    .(relativeResistanceGrouped),
                    summarize,
                    reactionTimeSd=sd(reactionTime,na.rm=T),
                    accSd=sd(acc),
                    reactionTimeMeanSd=paste(paste(round(mean(reactionTime,na.rm=T),2)," (",round(reactionTimeSd,2),")",sep="")),
                    accMeanSd=paste(paste(round(mean(acc),2)," (",round(accSd,2),")",sep="")))
#write table
write.table(datasetByStep,file=paste("output\\MeanMRByStep",".csv",sep=""),sep=";", col.names=NA)

#physical data (and crpe)
dataset=datasetPhysNoOutlier
dataset$relativeResistanceGrouped=ceiling(dataset$relativeResistanceUnscaled*10)/10
#only one measurement per step -> average over IDs
datasetByStep=ddply(dataset,
                    .(relativeResistanceGrouped,condition),
                    summarize,
                    cRPESd=sd(cRPE),
                    RPESd=sd(RPE),
                    heartRateSd=sd(heartRate),
                    cadenceSd=sd(cadence,na.rm=T),
                    cRPEMeanSd=paste(paste(round(mean(cRPE),2)," (",round(cRPESd,2),")",sep="")),
                    RPEMeanSd=paste(paste(round(mean(RPE),2)," (",round(RPESd,2),")",sep="")),
                    heartRateMeanSd=paste(paste(round(mean(heartRate),2)," (",round(heartRateSd,2),")",sep="")),
                    cadenceMeanSd=paste(paste(round(mean(cadence,na.rm=T),2)," (",round(cadenceSd,2),")",sep="")))
#write table
write.table(datasetByStep,file=paste("output\\MeanPhysByStep",".csv",sep=""),sep=";", col.names=NA)
#maximal physical data
dataset=datasetPhysMaxPerformance
#only one measurement per step -> average over IDs
datasetByCondition=ddply(dataset,
                    .(condition),
                    summarize,
                    maxPowerConditionSd=sd(maxPowerCondition),
                    maxHRConditionSd=sd(maxHRCondition),
                    maxPowerConditionRelativeSd=sd(maxPowerConditionRelative),
                    maxPowerConditionMeanSd=paste(paste(round(mean(maxPowerCondition),2)," (",round(maxPowerConditionSd,2),")",sep="")),
                    maxHRConditionMeanSd=paste(paste(round(mean(maxHRCondition),2)," (",round(maxHRConditionSd,2),")",sep="")),
                    maxPowerConditionRelativeMeanSd=paste(paste(round(mean(maxPowerConditionRelative),2)," (",round(maxPowerConditionRelativeSd,2),")",sep="")))
#write table
write.table(datasetByCondition,file=paste("output\\MeanMaxPhys",".csv",sep=""),sep=";", col.names=NA)
#by gender
datasetByConditionGender=ddply(dataset,
                         .(condition,Gender),
                         summarize,
                         maxPowerConditionSd=sd(maxPowerCondition),
                         maxHRConditionSd=sd(maxHRCondition),
                         maxPowerConditionRelativeSd=sd(maxPowerConditionRelative),
                         maxPowerConditionMeanSd=paste(paste(round(mean(maxPowerCondition),2)," (",round(maxPowerConditionSd,2),")",sep="")),
                         maxHRConditionMeanSd=paste(paste(round(mean(maxHRCondition),2)," (",round(maxHRConditionSd,2),")",sep="")),
                         maxPowerConditionRelativeMeanSd=paste(paste(round(mean(maxPowerConditionRelative),2)," (",round(maxPowerConditionRelativeSd,2),")",sep="")))
#write table
write.table(datasetByConditionGender,file=paste("output\\MeanMaxPhysGender",".csv",sep=""),sep=";", col.names=NA)
