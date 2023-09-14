### power estimation using simulations and superpower package
#     Copyright (C) 2023  Leonardo Jost
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

###functions

#generate dataset
#n - number of participants
#N - number of measurement categories in total 
#rmCorrelation - correlation of repeated measures
#totalSd - overall standard deviation
generateData=function(n,rmCorrelation=0.66,totalSd=1) {
  #calculate within and between standard deviation
  sdWithin=sqrt(rmCorrelation)*totalSd
  sdBetween=sqrt(1-rmCorrelation)*totalSd
  #generate dataframe
  testdata=data.frame(ids=as.factor(rep(1:n,each=4)),
                      factor1=factor(rep(c("typeA","typeB"),each=2,n)),
                      factor2=factor(rep(c("type1","type2"),n*2)))
  #set mean values für categories
  testdata$baseMean=ifelse(testdata$factor1=="typeA" & testdata$factor2=="type1" ,0,
                    ifelse(testdata$factor1=="typeB" & testdata$factor2=="type1" ,0,
                    ifelse(testdata$factor1=="typeA" & testdata$factor2=="type2" ,0,
                    ifelse(testdata$factor1=="typeB" & testdata$factor2=="type2" ,0.8/sqrt(2), #cohens d 0.4 for both main effects and the interaction
                    0))))
  #convert to numeric to test for main effects as part of interactions
  testdata$factor1Numeric=sapply(testdata$factor1,function(i) contr.sum(2)[i,])*0.5
  testdata$factor2Numeric=sapply(testdata$factor2,function(i) contr.sum(2)[i,])*0.5
  #get base probability for each participant, corr coeff of .66
  testdata$randomIntercept=rep(rnorm(n),each=4)*sdWithin
  #get resulting values (mean for category, random intercept, random error)
  testdata$value=testdata$baseMean+testdata$randomIntercept+rnorm(n*4)*sdBetween
  return(testdata)
}

#get p values of effects of interest
getSignificantLmer=function(testdata){
  lmerModel=lmer(value~factor1Numeric*factor2Numeric+(1|ids),
                   data=testdata,REML=FALSE,
                   control = lmerControl(optimizer = "optimx",optCtrl = list(method = "bobyqa")))
  lmerModel1=update(lmerModel,formula = ~. -factor1Numeric:factor2Numeric)
  lmerModel2=update(lmerModel,formula = ~. -factor1Numeric)
  lmerModel3=update(lmerModel,formula = ~. -factor2Numeric)
  return(c(anova(lmerModel,lmerModel1)$"Pr(>Chisq)"[2],
           anova(lmerModel,lmerModel2)$"Pr(>Chisq)"[2],
           anova(lmerModel,lmerModel3)$"Pr(>Chisq)"[2]))
}
#iterate over random data
#ns - vector of number of participants
#reps - number of simulations
randSim=function(ns,reps=1000,rmCorrelation=0.66,totalSd=1){
  #loop over number of participants
  significantDataFrame=data.frame(n=ns,
                                  effects=rep(c("factor1*factor2","factor1","factor2"),length(ns)),
                                  propSignificant=rep(0,length(ns)))
  for(n in ns) {
    #to get some sense of progress
    print(n)
    significant = matrix(nrow=reps, ncol=3)
    for(i in 1:reps){
      testdata=generateData(n,rmCorrelation,totalSd)
      significant[i,]=getSignificantLmer(testdata)<0.1 
    }
    #save proportion to data frame
    significantDataFrame$propSignificant[which(significantDataFrame$n==n & significantDataFrame$effects=="factor1*factor2")]=
      sum(significant[,1])/reps
    significantDataFrame$propSignificant[which(significantDataFrame$n==n & significantDataFrame$effects=="factor1")]=
      sum(significant[,2])/reps
    significantDataFrame$propSignificant[which(significantDataFrame$n==n & significantDataFrame$effects=="factor2")]=
      sum(significant[,3])/reps
  }
  return(significantDataFrame)
}

###script

#load libraries
library(lme4)
library(optimx)
library(ggplot2)
library(Superpower)
#generate random seed (this should be random enough)
#sample(0:100000,1)
#65622
set.seed(65622)

#power calculation using simulations
ns=c(41)
significantDataFrame=randSim(ns,rmCorrelation=0.5,totalSd=1)
significantDataFrame2=randSim(ns,rmCorrelation=0.66,totalSd=1)

#power calculation using superpower (divide by sqrt(2) to account for half the number of measurements compared with only 2w factor)
mu = c(0,0,0,0.8/sqrt(2)) #at sd of 1 this should equal cohens d of 0.4
n = 41 #for comparisons with GPower and between effects: here: n per group, GPower: total N
sd = 1 
r = .66
string = "2w*2w"
alpha_level = 0.1 #double alpha level because of directed effects/analysis
labelnames = c("condition","mr","control","step","1","2")
design_result = ANOVA_design(design = string,
                             n = n, 
                             mu = mu, 
                             sd = sd, 
                             r = r,
                             labelnames = labelnames,
                             plot=F)
design_result
exact_result = ANOVA_exact(design_result,
                           alpha_level = alpha_level,
                           verbose=T)
exact_result$main_results

#get effect size for power of .8
mu = c(0,0,0,0.66/sqrt(2)) # d =.33
n = 41 #for comparisons with GPower and between effects: here: n per group, GPower: total N
sd = 1 
r = .66
string = "2w*2w"
alpha_level = 0.1 #double alpha level because of directed effects/analysis
labelnames = c("condition","mr","control","step","1","2")
design_result = ANOVA_design(design = string,
                             n = n, 
                             mu = mu, 
                             sd = sd, 
                             r = r,
                             labelnames = labelnames,
                             plot=F)
design_result
exact_result = ANOVA_exact(design_result,
                           alpha_level = alpha_level,
                           verbose=T)
exact_result$main_results

#comparison: original power analysis for within-subjects effects and two groups
mu = c(0,0.4) #at sd of 1 this should equal cohens d of 0.4
n = 41 #for comparisons with GPower and between effects: here: n per group, GPower: total N
sd = 1 
r = .5
string = "2w"
alpha_level = 0.1 #double alpha level because of directed effects/analysis
labelnames = c("condition","mr","control")
design_result = ANOVA_design(design = string,
                             n = n, 
                             mu = mu, 
                             sd = sd, 
                             r = r,
                             labelnames = labelnames,
                             plot=F)
design_result
exact_result = ANOVA_exact(design_result,
                           alpha_level = alpha_level,
                           verbose=T)
exact_result$main_results

#comparison: two within-subjects effects with the same power 
mu = c(0,0,0,0.8) #at sd of 1 this should equal cohens d of 0.4
n = 21 #for comparisons with GPower and between effects: here: n per group, GPower: total N
sd = 1 
r = .5
string = "2w*2w"
alpha_level = 0.1 #double alpha level because of directed effects/analysis
labelnames = c("condition","mr","control","step","1","2")
design_result = ANOVA_design(design = string,
                             n = n, 
                             mu = mu, 
                             sd = sd, 
                             r = r,
                             labelnames = labelnames,
                             plot=F)
design_result
exact_result = ANOVA_exact(design_result,
                           alpha_level = alpha_level,
                           verbose=T)
exact_result$main_results

#comparison: divide effect size by sqrt(2) instead to account for fewer measurements
mu = c(0,0,0,0.8/sqrt(2)) #at sd of 1 this should equal cohens d of 0.4
n = 41 #for comparisons with GPower and between effects: here: n per group, GPower: total N
sd = 1 
r = .5
string = "2w*2w"
alpha_level = 0.1 #double alpha level because of directed effects/analysis
labelnames = c("condition","mr","control","step","1","2")
design_result = ANOVA_design(design = string,
                             n = n, 
                             mu = mu, 
                             sd = sd, 
                             r = r,
                             labelnames = labelnames,
                             plot=F)
design_result
exact_result = ANOVA_exact(design_result,
                           alpha_level = alpha_level,
                           verbose=T)
exact_result$main_results