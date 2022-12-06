### statistics for hypothesis 1a, reaction time
#     Copyright (C) 2022  Leonardo Jost
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

#load packages
using MixedModels
using DataFrames
using DataFramesMeta
using CSV
using StatsBase
using Gadfly

#read data
dataset=CSV.read("dataset\\datasetMRNoOutlier.csv", DataFrame)
#inspect data
show(first(dataset,6))
show(names(dataset))
show(eltype.(eachcol(dataset)))
#use only correct responses for reaction time
datasetRT=dataset[dataset[!,:type].=="hit",:]

##reaction time
#random slopes
#start with maximal model
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|ID)+
              (deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|modelNumber))
@elapsed slopesModel1=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
show(slopesModel1.optsum)
#remove random correlation by ID
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              zerocorr(deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|ID)+
              (deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|modelNumber))
@elapsed slopesModel11=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel11))
#slopesModel1 better
#remove random correlation by model
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|ID)+
              zerocorr(deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|modelNumber))
@elapsed slopesModel12=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel12))
#slopesModel1 better
#remove deg|ID
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (startTimeOfStimulus*relativeResistance+relativeResistance^2|ID)+
              (deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|modelNumber))
@elapsed slopesModel13=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#startTimeOfStimulus*relativeResistance|ID
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus+relativeResistance+relativeResistance^2|ID)+
              (deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|modelNumber))
@elapsed slopesModel14=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#relativeResistance^2|ID
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus*relativeResistance|ID)+
              (deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|modelNumber))
@elapsed slopesModel15=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#deg|modelNumber
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|ID)+
              (startTimeOfStimulus*relativeResistance+relativeResistance^2|modelNumber))
@elapsed slopesModel16=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#startTimeOfStimulus*relativeResistance|modelNumber
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|ID)+
              (deg+startTimeOfStimulus+relativeResistance+relativeResistance^2|modelNumber))
@elapsed slopesModel17=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#relativeResistance^2|modelNumber
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|ID)+
              (deg+startTimeOfStimulus*relativeResistance|modelNumber))
@elapsed slopesModel18=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#comparison
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel13))
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel14))
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel15))
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel16))
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel17))
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel18))
#largest p for 18
slopesModel2=slopesModel18

#deg|ID
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (startTimeOfStimulus*relativeResistance+relativeResistance^2|ID)+
              (deg+startTimeOfStimulus*relativeResistance|modelNumber))
@elapsed slopesModel21=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#startTimeOfStimulus*relativeResistance|ID
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus+relativeResistance+relativeResistance^2|ID)+
              (deg+startTimeOfStimulus*relativeResistance|modelNumber))
@elapsed slopesModel22=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#relativeResistance^2|ID
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus*relativeResistance|ID)+
              (deg+startTimeOfStimulus*relativeResistance|modelNumber))
@elapsed slopesModel23=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#deg|modelNumber
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|ID)+
              (startTimeOfStimulus*relativeResistance|modelNumber))
@elapsed slopesModel24=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#startTimeOfStimulus*relativeResistance|modelNumber
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|ID)+
              (deg+startTimeOfStimulus+relativeResistance|modelNumber))
@elapsed slopesModel25=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#comparison
show(MixedModels.likelihoodratiotest(slopesModel2,slopesModel21))
show(MixedModels.likelihoodratiotest(slopesModel2,slopesModel22))
show(MixedModels.likelihoodratiotest(slopesModel2,slopesModel23))
show(MixedModels.likelihoodratiotest(slopesModel2,slopesModel24))
show(MixedModels.likelihoodratiotest(slopesModel2,slopesModel25))
#largest p for 22
slopesModel3=slopesModel22
#deg|ID
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (startTimeOfStimulus+relativeResistance+relativeResistance^2|ID)+
              (deg+startTimeOfStimulus*relativeResistance|modelNumber))
@elapsed slopesModel31=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#startTimeOfStimulus|ID
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+relativeResistance+relativeResistance^2|ID)+
              (deg+startTimeOfStimulus*relativeResistance|modelNumber))
@elapsed slopesModel32=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#relativeResistance^2|ID
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus+relativeResistance|ID)+
              (deg+startTimeOfStimulus*relativeResistance|modelNumber))
@elapsed slopesModel33=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#deg|modelNumber
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus+relativeResistance+relativeResistance^2|ID)+
              (startTimeOfStimulus*relativeResistance|modelNumber))
@elapsed slopesModel34=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#startTimeOfStimulus*relativeResistance|modelNumber
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus+relativeResistance+relativeResistance^2|ID)+
              (deg+startTimeOfStimulus+relativeResistance|modelNumber))
@elapsed slopesModel35=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#comparison
show(MixedModels.likelihoodratiotest(slopesModel3,slopesModel31))
show(MixedModels.likelihoodratiotest(slopesModel3,slopesModel32))
show(MixedModels.likelihoodratiotest(slopesModel3,slopesModel33))
show(MixedModels.likelihoodratiotest(slopesModel3,slopesModel34))
show(MixedModels.likelihoodratiotest(slopesModel3,slopesModel35))
#35 n.s.
slopesModel4=slopesModel35
#deg|ID
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (startTimeOfStimulus+relativeResistance+relativeResistance^2|ID)+
              (deg+startTimeOfStimulus+relativeResistance|modelNumber))
@elapsed slopesModel41=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#startTimeOfStimulus|ID
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+relativeResistance+relativeResistance^2|ID)+
              (deg+startTimeOfStimulus+relativeResistance|modelNumber))
@elapsed slopesModel42=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#relativeResistance^2|ID
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus+relativeResistance|ID)+
              (deg+startTimeOfStimulus+relativeResistance|modelNumber))
@elapsed slopesModel43=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#degmodelNumber
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus+relativeResistance+relativeResistance^2|ID)+
              (startTimeOfStimulus+relativeResistance|modelNumber))
@elapsed slopesModel44=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#startTimeOfStimulus|modelNumber
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus+relativeResistance+relativeResistance^2|ID)+
              (deg+relativeResistance|modelNumber))
@elapsed slopesModel45=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#relativeResistance|modelNumber
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus+relativeResistance+relativeResistance^2|ID)+
              (deg+startTimeOfStimulus|modelNumber))
@elapsed slopesModel46=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#comparison
show(MixedModels.likelihoodratiotest(slopesModel4,slopesModel41))
show(MixedModels.likelihoodratiotest(slopesModel4,slopesModel42))
show(MixedModels.likelihoodratiotest(slopesModel4,slopesModel43))
show(MixedModels.likelihoodratiotest(slopesModel4,slopesModel44))
show(MixedModels.likelihoodratiotest(slopesModel4,slopesModel45))
show(MixedModels.likelihoodratiotest(slopesModel4,slopesModel46))
#45 n.s.
slopesModel5=slopesModel45
#deg|ID
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (startTimeOfStimulus+relativeResistance+relativeResistance^2|ID)+
              (deg+relativeResistance|modelNumber))
@elapsed slopesModel51=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#startTimeOfStimulus|ID
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+relativeResistance+relativeResistance^2|ID)+
              (deg+relativeResistance|modelNumber))
@elapsed slopesModel52=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#relativeResistance^2|ID
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus+relativeResistance|ID)+
              (deg+relativeResistance|modelNumber))
@elapsed slopesModel53=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#deg|modelNumber
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus+relativeResistance+relativeResistance^2|ID)+
              (relativeResistance|modelNumber))
@elapsed slopesModel54=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#relativeResistance|modelNumber
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus+relativeResistance+relativeResistance^2|ID)+
              (deg|modelNumber))
@elapsed slopesModel55=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#comparison
show(MixedModels.likelihoodratiotest(slopesModel5,slopesModel51))
show(MixedModels.likelihoodratiotest(slopesModel5,slopesModel52))
show(MixedModels.likelihoodratiotest(slopesModel5,slopesModel53))
show(MixedModels.likelihoodratiotest(slopesModel5,slopesModel54))
show(MixedModels.likelihoodratiotest(slopesModel5,slopesModel55))
#all significant
#test random correlation
#ID
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              zerocorr(deg+startTimeOfStimulus+relativeResistance+relativeResistance^2|ID)+
              (deg+relativeResistance|modelNumber))
@elapsed slopesModel56=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#modelNumber
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus+relativeResistance+relativeResistance^2|ID)+
              zerocorr(deg+relativeResistance|modelNumber))
@elapsed slopesModel57=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#comparison
show(MixedModels.likelihoodratiotest(slopesModel5,slopesModel56))
show(MixedModels.likelihoodratiotest(slopesModel5,slopesModel57))
#significant
m1=slopesModel5


## fixed effects
#deg
modelFormula=@formula(reactionTime~startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus+relativeResistance+relativeResistance^2|ID)+
              (deg+relativeResistance|modelNumber))
@elapsed m11=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#startTimeOfStimulus*relativeResistance
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus+relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus+relativeResistance+relativeResistance^2|ID)+
              (deg+relativeResistance|modelNumber))
@elapsed m12=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#relativeResistance^2
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus*relativeResistance+
              (deg+startTimeOfStimulus+relativeResistance+relativeResistance^2|ID)+
              (deg+relativeResistance|modelNumber))
@elapsed m13=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#startTimeOfStimulus
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus&relativeResistance+relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus+relativeResistance+relativeResistance^2|ID)+
              (deg+relativeResistance|modelNumber))
@elapsed m14=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#relativeResistance
modelFormula=@formula(reactionTime~deg+startTimeOfStimulus&relativeResistance+startTimeOfStimulus+relativeResistance^2+
              (deg+startTimeOfStimulus+relativeResistance+relativeResistance^2|ID)+
              (deg+relativeResistance|modelNumber))
@elapsed m15=fit(LinearMixedModel,modelFormula, datasetRT,REML=false)
#comparison
show(MixedModels.likelihoodratiotest(m1,m11))
show(MixedModels.likelihoodratiotest(m1,m12))
show(MixedModels.likelihoodratiotest(m1,m13))
show(MixedModels.likelihoodratiotest(m1,m14))
show(MixedModels.likelihoodratiotest(m1,m15))

#residual plot
plot(x=StatsBase.residuals(m1),y=fitted(m1))
plot(x=StatsBase.residuals(m1), Geom.histogram)
