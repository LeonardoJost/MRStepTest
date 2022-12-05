### statistics for hypothesis 1a, accuracy
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
#convert response to bool for accuracy analysis
dataset.responseCorrect=dataset.type.=="hit"


##reaction time
#random slopes
#start with maximal model
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|ID)+
              (deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|modelNumber))
@elapsed slopesModel1=fit(MixedModel,modelFormula,dataset,Binomial())
show(slopesModel1.optsum)
#remove random correlation by ID
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              zerocorr(deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|ID)+
              (deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|modelNumber))
@elapsed slopesModel11=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel11))
#slopesModel11 better
#remove random correlation by model
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|ID)+
              zerocorr(deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|modelNumber))
@elapsed slopesModel12=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel12))
#slopesModel11 better
#remove both random correlations
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              zerocorr(deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|ID)+
              zerocorr(deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|modelNumber))
@elapsed slopesModel13=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(slopesModel11,slopesModel13))
#13 better
#remove random slopes
#remove deg|ID
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              zerocorr(startTimeOfStimulus*relativeResistance+relativeResistance^2|ID)+
              zerocorr(deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|modelNumber))
@elapsed slopesModel14=fit(MixedModel,modelFormula,dataset,Binomial())
#startTimeOfStimulus*relativeResistance|ID
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              zerocorr(deg+startTimeOfStimulus+relativeResistance+relativeResistance^2|ID)+
              zerocorr(deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|modelNumber))
@elapsed slopesModel15=fit(MixedModel,modelFormula,dataset,Binomial())
#relativeResistance^2|ID
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              zerocorr(deg+startTimeOfStimulus*relativeResistance|ID)+
              zerocorr(deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|modelNumber))
@elapsed slopesModel16=fit(MixedModel,modelFormula,dataset,Binomial())
#deg|modelNumber
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              zerocorr(deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|ID)+
              zerocorr(startTimeOfStimulus*relativeResistance+relativeResistance^2|modelNumber))
@elapsed slopesModel17=fit(MixedModel,modelFormula,dataset,Binomial())
#startTimeOfStimulus*relativeResistance|modelNumber
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              zerocorr(deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|ID)+
              zerocorr(deg+startTimeOfStimulus+relativeResistance+relativeResistance^2|modelNumber))
@elapsed slopesModel18=fit(MixedModel,modelFormula,dataset,Binomial())
#relativeResistance^2|modelNumber
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              zerocorr(deg+startTimeOfStimulus*relativeResistance+relativeResistance^2|ID)+
              zerocorr(deg+startTimeOfStimulus*relativeResistance|modelNumber))
@elapsed slopesModel19=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(slopesModel13,slopesModel14))
show(MixedModels.likelihoodratiotest(slopesModel13,slopesModel15))
show(MixedModels.likelihoodratiotest(slopesModel13,slopesModel16))
show(MixedModels.likelihoodratiotest(slopesModel13,slopesModel17))
show(MixedModels.likelihoodratiotest(slopesModel13,slopesModel18))
show(MixedModels.likelihoodratiotest(slopesModel13,slopesModel19))
#19 for no variance
#also remove startTimeOfStimulus*relativeResistance|ID because of continued 0 variance in 19
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              zerocorr(deg+startTimeOfStimulus+relativeResistance+relativeResistance^2|ID)+
              zerocorr(deg+startTimeOfStimulus*relativeResistance|modelNumber))
@elapsed slopesModel2=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(slopesModel19,slopesModel2))
#also remove startTimeOfStimulus+relativeResistance|ID because of continued 0 variance in 2
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              zerocorr(deg+relativeResistance^2|ID)+
              zerocorr(deg+startTimeOfStimulus*relativeResistance|modelNumber))
@elapsed slopesModel3=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(slopesModel2,slopesModel3))
#test removals
#deg|ID
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              zerocorr(relativeResistance^2|ID)+
              zerocorr(deg+startTimeOfStimulus*relativeResistance|modelNumber))
@elapsed slopesModel31=fit(MixedModel,modelFormula,dataset,Binomial())
#relativeResistance^2|ID
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              zerocorr(deg|ID)+
              zerocorr(deg+startTimeOfStimulus*relativeResistance|modelNumber))
@elapsed slopesModel32=fit(MixedModel,modelFormula,dataset,Binomial())
#deg|modelNumber
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              zerocorr(deg+relativeResistance^2|ID)+
              zerocorr(startTimeOfStimulus*relativeResistance|modelNumber))
@elapsed slopesModel33=fit(MixedModel,modelFormula,dataset,Binomial())
#startTimeOfStimulus*relativeResistance|modelNumber
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              zerocorr(deg+relativeResistance^2|ID)+
              zerocorr(deg+startTimeOfStimulus+relativeResistance|modelNumber))
@elapsed slopesModel34=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(slopesModel3,slopesModel31))
show(MixedModels.likelihoodratiotest(slopesModel3,slopesModel32))
show(MixedModels.likelihoodratiotest(slopesModel3,slopesModel33))
show(MixedModels.likelihoodratiotest(slopesModel3,slopesModel34))
#largest p for 31
slopesModel4=slopesModel31
#relativeResistance^2|ID
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (1|ID)+
              zerocorr(deg+startTimeOfStimulus*relativeResistance|modelNumber))
@elapsed slopesModel41=fit(MixedModel,modelFormula,dataset,Binomial())
#deg|modelNumber
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              zerocorr(relativeResistance^2|ID)+
              zerocorr(startTimeOfStimulus*relativeResistance|modelNumber))
@elapsed slopesModel42=fit(MixedModel,modelFormula,dataset,Binomial())
#startTimeOfStimulus*relativeResistance|modelNumber
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              zerocorr(relativeResistance^2|ID)+
              zerocorr(deg+startTimeOfStimulus+relativeResistance|modelNumber))
@elapsed slopesModel43=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(slopesModel4,slopesModel41))
show(MixedModels.likelihoodratiotest(slopesModel4,slopesModel42))
show(MixedModels.likelihoodratiotest(slopesModel4,slopesModel43))
#43 n.s.
slopesModel5=slopesModel43
#startTimeOfStimulus|modelNumber for 0 variance
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              zerocorr(relativeResistance^2|ID)+
              zerocorr(deg+relativeResistance|modelNumber))
@elapsed slopesModel6=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(slopesModel5,slopesModel6))
#relativeResistance^2|ID
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (1|ID)+
              zerocorr(deg+relativeResistance|modelNumber))
@elapsed slopesModel61=fit(MixedModel,modelFormula,dataset,Binomial())
#deg|modelNumber
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              zerocorr(relativeResistance^2|ID)+
              zerocorr(relativeResistance|modelNumber))
@elapsed slopesModel62=fit(MixedModel,modelFormula,dataset,Binomial())
#relativeResistance|modelNumber
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              zerocorr(relativeResistance^2|ID)+
              zerocorr(deg|modelNumber))
@elapsed slopesModel63=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(slopesModel6,slopesModel61))
show(MixedModels.likelihoodratiotest(slopesModel6,slopesModel62))
show(MixedModels.likelihoodratiotest(slopesModel6,slopesModel63))
#63 n.s.
slopesModel7=slopesModel63
#relativeResistance^2|ID
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (1|ID)+
              zerocorr(deg|modelNumber))
@elapsed slopesModel71=fit(MixedModel,modelFormula,dataset,Binomial())
#deg|modelNumber
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              zerocorr(relativeResistance^2|ID)+
              (1|modelNumber))
@elapsed slopesModel72=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(slopesModel7,slopesModel71))
show(MixedModels.likelihoodratiotest(slopesModel7,slopesModel72))
#both significant
#readd random correlation
#ID
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (relativeResistance^2|ID)+
              zerocorr(deg|modelNumber))
@elapsed slopesModel73=fit(MixedModel,modelFormula,dataset,Binomial())
#modelNumber
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              zerocorr(relativeResistance^2|ID)+
              (deg|modelNumber))
@elapsed slopesModel74=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(slopesModel7,slopesModel73))
show(MixedModels.likelihoodratiotest(slopesModel7,slopesModel74))
#73 better
slopesModel8=slopesModel73
#modelNumber
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (relativeResistance^2|ID)+
              (deg|modelNumber))
@elapsed slopesModel81=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(slopesModel8,slopesModel81))
#8 best
#test effect of order
modelFormula=@formula(responseCorrect~startTimeOfStimulus*relativeResistance+relativeResistance^2+deg+
              (relativeResistance^2|ID)+
              zerocorr(deg|modelNumber))
@elapsed slopesModel82=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(slopesModel8,slopesModel82))
modelFormula=@formula(responseCorrect~relativeResistance^2+deg+startTimeOfStimulus*relativeResistance+
              (relativeResistance^2|ID)+
              zerocorr(deg|modelNumber))
@elapsed slopesModel83=fit(MixedModel,modelFormula,dataset,Binomial())
show(MixedModels.likelihoodratiotest(slopesModel8,slopesModel83))
#good
m1=slopesModel8
#get fixed effects
#deg
modelFormula=@formula(responseCorrect~startTimeOfStimulus*relativeResistance+relativeResistance^2+
              (relativeResistance^2|ID)+
              zerocorr(deg|modelNumber))
@elapsed m11=fit(MixedModel,modelFormula,dataset,Binomial())
#startTimeOfStimulus*relativeResistance
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus+relativeResistance+relativeResistance^2+
              (relativeResistance^2|ID)+
              zerocorr(deg|modelNumber))
@elapsed m12=fit(MixedModel,modelFormula,dataset,Binomial())
#relativeResistance^2
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus*relativeResistance+
              (relativeResistance^2|ID)+
              zerocorr(deg|modelNumber))
@elapsed m13=fit(MixedModel,modelFormula,dataset,Binomial())
#startTimeOfStimulus
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus&relativeResistance+relativeResistance+relativeResistance^2+
              (relativeResistance^2|ID)+
              zerocorr(deg|modelNumber))
@elapsed m14=fit(MixedModel,modelFormula,dataset,Binomial())
#relativeResistance
modelFormula=@formula(responseCorrect~deg+startTimeOfStimulus&relativeResistance+startTimeOfStimulus+relativeResistance^2+
              (relativeResistance^2|ID)+
              zerocorr(deg|modelNumber))
@elapsed m15=fit(MixedModel,modelFormula,dataset,Binomial())
#comparison
show(MixedModels.likelihoodratiotest(m1,m11))
show(MixedModels.likelihoodratiotest(m1,m12))
show(MixedModels.likelihoodratiotest(m1,m13))
show(MixedModels.likelihoodratiotest(m1,m14))
show(MixedModels.likelihoodratiotest(m1,m15))
