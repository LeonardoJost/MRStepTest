### statistics for hypothesis 1
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
using Random
using DataFrames
using DataFramesMeta
using Gadfly
using CSV
#read data
dataset=CSV.read("dataset\\datasetPhysNoOutlier.csv", DataFrame)
#inspect data
show(first(dataset,6))
show(names(dataset))
show(eltype.(eachcol(dataset)))
#use only mental rotation data
datasetMR=dataset[dataset[!,:condition].=="test",:]

#random slopes
#start with maximal model
modelFormula=@formula(cRPE~relativeResistance+(relativeResistance|ID))
@elapsed slopesModel1=fit(LinearMixedModel,modelFormula, datasetMR,REML=false)
show(slopesModel1.optsum)
#remove random correlation
modelFormula=@formula(cRPE~relativeResistance+zerocorr(relativeResistance|ID))
@elapsed slopesModel11=fit(LinearMixedModel,modelFormula, datasetMR,REML=false)
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel11))
#slopesModel11 is better
#remove relativeResistance|ID
modelFormula=@formula(cRPE~relativeResistance+(1|ID))
@elapsed slopesModel12=fit(LinearMixedModel,modelFormula, datasetMR,REML=false)
show(MixedModels.likelihoodratiotest(slopesModel11,slopesModel12))
#slopesModel11 is best

#get fixed effect
m1=slopesModel11
#remove relativeResistance
modelFormula=@formula(cRPE~1+zerocorr(relativeResistance|ID))
@elapsed m2=fit(LinearMixedModel,modelFormula, datasetMR,REML=false)
show(MixedModels.likelihoodratiotest(m1,m2))

#residual plot
plot(x=StatsBase.residuals(m1),Geom.histogram)
plot(x=StatsBase.residuals(m1),y=fitted(m1))

#exploratory
#compare with control condition
#random slopes
#start with maximal model
modelFormula=@formula(cRPE~relativeResistance*conditionContrasts+(relativeResistance*conditionContrasts|ID))
@elapsed slopesModel1=fit(LinearMixedModel,modelFormula, dataset,REML=false)
show(slopesModel1.optsum)
#remove random correlation
modelFormula=@formula(cRPE~relativeResistance*conditionContrasts+zerocorr(relativeResistance*conditionContrasts|ID))
@elapsed slopesModel11=fit(LinearMixedModel,modelFormula, dataset,REML=false)
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel11))
#slopesModel1 is better
#remove interaction|ID
modelFormula=@formula(cRPE~relativeResistance*conditionContrasts+(relativeResistance+conditionContrasts|ID))
@elapsed slopesModel12=fit(LinearMixedModel,modelFormula, dataset,REML=false)
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel12))
#slopesModel1 is best

#get fixed effect
m1=slopesModel1
#relativeResistance*conditionContrasts
modelFormula=@formula(cRPE~relativeResistance+conditionContrasts+(relativeResistance*conditionContrasts|ID))
@elapsed m11=fit(LinearMixedModel,modelFormula, dataset,REML=false)
show(MixedModels.likelihoodratiotest(m1,m11))
#relativeResistance*conditionContrasts
modelFormula=@formula(cRPE~relativeResistance&conditionContrasts+relativeResistance+(relativeResistance*conditionContrasts|ID))
@elapsed m12=fit(LinearMixedModel,modelFormula, dataset,REML=false)
show(MixedModels.likelihoodratiotest(m1,m12))

#residual plot
plot(x=StatsBase.residuals(m1),Geom.histogram)
plot(x=StatsBase.residuals(m1),y=fitted(m1))
