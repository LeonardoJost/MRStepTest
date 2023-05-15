### statistics for hypothesis 4
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
using Cairo
#read data
dataset=CSV.read("dataset\\datasetPhysMaxPerformance.csv", DataFrame)
#inspect data
show(first(dataset,6))
show(names(dataset))
show(eltype.(eachcol(dataset)))


#random slopes
#start with maximal model
modelFormula=@formula(maxHRCondition~conditionContrasts+(conditionContrasts|ID))
@elapsed slopesModel1=fit(LinearMixedModel,modelFormula, dataset,REML=false)
show(slopesModel1.optsum)
#remove random correlation
modelFormula=@formula(maxHRCondition~conditionContrasts+zerocorr(conditionContrasts|ID))
@elapsed slopesModel11=fit(LinearMixedModel,modelFormula, dataset,REML=false)
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel11))
#slopesModel1 is better
#remove conditionContrasts|ID
modelFormula=@formula(maxHRCondition~conditionContrasts+(1|ID))
@elapsed slopesModel12=fit(LinearMixedModel,modelFormula, dataset,REML=false)
show(MixedModels.likelihoodratiotest(slopesModel1,slopesModel12))
#slopesModel12 is best

#get fixed effect
m1=slopesModel12
#conditionContrasts
modelFormula=@formula(maxHRCondition~1+(1|ID))
@elapsed m11=fit(LinearMixedModel,modelFormula, dataset,REML=false)
#comparison
show(MixedModels.likelihoodratiotest(m1,m11))


#residual plot
hist=plot(x=StatsBase.residuals(m1),Geom.histogram, Guide.xlabel("Residuals"))
resplot=plot(x=StatsBase.residuals(m1),y=fitted(m1), Guide.xlabel("Residuals"),Guide.ylabel("Fitted values"))
draw(PNG("figs/Residual Plots/hist4.png"),hist)
draw(PNG("figs/Residual Plots/resplot4.png"),resplot)
