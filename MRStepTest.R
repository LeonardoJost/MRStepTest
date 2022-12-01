### main script
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

source("functions/helpers.R")
source("functions/readData.R", encoding="utf-8")
source("functions/generateGraphsAndTables.R", encoding="utf-8")

##create output directories, if they don't exist (outputs warnings otherwise)
dir.create("figs")
dir.create("output")
dir.create("figs/MR")

##options, parameters
options(digits=6)
software=("OpenSesame") #OSWeb or OpenSesame (classic)
#set data folder
osLogfiles="logfiles\\osLogfiles\\"
physLogfiles="logfiles\\physLogfiles\\"
verbose=3 #detail of output
questionnaireOutFile="output\\questionnaire" #.csv added at end, leave empty if no output desired
outlierFactor=3 #factor of sd to define outliers in MR
block=c("main")#name of interesting block of data
questionnaireDataCols=c("ID","Gender","Experience","Weight") #which questionaire columns shall be kept for statistical analysis

##read and write data
#read data
questionnaireData=getQuestionnaireData(software,verbose,osLogfiles)
questionnaireDataRPE=getRPEQuestionnaireData(software,verbose,osLogfiles)
MRData=getMRData(software,verbose,osLogfiles,block)
physData=getPhysData(verbose,physLogfiles)
#modify data 
questionnaireData=modifyQuestionnaireData(questionnaireData,c("Gender"),c("Age","Weight"),c())
MRData=modifyMRData(verbose,MRData,outlierFactor)
questionnaireDataRPE=modifyQuestionnaireDataRPE(questionnaireDataRPE)
#calculate means from questionaire (and save to csv)
calculateMeansQuestionnaire(verbose,questionnaireData,questionnaireOutFile,"")
#remove not analyzed questionaire data to protect participant identity
questionnaireData=subset(questionnaireData,select=questionnaireDataCols)
#merge questionnaireData with physData
physData=merge(physData,questionnaireData,by="ID")
#merge physData also with RPE data
physData=merge(physData,questionnaireDataRPE,by=c("ID","condition","step"))
#modify physData (gender and weight needed)
physData=modifyPhysData(verbose,physData)
#unify data
datasetMR=merge(MRData,physData[physData$condition=="test",c("ID","step","outlierPhys","relativeResistance")],by=c("ID","step"))
datasetPhys=mergePhysDataRows(verbose,physData)

#anonymise IDs to protect participant identity
datasetMR$ID=as.factor(datasetMR$ID)
levels(datasetMR$ID)=paste("id",sample.int(length(levels(datasetMR$ID))),sep="")
datasetPhys$ID=as.factor(datasetPhys$ID)
levels(datasetPhys$ID)=levels(datasetMR$ID)

#save full dataset to csv
write.table(datasetMR,file="output\\datasetMR.csv",sep=";", row.names = F)
write.table(datasetPhys,file="output\\datasetPhys.csv",sep=";", row.names = F)

#generate datasets for analysis
#no outliers
datasetMRNoOutlier=datasetMR[which(!datasetMR$outlier & datasetMR$outlierPhys=="noOutlier"),]
datasetPhysNoOutlier=datasetPhys[which(datasetPhys$outlierPhys=="noOutlier"),]
#for analysis of maximal performance
datasetPhysMaxPerformance=unique(datasetPhysNoOutlier[,c("ID","condition","Gender","maxPowerCondition","maxHRCondition")])
#save datasets to csv
write.table(datasetMRNoOutlier,file="output\\datasetMRNoOutlier.csv",sep=";", row.names = F)
write.table(datasetPhysNoOutlier,file="output\\datasetPhysNoOutlier.csv",sep=";", row.names = F)
write.table(datasetPhysMaxPerformance,file="output\\datasetPhysMaxPerformance.csv",sep=";", row.names = F)
