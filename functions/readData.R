### Functions to read and modify data
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

#get questionnaireData
#verbose: detail of output
#folder: folder to search in for data
getQuestionnaireData=function(software,verbose,folder){
  if (verbose>1) {
    print("Reading questionnaire data from files ...")
  }
  if (software=="OSWeb"){
    questionnaireData=getDataOSWeb(verbose,folder,part="questionnaire")
    questionnaireData=subset(questionnaireData,select=c("ID","questionID","answer"))
    questionnaireData=reshape(questionnaireData, idvar = "ID", timevar = "questionID", direction = "wide")
    names(questionnaireData)=gsub("answer.","",names(questionnaireData))
    
  }else if (software=="OpenSesame"){
    questionnaireData=getQuestionnaireDataOpenSesame(verbose,folder, pattern="Test", part=c("questionaire","questionnaire","questionaireQuestions"),ending="csv")
  }else if (software=="jQuery"){
    questionnaireData=getQuestionnaireDataJQuery(verbose,folder, preText="",ending="csv")
  }
  if (verbose>1) {
    print(paste("Questionnaire data from",nrow(questionnaireData),"participants was read."))
  }
  return(questionnaireData)
}

#get questionnairedata for RPE questions (different block)
getRPEQuestionnaireData=function(software,verbose,folder){
  if (verbose>1) {
    print("Reading RPEquestionnaire data from files ...")
  }
  if (software=="OpenSesame"){
    questionnaireData=getRPEQuestionnaireDataOpenSesame(verbose,folder, preText="", c("mainQuestions"),ending="csv")
  }else {
    #not implemented
  }
  if (verbose>1) {
    print(paste("RPEQuestionnaire data from",nrow(questionnaireData),"participants was read."))
  }
  return(questionnaireData)
}

#get mental rotation data
#verbose: detail of output
#folder: folder to search in for data
#block: name of block of interest
getMRData=function(software,verbose,folder,block="main"){
  if (verbose>1) {
    print(paste("Reading mental rotation data for block",block,"from files"))
  }
  if (software=="OSWeb"){
    MRData=getDataOSWeb(verbose,folder,part=block)
  }else if (software=="OpenSesame"){
    MRData=getDataOpenSesame(verbose,folder,part=block)
  }
  if (verbose>1) {
    print(paste("Mental rotation data from",length(unique(MRData$ID)),"participants was read. (",nrow(MRData),"trials in total)"))
  }
  return(MRData)
}

#modifies the questionnairedata, calculates some additional information
#questionnaireData: dataset
modifyQuestionnaireData=function(questionnaireData,toFirstChars,toNums,cleanWhiteSpaces) {
  if (verbose>1) {
    print("Doing calculations on questionnaire data ...")
  }
  #transform values to numeric, remove white spaces, unify gender
  questionnaireData=cleanData(questionnaireData,toFirstChars,toNums,cleanWhiteSpaces)
  #rename columns to different names
  #colnames(questionnaireData) = make.unique(names(questionnaireData))
  if (verbose>1) {
    print("Calculations on questionnaire data finished.")
  }
  return(questionnaireData)
}

modifyQuestionnaireDataRPE=function(questionnaireDataRPE){
  #rename variables and delete unnecessary columns
  questionnaireDataRPE$step=questionnaireDataRPE$subblock
  questionnaireDataRPE$subblock=NULL
  questionnaireDataRPE$block=NULL
  #rename last block(20) to last block of condition
  for(thisID in unique(questionnaireDataRPE$ID)){
    for(thisCondition in unique(questionnaireDataRPE$condition)){
      questionnaireDataRPE$step[questionnaireDataRPE$step==20 & questionnaireDataRPE$ID==thisID & questionnaireDataRPE$condition==thisCondition]=
        max(questionnaireDataRPE$step[questionnaireDataRPE$step<20 & questionnaireDataRPE$ID==thisID & questionnaireDataRPE$condition==thisCondition])
    }
  }
  #reshape
  questionnaireDataRPE=reshape(questionnaireDataRPE,idvar=c("ID","step","condition"),timevar="type",direction="wide")
  #rename variables
  questionnaireDataRPE$cRPE=toNumeric(questionnaireDataRPE$value.cRPE)
  questionnaireDataRPE$RPE=toNumeric(questionnaireDataRPE$value.RPE)
  questionnaireDataRPE$value.cRPE=NULL
  questionnaireDataRPE$value.RPE=NULL
  #change names of condition to english
  questionnaireDataRPE$condition=ifelse(questionnaireDataRPE$condition=="Kontroll","control","test")
  return(questionnaireDataRPE)
}

#modifies the mental rotation data, calculates some additional information
#verbose: detail of output
#MRData: dataset
#outlierFactor: trials deviating by more than outlierFactor*sd from mean will be classified as outliers
modifyMRData=function(verbose,MRData,outlierFactor) {
  if (verbose>1) {
    print("Doing calculations on mental rotation data ...")
  }
  #rename variables
  MRData$deg=toNumeric(MRData$angle)
  MRData$reactionTime=toNumeric(MRData$response_time)
  MRData$step=MRData$aaSubBlock
  MRData$angle=NULL
  MRData$response_time=NULL
  MRData$aaSubBlock=NULL
  #mark outliers
  MRData=sortOutliers(verbose,MRData,outlierFactor)
  if (verbose>1) {
    print(paste(sum(MRData$outlier),"outliers detected (deviating by more than",
                outlierFactor,"standard deviations from mean (by degree)"))
  }
  MRData$type=ifelse(MRData$correct==1,"hit","incorrect")
  MRData$typeOutlier=ifelse(MRData$outlier,paste(toChar(MRData$type),"Outlier",sep=""),toChar(MRData$type))
  MRData$correctSide=MRData$correct_response
  MRData$modelNumber=paste("m",stringToNum(MRData$model),sep="")
  #save original degrees of rotation
  MRData$originalDegrees=MRData$deg
  #modify angles to 360-angle if angle>180, but keep information
  MRData$direction=ifelse(MRData$deg>180,"-",ifelse(MRData$deg==0 | MRData$deg==180,"0","+"))
  MRData$deg=ifelse(MRData$deg>180,360-MRData$deg,MRData$deg)
  #drop unnecessary rows
  MRData[,c("aaTestControl","stimulusBottom","stimulusLeft","stimulusRight","correct_response","axis","model")]=NULL
  return(MRData)
}

#summarizes baseline pupil size per trial
summarizeShowStimulusMRData=function(verbose,showStimulusMRData) {
  #choose first 3 values for baseline pupil size
  showStimulusMRData$baselinePupilSizes=showStimulusMRData$pupilSize
  showStimulusMRData$baselinePupilSizes[showStimulusMRData$frames>3]=NA
  showStimulusMRData$baselinePupilSizes[showStimulusMRData$baselinePupilSizes<1]=NA
  #group data by stimulus
  library(plyr)
  showStimulusMRDataSummarized=ddply(showStimulusMRData,
                            .(ID,block,startTimeOfStimulus,startTimeOfBlock),
                            summarize,
                            baselinePupilSize=median(baselinePupilSizes,na.rm=T))
  #remove "ShowStimulus" for merging blocks later
  showStimulusMRDataSummarized$block=lapply(strsplit(showStimulusMRDataSummarized$block,split="Show"),`[[`,1)
  return(showStimulusMRDataSummarized)
}


#mark outliers, which deviate by more than sdFactor*sd from the mean by degree
sortOutliers=function(verbose,MRData,sdFactor) {
  degrees=levels(as.factor(MRData$deg))
  #allData$type=toChar(allData$type)
  MRData$outlier=FALSE
  for(degree in degrees) {
    degreeSubset=MRData[which(MRData$deg==degree),]
    meanRT=mean(degreeSubset$reactionTime)
    sdRT=sd(degreeSubset$reactionTime)
    if (verbose>2){
      print(paste("mean+-sd at angle ",degree," : ",meanRT,"+-",sdRT,sep=""))
    }
    MRData$outlier[which(MRData$deg==degree & abs(MRData$reactionTime-meanRT)>sdFactor*sdRT)]=TRUE
  }
  #allData$type=as.factor(allData$type)
  return(MRData)
}


#reads data from files
#verbose: detail of output
#folder: folder to search in for files
#pattern: Filter, only get files which contain pattern
#part: Filter, only get parts of data in blocks whose name matches part
#ending: filetype of files
getDataOpenSesame=function(verbose, folder, pattern="Test", part="main",ending="csv") {
  #get files in folger (Reaction Time Data)
  fileNames=getFileNames(folder,pattern,ending)
  if (verbose>2) {
    print("list of files:")
    print(fileNames)
  }
  #initialize empty dataframe
  dat=data.frame()
  #loop through all files
  for (fileIndex in 1:length(fileNames)) {
    fileName=fileNames[fileIndex]
    #read data in file as table
    rawData=read.csv(paste(folder,fileName,sep=""),header=TRUE,fill=TRUE, sep=",")
    #choose only specified block
    dataset=rawData[rawData$aaBlock %in% part,]
    # dataset$numberInBlock=ave(dataset[,1],                 # Create numbering variable
    #                           dataset$aaBlock,
    #                           FUN = seq_along)
    if (verbose>3) {
      print(paste("read", nrow(dataset), "values from file:",fileName,"\n"))
    }
    #add to dataset
    dat=rbind(dat,dataset)
  }
  #change names
  dat$block=dat$aaBlock
  dat$ID=dat$aaID
  dat$aaBlock=NULL
  dat$aaID=NULL
  return(dat)
}

getRPEQuestionnaireDataOpenSesame=function(verbose, folder, preText="", part=c("mainQuestions"),ending="csv") {
  #get files in folder
  fileNames=getFileNames(folder,preText,ending)
  if (verbose>2) {
    print("list of files:")
    print(fileNames)
  }
  #initialize empty dataframe
  dat=data.frame()
  #loop through all files
  for (fileIndex in 1:length(fileNames)) {
    fileName=fileNames[fileIndex]
    #read data in file as table
    rawData=read.csv(paste(folder,fileName,sep=""),header=TRUE,fill=TRUE, sep=",")
    #choose only specified block
    dataset=rawData[rawData$aaBlock %in% part,]
    if (verbose>3) {
      print(paste("read", nrow(dataset), "values from file:",fileName,"\n"))
    }
    #add to dataset
    dat=rbind(dat,dataset)
  }
  #change names and keep only 4 columns
  names(dat)[1:6]=c("block","ID","subblock","condition","value","type")
  dat[,7:ncol(dat)]=NULL
  return(dat)
}

#reads data from OSWeb/JATOS files
#verbose: detail of output
#folder: folder to search in for files
#preText: Filter, only get files which start with preText
#part: Filter, only get parts of data in blocks whose name matches part
#ending: filetype of files
getDataOSWeb=function(verbose, folder, preText="", part="main",ending="csv") {
  #get files in folder (Reaction Time Data)
  fileNames=getFileNames(folder,preText,ending)
  if (verbose>2) {
    print("list of files:")
    print(fileNames)
  }
  #initialize empty dataframe
  dat=data.frame()
  #loop through all files
  for (fileIndex in 1:length(fileNames)) {
    fileName=fileNames[fileIndex]
    #read data in file as table
    rawData=read.csv(paste(folder,fileName,sep=""),header=TRUE,fill=TRUE, sep=",")
    #choose only specified block
    dataset=rawData[rawData$aaBlock %in% part,]
    if (verbose>3) {
      print(paste("read", nrow(dataset), "values from file:",fileName,"\n"))
    }
    #add to dataset
    dat=rbind(dat,dataset)
  }
  #change names
  dat$block=dat$aaBlock
  dat$ID=dat$aaID #aaID for old version
  dat$aaBlock=NULL
  dat$aaID=NULL
  return(dat)
}

#modified 
getQuestionnaireDataJQuery=function(verbose, folder, preText="",ending="csv") {
  #get files in folger (Reaction Time Data)
  fileNames=getFileNames(folder,preText,ending)
  if (verbose>2) {
    print("list of files:")
    print(fileNames)
  }
  #initialize empty dataframe
  dat=data.frame()
  #loop through all files
  for (fileIndex in 1:length(fileNames)) {
    fileName=fileNames[fileIndex]
    #read data in file as table
    rawData=read.csv(paste(folder,fileName,sep=""),header=TRUE,fill=TRUE, sep=",")
    #choose only specified block
    dataset=rawData[rawData$aaBlock=="",]
    if (verbose>3) {
      print(paste("read", nrow(dataset), "values from file:",fileName,"\n"))
    }
    #add to dataset
    dat=rbind(dat,dataset)
  }
  #change names
  #dat$block="questionnaire"
  dat$ID=dat$workerId #aaId for old version
  dat$aaBlock=NULL
  dat$workerId=NULL
  return(dat)
}

#reads data from files
#verbose: detail of output
#folder: folder to search in for files
#pattern: Filter, only get files which contain pattern
#part: Filter, only get parts of data in blocks whose name matches part
#ending: filetype of files
getQuestionnaireDataOpenSesame=function(verbose, folder, pattern="Test", part=c("questionaire","questionnaire","questionaireQuestions"),ending="csv") {
  #get files in folder
  fileNames=getFileNames(folder,pattern,ending)
  if (verbose>2) {
    print("list of files:\n")
    print(fileNames)
  }
  #initialize empty dataframe
  dat=data.frame()
  #loop through all files
  for (fileIndex in 1:length(fileNames)) {
    fileName=fileNames[fileIndex]
    #read data in file as table
    rawData=read.csv(paste(folder,fileName,sep=""),header=TRUE,fill=TRUE, sep=",")
    #choose only specified block
    dataset=rawData[rawData$aaBlock %in% part,]
    #add interesting data to vector 
    values=append(toChar(dataset[,5]),dataset$aaID[1])
    if (verbose>3) {
      print(paste("read values for file:",fileName,"\n"))
      print(values)
    }
    #add to dataset
    dat=rbind(dat,values,stringsAsFactors = FALSE)
    #set names according to questionIDs
    if (fileIndex==1) {
      names(dat)=append(toChar(dataset[,6]),"ID")
    }
  }
  return(dat)
}

#read physiological data from files
getPhysData=function(verbose,folder,pattern="",ending="csv"){
  #get files in folger (Reaction Time Data)
  fileNames=getFileNames(folder,pattern,ending)
  if (verbose>2) {
    print("list of files:")
    print(fileNames)
  }
  #initialize empty dataframe
  dat=data.frame()
  #loop through all files
  for (fileIndex in 1:length(fileNames)) {
    fileName=fileNames[fileIndex]
    #read data in file as table
    dataset=read.csv(paste(folder,fileName,sep=""),header=TRUE,fill=TRUE, sep=";")
    dataset=dataset[dataset$condition!="",] #remove empty rows
    if (verbose>3) {
      print(paste("read", nrow(dataset), "values from file:",fileName,"\n"))
    }
    #add to dataset
    dat=rbind(dat,dataset)
  }
  #change names
  dat$ID=dat$VP
  dat$VP=NULL
  return(dat)
}

modifyPhysData=function(verbose,physData){
  if (verbose>1) {
    print("Doing calculations on physiological data ...")
  }
  #convert duration to seconds
  physData$durationSeconds=minsToSeconds(physData$duration)
  #adapt for ending during RPE collection
  physData$durationSeconds=ifelse(physData$durationSeconds>165,180,physData$durationSeconds)
  #cadence to numeric ("na" entries to NA)
  physData$cadence=toNumeric(physData$cadence)
  #get max power for unfinished steps
  physData$stepsize=ifelse(physData$Gender=="w",20,30)
  physData$timeCorrectedPower=physData$power_output-physData$stepsize*(6-physData$durationSeconds%/%30)/6
  #get max power and hr by condition and overall, detect outliers by less than 3 or more than 9 steps
  physData$maxPower=0
  physData$maxHR=0
  physData$maxPowerCondition=0
  physData$maxHRCondition=0
  physData$outlierPhys="noOutlier"
  outlierTooFew=0
  outlierTooMany=0
  for(thisID in unique(physData$ID)){
    for(thisCondition in unique(physData$condition)){
      thisIDmaxPowerCondition=max(physData$timeCorrectedPower[physData$ID==thisID & physData$condition==thisCondition])
      physData$maxPowerCondition[physData$ID==thisID & physData$condition==thisCondition]=thisIDmaxPowerCondition
      thisIDmaxHRCondition=max(physData$heart_rate[physData$ID==thisID & physData$condition==thisCondition])
      physData$maxHRCondition[physData$ID==thisID & physData$condition==thisCondition]=thisIDmaxHRCondition
    }
    thisIDmaxPower=max(physData$timeCorrectedPower[physData$ID==thisID])
    physData$maxPower[physData$ID==thisID]=thisIDmaxPower
    thisIDmaxHR=max(physData$heart_rate[physData$ID==thisID])
    physData$maxHR[physData$ID==thisID]=thisIDmaxHR
    #get starting power for outlier detection
    thisIDstartPower=min(physData$timeCorrectedPower[physData$ID==thisID])
    #compare number of steps
    if(thisIDmaxPower-thisIDstartPower<2*physData$stepsize[physData$ID==thisID][1]){
      physData$outlierPhys[physData$ID==thisID]="outlierTooFewSteps"
      outlierTooFew=outlierTooFew+1
    }
    if(thisIDmaxPower-thisIDstartPower>9*physData$stepsize[physData$ID==thisID][1]){
      physData$outlierPhys[physData$ID==thisID]="outlierTooManySteps"
      outlierTooMany=outlierTooMany+1
    }
  }
  if (verbose>1) {
    print(paste(outlierTooFew," outliers with too few steps"))
    print(paste(outlierTooMany," outliers with too many steps"))
  } 
  #get relative max power by condition
  physData$maxPowerConditionRelative=physData$maxPowerCondition/physData$Weight
  #get relative resistance
  physData$relativeResistance=physData$timeCorrectedPower/physData$maxPower
  return(physData)
}

#merge rows of physData with same power values in different steps (last step <30 seconds)
mergePhysDataRows=function(verbose,physData){
  if (verbose>1) {
    print("Merging same power rows of physiological data ...")
  } 
  rows1=nrow(physData)
  library(plyr)
  physData=ddply(physData,
                 .(ID,condition,Gender,Experience,Weight,timeCorrectedPower,maxPower,maxHR,maxPowerCondition,maxHRCondition,outlierPhys,maxPowerConditionRelative,relativeResistance),
                 summarize,
                 step=min(step), #choose lower step if higher step not finished
                 heartRate=max(heart_rate),
                 power=min(power_output),
                 cadence=weighted.mean(cadence,durationSeconds),
                 cRPE=max(cRPE),
                 RPE=max(RPE),
                 durationSeconds=max(durationSeconds))
  if (verbose>1) {
    print(paste(rows1-nrow(physData)," rows merged"))
  } 
  return(physData)
}
