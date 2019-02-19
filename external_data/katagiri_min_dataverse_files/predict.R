setwd("[current directory]/data")

#%%%%%%%%%%%%%%%%%
####  SET-UP  ####
#%%%%%%%%%%%%%%%%%

# Load packages
require(caret)
require(randomForest)
require(e1071)
require(devtools)
install_github("swager/randomForestCI")
require(randomForestCI)

# Function for F-score
FbScore = function(conf, b=1) {
  factor = (1 + b^2)
  tp = conf[1,1]
  fn = conf[2,1]
  fp = conf[1,2]
  return((factor * tp)/(factor * tp + b^2 * fn + fp))
}

# Weekly date sequence
dinit = as.Date("01-01-1958", "%m-%d-%Y")
dfinal = as.Date("11-26-1963", "%m-%d-%Y")
weeks = seq(from = dinit, to = dfinal, by='weeks')


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
####  CREATE PREDICTED DATA  ####
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Define the collections
collections = c("wh", "dos", "fbis")

# Generate predicted data for each collection
for (k in 1:length(collections)) {
  collection = collections[k]
  
  print(paste("Producing", toupper(collection), "data...", sep=" "))
  
  ### Set up the data 
  
  # ID and date for all documents
  datAll = read.csv(paste(collection, "All.csv", sep=""))
  datAll = datAll[,-which(names(datAll)=="X")]
  datAll$date = as.Date(as.character(datAll$date), "%m/%d/%Y")
  
  # Document-term matrix for all documents
  dfAll = read.csv(paste(collection, "DTMAll.csv", sep="")) 
  dfAll = dfAll[,-which(names(dfAll)=="X")]
  
  # Document-term matrix for randomly selected sample
  df = read.csv(paste(collection, "DTM.csv", sep=""))
  df = df[,-which(names(df)=="X")]
  
  # Manual coding of resolve for randomly selected sample 
  tf = read.csv(paste(collection, "Threat.csv", sep="")) 
  
  # Make training and test data
  set.seed(160116)
  
  wTrain = createDataPartition(y = tf$resolve,
                               p = 0.75,
                               list = FALSE)
  
  wTrainSet = data.frame(df, Resolve=tf$resolve)[wTrain,]
  wTestSet = data.frame(df, Resolve=tf$resolve)[-wTrain,]
  
  wTrainSet$Resolve = (ifelse(wTrainSet$Resolve=="None", "No", "Yes"))
  wTrainSet$Resolve = factor(wTrainSet$Resolve)
  wTestSet$Resolve = (ifelse(wTestSet$Resolve=="None", "No", "Yes"))
  wTestSet$Resolve = factor(wTestSet$Resolve)
  
  Xtrain = wTrainSet[,1:(ncol(wTrainSet)-1)]
  Xtest = wTestSet[,1:(ncol(wTrainSet)-1)]
  Ytrain = factor(as.numeric(wTrainSet[,ncol(wTrainSet)])-1)
  Ytest = factor(as.numeric(wTestSet[,ncol(wTestSet)])-1)
  
  
  ### Run balanced random forest
  nmin = sum(wTrainSet$Resolve=="Yes")
  
  rfFit = randomForest(Xtrain, Ytrain, data=wTrainSet, ntree = 2000,
                       strata = Ytrain, mtry = 40,
                       sampsize = rep(nmin, 2), keep.inbag=TRUE)
  
  rfProbs = 1-predict(rfFit, type = "prob", newdata = wTestSet)[,1] # Predictions
  rfClasses = ifelse(rfProbs >= 0.5, 1, 0) # Dichotomize
  cm = confusionMatrix(data = factor(rfClasses), factor(as.numeric(wTestSet$Resolve)-1)) # Confusion matrix
  
  # Diagnostics (see Appendix C)
  print(paste("F1 score:", round(FbScore(cm$table, 1), 3), sep=" "))
  print(paste("F2 score:", round(FbScore(cm$table, 2), 3), sep=" "))
  print(paste("Kappa:", round(cm$overall["Kappa"], 3), sep=" "))
  print(paste("Accuracy:", round(cm$overall["Accuracy"], 3), sep=" "))
  print(paste("Sensitivity:", round(cm$byClass["Sensitivity"], 3), sep=" "))
  print(paste("Specificity:", round(cm$byClass["Specificity"], 3), sep=" "))
  
  # Variances (Wager et al. 2014) for training data
  ij = randomForestInfJack(rfFit, Xtest, calibrate=T)
  varPlot = qplot(ij$y.hat, ij$var.hat, geom="point") + theme_bw() + xlab("y.hat") + ylab("var.hat") +
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14))
  
  # Variances (Wager et al. 2014) for all data
  ij2 = randomForestInfJack(rfFit, dfAll, calibrate=TRUE)
  varAllPlot = qplot(ij2$y.hat, ij2$var.hat, geom="point") + theme_bw() + xlab("y.hat") + ylab("var.hat") + 
    theme(axis.text=element_text(size=12), axis.title=element_text(size=14))

  ### Export predicted probabilities
  datAll$predMean = ij2[,1]
  datAll$predVar = ij2[,2]
  write.csv(datAll, paste(collection, "AllPred.csv", sep=""))
  
  # Save histogram of predictions (see Appendix C)
  predHist = qplot(datAll$predMean, bins=30) + xlab("Predicted Probability") + ylab("Count") + theme_bw()
  ggsave(paste("./../supplementary/", collection, "PredHist.pdf", sep=""), predHist, width = 5, height = 3.5)
  
  # Save scatterplot of variances (see Appendix C)
  ggsave(paste("./../supplementary/", collection, "AllCI.pdf", sep=""), varAllPlot, width = 5, height = 3.5)
  
  
  ### Combine predictions into weekly data
  doc.id = unique(datAll$doc.id)
  date = datAll$date[datAll$doc.id %in% doc.id]
  
  # First, combine segments back into documents
  threatAvg = day = NA
  
  for (i in 1:length(doc.id)) {
    docSegments = datAll[datAll$doc.id==doc.id[i],]
    nSegments = nrow(docSegments)
    avg = sum(docSegments$predMean)/nSegments
    threatAvg[i] = ifelse(avg >= 0.5, 1, 0)
    day[i] = as.character(docSegments$date[1])
  }
  
  threats = data.frame(doc.id, date=day, threatAvg)
  threats$date = as.Date(threats$date)
  
  # Aggregate documents by week 
  wsumAvg = ndoc = NA
  
  for (i in 1:(length(weeks)-1)) {
    oneweek = threats[(threats$date >= weeks[i] & threats$date < weeks[i+1]),]
    oneweek = oneweek[!is.na(oneweek$date),]
    wsumAvg[i] = sum(oneweek$threatAvg, na.rm=T)
    ndoc[i] = nrow(oneweek)
  }
  
  weeklyPred = data.frame(week=weeks[-length(weeks)], wsumAvg, ndoc)
  write.csv(weeklyPred, paste(collection, "WeekPred.csv", sep=""))

  
  ### Make subsets of DOS and FBIS data for governmental and high-level officials
  if (collection=="dos") {
    
    # Get data for only high-level DOS cables
    highDOS = datAll[datAll$highLevel==1,]
    highDOS$doc.id = as.character(highDOS$doc.id)
    datAll$doc.id = as.character(datAll$doc.id)
    doc.id = unique(datAll$doc.id)
    date = datAll$date[datAll$doc.id %in% doc.id]
    
    threatAvgM = dayM = NA
    
    for (i in 1:length(doc.id)) {
      docSegments = datAll[datAll$doc.id==doc.id[i],]
      nSegments = nrow(docSegments)
      avg = sum(docSegments$predMean)/nSegments
      threatAvgM[i] = ifelse(doc.id[i] %in% highDOS$doc.id & avg >= 0.5, 1, 0)
      dayM[i] = as.character(docSegments$date[1])
    }
    
    threatsM = data.frame(doc.id, date=dayM, threatAvgM)
    threatsM$date = as.Date(threatsM$date)
    
    # Aggregate documents by week
    wsumAvg = ndoc = NA
    
    for (i in 1:(length(weeks)-1)) {
      oneweek = threatsM[(threatsM$date >= weeks[i] & threatsM$date < weeks[i+1]),]
      oneweek = oneweek[!is.na(oneweek$date),]
      wsumAvg[i] = sum(oneweek$threatAvg, na.rm=T)
      ndoc[i] = nrow(oneweek)
    }
    
    weeklyPredM = data.frame(week=weeks[-length(weeks)], wsumAvg, ndoc)
    write.csv(weeklyPredM, "dosWeekPredHigh.csv")
    
  } else if (collection=="fbis") {
    
    ## Redo for mid-level FBIS only
    midFBIS = datAll[datAll$midLevel==1,]  
    midFBIS$doc.id = as.character(midFBIS$doc.id)
    datAll$doc.id = as.character(datAll$doc.id)
    doc.id = unique(datAll$doc.id)
    date = datAll$date[datAll$doc.id %in% doc.id]
    
    threatAvg1 = day1 = NA
    
    for (i in 1:length(doc.id)) {
      docSegments = datAll[datAll$doc.id==doc.id[i],]
      nSegments = nrow(docSegments)
      avg = sum(docSegments$predMean)/nSegments
      maj = sum(docSegments$predMean >= 0.5)/nSegments
      threatAvg1[i] = ifelse(doc.id[i] %in% midFBIS$doc.id & avg >= 0.5, 1, 0)
      day1[i] = as.character(docSegments$date[1])
    }
    
    threats1 = data.frame(doc.id, date=day1, threatAvg1)
    threats1$date = as.Date(threats1$date)
    
    # Aggregate documents by week
    wsumAvg = ndoc = NA
    
    for (i in 1:(length(weeks)-1)) {
      oneweek = threats1[(threats1$date >= weeks[i] & threats1$date < weeks[i+1]),]
      oneweek = oneweek[!is.na(oneweek$date),]
      wsumAvg[i] = sum(oneweek$threatAvg, na.rm=T)
      ndoc[i] = nrow(oneweek)
    }
    
    weeklyPred1 = data.frame(week=weeks[-length(weeks)], wsumAvg, ndoc)
    write.csv(weeklyPred1, "fbisWeekPredMid.csv")
    
    ## Redo for high-level FBIS only
    highFBIS = datAll[datAll$highLevel==1,]  
    highFBIS$doc.id = as.character(highFBIS$doc.id)
    datAll$doc.id = as.character(datAll$doc.id)
    doc.id = unique(datAll$doc.id)
    date = datAll$date[datAll$doc.id %in% doc.id]
    
    threatMaj2 = threatAvg2 = threatMean2 = day2 = NA
    
    for (i in 1:length(doc.id)) {
      docSegments = datAll[datAll$doc.id==doc.id[i],]
      nSegments = nrow(docSegments)
      avg = sum(docSegments$predMean)/nSegments
      maj = sum(docSegments$predMean >= 0.5)/nSegments
      threatAvg2[i] = ifelse(doc.id[i] %in% highFBIS$doc.id & avg >= 0.5, 1, 0)
      day2[i] = as.character(docSegments$date[1])
    }
    
    threats2 = data.frame(doc.id, date=day2, threatAvg2)
    threats2$date = as.Date(threats2$date)
    
    # Aggregate documents by week 
    wsumAvg = ndoc = NA
    for (i in 1:(length(weeks)-1)) {
      oneweek = threats2[(threats2$date >= weeks[i] & threats2$date < weeks[i+1]),]
      oneweek = oneweek[!is.na(oneweek$date),]
      wsumAvg[i] = sum(oneweek$threatAvg, na.rm=T)
      ndoc[i] = nrow(oneweek)
    }
    weeklyPred2 = data.frame(week=weeks[-length(weeks)], wsumAvg, ndoc)
    write.csv(weeklyPred2, "fbisWeekPredHigh.csv")
  }
  
  print(paste(toupper(collection), "data completed.", sep=" "))
}