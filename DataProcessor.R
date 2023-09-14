# Data processor
# Belonging to "Developing an Accessibility Metric for VR Games Based on Motion Data Captured Under Game Conditions"
# Felix J. Thiel
# 2022


# Libraries ----

library(readr)
library(fmsb)


# Functions ----

# Helper function to normalise vectors
normaliseVector <- function(x) {x / sqrt(sum(x^2))}

# Helper function to do the dot product
dotProduct <- function(u, v){
  unorm <- normaliseVector(u)
  vnorm <- normaliseVector(v)
  
  #dot <- unorm %*% vnorm
  dot <- sum(unorm * vnorm)
  return(dot)
}

# Wrap up data into one object
WrapUserData <- function(aGameName, aMotionData, aHand1Index, aHand2Index, aInputData){
  
  wrappedData <- list(game = aGameName,
                      motionData = aMotionData,
                      hand1Index = aHand1Index,
                      hand2Index = aHand2Index,
                      inputData = aInputData)
  
  return(wrappedData)
}

GetMetricForGames <- function(wrappedData){
  # Batch process data
  impList <- BatchProcessWrappedSetsImp(wrappedData)
  engList <- BatchProcessWrappedSetsEng(wrappedData, 70)
  dirList <- BatchProcessWrappedSetsDir(wrappedData)
  jerList <- BatchProcessWrappedSetsJer(wrappedData, TRUE, 0.005)
  expList <- BatchProcessWrappedSetsExp(wrappedData)
  vList <- BatchProcessWrappedSetsV(wrappedData)
  aList <- BatchProcessWrappedSetsA(wrappedData)
  
  # Merge data into one each
  mergedImp <- MergeImpSets(impList)
  mergedEng <- MergeEngSets(engList)
  mergedDir <- MergeDirSets(dirList)
  mergedJer <- MergeJerSets(jerList)
  mergedExp <- MergeExpSets(expList)
  mergedV <- MergeVSets(vList)
  mergedA <- MergeAccSets(aList)
  
  # Extract metrics from the merged data sets
  metricsImp <- MetricsFromGiraudImp(mergedImp)
  metricsEng <- MetricsFromGiraudEng(mergedEng)
  metricsDir <- MetricsFromGiraudDir(mergedDir)
  metricsJer <- MetricsFromGiraudJer(mergedJer)
  metricsExp <- MetricsFromGiraudExp(mergedExp)
  metricsV <- MetricsFromV(mergedV)
  metricsA <- MetricsFromA(mergedA)

  summaryGames <- list(Imp = metricsImp, Eng = metricsEng, Dir = metricsDir, Jer = metricsJer, Exp = metricsExp, V = metricsV, A = metricsA)  
}


 # Take a list of wrapped data and group it by game
SortDataByGame <- function(listOfWrappedData){
  
  gameList = c()
  
  buckets = list()
  
  # walk through the list
  
  for(i in 1:length(listOfWrappedData)){
    
    # get element
    element <- listOfWrappedData[[i]]
    
    # get game the data comes from
    game <- element$game
    
    # do we already have a bucket for that game?
    alreadyExists <- is.element(game, gameList)
    
    if(!alreadyExists){
      # If it does not exist yet, create one
      buckets[[length(buckets)+1]] <- list(name = game, elements = c())
      gameList <- append(gameList, game)
    }
    
    # get index of the bucket
    bucketIndex <- match(game, gameList)
    
    # add data to the bucket
    bucketLength <- length(buckets[[bucketIndex]]$elements)
    buckets[[bucketIndex]]$elements[[bucketLength+1]] <- element
  }
  
  # return the buckets
  return(buckets)
}

# Merge a list of wrapped data together
# Probably won't use this
MergeDataBatch <- function(listOfWrappedData){
  
  mergedElements <- list()
  
  for(i in 1:length(listOfWrappedData)){
    mergedDataGame <- MergeDataSingleGame(listOfWrappedData[[i]])
    mergedElements <- list(mergedElements, mergedDataGame)
  }
  
  return(mergedElements)
}

# Merge a list of wrapped data together by game
# Probably won't use this
MergeDataSingleGame <- function(WrappedData){
  
  # from each element get the head and hand data and merge it
  
  # there is always at least one, so we take that as base
  motionData <- WrappedData[[1]]$motionData
  hand1Index <- WrappedData[[1]]$hand1Index
  hand2Index <- WrappedData[[1]]$hand2Index
  headData <- motionData[motionData$deviceIndex == 0]
  hand1Data <- motionData[motionData$deviceIndex == hand1Index]
  hand2Data <- motionData[motionData$deviceIndex == hand2Index]
  inputData <- WrappedData[[1]]$inputData
  
  # then append
  for(i in 2:length(WrappedData)){
    
    motionData <- WrappedData[[i]]$motionData
    hand1Index <- WrappedData[[i]]$hand1Index
    hand2Index <- WrappedData[[i]]$hand2Index
    
    # Todo: Make this append
    headData <- motionData[motionData$deviceIndex == 0]
    hand1Data <- motionData[motionData$deviceIndex == hand1Index]
    hand2Data <- motionData[motionData$deviceIndex == hand2Index]
    inputData <- WrappedData[[i]]$inputData
  }
  
  mergedData <- list(game = WrappedData$game,
                     headMotion = headData,
                     hand1Motion = handData,
                     hand2Motion = hand2Data,
                     inputData = inputData)
  
  return(mergedData)
}

# Functions to read and wrap data ----

FindHandIndices <- function(data, controllerType){
  handIndices <- unique(na.omit(data[data$controllerType == controllerType,]$deviceIndex))
  
  if(length(handIndices) != 2)
  {
    stop("Length Incorrect.")
  }
  
  return(handIndices)
}

ReadAndWrapSingleFile <- function(filePath, game, controllerType){
  
  data <- read_csv(filePath)
  
  # find the indices for the hands
  handIndices <- FindHandIndices(data, controllerType)
  
  wrapped <- WrapUserData(game, data, handIndices[1], handIndices[2], NA)
  
  return(wrapped)
}

ReadAndWrapBatch <- function(fileNames, folderPath, game, controllerType){
  wrappedSets <- list()
  for(i in 1:length(fileNames)){
    fileName <- fileNames[i]
    fullPath <- paste(folderPath, fileName, sep="")
    wrappedSet <- ReadAndWrapSingleFile(fullPath, game, controllerType)
    wrappedSets[[length(wrappedSets)+1]] <- wrappedSet
  }
  
  return(wrappedSets)
}


# Functions to take a single piece of wrapped data and calculate the Giraud series for it ----

# Takes a wrapped data set, calculates the impulsiveness series for this data set and returns a new object containing the impulsiveness values
WrappedDataToImpData <- function(WrappedData){
  motionData <- WrappedData$motionData
  hand1Index <- WrappedData$hand1Index
  hand2Index <- WrappedData$hand2Index
  
  headData <- motionData[motionData$deviceIndex == 0,]
  hand1Data <- motionData[motionData$deviceIndex == hand1Index,]
  hand2Data <- motionData[motionData$deviceIndex == hand2Index,]
  
  impHead <- CalculateImpulsivenessSeries(headData)
  impHand1 <- CalculateImpulsivenessSeries(hand1Data)
  impHand2 <- CalculateImpulsivenessSeries(hand2Data)
  
  impData <- list(game = WrappedData$game,
                  impSeriesHead = impHead,
                  impSeriesHand1 = impHand1,
                  impSeriesHand2 = impHand2)
  
  return(impData)
}

# Takes a wrapped data set, calculates the Energy series for this data set and returns a new object containing the Energy values
WrappedDataToEngData <- function(WrappedData, weight){
  motionData <- WrappedData$motionData
  hand1Index <- WrappedData$hand1Index
  hand2Index <- WrappedData$hand2Index
  
  headData <- motionData[motionData$deviceIndex == 0,]
  hand1Data <- motionData[motionData$deviceIndex == hand1Index,]
  hand2Data <- motionData[motionData$deviceIndex == hand2Index,]
  
  massHand <- 0.050 * weight
  massHead <- 0.081 * weight
  
  engHead <- CalculateEnergySeries(headData, massHead)
  engHand1 <- CalculateEnergySeries(hand1Data, massHand)
  engHand2 <- CalculateEnergySeries(hand2Data, massHand)
  
  engData <- list(game = WrappedData$game,
                  engSeriesHead = engHead,
                  engSeriesHand1 = engHand1,
                  engSeriesHand2 = engHand2)
  
  return(engData)
}

# Takes a wrapped data set, calculates the Directness series for this data set and returns a new object containing the Directness values
WrappedDataToDirData <- function(WrappedData){
  motionData <- WrappedData$motionData
  hand1Index <- WrappedData$hand1Index
  hand2Index <- WrappedData$hand2Index
  
  headData <- motionData[motionData$deviceIndex == 0,]
  hand1Data <- motionData[motionData$deviceIndex == hand1Index,]
  hand2Data <- motionData[motionData$deviceIndex == hand2Index,]
  
  
  dirHead <- CalculateDirectnessSeries(headData, headData)
  dirHand1 <- CalculateDirectnessSeries(hand1Data, headData)
  dirHand2 <- CalculateDirectnessSeries(hand2Data, headData)
  
  dirData <- list(game = WrappedData$game,
                  dirSeriesHead = dirHead,
                  dirSeriesHand1 = dirHand1,
                  dirSeriesHand2 = dirHand2)
  
  return(dirData)
}

# Takes a wrapped data set, calculates the Jerkiness series for this data set and returns a new object containing the Jerkiness values
WrappedDataToJerData <- function(WrappedData, filter, threshold){ # 0.001 m/s are 1 mm/s. Very VERY minor movement
  motionData <- WrappedData$motionData
  hand1Index <- WrappedData$hand1Index
  hand2Index <- WrappedData$hand2Index
  
  headData <- motionData[motionData$deviceIndex == 0,]
  hand1Data <- motionData[motionData$deviceIndex == hand1Index,]
  hand2Data <- motionData[motionData$deviceIndex == hand2Index,]
  
  
  jerHead <- CalculateJerkinessSeries(headData)
  jerHand1 <- CalculateJerkinessSeries(hand1Data)
  jerHand2 <- CalculateJerkinessSeries(hand2Data)
  
  
  # Jerkinesss velocity filter 
  if(filter){
    indicesBelowThreshold <- sqrt(headData$v.x ^ 2 + headData$v.y ^ 2 + headData$v.z ^ 2) < threshold
    jerHead$jerkiness <- replace(jerHead$jerkiness, indicesBelowThreshold, 0)
    
    indicesBelowThreshold <- sqrt(hand1Data$v.x ^ 2 + hand1Data$v.y ^ 2 + hand1Data$v.z ^ 2) < threshold
    jerHand1$jerkiness <- replace(jerHand1$jerkiness, indicesBelowThreshold, 0)
    
    indicesBelowThreshold <- sqrt(hand2Data$v.x ^ 2 + hand2Data$v.y ^ 2 + hand2Data$v.z ^ 2) < threshold
    jerHand2$jerkiness <- replace(jerHand2$jerkiness, indicesBelowThreshold, 0)
  }
  
  
  jerData <- list(game = WrappedData$game,
                  jerSeriesHead = jerHead,
                  jerSeriesHand1 = jerHand1,
                  jerSeriesHand2 = jerHand2)
  
  return(jerData)
}

# Takes a wrapped data set, calculates the Expansiveness series for this data set and returns a new object containing the Expansiveness values
WrappedDataToExpData <- function(WrappedData){
  motionData <- WrappedData$motionData
  hand1Index <- WrappedData$hand1Index
  hand2Index <- WrappedData$hand2Index
  
  headData <- motionData[motionData$deviceIndex == 0,]
  hand1Data <- motionData[motionData$deviceIndex == hand1Index,]
  hand2Data <- motionData[motionData$deviceIndex == hand2Index,]
  
  
  exp <- CalculateExpansivenessSeries(headData, hand1Data, hand2Data)
  
  expData <- list(game = WrappedData$game,
                  expSeries = exp)
  
  return(expData)
}

WrappedDataToVData <- function(WrappedData){
  motionData <- WrappedData$motionData
  hand1Index <- WrappedData$hand1Index
  hand2Index <- WrappedData$hand2Index
  
  headData <- motionData[motionData$deviceIndex == 0,]
  hand1Data <- motionData[motionData$deviceIndex == hand1Index,]
  hand2Data <- motionData[motionData$deviceIndex == hand2Index,]
  
  vHead <- CalculateVelocitySeries(headData)
  vHand1 <- CalculateVelocitySeries(hand1Data)
  vHand2 <- CalculateVelocitySeries(hand2Data)
  
  vData <- list(game = WrappedData$game,
                  vSeriesHead = vHead,
                vSeriesHand1 = vHand1,
                vSeriesHand2 = vHand2)
  
  return(vData)
}

WrappedDataToAccData <- function(WrappedData){
  motionData <- WrappedData$motionData
  hand1Index <- WrappedData$hand1Index
  hand2Index <- WrappedData$hand2Index
  
  headData <- motionData[motionData$deviceIndex == 0,]
  hand1Data <- motionData[motionData$deviceIndex == hand1Index,]
  hand2Data <- motionData[motionData$deviceIndex == hand2Index,]
  
  
  
  aHeadx <- CalculateAxialAcceleration(headData$v.x, headData$timestamp)
  aHeady <- CalculateAxialAcceleration(headData$v.y, headData$timestamp)
  aHeadz <- CalculateAxialAcceleration(headData$v.z, headData$timestamp)
  
  aHand1x <- CalculateAxialAcceleration(hand1Data$v.x, hand1Data$timestamp)
  aHand1y <- CalculateAxialAcceleration(hand1Data$v.y, hand1Data$timestamp)
  aHand1z <- CalculateAxialAcceleration(hand1Data$v.z, hand1Data$timestamp)
  
  aHand2x <- CalculateAxialAcceleration(hand2Data$v.x, hand2Data$timestamp)
  aHand2y <- CalculateAxialAcceleration(hand2Data$v.y, hand2Data$timestamp)
  aHand2z <- CalculateAxialAcceleration(hand2Data$v.z, hand2Data$timestamp)
  
  
  aData <- list(game = WrappedData$game,
                aSeriesHeadx = aHeadx,
                aSeriesHeady = aHeady,
                aSeriesHeadz = aHeadz,
                aSeriesHand1x = aHand1x,
                aSeriesHand1y = aHand1y,
                aSeriesHand1z = aHand1z,
                aSeriesHand2x = aHand2x,
                aSeriesHand2y = aHand2y,
                aSeriesHand2z = aHand2z)
  
  return(aData)
}


# Functions to take a list of wrapped data sets and batch-processes them to get a list of Giraud series ----

BatchProcessWrappedSetsImp <- function(wrappedSets){
  impList <- list()
  for(i in 1:length(wrappedSets)){
    wrapped <- wrappedSets[[i]]
    imp <- WrappedDataToImpData(wrapped)
    impList[[length(impList)+1]] <- imp
  }  
  return(impList)
}

BatchProcessWrappedSetsEng <- function(wrappedSets, weight){
  engList <- list()
  for(i in 1:length(wrappedSets)){
    wrapped <- wrappedSets[[i]]
    eng <- WrappedDataToEngData(wrapped, weight)
    engList[[length(engList)+1]] <- eng
  }  
  return(engList)
}

BatchProcessWrappedSetsDir <- function(wrappedSets){
  dirList <- list()
  for(i in 1:length(wrappedSets)){
    wrapped <- wrappedSets[[i]]
    dir <- WrappedDataToDirData(wrapped)
    dirList[[length(dirList)+1]] <- dir
  }  
  return(dirList)
}

BatchProcessWrappedSetsJer <- function(wrappedSets, filter, threshold){
  jerList <- list()
  for(i in 1:length(wrappedSets)){
    wrapped <- wrappedSets[[i]]
    jer <- WrappedDataToJerData(wrapped, filter, threshold)
    jerList[[length(jerList)+1]] <- jer
  }  
  return(jerList)
}

BatchProcessWrappedSetsExp <- function(wrappedSets){
  expList <- list()
  for(i in 1:length(wrappedSets)){
    wrapped <- wrappedSets[[i]]
    exp <- WrappedDataToExpData(wrapped)
    expList[[length(expList)+1]] <- exp
  }  
  return(expList)
}

BatchProcessWrappedSetsV <- function(wrappedSets){
  vList <- list()
  for(i in 1:length(wrappedSets)){
    wrapped <- wrappedSets[[i]]
    v <- WrappedDataToVData(wrapped)
    vList[[length(vList)+1]] <- v
  }  
  return(vList)
}

BatchProcessWrappedSetsA <- function(wrappedSets){
  aList <- list()
  for(i in 1:length(wrappedSets)){
    wrapped <- wrappedSets[[i]]
    a <- WrappedDataToAccData(wrapped)
    aList[[length(aList)+1]] <- a
  }  
  return(aList)
}



# Functions to take a list of data sets already converted to Giraud series and merges them together ----

MergeImpSets <- function(setsImp){
  impHead <- c()
  impHand1 <- c()
  impHand2 <- c()
  for(i in 1:length(setsImp)){
    element <- setsImp[[i]]
    impHead <- c(impHead, element$impSeriesHead)
    impHand1 <- c(impHand1, element$impSeriesHand1)
    impHand2 <- c(impHand2, element$impSeriesHand2)
  }
  
  merged <- list(game = setsImp[[1]]$game, impSeriesHead = impHead, impSeriesHand1 = impHand1, impSeriesHand2 = impHand2)
  return(merged)
}

MergeEngSets <- function(setsEng){
  engHead <- c()
  engHand1 <- c()
  engHand2 <- c()
  for(i in 1:length(setsEng)){
    element <- setsEng[[i]]
    engHead <- c(engHead, element$engSeriesHead)
    engHand1 <- c(engHand1, element$engSeriesHand1)
    engHand2 <- c(engHand2, element$engSeriesHand2)
  }
  
  merged <- list(game = setsEng[[1]]$game, engSeriesHead = engHead, engSeriesHand1 = engHand1, engSeriesHand2 = engHand2)
  return(merged)
}

MergeDirSets <- function(setsDir){
  dirHead <- c()
  dirHand1 <- c()
  dirHand2 <- c()
  for(i in 1:length(setsDir)){
    element <- setsDir[[i]]
    dirHead <- c(dirHead, element$dirSeriesHead)
    dirHand1 <- c(dirHand1, element$dirSeriesHand1)
    dirHand2 <- c(dirHand2, element$dirSeriesHand2)
  }
  
  merged <- list(game = setsDir[[1]]$game, dirSeriesHead = dirHead, dirSeriesHand1 = dirHand1, dirSeriesHand2 = dirHand2)
  return(merged)
}

MergeJerSets <- function(setsJer){
  jer <- c()
  num <- c()
  denom <- c()
  for(i in 1:length(setsJer)){
    element <- setsJer[[i]]$jerSeriesHead
    jer <- c(jer, element$jerkiness)
    num <- c(num, element$numerator)
    denom <- c(denom, element$denominator)
  }
  mergedHead <- list(jerkiness = jer, numerator = num, denominator = denom)
  
  jer <- c()
  num <- c()
  denom <- c()
  for(i in 1:length(setsJer)){
    element <- setsJer[[i]]$jerSeriesHand1
    jer <- c(jer, element$jerkiness)
    num <- c(num, element$numerator)
    denom <- c(denom, element$denominator)
  }
  mergedHand1 <- list(jerkiness = jer, numerator = num, denominator = denom)
  
  jer <- c()
  num <- c()
  denom <- c()
  for(i in 1:length(setsJer)){
    element <- setsJer[[i]]$jerSeriesHand2
    jer <- c(jer, element$jerkiness)
    num <- c(num, element$numerator)
    denom <- c(denom, element$denominator)
  }
  mergedHand2 <- list(jerkiness = jer, numerator = num, denominator = denom)
  
  merged <- list(game = setsJer[[1]]$game, jerSeriesHead = mergedHead, jerSeriesHand1 = mergedHand1, jerSeriesHand2 = mergedHand2)
  
  return(merged)
}

MergeExpSets <- function(setsExp){
  expSer <- c()
  for(i in 1:length(setsExp)){
    element <- setsExp[[i]]
    expSer <- c(expSer, element$expSeries)
  }
  
  merged <- list(game = setsExp[[1]]$game, expSeries = expSer)
  return(merged)
}

MergeVSets <- function(setsV){
  vHead <- c()
  vHand1 <- c()
  vHand2 <- c()
  for(i in 1:length(setsV)){
    element <- setsV[[i]]
    vHead <- c(vHead, element$vSeriesHead)
    vHand1 <- c(vHand1, element$vSeriesHand1)
    vHand2 <- c(vHand2, element$vSeriesHand2)
  }
  
  merged <- list(game = setsV[[1]]$game, vSeriesHead = vHead, vSeriesHand1 = vHand1, vSeriesHand2 = vHand2)
  
  return(merged)
}

MergeAccSets <- function(setsA){
  aHeadx <- c()
  aHeady <- c()
  aHeadz <- c()
  
  aHand1x <- c()
  aHand1y <- c()
  aHand1z <- c()
  
  aHand2x <- c()
  aHand2y <- c()
  aHand2z <- c()
  
  for(i in 1:length(setsA)){
    element <- setsA[[i]]
    aHeadx <- c(aHeadx, element$aSeriesHeadx)
    aHeady <- c(aHeady, element$aSeriesHeady)
    aHeadz <- c(aHeadz, element$aSeriesHeadz)
    
    aHand1x <- c(aHand1x, element$aSeriesHand1x)
    aHand1y <- c(aHand1y, element$aSeriesHand1y)
    aHand1z <- c(aHand1z, element$aSeriesHand1z)
    
    aHand2x <- c(aHand2x, element$aSeriesHand2x)
    aHand2y <- c(aHand2y, element$aSeriesHand2y)
    aHand2z <- c(aHand2z, element$aSeriesHand2z)
  }
  
  merged <- list(game = setsA[[1]]$game,
                 aSeriesHeadx = aHeadx,
                 aSeriesHeady = aHeady,
                 aSeriesHeadz = aHeadz,
                 aSeriesHand1x = aHand1x,
                 aSeriesHand1y = aHand1y,
                 aSeriesHand1z = aHand1z,
                 aSeriesHand2x = aHand2x,
                 aSeriesHand2y = aHand2y,
                 aSeriesHand2z = aHand2z)
  
  return(merged)
}


# Functions to take a Giraud series and calculate metrics from it ----

MetricsFromGiraudImp <- function(merged){
  impHead <- merged$impSeriesHead
  impHand1 <- merged$impSeriesHand1
  impHand2 <- merged$impSeriesHand2
  
  impBody <- c(impHead, impHand1, impHand2)
  impHands <- c(impHand1, impHand2)
  
  meanImp <- mean(impBody, na.rm = TRUE)
  sdImp = sd(impBody, na.rm = TRUE)
  medianImp <- median(impBody, na.rm = TRUE)
  minImp = min(impBody, na.rm = TRUE)
  maxImp = max(impBody, na.rm = TRUE)
  sumBody <- list(mean = meanImp, sd = sdImp, median = medianImp, min = minImp, max = maxImp)
  
  meanImpHands <- mean(impHands, na.rm = TRUE)
  sdImpHands = sd(impHands, na.rm = TRUE)
  medianImpHands <- median(impHands, na.rm = TRUE)
  minImpHands = min(impHands, na.rm = TRUE)
  maxImpHands = max(impHands, na.rm = TRUE)
  sumHands <- list(mean = meanImpHands, sd = sdImpHands, median = medianImpHands, min = minImpHands, max = maxImpHands)
  
  meanImpHead <- mean(impHead, na.rm = TRUE)
  sdImpHead <- sd(impHead, na.rm = TRUE)
  medianImpHead <- median(impHead, na.rm = TRUE)
  minImpHead <- min(impHead, na.rm = TRUE)
  maxImpHead <- max(impHead, na.rm = TRUE)
  sumHead <- list(mean = meanImpHead, sd = sdImpHead, median = medianImpHead, min = minImpHead, max = maxImpHead)
  
  meanImpHand1 <- mean(impHand1, na.rm = TRUE)
  sdImpHand1 <- sd(impHand1, na.rm = TRUE)
  medianImpHand1 <- median(impHand1, na.rm = TRUE)
  minImpHand1 <- min(impHand1, na.rm = TRUE)
  maxImpHand1 <- max(impHand1, na.rm = TRUE)
  sumHand1 <- list(mean = meanImpHand1, sd = sdImpHand1, median = medianImpHand1, min = minImpHand1, max = maxImpHand1)
  
  meanImpHand2 <- mean(impHand2, na.rm = TRUE)
  sdImpHand2 <- sd(impHand2, na.rm = TRUE)
  medianImpHand2 <- median(impHand2, na.rm = TRUE)
  minImpHand2 <- min(impHand2, na.rm = TRUE)
  maxImpHand2 <- max(impHand2, na.rm = TRUE)
  sumHand2 <- list(mean = meanImpHand2, sd = sdImpHand2, median = medianImpHand2, min = minImpHand2, max = maxImpHand2)
  
  sumImp <- list(game = merged$game, body = sumBody, hands = sumHands, head = sumHead, hand1 = sumHand1, hand2 = sumHand2)
  
  return(sumImp)
}

MetricsFromGiraudEng <- function(merged){
  engHead <- merged$engSeriesHead
  engHand1 <- merged$engSeriesHand1
  engHand2 <- merged$engSeriesHand2
  
  engBody <- c(engHead, engHand1, engHand2)
  engHands <- c(engHand1, engHand2)
  
  meanEng <- mean(engBody, na.rm = TRUE)
  sdEng = sd(engBody, na.rm = TRUE)
  medianEng <- median(engBody, na.rm = TRUE)
  minEng = min(engBody, na.rm = TRUE)
  maxEng = max(engBody, na.rm = TRUE)
  sumBody <- list(mean = meanEng, sd = sdEng, median = medianEng, min = minEng, max = maxEng)
  
  meanEngHands <- mean(engHands, na.rm = TRUE)
  sdEngHands = sd(engHands, na.rm = TRUE)
  medianEngHands <- median(engHands, na.rm = TRUE)
  minEngHands = min(engHands, na.rm = TRUE)
  maxEngHands = max(engHands, na.rm = TRUE)
  sumHands <- list(mean = meanEngHands, sd = sdEngHands, median = medianEngHands, min = minEngHands, max = maxEngHands)
  
  meanEngHead <- mean(engHead, na.rm = TRUE)
  sdEngHead <- sd(engHead, na.rm = TRUE)
  medianEngHead <- median(engHead, na.rm = TRUE)
  minEngHead <- min(engHead, na.rm = TRUE)
  maxEngHead <- max(engHead, na.rm = TRUE)
  sumHead <- list(mean = meanEngHead, sd = sdEngHead, median = medianEngHead, min = minEngHead, max = maxEngHead)
  
  meanEngHand1 <- mean(engHand1, na.rm = TRUE)
  sdEngHand1 <- sd(engHand1, na.rm = TRUE)
  medianEngHand1 <- median(engHand1, na.rm = TRUE)
  minEngHand1 <- min(engHand1, na.rm = TRUE)
  maxEngHand1 <- max(engHand1, na.rm = TRUE)
  sumHand1 <- list(mean = meanEngHand1, sd = sdEngHand1, median = medianEngHand1, min = minEngHand1, max = maxEngHand1)
  
  meanEngHand2 <- mean(engHand2, na.rm = TRUE)
  sdEngHand2 <- sd(engHand2, na.rm = TRUE)
  medianEngHand2 <- median(engHand2, na.rm = TRUE)
  minEngHand2 <- min(engHand2, na.rm = TRUE)
  maxEngHand2 <- max(engHand2, na.rm = TRUE)
  sumHand2 <- list(mean = meanEngHand2, sd = sdEngHand2, median = medianEngHand2, min = minEngHand2, max = maxEngHand2)
  
  sumEng <- list(game = merged$game, body = sumBody, hands = sumHands, head = sumHead, hand1 = sumHand1, hand2 = sumHand2)
  
  return(sumEng)
}

MetricsFromGiraudDir <- function(merged){
  dirHead <- merged$dirSeriesHead
  dirHand1 <- merged$dirSeriesHand1
  dirHand2 <- merged$dirSeriesHand2
  
  dirBody <- c(dirHead, dirHand1, dirHand2)
  dirHands <- c(dirHand1, dirHand2)
  
  meanDir <- mean(dirBody, na.rm = TRUE)
  sdDir = sd(dirBody, na.rm = TRUE)
  medianDir <- median(dirBody, na.rm = TRUE)
  minDir = min(dirBody, na.rm = TRUE)
  maxDir = max(dirBody, na.rm = TRUE)
  sumBody <- list(mean = meanDir, sd = sdDir, median = medianDir, min = minDir, max = maxDir)
  
  meanDirHands <- mean(dirHands, na.rm = TRUE)
  sdDirHands = sd(dirHands, na.rm = TRUE)
  medianDirHands <- median(dirHands, na.rm = TRUE)
  minDirHands = min(dirHands, na.rm = TRUE)
  maxDirHands = max(dirHands, na.rm = TRUE)
  sumHands <- list(mean = meanDirHands, sd = sdDirHands, median = medianDirHands, min = minDirHands, max = maxDirHands)
  
  meanDirHead <- mean(dirHead, na.rm = TRUE)
  sdDirHead <- sd(dirHead, na.rm = TRUE)
  medianDirHead <- median(dirHead, na.rm = TRUE)
  minDirHead <- min(dirHead, na.rm = TRUE)
  maxDirHead <- max(dirHead, na.rm = TRUE)
  sumHead <- list(mean = meanDirHead, sd = sdDirHead, median = medianDirHead, min = minDirHead, max = maxDirHead)
  
  meanDirHand1 <- mean(dirHand1, na.rm = TRUE)
  sdDirHand1 <- sd(dirHand1, na.rm = TRUE)
  medianDirHand1 <- median(dirHand1, na.rm = TRUE)
  minDirHand1 <- min(dirHand1, na.rm = TRUE)
  maxDirHand1 <- max(dirHand1, na.rm = TRUE)
  sumHand1 <- list(mean = meanDirHand1, sd = sdDirHand1, median = medianDirHand1, min = minDirHand1, max = maxDirHand1)
  
  meanDirHand2 <- mean(dirHand2, na.rm = TRUE)
  sdDirHand2 <- sd(dirHand2, na.rm = TRUE)
  medianDirHand2 <- median(dirHand2, na.rm = TRUE)
  minDirHand2 <- min(dirHand2, na.rm = TRUE)
  maxDirHand2 <- max(dirHand2, na.rm = TRUE)
  sumHand2 <- list(mean = meanDirHand2, sd = sdDirHand2, median = medianDirHand2, min = minDirHand2, max = maxDirHand2)
  
  sumDir <- list(game = merged$game, body = sumBody, hands = sumHands, head = sumHead, hand1 = sumHand1, hand2 = sumHand2)
  
  return(sumDir)
}

MetricsFromGiraudJer <- function(merged){
  jerHead <- merged$jerSeriesHead$jerkiness
  jerHand1 <- merged$jerSeriesHand1$jerkiness
  jerHand2 <- merged$jerSeriesHand2$jerkiness
  
  jerBody <- c(jerHead, jerHand1, jerHand2)
  jerHands <- c(jerHand1, jerHand2)
  
  meanJer <- mean(jerBody, na.rm = TRUE)
  sdJer = sd(jerBody, na.rm = TRUE)
  medianJer <- median(jerBody, na.rm = TRUE)
  minJer = min(jerBody, na.rm = TRUE)
  maxJer = max(jerBody, na.rm = TRUE)
  sumBody <- list(mean = meanJer, sd = sdJer, median = medianJer, min = minJer, max = maxJer)
  
  meanJerHands <- mean(jerHands, na.rm = TRUE)
  sdJerHands = sd(jerHands, na.rm = TRUE)
  medianJerHands <- median(jerHands, na.rm = TRUE)
  minJerHands = min(jerHands, na.rm = TRUE)
  maxJerHands = max(jerHands, na.rm = TRUE)
  sumHands <- list(mean = meanJerHands, sd = sdJerHands, median = medianJerHands, min = minJerHands, max = maxJerHands)
  
  meanJerHead <- mean(jerHead, na.rm = TRUE)
  sdJerHead <- sd(jerHead, na.rm = TRUE)
  medianJerHead <- median(jerHead, na.rm = TRUE)
  minJerHead <- min(jerHead, na.rm = TRUE)
  maxJerHead <- max(jerHead, na.rm = TRUE)
  sumHead <- list(mean = meanJerHead, sd = sdJerHead, median = medianJerHead, min = minJerHead, max = maxJerHead)
  
  meanJerHand1 <- mean(jerHand1, na.rm = TRUE)
  sdJerHand1 <- sd(jerHand1, na.rm = TRUE)
  medianJerHand1 <- median(jerHand1, na.rm = TRUE)
  minJerHand1 <- min(jerHand1, na.rm = TRUE)
  maxJerHand1 <- max(jerHand1, na.rm = TRUE)
  sumHand1 <- list(mean = meanJerHand1, sd = sdJerHand1, median = medianJerHand1, min = minJerHand1, max = maxJerHand1)
  
  meanJerHand2 <- mean(jerHand2, na.rm = TRUE)
  sdJerHand2 <- sd(jerHand2, na.rm = TRUE)
  medianJerHand2 <- median(jerHand2, na.rm = TRUE)
  minJerHand2 <- min(jerHand2, na.rm = TRUE)
  maxJerHand2 <- max(jerHand2, na.rm = TRUE)
  sumHand2 <- list(mean = meanJerHand2, sd = sdJerHand2, median = medianJerHand2, min = minJerHand2, max = maxJerHand2)
  
  sumJer <- list(game = merged$game, body = sumBody, hands = sumHands, head = sumHead, hand1 = sumHand1, hand2 = sumHand2)
  
  return(sumJer)
}

MetricsFromGiraudExp <- function(merged){
  expSer <- merged$expSeries
  
  meanExp <- mean(expSer, na.rm = TRUE)
  sdExp = sd(expSer, na.rm = TRUE)
  medianExp <- median(expSer, na.rm = TRUE)
  minExp = min(expSer, na.rm = TRUE)
  maxExp = max(expSer, na.rm = TRUE)
  sumExp <- list(game = merged$game, mean = meanExp, sd = sdExp, median = medianExp, min = minExp, max = maxExp)
  
  return(sumExp)
}


MetricsFromA <- function(merged){
  aHeadx <- merged$aSeriesHeadx
  aHeady <- merged$aSeriesHeady
  aHeadz <- merged$aSeriesHeadz
  
  aHand1x <- merged$aSeriesHand1x
  aHand1y <- merged$aSeriesHand1y
  aHand1z <- merged$aSeriesHand1z
  
  aHand2x <- merged$aSeriesHand2x
  aHand2y <- merged$aSeriesHand2y
  aHand2z <- merged$aSeriesHand2z
  
  aBodyx <- c(aHeadx, aHand1x, aHand2x)
  aBodyy <- c(aHeady, aHand1y, aHand2y)
  aBodyz <- c(aHeadz, aHand1z, aHand2z)
  
  aHandsx <- c(aHand1x, aHand2x)
  aHandsy <- c(aHand1y, aHand2y)
  aHandsz <- c(aHand1z, aHand2z)
  

  meanAx <- mean(aBodyx, na.rm = TRUE)
  sdAx = sd(aBodyx, na.rm = TRUE)
  medianAx <- median(aBodyx, na.rm = TRUE)
  minAx = min(aBodyx, na.rm = TRUE)
  maxAx = max(aBodyx, na.rm = TRUE)
  sumBodyx <- list(mean = meanAx, sd = sdAx, median = medianAx, min = minAx, max = maxAx)
  
  meanAy <- mean(aBodyy, na.rm = TRUE)
  sdAy = sd(aBodyy, na.rm = TRUE)
  medianAy <- median(aBodyy, na.rm = TRUE)
  minAy = min(aBodyy, na.rm = TRUE)
  maxAy = max(aBodyy, na.rm = TRUE)
  sumBodyy <- list(mean = meanAy, sd = sdAy, median = medianAy, min = minAy, max = maxAy)
  
  meanAz <- mean(aBodyz, na.rm = TRUE)
  sdAz = sd(aBodyz, na.rm = TRUE)
  medianAz <- median(aBodyz, na.rm = TRUE)
  minAz = min(aBodyz, na.rm = TRUE)
  maxAz = max(aBodyz, na.rm = TRUE)
  sumBodyz <- list(mean = meanAz, sd = sdAz, median = medianAz, min = minAz, max = maxAz)
  
  
  
  
  meanAHandsx <- mean(aHandsx, na.rm = TRUE)
  sdAHandsx = sd(aHandsx, na.rm = TRUE)
  medianAHandsx <- median(aHandsx, na.rm = TRUE)
  minAHandsx = min(aHandsx, na.rm = TRUE)
  maxAHandsx = max(aHandsx, na.rm = TRUE)
  sumHandsx <- list(mean = meanAHandsx, sd = sdAHandsx, median = medianAHandsx, min = minAHandsx, max = maxAHandsx)
  
  meanAHandsy <- mean(aHandsy, na.rm = TRUE)
  sdAHandsy = sd(aHandsy, na.rm = TRUE)
  medianAHandsy <- median(aHandsy, na.rm = TRUE)
  minAHandsy = min(aHandsy, na.rm = TRUE)
  maxAHandsy = max(aHandsy, na.rm = TRUE)
  sumHandsy <- list(mean = meanAHandsy, sd = sdAHandsy, median = medianAHandsy, min = minAHandsy, max = maxAHandsy)
  
  meanAHandsz <- mean(aHandsz, na.rm = TRUE)
  sdAHandsz = sd(aHandsz, na.rm = TRUE)
  medianAHandsz <- median(aHandsz, na.rm = TRUE)
  minAHandsz = min(aHandsz, na.rm = TRUE)
  maxAHandsz = max(aHandsz, na.rm = TRUE)
  sumHandsz <- list(mean = meanAHandsz, sd = sdAHandsz, median = medianAHandsz, min = minAHandsz, max = maxAHandsz)
  
  
  
  
  meanAHeadx <- mean(aHeadx, na.rm = TRUE)
  sdAHeadx <- sd(aHeadx, na.rm = TRUE)
  medianAHeadx <- median(aHeadx, na.rm = TRUE)
  minAHeadx <- min(aHeadx, na.rm = TRUE)
  maxAHeadx <- max(aHeadx, na.rm = TRUE)
  sumHeadx <- list(mean = meanAHeadx, sd = sdAHeadx, median = medianAHeadx, min = minAHeadx, max = maxAHeadx)
  
  meanAHeady <- mean(aHeady, na.rm = TRUE)
  sdAHeady <- sd(aHeady, na.rm = TRUE)
  medianAHeady <- median(aHeady, na.rm = TRUE)
  minAHeady <- min(aHeady, na.rm = TRUE)
  maxAHeady <- max(aHeady, na.rm = TRUE)
  sumHeady <- list(mean = meanAHeady, sd = sdAHeady, median = medianAHeady, min = minAHeady, max = maxAHeady)
  
  meanAHeadz <- mean(aHeadz, na.rm = TRUE)
  sdAHeadz <- sd(aHeadz, na.rm = TRUE)
  medianAHeadz <- median(aHeadz, na.rm = TRUE)
  minAHeadz <- min(aHeadz, na.rm = TRUE)
  maxAHeadz <- max(aHeadz, na.rm = TRUE)
  sumHeadz <- list(mean = meanAHeadz, sd = sdAHeadz, median = medianAHeadz, min = minAHeadz, max = maxAHeadz)
  
  
  
  meanAHand1x <- mean(aHand1x, na.rm = TRUE)
  sdAHand1x <- sd(aHand1x, na.rm = TRUE)
  medianAHand1x <- median(aHand1x, na.rm = TRUE)
  minAHand1x <- min(aHand1x, na.rm = TRUE)
  maxAHand1x <- max(aHand1x, na.rm = TRUE)
  sumHand1x <- list(mean = meanAHand1x, sd = sdAHand1x, median = medianAHand1x, min = minAHand1x, max = maxAHand1x)
  
  meanAHand1y <- mean(aHand1y, na.rm = TRUE)
  sdAHand1y <- sd(aHand1y, na.rm = TRUE)
  medianAHand1y <- median(aHand1y, na.rm = TRUE)
  minAHand1y <- min(aHand1y, na.rm = TRUE)
  maxAHand1y <- max(aHand1y, na.rm = TRUE)
  sumHand1y <- list(mean = meanAHand1y, sd = sdAHand1y, median = medianAHand1y, min = minAHand1y, max = maxAHand1y)
  
  meanAHand1z <- mean(aHand1z, na.rm = TRUE)
  sdAHand1z <- sd(aHand1z, na.rm = TRUE)
  medianAHand1z <- median(aHand1z, na.rm = TRUE)
  minAHand1z <- min(aHand1z, na.rm = TRUE)
  maxAHand1z <- max(aHand1z, na.rm = TRUE)
  sumHand1z <- list(mean = meanAHand1z, sd = sdAHand1z, median = medianAHand1z, min = minAHand1z, max = maxAHand1z)
  
  
  
  
  meanAHand2x <- mean(aHand2x, na.rm = TRUE)
  sdAHand2x <- sd(aHand2x, na.rm = TRUE)
  medianAHand2x <- median(aHand2x, na.rm = TRUE)
  minAHand2x <- min(aHand2x, na.rm = TRUE)
  maxAHand2x <- max(aHand2x, na.rm = TRUE)
  sumHand2x <- list(mean = meanAHand2x, sd = sdAHand2x, median = medianAHand2x, min = minAHand2x, max = maxAHand2x)
  
  meanAHand2y <- mean(aHand2y, na.rm = TRUE)
  sdAHand2y <- sd(aHand2y, na.rm = TRUE)
  medianAHand2y <- median(aHand2y, na.rm = TRUE)
  minAHand2y <- min(aHand2y, na.rm = TRUE)
  maxAHand2y <- max(aHand2y, na.rm = TRUE)
  sumHand2y <- list(mean = meanAHand2y, sd = sdAHand2y, median = medianAHand2y, min = minAHand2y, max = maxAHand2y)
  
  meanAHand2z <- mean(aHand2z, na.rm = TRUE)
  sdAHand2z <- sd(aHand2z, na.rm = TRUE)
  medianAHand2z <- median(aHand2z, na.rm = TRUE)
  minAHand2z <- min(aHand2z, na.rm = TRUE)
  maxAHand2z <- max(aHand2z, na.rm = TRUE)
  sumHand2z <- list(mean = meanAHand2z, sd = sdAHand2z, median = medianAHand2z, min = minAHand2z, max = maxAHand2z)
  
  
  sumA <- list(game = merged$game,
               bodyX = sumBodyx,
               bodyY = sumBodyy,
               bodyZ = sumBodyz,
               handsX = sumHandsx,
               handsY = sumHandsy,
               handsZ = sumHandsz,
               headX = sumHeadx,
               headY = sumHeady,
               headZ = sumHeadz,
               hand1X = sumHand1x,
               hand1Y = sumHand1y,
               hand1Z = sumHand1z,
               hand2X = sumHand2x,
               hand2Y = sumHand2y,
               hand2Z = sumHand2z)
  
  return(sumA)
}

MetricsFromV <- function(merged){
  vHead <- merged$vSeriesHead
  vHand1 <- merged$vSeriesHand1
  vHand2 <- merged$vSeriesHand2
  
  vBody <- c(vHead, vHand1, vHand2)
  vHands <- c(vHand1, vHand2)
  
  meanV <- mean(vBody, na.rm = TRUE)
  sdV = sd(vBody, na.rm = TRUE)
  medianV <- median(vBody, na.rm = TRUE)
  minV = min(vBody, na.rm = TRUE)
  maxV = max(vBody, na.rm = TRUE)
  sumBody <- list(mean = meanV, sd = sdV, median = medianV, min = minV, max = maxV)
  
  meanVHands <- mean(vHands, na.rm = TRUE)
  sdVHands = sd(vHands, na.rm = TRUE)
  medianVHands <- median(vHands, na.rm = TRUE)
  minVHands = min(vHands, na.rm = TRUE)
  maxVHands = max(vHands, na.rm = TRUE)
  sumHands <- list(mean = meanVHands, sd = sdVHands, median = medianVHands, min = minVHands, max = maxVHands)
  
  meanVHead <- mean(vHead, na.rm = TRUE)
  sdVHead <- sd(vHead, na.rm = TRUE)
  medianVHead <- median(vHead, na.rm = TRUE)
  minVHead <- min(vHead, na.rm = TRUE)
  maxVHead <- max(vHead, na.rm = TRUE)
  sumHead <- list(mean = meanVHead, sd = sdVHead, median = medianVHead, min = minVHead, max = maxVHead)
  
  meanVHand1 <- mean(vHand1, na.rm = TRUE)
  sdVHand1 <- sd(vHand1, na.rm = TRUE)
  medianVHand1 <- median(vHand1, na.rm = TRUE)
  minVHand1 <- min(vHand1, na.rm = TRUE)
  maxVHand1 <- max(vHand1, na.rm = TRUE)
  sumHand1 <- list(mean = meanVHand1, sd = sdVHand1, median = medianVHand1, min = minVHand1, max = maxVHand1)
  
  meanVHand2 <- mean(vHand2, na.rm = TRUE)
  sdVHand2 <- sd(vHand2, na.rm = TRUE)
  medianVHand2 <- median(vHand2, na.rm = TRUE)
  minVHand2 <- min(vHand2, na.rm = TRUE)
  maxVHand2 <- max(vHand2, na.rm = TRUE)
  sumHand2 <- list(mean = meanVHand2, sd = sdVHand2, median = medianVHand2, min = minVHand2, max = maxVHand2)
  
  sumV <- list(game = merged$game, body = sumBody, hands = sumHands, head = sumHead, hand1 = sumHand1, hand2 = sumHand2)
  
  return(sumV)
}


# Functions to visualise Giraud series given the metrics ----

CreateRadarChart <- function(gamesMetrics){
  # Spider Chart Custom (mixed metrics) - Hands
  par(mfrow = c(1,2))
  
  gamesData <- c()
  gamesNames <- c()
  for(i in 1:length(gamesMetrics)){
    metrics <- gamesMetrics[[i]]
    gameData <- c(metrics$Imp$hands$mean, metrics$Eng$hands$mean, metrics$Dir$hands$mean, metrics$Jer$hands$mean, metrics$Exp$mean, metrics$Exp$sd)
    gamesData <- c(gamesData, gameData)
    gamesNames <- c(gamesNames, metrics$Imp$game)
  }
  
  data <- as.data.frame(matrix(gamesData, ncol = 6, byrow = TRUE))
  colnames(data) <- c("Impulsiveness", "Energy", "Directness", "Jerkiness", "Expansiveness - Mean", "Expansiveness - Deviation")
  rownames(data) <- gamesNames
  
  mins <- c(0, 0, 0, 0, 0)
  #maxs <- c(max(data[1]), max(data[2]), 1.0, max(data[4]), 0.2076351, 0.2076351)
  maxs <- c(max(data[1]), max(data[2]), max(data[3]), max(data[4]), max(data[5]), max(data[6]))
  
  data <- rbind(maxs, mins, data)
  
  
  radarchart(data, axistype = 0,
             pcol = 1:length(gamesMetrics), plwd = 4, plty = 1,
             cglcol = "grey", cglty = 1, axislabcol = "grey", cglwd = 0.9,
             vlcex = 0.8, title = "Hands")
  
  legend(x = 0.6, y = 1.3, legend = rownames(data[-c(1, 2),]), bty = "n", pch = 20, col = 1:length(gamesMetrics), text.col = "grey", cex = 1.2, pt.cex = 3)
  
  
  
  # Spider Chart Custom (mixed metrics) - Head
  gamesData <- c()
  gamesNames <- c()
  for(i in 1:length(gamesMetrics)){
    metrics <- gamesMetrics[[i]]
    gameData <- c(metrics$Imp$head$mean, metrics$Eng$head$mean, metrics$Jer$head$mean)
    gamesData <- c(gamesData, gameData)
    gamesNames <- c(gamesNames, metrics$Imp$game)
  }
  
  data <- as.data.frame(matrix(gamesData, ncol = 3, byrow = TRUE))
  colnames(data) <- c("Impulsiveness", "Energy", "Jerkiness")
  rownames(data) <- gamesNames
  
  mins <- c(0, 0, 0)
  maxs <- c(max(data[1]), max(data[2]), max(data[3]))
  
  data <- rbind(maxs, mins, data)
  
  # Colour vector
  colours_border = c(rgb(1.0, 0.0, 0.0, 0.9), rgb(0.1333333, 0.545098, 0.1333333, 0.9), rgb(1.0, 0.4941176, 0.2784314, 0.9))
  colours_in = c(rgb(1.0, 0.0, 0.0, 0.4), rgb(0.1333333, 0.545098, 0.1333333, 0.4), rgb(1.0, 0.4941176, 0.2784314, 0.4))
  
  
  radarchart(data, axistype = 0,
             pcol = 1:length(gamesMetrics), plwd = 4, plty = 1,
             cglcol = "grey", cglty = 1, axislabcol = "grey", cglwd = 0.9,
             vlcex = 0.8, title="Head")
  
  legend(x = 0.6, y = 1.3, legend = rownames(data[-c(1, 2),]), bty = "n", pch = 20, col = 1:length(gamesMetrics), text.col = "grey", cex = 1.2, pt.cex = 3)
  
}


# Giraud Functions ----

CalculateVelocitySeries <- function(dataSet) {
  
  # get velocity along direction 
  velocities <- numeric(nrow(dataSet))
  
  
  for (row in 1:nrow(dataSet)) {
    
    vx <- dataSet$v.x[row]
    vy <- dataSet$v.y[row]
    vz <- dataSet$v.z[row]
    
    v = sqrt(sum(vx ^ 2, vy ^ 2, vz ^ 2))
    
    velocities[row] <- v
  }
  return(velocities)
}

CalculateImpulsivenessSeries <- function(dataSet) {
  
  
  # get velocity along direction 
  V <- CalculateVelocitySeries(dataSet)
  
  netAcceleration <- numeric(nrow(dataSet))
  
  for (row in 2:nrow(dataSet)) {
    
    # calculate the net acceleration
    v1 <- V[row]
    v0 <- V[row - 1]
    
    t1 <- dataSet$timestamp[row] * 1e-9 # converted to seconds
    t0 <- dataSet$timestamp[row - 1] * 1e-9 # converted to seconds
    
    a = abs(v1 - v0) / (t1 - t0)
    
    netAcceleration[row] <- a
  }
  
  return(netAcceleration)
}


CalculateEnergySeries <- function(dataSet, mmember) {
  
  # get velocity along direction 
  V <- CalculateVelocitySeries(dataSet)
  
  # Energy
  energy <- numeric(nrow(dataSet))
  
  for (row in 1:nrow(dataSet)) {
    
    v = V[row]
    
    e <- 0.5 * mmember * (v ^ 2)
    
    energy[row] <- e
  }
  
  return(energy)
}

CalculateDirectnessSeries <- function(dataSet, trajectoryHead) {
  
  directness <- numeric(nrow(dataSet))
  
  for (row in 1:nrow(dataSet)) {
    
    vx <- dataSet$v.x[row]
    vy <- dataSet$v.y[row]
    vz <- dataSet$v.z[row]
    v <- c(vx, vy, vz)
    
    
    vhx <- trajectoryHead$v.x[row]
    vhy <- trajectoryHead$v.y[row]
    vhz <- trajectoryHead$v.z[row]
    vh <- c(vhx, vhy, vhz)
    
    d <- dotProduct(v,vh)
    
    d <- abs(d)
    
    directness[row] <- d
  }
  
  return(directness)
}

CalculateAxialAcceleration <- function(Velocities, Timestamps) {
  A <- numeric(length(Velocities))
  
  for (i in 2:length(Velocities)) {
    
    v1 <- Velocities[i]
    
    v0 <- Velocities[i - 1]
    
    t1 <- Timestamps[i] * 1e-9 # converted to seconds
    t0 <- Timestamps[i - 1] * 1e-9 # converted to seconds
    
    a <- (v1 - v0) / (t1 - t0)
    
    A[i] <- a
  }
  
  return(A)
}

CalculateJerkinessSeries <- function(dataSet) {
  
  result <- list(jerkiness = numeric(nrow(dataSet)),
                 numerator = numeric(nrow(dataSet)),
                 denominator = numeric(nrow(dataSet)))
  
  # apply constant to velocities used for jerkiness calculations
  Vx <- dataSet$v.x# + 1
  Vy <- dataSet$v.y# + 1
  Vz <- dataSet$v.z# + 1
  
  
  
  # calculate the individual accelerations along axes first
  Ax <- CalculateAxialAcceleration(Vx, dataSet$timestamp)
  Ay <- CalculateAxialAcceleration(Vy, dataSet$timestamp)
  Az <- CalculateAxialAcceleration(Vz, dataSet$timestamp)
  
  # calculate Jerkiness
  
  
  
  for (row in 1:nrow(dataSet)) {
    vx <- Vx[row]
    vy <- Vy[row]
    vz <- Vz[row]
    
    ax <- Ax[row]
    ay <- Ay[row]
    az <- Az[row]
    
    numerator <- sqrt((vx * ay - vy * ax) ^ 2 + (vz * ax - vx * az) ^ 2 + (vy * az - vz * ay) ^ 2)
    denominator <- (vx ^ 2 + vy ^ 2 + vz ^ 2) ^ (3 / 2)
    
    result$numerator[row] <- numerator
    result$denominator[row] <- denominator
    result$jerkiness[row] <- numerator / denominator
    
    
  }    
  
  return(result)
}


CalculateExpansivenessSeries <- function(dataSetHead, dataSetHand1, dataSetHand2) {
  
  expansiveness <- numeric(nrow(dataSetHead))
  
  dataSetCoG <- dataSetHead
  for (row in 1:nrow(dataSetCoG)) {
    dataSetCoG$position.y[row] <- dataSetCoG$position.y[row] - 0.2
  }
  
  
  for (row in 1:nrow(dataSetHead)) {
    
    xHead <- dataSetHead$position.x[row]
    yHead <- dataSetHead$position.y[row]
    zHead <- dataSetHead$position.z[row]
    
    xHand1 <- dataSetHand1$position.x[row]
    yHand1 <- dataSetHand1$position.y[row]
    zHand1 <- dataSetHand1$position.z[row]
    
    xHand2 <- dataSetHand2$position.x[row]
    yHand2 <- dataSetHand2$position.y[row]
    zHand2 <- dataSetHand2$position.z[row]
    
    xCoG <- dataSetCoG$position.x[row]
    yCoG <- dataSetCoG$position.y[row]
    zCoG <- dataSetCoG$position.z[row]
    
    DIx <- 1 / 3 * sum(sqrt((xHead - xCoG) ^ 2), sqrt((xHand1 - xCoG) ^ 2), sqrt((xHand2 - xCoG) ^ 2))
    DIy <- 1 / 3 * sum(sqrt((yHead - yCoG) ^ 2), sqrt((yHand1 - yCoG) ^ 2), sqrt((yHand2 - yCoG) ^ 2))
    DIz <- 1 / 3 * sum(sqrt((zHead - zCoG) ^ 2), sqrt((zHand1 - zCoG) ^ 2), sqrt((zHand2 - zCoG) ^ 2))
    
    Ex = (4 / 3) * pi * DIx * DIy * DIz
    
    expansiveness[row] = Ex
  }
  
  return(expansiveness)
  
}
