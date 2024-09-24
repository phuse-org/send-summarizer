#rm(list =ls())
#' @title get LB score for a given studyid
#' @param studyid Mandatory, character \cr
#'   Studyid number
#' @param database_path Mandatory, character \cr
#'   path of database
#' @return score
#'
#' @examples
#' \dontrun{
#' get_liver_lb_score(studyid='1234123', database_path = dbtoken)
#' }
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

get_random_forest_model <- function(liver_om_lb_mi_tox_score_list, not_liver_om_lb_mi_tox_score_list){
  
  ###-----------------------------------------------------------------
  `%ni%` <- Negate('%in%')
  Impute <- T
  ErrorMethod <- 'Prune' # Choose: "Flip" or "Prune" or "None"
  Undersample <- T
  reps <- 0
  #rfDataRDS <- 'rfData_1000000_0.05_1_Prune_Round_8.rds'
  threshold <- 0.05
  holdback <- 1
  testReps <- 5
  testHoldBack <- 0.2
  Round <- T
  generateBarPlot <- T
  nTopImportance <- 20
  indeterminateUpper <- .75
  indeterminateLower <- .25
  Type = 1
  hyperparameter_tuning <- F
  removeEndpoints <- c('Infiltrate', 'UNREMARKABLE', 'THIKENING', 'POSITIVE')
  
  ###-------------------------
  # get lb score for Liver 
  Liver_master_LB_list <- Liver_get_liver_om_lb_mi_tox_score_list[['master_lb_score_six']]
  Liver_master_LB_list$indst_TO <- "Liver"
  
  # get LivertoBW score for Liver
  Liver_master_liverToBW <- Liver_get_liver_om_lb_mi_tox_score_list[['master_liverToBW']]
  Liver_master_liverToBW $indst_TO   <- "Liver"
  
  # For mi score for Liver
  Liver_master_MI_list <- Liver_get_liver_om_lb_mi_tox_score_list[['master_mi_df']]
  Liver_master_MI_list$indst_TO <- "Liver"
  
  # Reorder the columns to make `indst_TO` the first column
  Liver_master_MI_list <- Liver_master_MI_list[, c("indst_TO", setdiff(names(Liver_master_MI_list), "indst_TO"))]
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # get lb score for not_Liver
  not_Liver_master_LB_list <- not_Liver_get_liver_om_lb_mi_tox_score_list[['master_lb_score_six']]
  not_Liver_master_LB_list$indst_TO  <- "not_Liver"
  
  # get LivertoBW score for not_Liver
  not_Liver_master_liverToBW <- not_Liver_get_liver_om_lb_mi_tox_score_list[['master_liverToBW']]
  not_Liver_master_liverToBW$indst_TO  <- "not_Liver"
  
  # For mi score for not_Liver
  not_Liver_master_MI_list <- not_Liver_get_liver_om_lb_mi_tox_score_list[['master_mi_df']]
  not_Liver_master_MI_list$indst_TO <- "not_Liver"
  
  # Reorder the columns to make `indst_TO` the first column
  not_Liver_master_MI_list <- not_Liver_master_MI_list[, c("indst_TO", setdiff(names(not_Liver_master_MI_list), "indst_TO"))]
  
  
  #--------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  LB <- rbind(Liver_master_LB_list, not_Liver_master_LB_list)
  
  # Move the last column to the first position
  LB <- LB[, c(ncol(LB), 1:(ncol(LB)-1))]
  
  
  OM <- rbind(Liver_master_liverToBW, not_Liver_master_liverToBW)
  
  # Move the last column to the first position
  OM <- OM[, c(ncol(OM), 1:(ncol(OM)-1))]
  
  ###------------------------
  MIl <- Liver_master_MI_list
  # Replace spaces and commas in column names with dots
  colnames(MIl) <- gsub(' ', '.', colnames(MIl))
  colnames(MIl) <- gsub(',', '.', colnames(MIl))
  colnames(MIl) <- gsub('/', '.', colnames(MIl))
  
  
  MIn <- not_Liver_master_MI_list 
  # Replace spaces and commas in column names with dots
  colnames(MIn) <- gsub(' ', '.', colnames(MIn))
  colnames(MIn) <- gsub(',', '.', colnames(MIn))
  colnames(MIn) <- gsub('/', '.', colnames(MIn))
  
  
  
  MIinter <- intersect(colnames(MIl), colnames(MIn))
  MI <- rbind(MIl[, MIinter], MIn[, MIinter])
  
  MIextraL <- setdiff(colnames(MIl), colnames(MIn))
  MIextraN <- setdiff(colnames(MIn), colnames(MIl))
  for (j in MIextraL) {
    MI[, j] <- NA
    MI[seq(nrow(MIl)), j] <- MIl[, j]
  }
  for (j in MIextraN) {
    MI[, j] <- NA
    MI[seq((nrow(MIl) + 1), nrow(MI)), j] <- MIn[, j]
  }
  
  MI[is.na(MI)] <- 0
  
  #Identify Columns with Periods
  findings2replaceIndex <- grep('.', colnames(MI), fixed = T)
  
  # Store Column Names with Periods
  f2replace <- colnames(MI)[findings2replaceIndex]
  
  #Identify Unique Column Names without Periods
  fn2replace <- unique(toupper(colnames(MI)[-findings2replaceIndex]))
  
  #Remove Specific Columns from Processing
  removeIndex <- which(fn2replace %in% c('INDST_TO',
                                         'STUDYID',
                                         'UNREMARKABLE',
                                         'THIKENING',
                                         'POSITIVE'))
  fn2replace <- fn2replace[-removeIndex]
  
  #Harmonize Synonym Columns
  for (finding in fn2replace) {
    synonyms <- grep(finding, f2replace, ignore.case = T, value = T)
    for (synonym in synonyms) {
      index <- which(MI[[synonym]] > 0)
      for (i in index) {
        if (MI[[synonym]][i] > MI[[finding]][i]) {
          MI[[finding]][i] <- MI[[synonym]][i]
        }
      }
    }
  }
  
  #Remove Synonym Columns
  MI <- MI[,-findings2replaceIndex]
  
  #return(list(LB,OM,MI))
  
  ###------------------------
  commonStudies <- Reduce(intersect, list(LB$STUDYID, OM$STUDYID, MI$STUDYID))
  extraDomains <- c('OM', 'MI')
  count <- 0
  for (study in commonStudies) {
    count <- count + 1
    newRow <- LB[which(LB$STUDYID == study),]
    for (domain in extraDomains) {
      domainData <- get(domain)
      studyIndex <- which(domainData$STUDYID == study)
      for (j in colnames(domainData)[3:ncol(domainData)]) {
        newRow[[j]] <- domainData[studyIndex, j]
      }
    }
    if (count == 1) {
      Data <- newRow
    } else {
      Data <- rbind(Data, newRow)
    }
  }
  
  
  removeIndex <- which(colnames(Data) %in% removeEndpoints)
  Data <- Data[, -removeIndex]
  
  if (Round == T) {
    zscoreIndex <- c(grep('avg_', colnames(Data)), grep('liver', colnames(Data)))
    for (i in zscoreIndex) {
      Data[, i] <- floor(Data[, i])
      maxIndex <- which(Data[, i] > 5)
      Data[maxIndex, i] <- 5
    }
    histoIndex <- which(substr(colnames(Data), 1, 1) %in% toupper(letters))
    histoIndex <- histoIndex[-1]
    for (i in histoIndex) {
      Data[, i] <- ceiling(Data[, i])
    }
  }
  
  columnSums <- sort(colSums(Data[,3:ncol(Data)], na.rm = T), decreasing = T)
  Data[,3:ncol(Data)] <- Data[, names(columnSums)]
  colnames(Data)[3:ncol(Data)] <- names(columnSums)
  
  #write.csv(Data, 'mergedData.csv', row.names = F)
  ##---------------
  if (generateBarPlot == T) {
    Finding <- NULL
    LIVER <- NULL
    Value <- NULL
    for (finding in colnames(Data)[3:ncol(Data)]) {
      Finding <- c(Finding, finding)
      LIVER <- c(LIVER, 'Y')
      Value <- c(Value, mean(Data[which(Data$indst_TO == "Liver"), finding], na.rm = T))
      
      Finding <- c(Finding, finding)
      LIVER <- c(LIVER, 'N')
      Value <- c(Value, mean(Data[which(Data$indst_TO != "Liver"), finding], na.rm = T))
      
    }
    plotData <- as.data.frame(cbind(Finding, LIVER, Value))
    plotData$LIVER <- factor(plotData$LIVER)
    plotData$Finding <- factor(plotData$Finding)
    plotData$Value = as.numeric(plotData$Value)
    # plotData$Value = log(as.numeric(plotData$Value), base = 10)
    
    ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~check and double check 
    p <- ggplot2::ggplot(plotData, aes(x = Finding, y = Value, fill = LIVER)) + 
      ggplot2::geom_bar(stat="identity", position = 'dodge') +
      ggplot2::theme(text = element_text(size = 20),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggplot2::ylab('Average Score')
    print(p)
  }
  
  #}
  
  #'@********************************@ random forest model @********************
  
#########--------------####### Random Forest Modeling ########################
###-----------------------rfData preparation------------------------------------
  rfData <- Data[, -2]
  rfData[which(rfData$indst_TO == 'Liver'), 1] <- 1
  rfData[which(rfData$indst_TO == 'not_Liver'), 1] <- 0
  # rfData[,1] <- as.numeric(rfData[,1])
  rfData[,1] <- factor(rfData[,1], levels = c(1, 0))
  
  # removeIndex <- which(colnames(rfData) %in% c('INFILTRATE'))
  # rfData <- rfData[, -removeIndex]
  
#######-----------------------missing values imputation---------------------------
   ##missing values imputation
  if (Impute == T) {
    rfData <- randomForest::rfImpute(indst_TO ~ ., rfData)
    
    if (Round == T) {
      zscoreIndex <- c(grep('avg_', colnames(rfData)), grep('liver', colnames(rfData)))
      for (i in zscoreIndex) {
        rfData[, i] <- floor(rfData[, i])
        maxIndex <- which(rfData[, i] > 5)
        rfData[maxIndex, i] <- 5
      }
      histoIndex <- which(substr(colnames(rfData), 1, 1) %in% toupper(letters))
      histoIndex <- histoIndex[-1]
      for (i in histoIndex) {
        rfData[, i] <- ceiling(rfData[, i])
      }
    }
  }
  
##------------------------------------------------------------------------
  

  #' Model Training Loop
  count <- 0
  if (reps > 0) {
    for (rep in seq(reps)) {
      print(paste0(rep/reps*100, '% Complete...'))
      
#####------------------------------------------------------------------------------      
########--------prepare training and testing data sets for model building-------------
      if (holdback == 1) {
        #If holdback == 1, a single instance is sampled for the test set,
        # and the rest are used for training:::  Randomly assigns each row to 
        #the training set (1) or test set (2) based on the holdback proportion.
        ind <- sample(seq(nrow(rfData)), 1) # selection of single rows
        train <- rfData[-ind,]
        test <- rfData[ind,]
        testIndex <- ind
      } else {
        #Otherwise, the data is split into training and testing sets based 
        # on the holdback proportion.
        ## categorizing all rows into two groups based on probabilities.
        ind <- sample(2, nrow(rfData), replace = T, prob = c((1- holdback), holdback))
        train <- rfData[ind==1,]
        test <- rfData[ind==2,]
        testIndex <- which(ind == 2)
      }
      
      # Under sampling for balancing the positive and negative instances
      if (Undersample == T) {
        #If Undersample == TRUE, the training set is balanced by 
        #undersampling the majority class
        posIndex <- which(train[,1] == 1)
        nPos <- length(posIndex)
        trainIndex <- c(posIndex, sample(which(train[,1] == 0), nPos, replace = F))
        train <- train[trainIndex,]
      }
      
###------------------perform hyperparameter tuning and get model -----------
    if(hyperparameter_tuning == T) {
      
       # "mtry" in Caret Package Grid Search
       #sets up the cross-validation method.
       control <- trainControl(method="repeatedcv", number=10, repeats=3)
       metric <- "Accuracy"
       mtry <- sqrt(ncol(train))
       tunegrid <- expand.grid(.mtry=mtry)
       
       #Grid and Random Search (caret package)
       rf_default <- train(indst_TO~., data=train, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
       rf_random <- train(indst_TO~., data=train, method="rf", metric=metric, tuneLength=15, trControl=control)
       print(rf_default)
       print(rf_random)
       
       #mtry in Random Forest Parameter Tuning
       #tuning on the mtry parameter
       mtry <- tuneRF(rfData[,-1], rfData[,1], ntreeTry=500,
                     stepFactor=1.5,improve=0.01, trace=F, plot=F)
       best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
       print(mtry)
       print(best.m)
    }
      
   
      best.m <- 4
      
      rf <- randomForest::randomForest(indst_TO ~ ., data=train, mytry = best.m, importance = F, ntree = 500, proximity = F)
      # rf <- tuneRF(train[,-1], train[,1], doBest = T, stepFactor = 1.5)
###------------Error-Handling-(Flip-or-Prune)from "model predictions"-----------
###------------Errored rows adjustment and final data set preparation-----------
      #' @Error-Handling-(Flip-or-Prune):
      if ((ErrorMethod == 'Flip')|(ErrorMethod == 'Prune')) {
        # Predict probabilities (using random_forest_package)
        p <- stats::predict(rf, test, type = 'prob')[,1]
        
        # Find indices to flip or prune
        flipLiverIndex <- testIndex[which((rfData[testIndex, 'indst_TO'] == 1)&(as.numeric(p) < threshold))]
        flipnot_LiverIndex <- testIndex[which((rfData[testIndex, 'indst_TO'] == 0)&(as.numeric(p) > (1 - threshold)))]
        
        #If there are indices to flip or prune, the data set is modified 
        # accordingly and saved to a file
        if (length(flipLiverIndex) > 0) {
          count <- count + 1
          if (ErrorMethod == 'Flip') {
            # Flip 'Liver' to 'not_Liver'
            rfData[flipLiverIndex, 1] <- 0
          } else {
            # Prune (remove) 'Liver' instances
            rfData <- rfData[-flipLiverIndex,]
          }
          if (Round == T) {
            # Save the modified dataset
            saveRDS(rfData, paste0('rfData_', as.integer(reps), '_', threshold, '_', holdback, '_', ErrorMethod, '_Round_', count, '.rds'))
          } else {
            # Save the modified dataset
            saveRDS(rfData, paste0('rfData_', as.integer(reps), '_', threshold, '_', holdback, '_', ErrorMethod, '_', count, '.rds'))
          }
        }
        if (length(flipnot_LiverIndex) > 0) {
          count <- count + 1
          if (ErrorMethod == 'Flip') {
            # Flip 'not_Liver' to 'Liver'
            rfData[flipnot_LiverIndex, 1] <- 1
          } else {
            # Prune (remove) 'not_Liver' instances
            rfData <- rfData[-flipnot_LiverIndex,]
          }
          if (Round == T) {
            # Save the modified dataset
            saveRDS(rfData, paste0('rfData_', as.integer(reps), '_', threshold, '_', holdback, '_', ErrorMethod, '_Round_', count, '.rds'))
          } else {
            # Save the modified dataset
            saveRDS(rfData, paste0('rfData_', as.integer(reps), '_', threshold, '_', holdback, '_', ErrorMethod, '_', count, '.rds'))
        
          }
        }
      }
    }
  }

###-----------------------Testing-the-Random-Forest-Model-----------------------
###---------------------after flipping and pruning, even-- --------------------
###--------------------even flipping and pruning didn't occur-------------------
###-------------------and confusion matrix is prepared-------------------------


  #rfData <- readRDS(rfDataRDS) # (if data were flipped or prunned)
  rfData <- rfData
  Sensitivity <- NULL
  Specificity <- NULL
  PPV <- NULL
  NPV <- NULL
  Prevalence <- NULL
  Accuracy <- NULL
  nRemoved <- NULL
  
  
  #-----create and prepare "`rfTestData data` frame" for storing predictions----
  rfTestData <- rfData
  #replaces the existing column names with simple numeric identifiers
  colnames(rfTestData) <- seq(ncol(rfTestData))
  #emptying the data frame.
  for (j in seq(ncol(rfTestData))) {
    rfTestData[,j] <- NA
  }
  
  #prepares rfTestData to maintain a consistent structure with the necessary
  #columns for storing predictions in subsequent iterations of the loop
  rfTestData <- rfTestData[,1:2]
  
  #remove 'gini' from the previous iteration
  if (exists('gini')) {rm(gini)}
  
  # model building and testing---------------------------
  for (i in seq(testReps)) {
    if (i == 1) {
      sampleIndicies <- seq(nrow(rfData))
    }
    if (i < testReps) {
      ind <- sample(seq(nrow(rfData)), floor((nrow(rfData)/testReps)-1), replace = F)
      sampleIndicies <- sampleIndicies[which(sampleIndicies %ni% ind)]
    } else {
      ind <- sampleIndicies
    }
    
    trainIndex <- which(seq(nrow(rfData)) %ni% ind)
    testIndex <- ind
    
    # ind <- sample(2, nrow(rfData), replace = T, prob = c((1- testHoldBack), testHoldBack))
    train <- rfData[trainIndex,]
  
    train_data_two <- train
    print(dim(train_data_two))
    
    test <- rfData[testIndex,]
    
    rfAll <- randomForest::randomForest(indst_TO ~ ., data=rfData, mytry = best.m, 
                          importance = F, ntree = 500, proximity = T)
    # print(rfAll)
    
    if (Undersample == T) {
      posIndex <- which(train[,1] == 1)
      nPos <- length(posIndex)
      trainIndex <- c(posIndex, sample(which(train[,1] == 0), nPos, replace = F))
      train <- train[trainIndex,]
      test <- rbind(train[-trainIndex,], test)
    }
    
    train_data_two <- train
    print(dim(train_data_two))
    

    #model building with current train data
    rf <- randomForest::randomForest(indst_TO ~ ., data=train, mytry = best.m, 
                       importance = T, ntree = 500, proximity = T)
    
    print(rf)
    
    #predictions with current model  with current test data
    
    #' @___________________this_line_has_problems_______
    p2r <- stats::predict(rf, test, type = 'prob')[,1] 
    
    #Store these predictions in a structured dataframe
    rfTestData[names(p2r), i] <- as.numeric(p2r)
    
    #Identifying Indeterminate Predictions (Tracking Indeterminate Predictions)
    #Keeps track of the proportion of indeterminate predictions in each iteration
    #Proportion Tracking
    indeterminateIndex <- which((p2r < indeterminateUpper)&(p2r > indeterminateLower))
    
    #Calculating the Proportion of Indeterminate Predictions
    #Sets the indeterminate predictions to NA, effectively marking them
    #as missing or invalid.
    nRemoved <- c(nRemoved, length(indeterminateIndex)/length(p2r))
    
    #Handling Indeterminate Predictions
    p2r[indeterminateIndex] <- NA
    
    #Rounding the Predictions:
    p2r <- round(p2r)
    
    
    #"confusionMatrix" function from the caret package 
    Results <- confusionMatrix(factor(p2r, levels = c(1, 0)), factor(test$indst_TO, levels = c(1, 0)))
    Sensitivity <- c(Sensitivity, Results$byClass[['Sensitivity']])
    Specificity <- c(Specificity, Results$byClass[['Specificity']])
    PPV <- c(PPV, Results$byClass[['Pos Pred Value']])
    NPV <- c(NPV, Results$byClass[['Neg Pred Value']])
    Prevalence <- c(Prevalence, Results$byClass[['Prevalence']])
    Accuracy <- c(Accuracy, Results$byClass[['Balanced Accuracy']])
    
    giniTmp <-  randomForest::importance(rf, type = Type)
    if (exists('gini')) {
      gini <- cbind(gini, giniTmp)
    } else {
      gini <- giniTmp
    }
  }
  
  #' @Performance-and-Importance-Analysis~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #performance summary for the model and  dotchart of the top important
  #variables based on their mean decrease in accuracy or Gini index
  PerformanceMatrix <- cbind(Sensitivity,
                             Specificity,
                             PPV, NPV,
                             Prevalence,
                             Accuracy,
                             nRemoved)
  PerformanceSummary <- colMeans(PerformanceMatrix, na.rm = T)
  print(PerformanceSummary)
  print(sort(rowMeans(gini), decreasing = T))
  
  imp <- as.matrix(rowMeans(gini)[1:nTopImportance])
  if (Type == 1) {
    colnames(imp) <- 'MeanDecreaseAccuracy'
  } else {
    colnames(imp) <- 'MeanDecreaseGini'
  }
  ord <- order(imp[,1])
  dotchart(imp[ord, 1], xlab = colnames(imp)[1], ylab = "", 
           main = paste0('Top ', nrow(imp), ' - Variable Importance'))#, xlim = c(xmin, max(imp[, i])))
  # varImpPlot(rf,
  #            sort = T,
  #            n.var = 20,
  #            main = "Top 20 - Variable Importance")
###-----------------------@ROC-Curve-and-AUC------------------------------------
  #' @ROC-Curve-and-AUC~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  pred1= stats::predict(rfAll,type = "prob")
  perf = ROCR::prediction(pred1[,1], levels(rfData[,1])[rfData[,1]])
  # 1. Area under curve
  auc = performance(perf, "auc")
  AUC <- auc@y.values[[1]]
  print(AUC)
  # 2. True Positive and Negative Rate
  pred3 = performance(perf, "tpr","fpr")
  # 3. Plot the ROC curve
  plot(pred3,main=paste0("ROC Curve for Random Forest (AUC = ", round(AUC, digits = 3), ")"),col=2,lwd=2)
  abline(a=0,b=1,lwd=2,lty=2,col="gray")
  
  #}
  
  
  # MDSplot(rf, train$indst_TO, k = 3, palette = rep(1, 3), pch = as.numeric(levels(train$indst_TO)))
  
###-----------------------Visualization-and-Saving-Results----------------------
  #' @Visualization-and-Saving-Results~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  reprtree:::plot.reprtree(reprtree::ReprTree(rfAll, train, metric='d2'))
  
  # saveRDS(rfData, paste0('rfData_', as.integer(reps), '_', threshold, '_', holdback, '_', ErrorMethod, '.rds'))
  
  histoData <- as.data.frame(cbind(rowMeans(rfTestData, na.rm = T), rfData[,1]))
  histoData[which(histoData[,2] == 1), 2] <- 'Y'
  histoData[which(histoData[,2] == 2), 2] <- 'N'
  colnames(histoData) <- c('Probability', 'LIVER')
  
  H <- p <- histoData %>%
    ggplot( aes(x=Probability, fill=LIVER)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#69b3a2", "#404080")) +
    # theme_ipsum() +
    labs(fill = "LIVER", x = "Model Prediction P(LIVER)", y = "Count")
  print(H)
  return(list(rf = rf, train_data_two = train_data_two))
}




#' @param rfData_1000000_0.05_1_Prune_Round_8.rds Mandatory, character \cr
#' @param -Liver_master_LB_list.csv- Mandatory, character \cr
#' @param -not_Liver_master_LB_list.csv- Mandatory, character \cr
#' @param -Liver_master_liverToBW.csv- Mandatory, character \cr
#' @param -not_Liver_master_liverToBW.csv- Mandatory, character \cr
#' @param -Liver_master_MI_list.csv- Mandatory, character \cr
#' @param -not_Liver_master_MI_list.csv- Mandatory, character \cr
#' @param -sa_liscr_mod_liver_result_df6_575.csv- Mandatory, character \cr
#' @param -sa_nLiver_scr_mod_liver_result_df7_575.csv- Mandatory, character \cr

