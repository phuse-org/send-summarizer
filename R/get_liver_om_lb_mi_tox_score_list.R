## This code is for getting liver toxicity score from SEND Studyid

## -----------------------------------------------------------------------------
##   Date                     Programmer
## ----------   --------------------------------------------------------------
##   May-07-2024    Md MD Aminul Islam Prodhan (mdaminulislam.prodhan@fda.hhs.gov)

get_liver_om_lb_mi_tox_score_list <- function (selected_studies, dbtoken ) {

# Source the get_data scripts
source("get_liver_compiledata.R", local = TRUE)
source("get_liver_dose_ranking.R", local = TRUE)
source("get_liver_bw_score.R", local = TRUE)
source("get_liver_livertobw_score.R", local = TRUE)
source("get_liver_lb_score.R", local = TRUE)
source("get_liver_mi_score.R", local = TRUE)


# master liverToBW_df 
master_liverToBW <-  data.frame(STUDYID = NULL, avg_liverToBW_zscore = NULL)
  
#selected_studies <- selected_studies

#master_liverToBW <- list()

#master_lbxx_list <- list()

#master_mixx_list <- list()

master_mi_df <- data.frame()

# Master LB list 
master_lb_score_six <- data.frame(STUDYID = NULL, avg_alb_zscore = NULL, avg_ast_zscore = NULL, avg_alp_zscore = NULL,
                             avg_alt_zscore = NULL, avg_bili_zscore = NULL, avg_ggt_zscore = NULL)
# master_MI_list 
#master_MI_list <- list()

# Create FOUR SCORE DATA FRAME for "LiverToBodyweight" , "LB" & "MI" Score 
FOUR_Liver_Score <-  data.frame(STUDYID = NA, liverToBW = NA, LB_score = NA, MI_score = NA, scored_liverToBW = NA, scored_LBScore = NA)


# Initialize an empty data frame to store the names of studies with errors
Error_studies <- list()

# Initialize the master error data frame to have the details of the errors
#master_error_df <- data.frame(STUDYID = character() , Block = character(), ErrorMessage = character(), Time = POSIXct(), stringsAsFactors = FALSE)
master_error_df <- data.frame(STUDYID = character() , 
                              Block = character(), 
                              ErrorMessage = character(), 
                              #Time = POSIXct(), 
                              stringsAsFactors = FALSE)
# Print local variables for debugging
#print(ls())

for (j in selected_studies){

  print(j)

  # Initialize a flag variable at the start of each iteration
  first_block_success <- TRUE

  # First Block with its own tryCatch for compiledata
  tryCatch({

    # Call "get_liver_compiledata" function to get the cleaned_compiledata
    output_get_liver_compiledata <- get_liver_compiledata(j, dbtoken) #return(list( tK_animals_df, cleaned_compiledata)

    #print(output_get_liver_compiledata)
    # GET the "cleaned_compiledata" -data frame- from the output of the function "get_liver_compiledata(j, dbtoken)"
    cleaned_compiledata <- output_get_liver_compiledata[["cleaned_compiledata"]]


    # GET the "tK_animals_df" -data frame- from the output of the "get_liver_compiledata(j, dbtoken)" function
    tK_animals_df <- output_get_liver_compiledata[["tK_animals_df"]]
    
    # GET the "ts" -data frame- from the output of the "get_liver_compiledata(j, dbtoken)" function
    ts <- output_get_liver_compiledata[["ts"]]
    
    # GET the "tx" -data frame- from the output of the "get_liver_compiledata(j, dbtoken)" function
    tx <- output_get_liver_compiledata[["tx"]]
    
    # GET the "bw" data frame 
    bw <- output_get_liver_compiledata[["bw"]]
    
    # GET the "om" data frame
    om <- output_get_liver_compiledata[["om"]]
    
    # GET the "lb" data frame
    lb <- output_get_liver_compiledata[["lb"]]
    
    # GET the "mi" data frame
    mi <- output_get_liver_compiledata[["mi"]]
    
    # GET the "id" data frame
    #id <- output_get_liver_compiledata[["id"]] 
    
    # GET the  "master_compiledata" -data frame- from the output of the --
    #"get_liver_dose_ranking" function which  return (master_compiledata)
    master_compiledata <- get_liver_dose_ranking(j, tx, cleaned_compiledata)
    
    # Create a copy of master_compiledata for the diagnostic purpose
    master_compiledata_copy <- master_compiledata

    }, error = function(e) {
    # Handling errors
    message("Error in BodyWeight Data Compilation calculation: ", e$message)

    # Log the error
    error_block1 <- data.frame(STUDYID = j,
                               Block = "compiledata",
                               ErrorMessage = e$message,
                               #Time = Sys.time(),
                               stringsAsFactors = FALSE)
    master_error_df <<- rbind(master_error_df, error_block1)
    #master_error_df <<- rbind(master_error_df, error_block1)

    # Set the flag to FALSE to indicate the first block failed
    first_block_success <<- FALSE

  })

  # Check the flag to decide whether to proceed to the next iteration of the loop
  if (!first_block_success) {

    # Append STUDYID  to the error_studies list
    Error_studies <<- c(Error_studies, j)

    next
  }

  #-----------------end of master_compiledata calculation----------------------

  #----------------------score_accumulation_df--------------------------------
  #This block for "Adding a new row for the current STUDYID in FOUR_Liver_Score"
  tryCatch({

    # Initialize the "FOUR_Liver_Score"
    # [[# Add a new row for the current STUDYID in FOUR_Liver_Score]]
    
    
    # ts <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM TS 
    #                             WHERE STUDYID = (:1)",
    #                             queryParams = j)
    

    new_row_in_four_liver_scr <- data.frame(STUDYID = unique(ts$STUDYID),
                                            liverToBW = NA,
                                            LB_score = NA,
                                            MI_score = NA,
                                            scored_liverToBW = NA,
                                            scored_LBScore = NA)
    
    FOUR_Liver_Score <- rbind(FOUR_Liver_Score, new_row_in_four_liver_scr)
    
  }, error = function(e) {
    # Handling errors of the secondary operation
    message("Error in FOUR_Liver_Score: ", e$message)

    # Log the error
    error_block_flscrdf <- data.frame(STUDYID = unique(ts$STUDYID),
                                      Block = "FOUR_Liver_Score",
                                      ErrorMessage = e$message,
                                      #Time = Sys.time(),
                                      stringsAsFactors = FALSE)
    master_error_df <<- rbind(master_error_df, error_block_flscrdf)

  })
  #-----------------END--of ---score_accumulation_df----------------------------

  #------------------Calculation_of--BodyWeight_zScore--------------------------

  tryCatch({

    BWzScore_vehicle_plus_highdose <-  get_liver_bw_score(j, bw, ts,  
                                                          master_compiledata, 
                                                               tK_animals_df )
    
    bwzscore_BW <- BWzScore_vehicle_plus_highdose[["bwzscore_BW"]]

  }, error = function(e) {
    # Handling errors of the secondary operation
    message("Error in BodyWeight_zScore calculation: ", e$message)

    # Log the error
    error_block2 <- data.frame(STUDYID = unique(ts$STUDYID),
                               Block = "BWZscore",
                               ErrorMessage = e$message,
                               #Time = Sys.time(),
                               stringsAsFactors = FALSE)
    master_error_df <<- rbind(master_error_df, error_block2)

  })
#---------------------------"OM_DATA"-(Liver_Organ to Body Weight zScore)-------
  tryCatch({

    final_liverToBW_df <- get_liver_livertobw_score(j, om, 
                                                    master_compiledata, 
                                                          bwzscore_BW)
    # Dynamically name the list element using the j (study_id)
    #master_liverToBW[[j]] <- final_liverToBW_df
    #master_liverToBW <- append(master_liverToBW, final_liverToBW_df)
    
    master_liverToBW <- rbind(master_liverToBW, final_liverToBW_df)

  }, error = function(e) {
    # Handling errors of the secondary operation
    message("Error in Liver_Organ to Body Weight zScore: ", e$message)

    # Log the error
    error_block3 <- data.frame(STUDYID = unique(ts$STUDYID),
                               Block = "LiverToBW",
                               ErrorMessage = e$message,
                               #Time = Sys.time(), 
                               stringsAsFactors = FALSE)
    master_error_df <<- rbind(master_error_df, error_block3)
  })

  #<><><><><><><><><><><><><><><><><><>"""LB"""" zscoring <><><><><><><><><><><>
  tryCatch({
    
    master_lb_scores <- get_liver_lb_score(j, ts, lb, 
                                                master_compiledata)
    
    #master_lbxx_list[[j]] <- lb_score_final_list
    master_lb_score_six <- rbind(master_lb_score_six ,master_lb_scores)
    

  }, error = function(e) {
    # Handling errors of the secondary operation
    message("Error in LB zscoring: ", e$message)

    # Log the error
    error_block4 <- data.frame(STUDYID = unique(ts$STUDYID), Block = "LB",
                               ErrorMessage = e$message, 
                               #Time = Sys.time(),
                               stringsAsFactors = FALSE)
    master_error_df <<- rbind(master_error_df, error_block4)
  })

  #<><><><><><><><><><><><><><><><><><>"""MI"""" zscoring <><><><><><><><><><><>
  tryCatch({
    
    #mi_score_final_list <- get_liver_mi_score(j, dbtoken, ts, master_compiledata)
    
    #master_mixx_list[[j]] <- mi_score_final_list
    
    mi_score_final_list_df <- get_liver_mi_score(j, ts, mi, 
                                              master_compiledata)
    
    master_mi_df <- dplyr::bind_rows(master_mi_df, mi_score_final_list_df)
    #master_mi_df <- rbind(master_mi_df, mi_score_final_list_df)
    

  }, error = function(e) {
    # Handling errors of the secondary operation

    # Log the error
    error_block5 <- data.frame(STUDYID = unique(ts$STUDYID),Block = "MI",
                               ErrorMessage = e$message, 
                               #Time = Sys.time(),
                               stringsAsFactors = FALSE)
    master_error_df <<- rbind(master_error_df, error_block5)

    # Create MI_final_score with NA values
    #return(data.frame(STUDYID = NA, avg_MI_score = NA))
  })

}
# write.csv(master_liverToBW, "dc_7396_master_livertobw_scores_df.csv", row.names = FALSE)
# write.csv(master_lb_score_six, "dc_7396_master_lb_scores_df.csv", row.names = FALSE)
# write.csv(master_mi_df , "dc_7396_master_mi_scores_df.csv", row.names = FALSE)

# Debugging: list variables in the function environment
#print(ls(envir = environment()))

return(list(master_liverToBW = master_liverToBW,
            master_lb_score_six = master_lb_score_six,
            master_mi_df  = master_mi_df 
            ))

}


#' @~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#get_om_lb_mi_toxicity_score_list <- function (studyid, dbtoken , source) {
# # remove objects from the environment
# rm(list = ls())
# 
# #libraries
# library(matrixStats)
# library(dplyr)
# library(sendigR)
# library(tidyverse)
# library(tidyr)
# library(this.path)
# library(reshape2)
# library(stringr)
# library(purrr)
# 
# # #Set File Path
# homePath <- dirname(this.path())
# setwd(homePath)
# 
# # #Database Load
# dbtoken <- sendigR::initEnvironment(dbType = 'sqlite',
#                                     dbPath = "/opt/rstudio/users/MdAminulIslam.Prodhan/DataCentral.db",
#                                     dbCreate = FALSE)
# 
# selected_studies <- c("1017-3581", "1470536", "P19-025-RD")



#selected_studies <- "1017-3581"
# selected_studies <- c("1017-3581", "1470536", "P19-025-RD")
#selected_studies <- c("1470536") # problematic bwzscore
#selected_studies <- c("2206-027", "1017-3581")
