
get_liver_livertobw_score <- function (studyid, 
                                       path_db, 
                                       fake_study = FALSE, 
                                       master_CompileData = NULL,
                                       bwzscore_BW = NULL, 
                                       score_in_list_format = FALSE){
  
  # om <- sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM OM WHERE STUDYID = (:1)",
  #                             queryParams = j)
  path <- path_db
  con <- DBI::dbConnect(DBI::dbDriver('SQLite'), dbname = path)
  
  con_db <- function(domain){
    domain <- toupper(domain)
    stat <- paste0('SELECT * FROM ', domain, " WHERE STUDYID = (:x)")
    domain <- DBI::dbGetQuery(con,
                              statement = stat,
                              params=list(x=studyid))
    domain
  }
  
  # Check if bwzscore_BW is NULL
  if (is.null(bwzscore_BW)) {
    # Call the master_CompileData function to generate the data frame
    bwzscore_BW <-  get_bw_score (studyid, 
                               path_db, 
                               fake_study = FALSE, 
                               master_CompileData = NULL,
                               score_in_list_format = TRUE)  
  } 
  
  
  
  #Pull relevant domain data for each domain
  om <- con_db('om')
  
  # Initialize data frames to store the OrganWeights_Liver data
  OrganWeights_Liver <- data.frame(USUBJID = character(0), OMSPEC = character(0), OMSTRESN = numeric(0), OMTEST = character(0))
  
  # Extract data for the current STUDYID
  StudyData_current_liver <- om
  
  # Pull index of the LIVER data
  Studyidx_liver <- which(stringr::str_detect(StudyData_current_liver$OMSPEC, "LIVER"))
  
  # Pull relevant OM Data for LIVER
  OMD_liver <- StudyData_current_liver[Studyidx_liver, c("USUBJID", "OMSPEC", "OMSTRESN", "OMTEST")]
  
  # Append to the OrganWeights_Liver  data frame
  OrganWeights_Liver <- rbind(OrganWeights_Liver, OMD_liver)
  
  # Filter the OrganWeights_Liver data frame
  OrganWeights_Liver_Weight <- OrganWeights_Liver %>%
    dplyr::filter(OMTEST == "Weight")
  
  # Filter the OrganWeights_Liver_Weight data frame and select specific columns ("USUBJID", "OMSTRESN")
  OrganWeights_Liver_Weight_Selected_Col <- OrganWeights_Liver_Weight %>%
    dplyr::filter(OMTEST == "Weight") %>%
    dplyr::select(USUBJID, OMSTRESN)
  
  #<><><><>... Remove TK animals and Recovery animals from "OrganWeights_Liver_Weight_Selected_Col"..<><><>....
  #<><><><><><><><> master_compiledataaa is free of TK animals and Recovery animals<><><><><><><><><><><><><><>
  # Filter the data frame for removing recovery and TK animals.....................................
  
  #' @get-master-compile-data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #browser()
  # Check if master_CompileData is NULL
  if (is.null(master_CompileData)) {
    fake_study = fake_study
    # Call the master_CompileData function to generate the data frame
    master_CompileData <- get_compile_data(studyid, path_db,fake_study = fake_study)  
  } 
  
  OrganWeights_Liver_filtered <- OrganWeights_Liver_Weight_Selected_Col %>%
    dplyr::filter(USUBJID %in% master_CompileData$USUBJID)
  
  # Perform a left join to match USUBJID and get ARMCD
  OrganWeights_Liver_with_ARMCD <- OrganWeights_Liver_filtered %>%
    dplyr::left_join(master_CompileData %>% 
                       dplyr::select(STUDYID, USUBJID, ARMCD), by = "USUBJID")
  
  
  # Add "BodyWeight" data to the "OrganWeights_Liver_with_ARMCD" data frame 
  OrganWeights_Liver_to_BWeight <- OrganWeights_Liver_with_ARMCD %>% 
    dplyr::left_join(bwzscore_BW %>% 
    dplyr::select(USUBJID, finalbodyweight), by = "USUBJID") %>%
    dplyr::mutate(liverToBW = OMSTRESN / finalbodyweight)
  
  # "liver_organ to BodyWeight" zscore calcualtion.............................................................
  # Create the "LiverZSCORE" column :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
  liver_zscore_df <- OrganWeights_Liver_to_BWeight %>%
    dplyr::group_by(STUDYID) %>%
    # Replace Inf and -Inf with NA in liverToBW
    dplyr::mutate(liverToBW = replace(liverToBW, is.infinite(liverToBW), NA)) %>%
    # Calculate mean and standard deviation for "vehicle" ARMCD
    dplyr::mutate(
      mean_vehicle_liverToBW = mean(liverToBW[ARMCD == "vehicle"], na.rm = TRUE),
      sd_vehicle_liverToBW = sd(liverToBW[ARMCD == "vehicle"], na.rm = TRUE)
    ) %>%
    dplyr::ungroup() %>%
    
    # Calculate z-score
    dplyr::mutate(
      liverToBW_zscore = (liverToBW - mean_vehicle_liverToBW) / sd_vehicle_liverToBW
    ) %>%
    # Optionally remove the mean_vehicle and sd_vehicle columns
    dplyr::select(-mean_vehicle_liverToBW, -sd_vehicle_liverToBW) %>%  
    dplyr::select(STUDYID, USUBJID,liverToBW_zscore, ARMCD) %>%
    # Convert z-score to its absolute value
    dplyr::mutate(liverToBW_zscore = abs(liverToBW_zscore))
    
  # Filter and select specific columns
  HD_liver_zscore <- liver_zscore_df %>%
    dplyr::filter(ARMCD == "HD") %>%
    dplyr::select(STUDYID, USUBJID, liverToBW_zscore, ARMCD)
  
  # Create final_liverToBW_df for the current STUDYID by averaging..................................
  final_liverToBW_df <- HD_liver_zscore %>%
    dplyr::group_by(STUDYID) %>%
    dplyr::mutate(liverToBW_zscore = replace(liverToBW_zscore, 
                                      is.infinite(liverToBW_zscore), NA)) %>%
    dplyr::summarize(avg_liverToBW_zscore = mean(liverToBW_zscore, na.rm = TRUE))%>%
    dplyr::mutate(avg_liverToBW_zscore = abs(avg_liverToBW_zscore))  %>% 
    dplyr::select(STUDYID, avg_liverToBW_zscore) %>% 
    dplyr::mutate(avg_liverToBW_zscore = ifelse(avg_liverToBW_zscore >= 3, 3,
                                   ifelse(avg_liverToBW_zscore >= 2, 2,
                                          ifelse(avg_liverToBW_zscore >= 1, 1, 0))))
  
  # create a empty list for storing the individual final_liverToBW_df 
  #empty_lb_score_list <- list()
  
  # # Add the liverToBW_zscore to "FOUR_Liver_Score" data frame.......................................... 
  # # Create "liverToBW_df" for FOUR_Liver_Score
  # liverToBW_df <- final_liverToBW_df %>% rename(liverToBW = avg_liverToBW_zscore)
  # 
  # # add liverToBW_df to master_liverToBW
  # master_liverToBW <- bind_rows(master_liverToBW, liverToBW_df)
  # 
  # # Extract the liverToBW value for the current STUDYID from liverToBW_df
  # calculated_liverToBW_value <- liverToBW_df$liverToBW[liverToBW_df$STUDYID == unique(ts$STUDYID)]
  # 
  # # Update the liverToBW value in FOUR_Liver_Score for the current STUDYID
  # FOUR_Liver_Score$liverToBW[FOUR_Liver_Score$STUDYID == unique(ts$STUDYID)] <- calculated_liverToBW_value
  # 
  # # Score the liverToBW values in the FOUR_Liver_Score data frame and fill "scored_liverToBW" column 
  # FOUR_Liver_Score$scored_liverToBW <- ifelse(FOUR_Liver_Score$liverToBW >= 3, 3,
  #                                             ifelse(FOUR_Liver_Score$liverToBW >= 2, 2,
  #                                                    ifelse(FOUR_Liver_Score$liverToBW >= 1, 1, 0)))
  # 
  # 
#return(final_liverToBW_df) 
  # Return based on score_in_list_format
  if (score_in_list_format) {
    return(HD_liver_zscore)
  } else {
    return(final_liverToBW_df)
  } 
  
}