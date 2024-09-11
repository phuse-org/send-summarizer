#' @title get treatment group
#' @param ind Mandatory, character or vector \cr
#'   Studies number
#' @return list
#'
#' @examples
#' \dontrun{
#' get_treatment_group(studies = '12345678', database_path = dbtoken)
#' }
#' @export
#' 
#' 


# source("get_liver_compiledata.R")
# 
# #Database Load
# dbtoken <- sendigR::initEnvironment(dbType = 'sqlite',
#                                     dbPath = paste0(homePath,"/DataCentral.db"),
#                                     dbCreate = FALSE)
# 
# j = "1017-3581"

# # Call "get_liver_compiledata" function to get the cleaned_compiledata 
# output_get_liver_compiledata <- get_liver_compiledata(j, dbtoken) #return(list( tK_animals_df, cleaned_compiledata)
# 
# # GET the "cleaned_compiledata" -data frame- from the output of the function "get_liver_compiledata(j, dbtoken)" 
# cleaned_compiledata <- output_get_liver_compiledata[["cleaned_compiledata"]] 
 

get_liver_dose_ranking <- function(studyid, tx, cleaned_compiledata){

      # tx <-sendigR::genericQuery(dbtoken, queryString = "SELECT * FROM TX WHERE STUDYID = (:1)",
      #                        queryParams = j)

    ##' @<><><><><><><><>.."vehicle"..and."HD..animals"..selection..
    ##' "for"cleaned_compiledata".....<><><><>..:::::::::::::::::<><><><>
    
    # tx table  filter by TXPARMCD
    cleaned_compiledata_filtered_tx <- tx %>% 
      dplyr::filter(TXPARMCD == "TRTDOS")
    
    
    #::::::::::::::::::::::::::::: Assign the dose level for 
    #"cleaned_compiledata_filtered_tx"  ::::::::::::::::::::::::::::::
    
    # Step 1:  Create a unified separator pattern
    cleaned_compiledata_separator_pattern <- ";|\\||-|/|:|,"  
    
    # Split and expand the TXVAL column, and add row_state
    cleaned_compiledata_expanded_tx_row_state_added <- cleaned_compiledata_filtered_tx %>%
      dplyr::mutate(
        # Flag rows that will be split
        is_split = stringr::str_detect(TXVAL, cleaned_compiledata_separator_pattern), 
        TXVAL = strsplit(as.character(TXVAL), cleaned_compiledata_separator_pattern)
      ) %>%
      tidyr::unnest(TXVAL) %>%
      dplyr::mutate(
        TXVAL = as.numeric(TXVAL),
        # If the row was split, mark as new_row, else old_row
        row_state = ifelse(is_split, "new_row", "old_row") 
      ) %>%
      dplyr::select(-is_split) # Remove the is_split column
    
    #<><><><><>:::::::::::::...Adding dose_ranking...::::::::<><><><><><><><><>
    
    # Initialize an empty data frame for dose_ranking
    dose_ranking <- data.frame()
    
    dose_ranking_prob_study <- data.frame()
    
    if (TRUE) {
      study_data <- cleaned_compiledata_expanded_tx_row_state_added
      
      # Check if all TXVAL values are NA for the STUDYID
      if (all(is.na(study_data$TXVAL))) {
        dose_ranking_prob_study <- rbind(dose_ranking_prob_study, study_data)
      } 
      # Check if all SETCD values are the same for the STUDYID
      else if (dplyr::n_distinct(study_data$SETCD) == 1) { 
        dose_ranking_prob_study <- rbind(dose_ranking_prob_study, study_data)
      } else {
        # Process for lowest TXVAL
        lowest_txval <- min(study_data$TXVAL, na.rm = TRUE)
        lowest_data <- study_data %>%
          dplyr::filter(TXVAL == lowest_txval) %>%
          dplyr::arrange(SETCD)
        
        if (nrow(lowest_data) == 1) {
          dose_ranking <- rbind(dose_ranking, lowest_data)
          
        } else {
          # Select the first old_row if available, else the first new_row
          selected_lowest <- dplyr::filter(lowest_data, 
                                           row_state == "old_row") %>% 
            dplyr::slice(1)
          if (nrow(selected_lowest) > 0) {
            dose_ranking <- rbind(dose_ranking, selected_lowest)
          } else {
            selected_lowest <- dplyr::filter(lowest_data, 
                                             row_state == "new_row") %>% 
              dplyr::slice(1)
            dose_ranking <- rbind(dose_ranking, selected_lowest)
          }
        }
        
        # Process for highest TXVAL
        highest_txval <- max(study_data$TXVAL, na.rm = TRUE)
        highest_data <- study_data %>%
          dplyr::filter(TXVAL == highest_txval) %>%
          dplyr::arrange(SETCD)
        
        if (nrow(highest_data) == 1) {
          dose_ranking <- rbind(dose_ranking, highest_data)
        }else if (nrow(highest_data) > 1) {
          selected_highest <- dplyr::filter(highest_data, 
                                            row_state == "old_row") %>% 
            dplyr::slice(1)
          if (nrow(selected_highest) > 0) {
            dose_ranking <- rbind(dose_ranking, selected_highest)
          } else {
            # If no old_row is found, select the first new_row
            selected_highest <- dplyr::filter(highest_data, 
                                              row_state == "new_row") %>% 
              dplyr::slice(1)
            if (nrow(selected_highest) > 0) {
              dose_ranking <- rbind(dose_ranking, selected_highest)
              
            }
          }
        }
      }
    }
    
    #<><><><>.....ADD DOSE_RANKING column in "selected_rows" data frame....<><>
    DOSE_RANKED_selected_rows <- dose_ranking %>%
      dplyr::group_by(STUDYID) %>%
      dplyr::mutate(
        MinTXVAL = min(TXVAL),
        MaxTXVAL = max(TXVAL),
        DOSE_RANKING = dplyr::case_when(
          TXVAL == MinTXVAL & TXVAL == MaxTXVAL ~ "Both",
          TXVAL == MinTXVAL ~ "vehicle",
          TXVAL == MaxTXVAL ~ "HD",
          TRUE ~ "Intermediate"
        )
      ) %>%
      dplyr::select(-MinTXVAL, -MaxTXVAL) %>%
      dplyr::ungroup()
    
    #Merging "DOSE_RANKED_selected_rows" and "cleaned_compiledata" data framed.
    DOSE_RANKED_plus_cleaned_compiledata <- dplyr::inner_join(cleaned_compiledata, 
                                                       DOSE_RANKED_selected_rows, 
                                                       by = c("STUDYID", "SETCD"))
    
    # rename the Data frame 
    master_compiledata1 <- DOSE_RANKED_plus_cleaned_compiledata [,c("STUDYID",
                                                                    "USUBJID",
                                                                    "Species", 
                                                                    "SEX", 
                                                                    "DOSE_RANKING",
                                                                    "SETCD")]
    
    # Rename the "DOSE_RANKING" column to ARMCD 
    # Rename "DOSE_RANKING" to "ARMCD" in master_compiledata
    master_compiledata <- master_compiledata1 %>% 
      dplyr::rename(ARMCD = DOSE_RANKING)
    
    # Return the final master compiledata 
    return ( master_compiledata =  master_compiledata )
    
}



