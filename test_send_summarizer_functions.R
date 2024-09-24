
rm(list = ls())

# Set working directory to the package root (if necessary)
setwd("C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/send-summarizer")

# Load all functions from the package
devtools::load_all(".")


# Example: Testing the send_cross_study_app function
# Replace 'path_to_database.db' with the actual path to your database
#sendSummarizer::send_cross_study_app('path_to_database.db')

# Add more function tests here
#xpt_dir

xpt_compile_data <- new_compileData(studyid='1017-3518', 
                                    path_db= "C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/real_xpt_dir/IND051292_1017-3581",
                                    xpt_files = TRUE,
                                    fake_study=FALSE)

















bw_score <- get_bw_score(studyid='10663', path_db='C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/liver_1.db', fake_study=TRUE)

bw_Lscore_TestDB <- get_bw_score(studyid ='511-21060018', path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db',
                                 fake_study = FALSE, master_CompileData = NULL, score_in_list_format = TRUE)



lb_score <- get_lb_score(studyid='10663', path_db='C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/liver_1.db',fake_study = TRUE, master_CompileData = NULL)


lb_score_TestDB <- get_lb_score(studyid='511-21060018', path_db='C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db',
                                fake_study=FALSE, master_CompileData = NULL, score_in_list_format = FALSE)


mi_score_TestDB <- get_mi_score(studyid='511-21060018', path_db='C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db',
                                fake_study = FALSE, master_CompileData = NULL, score_in_list_format = FALSE)




#save(bw_score_TestDB, file = "bw_score_TestDB.RData")
#load("c/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/send-summarizer/R/bw_org_score_TestDB.RData")




livertobw_ratio <- get_liver_livertobw_score (studyid =  '511-21060018', 
                                             path_db = 'C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db', 
                                                          fake_study = FALSE, 
                                                          master_CompileData = NULL,
                                                          bwzscore_BW = NULL, 
                                                          score_in_list_format = FALSE)

compile_data <- get_compile_data(studyid='876', path_db='C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db',
                             fake_study=FALSE)

fake_compile_data <- get_compile_data(studyid='10663', path_db='C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db',
                                 fake_study = TRUE)
#' @~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~o724~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 get_liver_compiledata <- function(studyid, database_path){ }
 #get_liver_dose_ranking <- function(studyid, tx, cleaned_compiledata){}
 get_liver_bw_score  <-  function(studyid, bw, ts, master_compiledata, tK_animals_df ){}
 get_liver_lb_score <- function (studyid, ts, lb,  master_compiledata) {}
 get_liver_mi_score <- function(studyid, ts, mi, master_compiledata){}
#' @~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~bcs~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 get_compile_data <- function(studyid, path_db,fake_study=FALSE) {}
 #get_doses <- function(studyid, path_db, xpt_dir=NULL) {}
 get_bw_score <- function(studyid, path_db,fake_study=FALSE, master_CompileData = NULL) {}
 get_mi_score <- function(studyid, path_db,fake_study=FALSE, master_CompileData = NULL) {}
 get_lb_score <- function(studyid, path_db,fake_study= FALSE, master_CompileData = NULL) {}
 
 #' @~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~xpt_file_testing~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 