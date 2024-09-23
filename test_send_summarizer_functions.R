# Set working directory to the package root (if necessary)
setwd("C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/send-summarizer")

# Load all functions from the package
devtools::load_all(".")


# Example: Testing the send_cross_study_app function
# Replace 'path_to_database.db' with the actual path to your database
sendSummarizer::send_cross_study_app('path_to_database.db')

# Add more function tests here

bw_score <- get_bw_score(studyid='10663', path_db='C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/liver_1.db', fake_study=TRUE)

lb_score <- get_lb_score(studyid='10663', path_db='C:/Users/mdaminulisla.prodhan/OneDrive - FDA/2023-2024_projects/FAKE_DATABASES/liver_1.db', fake_study=TRUE)


lb_score_TestDB <- get_lb_score(studyid='511-21060018', path_db='C:/Users/mdaminulisla.prodhan/OneDrive - FDA/TestDB.db', fake_study=FALSE)
