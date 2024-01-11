devtools::document()
devtools::load_all('.')
# path <-  'C:/Users/Md.Ali/OneDrive - FDA/yousuf/10_DATA/Biocelerate_shared_data/data/biocelerate.db'
path <-  'C:/Users/Md.Ali/OneDrive - FDA/yousuf/10_DATA/susan_db_cross_study/susan_test.db'
sendSummarizer::send_cross_study_app(database_path = path)
