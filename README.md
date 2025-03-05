# sendSummarizer 
sendSummarizer is an R package includes functions to calculate toxicity score
of a given repeat-dose toxicological study.  

- paper
- [paper link](https://academic.oup.com/toxsci/article/200/2/277/7690167?login=true) 

- Poster
- [poster](https://www.lexjansen.com/css-us/2022/POS_PP23.pdf)  



# Installation

clone the repo and follow the instructions.  

### for development

```
setwd('biocelerate-cross-study/')
devtools::load_all(".")
sendSummarizer::send_cross_study_app('path_database.db')

```

### for using the package

```
setwd('biocelerate-cross-study/')
devtools::install(".")
sendSummarizer::send_cross_study_app('path_database.db')

```

# more example

```{R}

path_db <- "C:/directory/send.db"
studyid <- '112344'

# app
send_cross_study_app(path_db)

# score
mi_score <- get_mi_score(studyid, path_db)
mi_score


lb_score <- get_lb_score(studyid, path_db)
lb_score

bw_score <- get_bw_score(studyid, path_db)
bw_score

all_score <- get_all_score(studyid, path_db, domain = c('lb', 'bw', 'mi'))
all_score



compile <- get_compile_data(studyid, path_db)
compile



```
