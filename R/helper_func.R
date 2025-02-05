#!/usr/bin/env Rscript

# This code is for creating
# -----------------------------------------------------------------------------
# Date                     Programmer
#----------   --------------------------------------------------------------
# Feb-04-2025    Md Yousuf Ali (MdYousuf.Ali@fda.hhs.gov)


## conn <- DBI::dbConnect(RSQLite::SQLite(), db_path)
## query <- 'SELECT *  FROM ID'
## all_ids <- DBI::dbGetQuery(conn = conn, query)
## all_ids <- data.table::setDT(all_ids)

get_studyid_title <- function(conn,ind,all_ids){

    select_studyid <- all_ids[APPID %in% ind,]
    studyid <- unique(select_studyid[,STUDYID])

    STITLE <- DBI::dbGetQuery(conn = conn,
                              'SELECT STUDYID, TSVAL FROM TS WHERE TSPARMCD = "STITLE" and STUDYID in (:x)',
                              params=list(x=studyid))
dbStudys <- merge(select_studyid, STITLE, by = "STUDYID")
dbStudys[, `:=`(nm=paste0(APPID,"-",STUDYID,": ",TSVAL))]
  st <- dbStudys$STUDYID
  names(st) <- dbStudys$nm
  st

}
