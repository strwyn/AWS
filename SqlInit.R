#work.dir<-"~/CLIPPERW/"
#work.dir<-"~/Documents/clipperdata"
#setwd(work.dir)
library(RPostgreSQL)
library(plyr)
if (exists("connection")){
  dbDisconnect(connection)
}
connection<-dbConnect(dbDriver("PostgreSQL"), 
                      dbname = "d43mg7o903brjv",
                      host = "ec2-50-16-134-232.compute-1.amazonaws.com", 
                      port = 5432,
                      user = "clipper_intern", 
                      password = {"xe8udetR"})
psql <- function(..., 
               con=connection){
  dbGetQuery(con, paste0(...))
}

psql("set client_encoding = 'UTF8'")
psql.drop<-function(..., 
                    con=connection){
  tryCatch(
    dbGetQuery(con, paste0(...)),
    error = function(e){}
  )
}

cread<-function(file){
  return(read.csv(file, stringsAsFactors = F))
}

dframe<-function(...){
  return(data.frame(..., stringsAsFactors = F))
}

NotePaste<-function(..., FUN = paste, sep = '', cop = '\n', note.pos = 1){
  strslist<-list(...)
  strslist<-lapply(strslist[], as.character)
  if (!note.pos){
    note.pos<-length(strslist)
  }
  #print(strslist)
  strs1<-FUN(
    unlist(
      strslist[0:(note.pos-1)][unlist(
        lapply(
          strslist[0:(note.pos-1)], 
          function(i){
            return(
              i != "" && length(unlist(i)) && !is.na(i)
            )
          }))]),
    sep = sep,
    collaspe = ''
  )
  #print(strs1)
  strs2<-FUN(
    unlist(
      strslist[-1:(-note.pos)][unlist(
        lapply(
          strslist[-1:(-note.pos)],
          function(i){ 
            return(
              i != "" && length(unlist(i)) && !is.na(i))}))]),
     sep = sep,
     collapse = ''
  )
  #strs1 <- FUN(strs1, collapse = '')
  #strs2 <- FUN(strs2, collapse = '')
  clpsstrs <- c(strs1, strslist[[note.pos]], strs2)
  clpsstrs <- clpsstrs[clpsstrs != '']
  #print(clpsstrs)
  return(FUN(clpsstrs, 
             collapse = cop))
  #combining strings will auto-remove null strings.
  #clpsstrs<-c(strs1, strslist[[note.pos]], str2)
  #while zero-length chars are not considered yet.
  #len.clpsstr<-length(clpsstrs)
  #for (i in 1:len.clpsstr){
  #}
  #new.pos<-which(clpsstrs == strslist[[note.pos]])
  #return()
  # if (note.pos == 1){
  #   if (strslist[[note.pos]] == ""){
  #     return(FUN(FUN(strs2, 
  #                    collapse = ''), 
  #                sep = sep))
  #   }else{
  #     return(FUN(strslist[[note.pos]], 
  #                FUN(strs2,
  #                    collapse = ''), 
  #                sep = sep))
  #   }
  # }else if (note.pos == length(strslist)){
  #   if (strslist[[note.pos]] == ""){
  #     return(FUN(FUN(strs1, 
  #                    collapse = ''),
  #                sep = sep))
  #   }else{
  #     return(FUN(FUN(strs1, 
  #                    collapse = ''), 
  #                strslist[[note.pos]],
  #                sep = sep))
  #   }
  # }else{
  #   if (strslist[[note.pos]] == ""){
  #     return(FUN(FUN(strs1, 
  #                    collapse = ''),
  #                FUN(strs2, 
  #                    collapse = ''), 
  #                sep = sep))
  #   }else{
  #     return(FUN(FUN(strs1, 
  #                    collapse = ''), 
  #                strslist[[note.pos]], 
  #                FUN(strs2,
  #                    collapse = ''), 
  #                sep = sep))
  #   }
  # }
}

GetPos<-function(pat, notes){
  return(grep(pat, notes[,1]))
}

pbind<-function(d1, d2){
  library(plyr)
  if (ncol(d1) >= ncol(d2)) {
    rbind.fill(d1, d2)
  }else{
    rbind.fill(d2, d1)
  }
}

inRange<-function(day, lower, upper){
  if ((day > lower && day <= upper) ||
    (day < lower && day >= upper)){
    return(TRUE)
  }
  return(FALSE)
}
GetTable <- function(tname, ..., where = ''){
  arg <- list(...)
  if (where != ''){
    where <- paste0(
      ' where ', where
    )
  }
  if (!length(arg)){
    return (psql("select * from ",
                 tname, where, ";"))
  }else{
    limit <- tail(arg, 1)[[1]]
    if (is.numeric(limit) && limit >= 0){
      cols <- head(arg, -1)
      if (length(cols) > 0){
        return (psql("select ", paste(cols, collapse = ', '),
                     " from ",
                     tname, where ," limit ",
                     limit, ";"))
      }else{
        return (psql("select * from ",
                     tname, where, " limit ",
                     limit, ";"))
      }
    }else{
      return (psql("select ", paste(..., sep = ', '),
                   " from ",
                   tname, where, ";"))
    }
  }
}

PrintTable<-function(tname, ...){
  print(GetTable(tname, ...))  
}

LazyTable <- function(...){
  do.call(GetTable, as.list(as.character(tail(match.call() , -1))))
}

DropTable<-function(tname) {
  psql.drop("drop table ", tname, ";")
}
DropTables<-function(...){
  lapply(list(...), FUN = DropTable)
}

CountTable<-function(tname){
  return(psql("select count(*) from ", tname)[1,1])
}

GetColAgg<-function(
  ...,
  limit = 10,
  cop = ', ',
  suffix1 = '...(',
  suffix2 = ' in total)'
){
  
  if (!limit) {
    return(paste0(..., collapse = cop))
  }else {
    len.vec <- max(unlist(lapply(list(...), length)))
    if (limit && len.vec > limit){
      return(
        paste0(paste0(
          do.call(
            paste0, 
            lapply(list(...), function(i){return(na.omit(i[1:limit]))})
          ), 
          collapse = cop),
          suffix1,
          len.vec,
          suffix2, collapse = '')
      )
    }
    return(paste0(..., collapse = cop))
  }
}

GetSqlAgg<-function(
  tname, 
  agg, 
  cond = "", 
  method = "",
  cond.value = "",
  logic = "and",
  sep = ',', 
  check.rows = T,#(cond == "" || method == "" || cond.value == ""),
  max.row = 10
){
# could be really slow if too many rows in tname
  if (check.rows && CountTable(tname) > max.row){
      #if.continue<-readline(
      # print(paste0('WARNING: more than ', 
      #                              max.row,
      #                             ' rows found in ',
      #                             tname, 
      #                             ' which could be really slow. \nDo you wanna continue?(Y/y to continue, end otherwise)'))
            #)
      # if (tolower(if.continue) != 'y'){
      #   return(warning("Process interrupted."))
      # }
    psql("select distinct(string_agg(", agg, "::text, '",sep,"')) from ", 
         tname)[1,1]
    psql("select distinct(string_agg(", agg, "::text, '",sep,"')) from ", 
         tname)[1,1]
  }
  else{
    psql("select distinct(string_agg(", agg, "::text, '",sep,"')) from ", 
         tname, 
         " where ",
         paste(cond, 
               method, 
               cond.value,
               sep = ' ',
               collapse = paste0(' ', logic, ' ')))[1,1]
  }
}

prepareTableGrade <- function(){
  psql("select cd_name as name, product_code as code, cmdty into temporary ",
       tname.cmdtycode,
       " from (select cmdty, cd_report from lookup.cmdty) a " ,
       "full join (select cd_name, cd_report, product_code from cat_product) b ",
       "on a.cd_report = b.cd_report " ,
       "where (cmdty != 'A' and cmdty != 'M' and product_code is not null) ",
       "or cmdty is null;")
  psql("insert into ",
       tname.cmdtycode,
       " select name, crude_code, 'C' constantvalue from cat_crude;")
  psql("alter table ", tname.cmdtycode, " alter column cmdty type varchar(32)")
  psql("update ", tname.cmdtycode,
       " set cmdty = 'A' where code = 'GS009';",
       "update ", tname.cmdtycode,
       " set cmdty = 'M' where code = 'CH037';")
  psql("insert into ",
       tname.cmdtycode,
       " values ('Ethanol', 'GS009', 'G')")
  return (GetTable(tname.cmdtycode))
}

prepareTableCmdtyPoi <- function(){
  psql("select coalesce(unload.poi, load.poi) as poi, unl_cmdty, load_cmdty into temporary ", 
               tname.cmdtypoi,
               " from (select poi, string_agg(cmdty, '') as unl_cmdty from poi_dir ", 
               "where loadunl like '%U%' group by poi) unload ",
               "full join (select poi, string_agg(cmdty, '') as load_cmdty from poi_dir ", 
               "where loadunl like '%L%' group by poi) load ", 
               "on unload.poi = load.poi;")
  return (GetTable(tname.cmdtypoi))
}

prepareTableReport <- function(){
  psql("select distinct(a.acd), b.cd_report, cmdty into temporary ", 
       tname.cmdtyreport , 
       " from (select cd_report as acd from cat_product) a ",
       "full join (select cd_report, cmdty from lookup.cmdty) b ",
       "on a.acd = b.cd_report;")
  return (GetTable(tname.cmdtyreport))
}

#Psql initialization

tname.cmdtycode<- 'cmdty_code'
tname.cmdtyreport<- 'cmdty_report'
tname.cmdtypoi<-'poi_cmdty'
tryCatch(
  DropTables(tname.cmdtycode,
             tname.cmdtyreport,
             tname.cmdtypoi),
  error = function(e){print("cao")})

#table.cmdtypoi<- 
prepareTableCmdtyPoi()
#table.cmdtyreport<-
prepareTableReport()
#table.cmdtycode<-
prepareTableGrade()

