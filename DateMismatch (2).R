maccb<-function(data){
  cb<-pipe('pbcopy', 'w')
  cat(data, file = cb)
  close(cb)
}

tosqlv<-function(acol, link = !tovec && T, tovec = F){
  init.line<- ""
  if (link){
    if (nrow(acol) > 1){
      sapply(1:(nrow(acol) -1), function(i){
        init.line<<-paste0(init.line,
                           "('", 
                           paste(acol[i, ], 
                                 collapse = "','"), 
                           "'), ", collpase = "")
      })
    }
  }else{
    if (tovec){
      return(sapply(1:nrow(acol), function(i){
              paste0("('", 
                    paste(acol[i, ], 
                          collapse = "','"), 
                    "')")
              }))
    }else{
      if (nrow(acol) > 1){
        sapply(1:(nrow(acol) -1), function(i){
          init.line<<-paste(init.line, '"' ,
                            "('", 
                            paste(acol[i, ], 
                                  collapse = "','"), 
                            "'), ", '",\n ' ,sep = "")
        })
      }
      return(cat(paste(init.line,
                       "('", paste(acol[nrow(acol),], 
                                   collapse = "','"),
                       "')" , '"',sep = '')))
    }
  }
}

wincb<-function(data){
  writeClipboard(data)
}

is.empty.df<-function(dfr){
  if (is.data.frame(dfr)
      && !nrow(dfr) && !ncol(dfr)){
    return (TRUE)
  }else{return (FALSE)}
}
  
GetDate<-function(line, 
                  up=25,
                  down = 25,
                  delistart = 'startdate',
                  deliend = 'enddate',
                  bargo = 'bargo'){
  link = as.character(as.matrix(line[bargo]))
  startpos<-regexpr(delistart, link)
  endpos<-regexpr(deliend, link)
  startDate<-as.Date(substr(link, 
                            startpos + 1 + nchar(delistart), 
                            startpos + 10 + nchar(delistart)))-up
  endDate<-as.Date(substr(link, 
                            endpos + 1 + nchar(deliend), 
                            endpos + 10 + nchar(deliend)))+down
  http<-paste0(substr(link, 1, startpos - 1), 
         delistart, '=' ,startDate, '&', 
         deliend, '=', endDate,
         substr(link, endpos + 11 + nchar(deliend), nchar(link)))
  #writeClipboard(http) #windows method.
  browseURL(GetPort(line))
  browseURL(http)
  print(http)
  print(line[c('imo',
               'grade', 
               'grade_raw',
               'operation_date', 
               'operation',
               'direction', 
               'port')])
}

GetPort<-function(line){
  paste0("https://www.google.com/maps/place/", 
               as.character(as.matrix(line['port'])))
}

str.ambi<-function(port){
  return(
    paste0("%", 
           paste(strsplit(port, ' ')[[1]], 
                 collapse = '%'), 
           "%"))
}

lagflag<-function(timediff){
  if (timediff > 0){
    return ("later")
  }else if (timediff < 0){
    return ("earlier")
  }else{
    return ("exactly")
  }
}

ismatchdir<-function(loadunl, dgh.arr, dgh.dpt){
  if ((loadunl == 'U' && dgh.dpt - dgh.arr < 0) ||
      (loadunl == 'L' && dgh.dpt - dgh.arr > 0) ){
    return (TRUE)
  }else{return (FALSE)}
}

isemptydir<-function(loadunl){
  if (loadunl == ' '){
    return (TRUE)
  }else{return (FALSE)}
}

isemptydgh<-function(dgh){
  if (is.na(dgh)){
    return (TRUE)
  }else{
    return (FALSE)
  }
}

ismatchdir.loadunl<-function(rec, loadunl){
  if ((rec == 'Load' && loadunl == 'L') ||
      (rec == 'Discharge' && loadunl == 'U')){
    return (TRUE)
  }else{return (FALSE)}
}

ismatchdir.dgh<-function(rec, dghdiff){
  if ((rec == 'Load' && dghdiff >= 0) ||
      (rec == 'Discharge' && dghdiff <= 0)){
    return (TRUE)
  }else{
    return (FALSE)
  }
}

ismatchpoi.cmdty<-function(cmdty, poi.cmdty){
  if (grepl(cmdty, poi.cmdty)){
    return (TRUE);
  }else{
    return (FALSE)
  }
}

GetCmdty <- function(dir, 
                     dir.load = 'Load', 
                     dir.unl = 'Discharge',
                     load.col = 'load_cmdty',
                     unl.col = 'unl_cmdty'){
  if (dir == dir.load) {
    return (load.col)
  }
  if (dir == dir.unl){
    return (unl.col)
  } 
  return (NA)
}

# NotePaste<-function(note, ... ){
#   if (note == "") {
#     return (paste0(...))
#   }else{
#     return (paste(note, paste0(...), sep = '\n'))
#   }
# }
iter <- function (x,
                  csv = ccsv, 
                  startinv = 10, 
                  endinv = 10){
  notes <- ""
  if (x > 1 
      && sum(csv[x,] != csv[x-1,], na.rm = T) == 0){
    notes <- NotePaste(notes, 'Duplicate record.')
    return(notes)
  }
  reason<-unique(csv[,1])
  if (csv[x,1] == reason[1]) {
    globalport.tname <- 't_global'
    port.raw <- str.ambi(csv[x,col.port])
    globalport.table<-psql("SELECT * INTO TEMPORARY ", 
                globalport.tname,
                " FROM port WHERE NAME ILIKE '", 
                port.raw, "';")
    if (!is.empty.df(globalport.table)){
      ctryport.tname<-"t_country"
      ctryport.table <- psql("select * into temporary", 
                             ctryport.tname,
                             "from",
                             globalport.tname,
                             " where country in ", 
                             "(select country from country where un_code ='",
                             csv[x, col.un_code],"')", ";")
      if (!is.empty.df(ctryport.table)){
        port.tname<-'t_port'
        port.table<-psql("select code into ",
                         port.tname,
                         " from ", 
                         ctryport.tname,
                         " where name ilike '", 
                         csv[x, col.port] ,"';")
        if (!is.empty.df(port.table)){
          record.port <- psql("select * from asvt_arrival ", 
                             "where poi in ",
                             "(select poi from as_poi where port in ",
                             "(select code from ", 
                             port.tname, 
                             ")) and date_arrive >= date'",
                             csv[x, col.opd],
                             "' and date_arrive <= date'",
                             csv[x, col.opd],
                             "' + integer'60' and vessel in ",
                             "(select vessel from asvt_position where imo = ",
                             csv[x, col.imo],
                             ") order by date_arrive desc;")
          if (!is.empty.df(record.port)){
            recrec <- record.port[1,]
            if (as.Date(recrec['date_arrive']) != 
                as.Date(csv[x,col.opd])){
              tmdiff<- as.Date(recrec['date_arrive']) - as.Date(csv[, col.opd])
                as.Date(csv[x,col.opd])
              notes<- paste0(notes, csv[x, col.op],
                             " found on ", recrec['date_arrive'], 
                             "(", abs(tmdiff), " days ", lagflag(tmdiff), ").")
            }else{
              notes<- paste0(notes, 'Date matches')
              is.datematch<- TRUE
            }
            is.completedgh <- !isemptydgh(recrec['draught_arrive']) &&
              !isemptydgh(recrec['draught_depart'])
            if (is.completedgh && !isemptydir(recrec['loadunl'])) {
              dghdiff<- recrec['draught_depart'] - recrec['draught_arrive']
              if (ismatchdir(recrec['loadunl'],
                             recrec['draught_arrive'],
                             recrec['draught_depart'])){
                if (!ismatchdir.loadunl(csv[x, col.dir],
                                        recrec['loadunl'])) {
                  if (is.datematch) {
                    notes<-paste0(notes, ', but\n')
                  }
                  notes<-paste0(notes, '\nDirection not match: ', 
                                recrec['loadunl'], 
                                ' in our database.')
                  has.othererror<- TRUE
                }
              }else{
                if (is.datematch) {
                  notes<-paste0(notes, ', but\n')
                }
                notes<-paste0(notes, '\nDraught change (', 
                              dghdiff , ') and loadunl (', 
                              recrec['loadunl'],') in database not match: 
                              agent showed ', csv[x, col.dir])
                has.othererror <- TRUE
              }
            }else if (!is.completedgh && !isemptydir(recrec['loadunl'])){
              if (!ismatchdir.loadunl(csv[x, col.dir],
                                      recrec['loadunl'])) {
                if (is.datematch) {
                  notes<-paste0(notes, ', but\n')
                }
                notes<-paste0(notes, '\nDirection not match: ', 
                              recrec['loadunl'], 
                              ' in our database.')
                
              }
              notes<- paste0(notes, '\nDraught change not found in database.')
              has.othererror <- TRUE
            }else if (is.completedgh && isemptydir(recrec['loadunl'])){
              dghdiff<- recrec['draught_depart'] - recrec['draught_arrive']
              if (!ismatchdir.dgh(csv[x, col.dir], dghdiff)){
                if (is.datematch) {
                  notes<-paste0(notes, ', but\n')
                }
                notes<-paste0(notes, '\nDirection not match: ', 
                              'Draught change ', dghdiff, ' in our database.')
              }
              notes<- paste0(notes, '\nLoad direction not found in database.')
              has.othererror <- TRUE
            }else{
              if (is.datematch) {
                notes<-paste0(notes, ', but\n')
              }
              notes<- paste0(notes, '\nNo draught records or loadunl found in database, 
                             direction not checked.')
              has.othererror <- TRUE
            }
            grade.cmdty<- psql("select cmdty from ",
                               tname.cmdtycode,
                               " where code = '",
                               csv[x, col.grade],"';")
            if (is.empty.df(grade.cmdty)){
              notes<-paste0(notes, "\nNo crude/product_codes found for grade ",
                            csv[x, col.grade], 
                            ". (Maybe not our focus?)")
            }else{
              poi.cmdty <- psql("select ", GetCmdty(csv[x, col.dir]),
                                " from ", tname.cmdtypoi,
                                " where poi = ",
                                csv[x, col.grade],  ";")
              if(nrow(grade.cmdty)>1){
                notes<-paste0(notes, "\nWarning: Found >1 crude/product_codes for grade ",
                              csv[x, col.grade], ".")
                if (sum(sapply(1: nrow(grade.cmdty), 
                               function(i){
                                 return(!grepl(grade.cmdty[i,'cmdty'], 
                                               poi.cmdty))
                               })) == 0){
                  notes<-paste0(notes, "\nWarning: Found >1 crude/product_codes for grade ",
                                csv[x, col.grade], ".")
                }
              }else{
                
              }
            }
          }else{
            notes<- paste0(notes, "No match between 10 and 60 days")
            return (notes)
          }
        }else{
          notes <- paste0(notes, "No match for port ", csv[x, col.port], '.')
          return (notes)
        }
      }else{
        notes <- paste0(notes, "No port named (or named like) ", 
                        csv[x, col.port], " is found in country",
                        csv[x, col.un_code],".")
        return(notes)
      }
    }else{
      notes <- paste0(notes, "No port named (or named like) ", 
                      csv[x, col.port], " is found.")
      return(notes)
    }
    
    #actual.record<-psql()
    #not considering multiple ports with same name in one country.
    #
  }else if(csv[x,1] == reason[2]){
    
  }else if(csv[x,3] == reason[3]){
    
  }else{
    #temp error report.
    return('Reason not identified. Record not checked.')
  }
}

#ccsv<-read.csv('C:/Users/Abudi.Zein/Desktop/CLIPPERW/DataCleaning.csv', stringsAsFactors = F)
#ccsv<-read.csv('~/Documents/clipperdata/DataCleaning.csv', stringsAsFactors = F)

ccsv.colnames<-colnames(ccsv)
col.bargo<-ccsv.colnames[25]
col.port<-ccsv.colnames[11]
col.vessel<-ccsv.colnames[2]
col.imo<-ccsv.colnames[3]
col.opd<-ccsv.colnames[4]
col.grade<-ccsv.colnames[7]
col.notes<-ccsv.colnames[26]
col.un_code<-ccsv.colnames[16]
col.op<-ccsv.colnames[13]
col.dir<-ccsv.colnames[15]