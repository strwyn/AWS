#read.csv("Inchcape SGS cleanup_0811.csv")
library(plyr)
no.enum<-c('port', 'ports', '[0-9]+', '[ivx]+')

CheckGradeRaw<-function(){
  
}

CheckPortCityCode<-function(){
#country and city code?
  
}

CheckQtDrft<-function(){
  
}

CheckCmdty<-function(){
  
}

plural<-function(str){
  len<-nchar(str)
  str.end<-substring(str, len)
  str.pre<-substr(str, 1, len -1)
  if (str.end == 'y'){
    return (paste0(str.pre, 'ies'))
  }else if( str.end == 'x' ||
            str.end == 's' ||
            str.end == 'z'){
    return (paste0(str,'es'))
  }else{
    return (paste0(str, 's'))
  }
}

PluralPhrase<-function(str, num){
  if (abs(num) > 1){
    return (paste(num, plural(str)))
  }else{
    return (paste(num, str))
  }
}

maccb<-function(data){
  cb<-pipe('pbcopy', 'w')
  cat(data, file = cb)
  close(cb)
}

tosqlv<-function(acol, link = !tovec, tovec = F){
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
  if(!is.data.frame(dfr)){
    stop(deparse(substitute(dfr)), " is not a data frame.")
  }else if (!nrow(dfr) && !ncol(dfr)){
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

getDateWindow<-function(date_arrive, 
                        date_depart, 
                        window.radius){
  if (!is.na(date_arrive) && !is.na(date_depart)) {
    return(paste0("((date_arrive >= date'", 
                  date_arrive, "' - ", window.radius,
                  " and date_arrive <= date'", 
                  date_arrive,"' + ", window.radius,
                  ") or (date_depart >= date'", 
                  date_depart, "' - ", window.radius,
                  " and date_depart <= date'", 
                  date_depart,"' + ", window.radius, "))"))
  }else if (!is.na(date_arrive)){
    return(paste0("(date_arrive >= date'", 
                  date_arrive, "' - ", window.radius,
                  " and date_arrive <= date'", 
                  date_arrive,"' + ", window.radius,
                  ")"))
  }else if (!is.na(date_depart)){
    return(paste0("(date_depart >= date'", 
                  date_depart, "' - ", window.radius,
                  " and date_depart <= date'", 
                  date_depart,"' + ", window.radius,
                  ")"))
  }else{
    return("")
  }
}

str.ambi<-function(name){
  return(
    paste0("%", 
           paste(strsplit(name, ' ')[[1]], 
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

daystonote<-function(day, 
                     has.star = TRUE,
                     reverse = FALSE,
                     lower = 10,
                     upper = 60,
                     equal.out ="Exact match."){
  if (reverse){
    day <- -day
  }
  if (!is.null(day) && !is.na(day)){
    if (day > 0){
      if (day == 1){
        return (paste0(day, " day later"))
      }else{
        raw.note<-paste0(day, " days later")
        if (inRange(day, lower, upper) &&
            has.star){
            return (paste0("*", raw.note))
        }else if (day > upper &&
                  has.star){
          return (paste0("**", raw.note))
        }
        return (raw.note)
      }
    }else if (day == 0){
      return (equal.out)
    }else{
      if (day == -1){
        return (paste0(abs(day), " day earlier"))
      }else{
        raw.note<-paste0(abs(day), " days earlier")
        if (inRange(day, -lower, -upper) &&
            has.star){
          return (paste0("*", raw.note))
        }else if (day < -upper &&
                  has.star){
          return (paste0("**", raw.note))
        }
        return (raw.note)
      }
    }
  }else{
    return (NA)
  }
}

NameEnum<-function(name, 
                   sep = '%', 
                   spl = ' ', 
                   ends = sep, 
                   has.head = (ends == sep)){
  ele<-strsplit(name, spl)[[1]]
  ele<-ele[ele != '']
  if (length(ele) == 1){
    if (has.head){
      return(c(paste0(sep, name, ends), ends))
    }
    return(c(paste0(ends, name, ends), ends))
  }else{
    rest<-paste(ele[-1], collapse = spl)
    if (has.head){
      return(c(paste0(sep, 
                      ele[1],
                      NameEnum(rest, sep, spl, ends, has.head)), 
               paste0(NameEnum(rest, sep, spl, ends, has.head))))
    }
    return(c(paste0(ends, 
                    ele[1],
                    NameEnum(rest, sep, spl, ends, has.head = T)), 
             paste0(NameEnum(rest, sep, spl, ends, has.head = F))))
  }
}

GetNameEnum<-function(name, 
                      sep = "%", 
                      ends = sep,
                      spl = ' ',
                      has.head = (ends == sep),
                      rmv = no.enum){
  #add name breaking down parenthesis here
  #if (grepl('\\(', name) && grepl('\\)', name)){
  #   
  #} 
  #
  enum.raw<-NameEnum(name, sep, spl, ends, has.head)
  enum.raw<-enum.raw[order(nchar(enum.raw), decreasing = T)]
  rmv<- paste(paste0("^", sep, rmv, sep, "$") ,collapse = "|")
  #print(rmv)
  enum.raw<-enum.raw[!grepl(rmv, tolower(enum.raw))]
  return(enum.raw[-length(enum.raw)])
}

GetNameQuery<-function(subnames, 
                       table.alias = '' ,
                       colname = 'name',
                       method = 'ilike',
                       logic = 'or'){
  #requires 'subnames' to be vector of character.
  logic <- paste0(' ', logic, ' ')
  subnames<-paste0("'", GetNameEnum(subnames), "'")
  if (table.alias == ''){
    query.keyword<-paste0(colname, ' ', method)
  }else{
    query.keyword<-paste0(table.alias, '.', colname, ' ', method) 
  }
  return(paste0('(', 
                 paste(paste(query.keyword,
                             subnames),
                       collapse = logic),
                 ')'))
}

CheckImoVessel<-function(
  csv,
  tname.vessel
){
  #tname.vessel<-"ves"
  #print(csv)
  is.emptyvessel<-F
  tmp.notes <- ""
  DropTable(tname.vessel)
  psql("select vessel, a.imo, a.name 
       into temporary ", tname.vessel, 
       " from tanker a, live.as_vessel_exp b where a.imo = ", csv[,'imo'], 
       " and a.imo = b.imo")
  #table.vessel<-GetTable(tname.vessel)
  if (!CountTable(tname.vessel)){
    tmp.notes<-paste0(tmp.notes, 
                      "No imo = ", 
                      csv[,'imo'] , 
                      " found in live.as_vessel_exp")
    table.imo<-psql("select * from tanker where imo = ", csv[,'imo'])
    if (!nrow(table.imo)){
      tmp.notes<-paste0(tmp.notes, " (Nor in table 'tanker'.)")
      DropTable(tname.vessel)
      #print(GetTable(tname.vessel))
      psql("select vessel, a.imo, a.name into temporary ",
           tname.vessel,
           " from tanker a, live.as_vessel_exp b where name ilike '", 
           csv[, 'vessel'], "' and a.imo = b.imo")
      #table.liketanker<-GetTable(tname.vessel)
      if (!CountTable(tname.vessel)) {
        DropTable(tname.vessel)
        psql("select vessel, a.imo, a.name into temporary ",
             tname.vessel,
             " from tanker a, live.as_vessel_exp b where ", 
             GetNameQuery(csv[, 'vessel'], 'a'),
             " and a.imo = b.imo")
        #table.liketanker<-GetTable(tname.vessel)
        if (!CountTable(tname.vessel)) {
          #DropTable(tname.vessel)
          tmp.notes<-paste0(tmp.notes, 
                            "\nAnd no vessel named '",
                            csv[, 'vessel'], 
                            "' or alike found in tanker/live.as_vessel_exp.")
          is.emptyvessel <-T
        }else{
          # tmp.notes<-paste0(tmp.notes,
          #                   "\nAnd only similar vessel(imo) ",
          #                   GetColAgg(tname.vessel, 
          #                             "name || '(' || imo || ')'"),
          #                   ' found in tanker/live.as_vessel_exp.')
        }
      }else{
        #print(1)
        # tmp.notes<-paste0(tmp.notes,
        #                   "\nVessel ", csv[, 'vessel'], " found in tanker, imo(s): ",
        #                   GetColAgg(tname.vessel, 'imo'))
      }
      # notes<-NotePaste("No imo = ", 
      #                  csv[,'imo'] , 
      #                  " found in live.as_vessel_exp.", 
      #                  notes, note.pos = 0)
      # return (data.frame(notes = notes))
    }else{
      #assume imo is unique.
      if (toupper(table.imo[,'name']) == toupper(csv[,'vessel'])){
        tmp.notes<-paste0(tmp.notes, " (But found in tanker, name matched: ", 
                          table.imo[,'name'],")")
      }else{
        tmp.notes<-paste0(tmp.notes, " (But found in tanker, name NOT matched: ", 
                         table.imo[,'name'], ")")
      }
    }
  }
  return (list(tmp.notes, is.emptyvessel))
} 

CheckArr<-function(csv,
                   closest.arr,
                   tname.vessel,
                   tname.cmdtycode,
                   tname.cmdtypoi,
                   notes){
  num.dscpc <- 0
  num.unkwn <- 0
  #check vessel name
  closest.vessel<-psql("select name, imo from ",
                       tname.vessel,
                       " where vessel = ",closest.arr[, 'vessel'])[1,] 
  if (toupper(closest.vessel[, 'name']) != toupper(csv[, 'vessel'])){
    notes<-NotePaste(notes,
                      "only similar vessel(imo) ",
                      closest.vessel[,"name"],
                     " (", closest.vessel[,'imo'], ")",
                      ' found in tanker/live.as_vessel_exp.')
    num.dscpc <- num.dscpc + 1
    if (closest.vessel[, 'imo'] != csv[, 'imo']){
      notes<-NotePaste(notes,
                       "Imo not matched either.", sep = ' ')
    }
  }else if (closest.vessel[, 'imo'] != csv[, 'imo']){
    notes<-NotePaste(notes,
                     "Vessel ", closest.vessel[, 'name'], " found with different imo(s): ",
                     closest.vessel[, 'imo'])
    num.dscpc <- num.dscpc + 1
  }
  #check cmdty
  grade.cmdty<- psql("select cmdty from ",
                     tname.cmdtycode,
                     " where code = '",
                     csv[, 'grade'],
                     "';")
  if (!nrow(grade.cmdty) || !any(!is.na(grade.cmdty[,'cmdty']))){
    notes<-NotePaste(notes, 
                     "No cmdty code(s) found for grade ",
                     csv[, 'grade'], 
                     ". (Maybe not our focus?)")
    num.unkwn <- num.unkwn + 1
  }else{
    grade.cmdty <- na.omit(grade.cmdty)
    if(nrow(grade.cmdty)>1){
      notes<-NotePaste(notes, 
                       "WARNING: Found >1 crude/product_codes for grade ",
                       csv[, 'grade'], 
                       ".")
    }
    if (tolower(csv[, 'direction']) == 'transfer'){
      notes<-NotePaste(notes, "Direction is 'Transfer': ignored.")
    }else{
      poi.cmdty <- psql("select ", GetCmdty(csv[, 'direction']),
                        " from ", tname.cmdtypoi,
                        " where poi = ",
                        closest.arr[, 'poi'],
                        ";")
      isnot.inpoicmdty <- sapply(grade.cmdty[,'cmdty'],
                                 function(i){
                                   return(!grepl(i, poi.cmdty))
                                 })
      if (any(isnot.inpoicmdty)){
        #print(csv)
        notes<-NotePaste(notes, 
                         'Poi ', closest.arr[,'poi'], 
                         ' may be able to ',
                         tolower(csv[, 'direction']),' ',
                         grade.cmdty[isnot.inpoicmdty, 
                                     'cmdty'])
        num.dscpc <- num.dscpc + 1
      }
      # print(grade.cmdty[sapply(grade.cmdty[,'cmdty'],
      #                          function(i){
      #                            return(!grepl(i, poi.cmdty))
      #                          }), 
      #                   'cmdty'])
    }
  }
  #check draught
  is.completedgh <- 
    !isemptydgh(closest.arr[,'draught_arrive']) &&
    !isemptydgh(closest.arr[,'draught_depart'])
  if (is.completedgh &&
      !isemptydir(closest.arr[,'loadunl'])) {
    dghdiff<- round(closest.arr[,'draught_depart'] - 
                      closest.arr[,'draught_arrive'], 1)
    if (ismatchdir(closest.arr[,'loadunl'],
                   closest.arr[,'draught_arrive'],
                   closest.arr[,'draught_depart'])){
      if (!ismatchdir.loadunl(csv[, 'direction'],
                              closest.arr[,'loadunl'])) {
        notes<-NotePaste(notes, 'Direction not matched: ', 
                         closest.arr[,'loadunl'], 
                         ' in asvt_arrival.')
        num.dscpc <- num.dscpc + 1
        #has.othererror<- TRUE
      }
    }else{
      #print (csv)
      notes<-NotePaste(notes, 
                       'Draught change (', 
                       dghdiff , 
                       ') and loadunl (', 
                       closest.arr[,'loadunl'],
                       ') in asvt_arrival not matched: agent showed ', 
                       csv[, 'direction'])
      num.dscpc <- num.dscpc + 1
      #has.othererror <- TRUE
    }
  }else if (!is.completedgh && 
            !isemptydir(closest.arr[,'loadunl'])){
    if (!ismatchdir.loadunl(csv[, 'direction'],
                            closest.arr[,'loadunl'])) {
      notes<-NotePaste(notes, 
                       'Direction not matched: ', 
                       closest.arr[,'loadunl'], 
                       ' in asvt_arrival.')
      num.dscpc <- num.dscpc + 1
    }else{
      notes<-NotePaste(notes,
                       '(Direction matches.)')
    }
    notes<- NotePaste(notes, 
                      'Draught change not found in asvt_arrival.')
    num.unkwn <- num.unkwn + 1
    #has.othererror <- TRUE
  }else if (is.completedgh && 
            isemptydir(closest.arr[,'loadunl'])){
    dghdiff<- round(closest.arr[,'draught_depart'] - 
                      closest.arr[,'draught_arrive'], 1)
    if (!ismatchdir.dgh(csv[, 'direction'], dghdiff)){
      notes<-NotePaste(notes, 
                       'Direction not matched: ', 
                       'Draught change ', 
                       dghdiff, 
                       ' in asvt_arrival.')
      num.dscpc <- num.dscpc + 1
    }else{
      notes<-NotePaste(notes, 
                       '(Draught change ', 
                       dghdiff, ' which matches.)')
    }
    notes<- NotePaste(notes, 
                      'Load direction not found in asvt_arrival.')
    num.unkwn <- num.unkwn + 1
    #has.othererror <- TRUE
  }else{
    notes<- NotePaste(notes, 
                      'No draught records or loadunl found in asvt_arrival, direction not checked.')
    num.unkwn <- num.unkwn + 2
    #has.othererror <- TRUE
  }
  
#check date, destination_arrive
  closest.poiportname<-psql("select port.name from as_poi, port where as_poi.port = port.code and poi = ",
                            closest.arr[, 'poi'])[1,1]
  ismatch.destination_arrive <- 
    !is.na(closest.arr[,'destination_arrive']) && 
    (toupper(closest.arr[,'destination_arrive']) == toupper(csv[,'port']))
  ismatch.poiportname<- 
    toupper(closest.poiportname) == toupper(csv[, 'port'])
  if (!closest.arr[,'diff']){
    if (!ismatch.destination_arrive && !ismatch.poiportname){
      print(closest.poiportname)
      print(csv[,'port'])
      if (any(sapply(GetNameEnum(toupper(csv[,'port']), 
                                 sep = ' ',
                                 ends = ''),
                     function(i){grepl(i, 
                                       toupper(closest.poiportname))}))){
        notes<-NotePaste(paste0(csv[, 'operation'], 
                                ": date_arrive matches operation_date by dubious port '", 
                                closest.poiportname, "' (poi ",
                                closest.arr[,'poi'],
                                ") not matching port ", 
                                csv[,'port'], " (",
                                PluralPhrase("discrepancy", num.dscpc + 1), ", ", 
                                num.unkwn, " unknown. See below if any)."),
                         notes, note.pos = 0)
      }else if (any(sapply(GetNameEnum(toupper(csv[,'port']),
                                       sep = ' ',
                                       ends = ''),
                           function(i){grepl(i, 
                                             toupper(closest.arr[,'destination_arrive']))}))){
        notes<-NotePaste(paste0(csv[, 'operation'], 
                                ": date_arrive matches operation_date by dubious destination_arrive '", 
                                closest.arr[,'destination_arrive'], "', while (poi ",
                                closest.arr[,'poi'],", port '", 
                                closest.poiportname,
                                "') not matching port ", 
                                csv[,'port'], " (",
                                PluralPhrase("discrepancy", num.dscpc + 2), ", ", 
                                num.unkwn,
                                " unknown. See below if any)."),
                         notes, note.pos = 0)
      }else{
        notes<-NotePaste("(will further check destination_arrive and port: ", 
                         closest.arr[,'destination_arrive'],', ',
                         closest.arr[,'poi'],' at ',
                         closest.poiportname,")",
                         notes, note.pos = 0)
      }
    }else if(!ismatch.destination_arrive && ismatch.poiportname){
      notes<-NotePaste(paste0(csv[, 'operation'],
                              ": date_arrive matches operation_date (",
                              PluralPhrase("discrepancy", num.dscpc), ", ",
                              num.unkwn,
                              " unknown. See below if any)."),
                       notes, note.pos = 0)
    }else if(ismatch.destination_arrive && !ismatch.poiportname){
      notes<-NotePaste(paste0(csv[, 'operation'], 
                              ": date_arrive matches operation_date by destination_arrive, while (poi ",
                              closest.arr[,'poi'],", port '", 
                              closest.poiportname,
                              "') not matching port ", 
                              csv[,'port'], " (",
                              PluralPhrase("discrepancy", num.dscpc + 1), ", ", 
                              num.unkwn,
                              " unknown. See below if any)."), 
                       notes, note.pos = 0)
    }else{
      notes<-NotePaste(paste0(csv[, 'operation'],
                              ": date_arrive matches operation_date (",
                              PluralPhrase("discrepancy", num.dscpc), ", ", 
                              num.unkwn,
                              " unknown. See below if any)."),
                       notes, note.pos = 0)
    }
  }else{
    if (!ismatch.destination_arrive && !ismatch.poiportname){
      if (any(sapply(GetNameEnum(toupper(csv[,'port'])),
                     function(i){grepl(i, 
                                       toupper(closest.poiportname))}))){
        notes<-NotePaste(paste0('Dubious ', 
                                daystonote(closest.arr[ ,'diff']),' ',
                                csv[, 'operation'], 
                                " found by similar port '", 
                                closest.poiportname, "' (poi ",
                                closest.arr[,'poi'],
                                ") not matching port ", 
                                csv[,'port'], " (",
                                PluralPhrase("discrepancy", num.dscpc + 2), ", ", 
                                num.unkwn,
                                " unknown. See below if any)."),
                         notes, note.pos = 0)
      }else if (any(sapply(GetNameEnum(toupper(csv[,'port'])),
                           function(i){grepl(i, 
                                             toupper(closest.arr[,'destination_arrive']))}))){
        notes<-NotePaste(paste0('Dubious ', 
                                daystonote(closest.arr[ ,'diff']),' ',
                                csv[, 'operation'], 
                                " found by similar destination_arrive '", 
                                closest.arr[,'destination_arrive'], "', while (poi ",
                                closest.arr[,'poi'],", port '", 
                                closest.poiportname,
                                "') not matching port ", 
                                csv[,'port'], " (",
                                PluralPhrase("discrepancy", num.dscpc + 3), ", ", 
                                num.unkwn,
                                " unknown. See below if any)."),
                         notes, note.pos = 0)
      }else{
        notes<-NotePaste("(will further check destination_arrive and port: ", 
                         closest.arr[,'destination_arrive'],', ',
                         closest.arr[,'poi'],' at ',
                         closest.poiportname,") and",
                         csv[,'port'],
                         notes, note.pos = 0)
      }
    }else if(!ismatch.destination_arrive && ismatch.poiportname){
      notes<-NotePaste(paste0('Dubious ', 
                              daystonote(closest.arr[ ,'diff']),' ',
                              csv[, 'operation'], 
                              " found (",
                              PluralPhrase("discrepancy", num.dscpc + 1), ", ",
                              num.unkwn,
                              " unknown. See below if any)."),
                       notes, note.pos = 0)
    }else if(ismatch.destination_arrive && !ismatch.poiportname){
      notes<-NotePaste(paste0('Dubious ', 
                              daystonote(closest.arr[ ,'diff']),' ',
                              csv[, 'operation'], 
                              " found by destination_arrive, while (poi ",
                              closest.arr[,'poi'],", port'" , 
                              closest.poiportname,
                              "') not matching port ", 
                              csv[,'port'], " (",
                              PluralPhrase("discrepancy", num.dscpc + 2), ", ", 
                              num.unkwn,
                              " unknown. See below if any)."), 
                       notes, note.pos = 0)
    }else{
      notes<-NotePaste(paste0('Dubious ', 
                              daystonote(closest.arr[ ,'diff']),' ',
                              csv[, 'operation'],
                              " (",
                              PluralPhrase("discrepancy", num.dscpc + 1), ", ", 
                              num.unkwn,
                              " unknown. See below if any)."),
                       notes, note.pos = 0)
    }
  #else{
  #   notes<-NotePaste(paste0(csv[,'operation'], 
  #                           ' date is ',
  #                           daystonote(table.arrall[1,'diff'], F), 
  #                           '.'), 
  #                    notes)
  # }
  }
  return(notes)
}

#vessel-imo: live.as_vessel_exp
IterInchcape<-function(csv){
  notes<-""
#check vessel
  tname.vessel<-"ves"
  #DropTable(tname.vessel)
  # psql("select vessel 
  #      into temporary ", tname.vessel, 
  #      " from live.as_vessel_exp where imo = ", csv[,'imo'])
  # #table.vessel<-GetTable(tname.vessel)
  # if (!CountTable(tname.vessel)){
  #   notes<-NotePaste("No imo = ", 
  #                    csv[,'imo'] , 
  #                    " found in live.as_vessel_exp", 
  #                    notes, note.pos = 0)
  #   table.imo<-psql("select * from tanker where imo = ", csv[,'imo'])
  #   if (!nrow(table.imo)){
  #     notes<-NotePaste(notes, " (Nor in table 'tanker'.)", sep = '')
  #     return (data.frame(notes = notes))
  #   }else{
  #     #assume imo is unique.
  #     if (toupper(table.imo[,'name']) == toupper(csv[,'vessel'])){
  #       notes<-NotePaste(notes, " (But found in tanker, name matched: ", 
  #                        table.imo[,'name'],")", 
  #                        sep = '')
  #     }else{
  #       notes<-NotePaste(notes, " (But found in tanker, name NOT matched: ", 
  #                        table.imo[,'name'],")", 
  #                        sep = '')
  #     }
  #   }
  # }
  result.checkimo<-CheckImoVessel(csv, tname.vessel)
  notes<-NotePaste(result.checkimo[[1]],
                   notes, note.pos = 0)
  if (result.checkimo[[2]]){
    #DropTable(tname.vessel)
    return (data.frame(notes = notes, stringsAsFactors = F))
  }
#check ports
  tname.portpoi<-'port_poi'
  DropTable(tname.portpoi)
  psql("select as_poi.poi, port.name into temporary ",
       tname.portpoi, " from as_poi, port
       where as_poi.port = port.code
       and port.name ilike '", csv[,'port'],"'")
  #table.portpoi<-GetTable(tname.portpoi)#!nrow(table.portpoi)
  #is.similarports<-FALSE
  if (!CountTable(tname.portpoi)){
    DropTable(tname.portpoi)
    psql("select as_poi.poi, port.name into temporary ",
         tname.portpoi, " from as_poi, port
         where as_poi.port = port.code
         and ", GetNameQuery(csv[,'port'], 'port'))
    #table.portpoi<-GetTable(tname.portpoi)
    if (!CountTable(tname.portpoi)){
      notes<-NotePaste(notes, "No port '",csv[,'port'],"' or alike found.")
    }else{
      notes<-NotePaste(notes, "Only similar ports like '",csv[,'port'],"' found.")
      #is.similarports<-TRUE
    }
  }
#check arrival from port, poi
  tname.arrpoi<-'arr_poi'
  DropTable(tname.arrpoi)
  psql("select
          * into temporary ", tname.arrpoi, "
        from 
          asvt_arrival
        where 
          poi in (select poi from ", tname.portpoi,")
        and vessel in (select vessel from ", tname.vessel,")")
  tname.arrdest<-'arr_dest'
  DropTable(tname.arrdest)
  # psql("select
  #        * into temporary ", tname.arrdest, "
  #      from
  #       asvt_arrival
  #      where
  #       destination_arrive ilike ", csv[,'port'],
  #      "and vessel in (select vessel from ", tname.vessel, ")")
  psql("select
        * into temporary ", tname.arrdest, "
       from 
        asvt_arrival
       where ", 
       GetNameQuery(csv[,'port'], '' ,'destination_arrive'), 
       "and vessel in (select vessel from ", tname.vessel, ")")
  if (!CountTable(tname.arrpoi) && CountTable(tname.arrdest) > 0){
    notes<-NotePaste(notes, 
                     "Poi ", 
                     psql("select string_agg(distinct(poi)::text, ', ') from ", 
                          tname.arrdest)[1,1], 
                     " seems not in port ", 
                     csv[,'port'],
                     ". (maybe in ",
                     psql("select string_agg(distinct(name)::text, ', ') from as_poi, port where as_poi.port = port.code and as_poi.poi in (select poi from ",
                          tname.arrdest,
                          ")")[1,1],
                     " ?)")
  }
  tname.arrall<-'arr_all'
  DropTable(tname.arrall)
  psql("select 
        abs(date_arrive::date - date '", csv[,'operation_date'],"'),
        (date_arrive::date - date '", csv[,'operation_date'],"') as diff,
        * into temporary ", tname.arrall, " 
      from(
        select * from ", tname.arrpoi, 
        " union
        select * from ", tname.arrdest,
        ") t
      order by abs")
  closest.arr <- GetTable(tname.arrall, 1)
  if (!nrow(closest.arr)){
    if (CountTable(tname.vessel) > 0){
      found.vessel<-psql("select name, imo from ", 
                       tname.vessel) 
    #print(found.vessel)
      if (any(toupper(found.vessel[, 'name']) != 
              toupper(csv[, 'vessel']))){
        notes<-NotePaste(notes,
                        "similar vessel(imo) ",
                        paste0(found.vessel[,"name"],
                               "(", found.vessel[,'imo'], ")", 
                               collapse = ', '),
                        ' found in tanker/live.as_vessel_exp.')
        # num.dscpc <- num.dscpc + 1
        # if (closest.vessel[, 'imo'] != csv[, 'imo']){
        #   notes<-NotePaste(notes,
        #                    "Imo not matched either.", sep = ' ')
        # }
      }else if (any(found.vessel[, 'imo'] != csv[, 'imo'])){
        notes<-NotePaste(notes,
                         "Vessel ", found.vessel[1, 'name'], " found with different imo(s): ",
                         paste0(found.vessel[, 'imo'], collapse = ','))
        #num.dscpc <- num.dscpc + 1
      } 
    }
    notes<-NotePaste(notes, "No records found in asvt_arrival.")
  }else{
    #print(table.arrall)
    #closest.arr <- table.arrall[1,]
    # if (!closest.arr[,'diff']){
    #   if (!is.na(closest.arr[,'destination_arrive']) 
    #       && (toupper(closest.arr[,'destination_arrive']) 
    #           != toupper(csv[,'port']))){
    #     notes<-NotePaste(paste0(csv[, 'operation'], 
    #                             " operation_date matches date_arrive, but destination_arrive '", 
    #                             closest.arr[,'destination_arrive'],
    #                             "' does not match. (see below if any)"), 
    #                      notes, note.pos = 0)
    #   }else{
    #     notes<-NotePaste(paste0(csv[, 'operation'],
    #                             " operation_date matches date_arrive. (see below if any)"),
    #                      notes, note.pos = 0)
    #   }
    # }else{
    notes<-CheckArr(csv,
                    closest.arr,
                    tname.vessel,
                    tname.cmdtycode,
                    tname.cmdtypoi,
                    notes)
    if (closest.arr[,'diff'] < 0){
      later.arr <- psql("select * from ",
                        tname.arrall,
                        " where diff > 0 limit 1")
      #need is.empty.df(.df) method if .df = SOMEEMPTYDF[1,].
      #it has zero cols but 1 row.
      if (nrow(later.arr) > 0){
        notes<- NotePaste(notes,
                          "Also found: ",
                          CheckArr(csv,
                                  later.arr,
                                  tname.vessel,
                                  tname.cmdtycode,
                                  tname.cmdtypoi,
                                  ""))
      }else{
        notes<-NotePaste(notes, 'No later arrivals found.')
      }
    }
    # }
  }
  # DropTables(tname.vessel,
  #            tname.portpoi,
  #            tname.arrdest,
  # tname.arrpoi)
  return (data.frame(notes = notes, stringsAsFactors = F))
}

walkinchcape<-function(start = 1, 
                       end = nrow(csv), 
                       csv=ccsv, 
                       erase = (start == 1), 
                       var){
  #csv<-data.frame(csv, stringsAsFactors = F)
  
  if (erase){
    eval(substitute(
      var <- data.frame(stringsAsFactors = F)
      ), 
      envir = .GlobalEnv)
    #inchcapecheck<<-data.frame()
  }
  for (i in start:end){
    print(i)
    eval(substitute(
      var<-rbind.fill(var, IterInchcape(csv[i,]))
      ),
      envir = .GlobalEnv)
    #inchcapecheck<<-rbind.fill(inchcapecheck, IterInchcape(csv[i,]))
  }
}

insertinchcape<-function(pos = GetPos(pat, vars),
                         start = pos[1],
                         end = pos[length(pos)],
                         csv,
                         pat = 'No imo = ',
                         vars = inchcapecheck.noimo
                         ){
  #notes <- ""
  for (i in pos[pos >= start & pos <= end]){
    print(i)
    #notes<-c(notes, IterInchcape(csv[i,]))
    #print(check)
    eval(substitute(vars[i,]<- IterInchcape(csv[i,])), envir = .GlobalEnv)
    #inchcapecheck.noimo[i,]<<- IterInchcape(csv[i,])
    #print(inchcapecheck.noimo[i,])
  }
  #eval(substitute(var[pos,]<- ), envir = .GlobalEnv)
}