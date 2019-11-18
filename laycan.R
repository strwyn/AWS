#read.csv('C:/Users/Abudi.Zein/desktop/clipperw/laycan.csv', stringsAsFactors = FALSE)->table.laycan
library(plyr)
table.laycan<-psql("select * from dev.zas_lukoil_vessels")
psql("with voy as(
     select 
     a.vessel,
     a.date_depart,
     a.poi_depart,
     b.cd_name as poiname_depart,
     c.name as port_depart,
     c.country as country_depart,
     c.area as region_depart,
     a.date_arrive,
     a.poi_arrive,
     d.cd_name as poiname_arrive,
     e.name as port_arrive,
     e.country as country_arrive,
     e.area as region_arrive,
     f.crude_code as crude_name,
     probability
     from 
     v_global_crude a,
     as_poi b,
     port c,
     as_poi d,
     port e,
     cat_crude f
     where
     f.crude_code = a.grade
     and a.poi_arrive = b.poi
     and b.port = c.code
     and a.poi_depart = d.poi
     and d.port = e.code
     and date_depart >= date '2017-01-01'
     and date_depart <= date '2017-08-17'
), dv as (
     select 
     distinct(vessel),
     tanker.name
     from 
     asvt_position,
     tanker
     where
     tanker.imo = asvt_position.imo
     and tanker.name ilike '%v.%k.%eddie%'
)select
     dv.name,
     voy.vessel,
     voy.date_depart,
     voy.poiname_depart,
     voy.port_depart,
     voy.country_depart,
     voy.region_depart,
     voy.date_arrive,
     voy.poiname_arrive,
     voy.port_arrive,
     voy.country_arrive,
     voy.region_arrive,
     voy.crude_name,
     probability
     from dv, voy
     where
     voy.vessel in (select vessel from dv)")->result

psql.getvglobalcrude<-function(rec, tanker, con = connection){
  dbGetQuery(con, paste0(
    "with voy as(
    select 
    a.vessel,
    a.date_depart,
    a.poi_depart,
    b.cd_name as poiname_depart,
    c.name as port_depart,
    c.country as country_depart,
    c.area as region_depart,
    a.date_arrive,
    a.poi_arrive,
    d.cd_name as poiname_arrive,
    e.name as port_arrive,
    e.country as country_arrive,
    e.area as region_arrive,
    f.crude_code as crude_name,
    probability
    from 
    v_global_crude a,
    as_poi b,
    port c,
    as_poi d,3
    port e,
    cat_crude f
    where
    f.crude_code = a.grade
    and a.poi_arrive = b.poi
    and b.port = c.code
    and a.poi_depart = d.poi
    and d.port = e.code
    and date_depart >= date'", rec[,'Laycan.From'],
    "' and date_arrive <= date'", rec[,'Laycan.To'], 
    "'), dv as (
    select 
    distinct(vessel)
    from 
    asvt_position
    where
    asvt_position.imo = ", tanker[,'imo'],
    ") select
    voy.vessel,
    voy.date_depart,
    voy.poiname_depart,
    voy.port_depart,
    voy.country_depart,
    voy.region_depart,
    voy.date_arrive,
    voy.poiname_arrive,
    voy.port_arrive,
    voy.country_arrive,
    voy.region_arrive,
    voy.crude_name,
    probability
    from dv, voy
    where
    voy.vessel in (select vessel from dv)"
  ))
}
tname.tanker.global <- "duptanker"
tname.vessel.global <- "vessel_tanker"
tname.rec.global <- "t_rec"
tname.cjpoi.global<-"cjpoi"
#not working, dont know why
tryCatch(
  DropTables(tname.cjpoi.global, 
            tname.rec.global, 
            tname.vessel.global, 
            tname.tanker.global),
  error = function(e){print("cao")})
psql("with cjport as ", 
     "(select code, name as port_arrive ,country as country_arrive, area as region_arrive from port where country ilike 'China' or country ilike 'Japan' or country ilike 'taiwan')" ,
     "select poi as poi_arrive, cd_name as poiname_arrive, port_arrive , country_arrive, region_arrive into temporary ",
     tname.cjpoi.global, " from as_poi, cjport where as_poi.port = cjport.code;")

bestmatch<-function(laycan, vglobal){
  lc.depart<-laycan[,'laycan_from']
  lc.arrive<-laycan[,'laycan_to']
  min.row<-which.min(abs(lc.depart - vglobal[,'date_depart'])
                     +abs (lc.arrive -vglobal[,'date_arrive']))
  return (vglobal[min.row,])
  #date.depart date.arrive
}

daystonote<-function(day, has.star = TRUE){
  if (day > 0){
    if (day == 1){
      return (paste0(day, " day earlier"))
    }else{
      if (day > 10 && has.star) {
        return (paste0("**", day, " days earlier"))
      }
      return (paste0(day, " days earlier"))
    }
  }else if (day == 0){
    return ("Exact match.")
  }else{
    if (day == -1){
      return (paste0(abs(day), " day later"))
    }else{
      if (day < -10 && has.star) {
        return (paste0("**", abs(day), " days later"))
      }
      return (paste0(abs(day), " days later"))
    }
  }
}

getcjpoicountry<-function(country){
  if (country == "China"){
    return ("country_arrive ilike 'china' or country_arrive ilike 'taiwan'")
  }
  if (country == "Japan"){
    return ("country_arrive ilike 'japan'")
  }
}

getsimilarcountry<-function(country){
  if (country == "China"){
    return ("China (or Taiwan, in case)")
  }
  #under construction
  return (country)
}

checkvoy<-function(csv, 
                   tname.tanker,
                   tname.vessel,
                   tname.rec,
                   tname.cjpoi,
                   table.tanker,
                   notes,
                   full.rec,
                   vesselname){
  #print(csv)
  psql("select distinct(vessel), a.imo, b.name into temporary "
       ,tname.vessel,
       " from asvt_position a, ", 
       tname.tanker, " b where a.imo in ",
       "(select imo from ", tname.tanker,");")
       #table.tanker[1,'imo'])
  psql("select * into temporary ", 
       tname.rec, " from v_global_crude where vessel in ",
       "(select vessel from asvt_position where imo in ",
       "(select imo from ", tname.tanker, ")",
       ") and ((probability != 1 and poi_arrive = 0) or poi_arrive in (select poi_arrive from cjpoi where ",
       getcjpoicountry(csv[, 'destination']),"));")
  table.rec <- GetTable(tname.rec)
  #may need 'order by date_depart'. 
  #Dont know if v_global_crude is recorded by chronological order
  if (!nrow(table.rec)){
    notes<-NotePaste(notes, '*No record found of this vessel arriving in ',
                     getsimilarcountry(csv[, "destination"]), '.')
    full.rec[1,"name"] <- paste0("*", vesselname)
  }else{
    is.dubious<-FALSE
    is.onwater<-FALSE
    closest.rec<-bestmatch(csv,table.rec)
    if (!closest.rec[1, 'poi_arrive']){
      is.onwater <- TRUE
      notes<-NotePaste(notes, "***Vessel may be still on the water.")
      table.water<-psql("select * from ", tname.rec ," where poi_arrive = 0;")
      water.rec<- tail(table.water, 1)
      full.rec<-psql("select a.vessel as vessel_code, b.name, date_depart - date'", csv[,"laycan_from"] ,"' as than_laycan_from, date_depart, poi as poi_depart, cd_name as poiname_depart , d.name as port_depart, d.country as country_depart, d.area as region_depart, ",
                     " date_arrive - date'", csv[, "laycan_to"],"' as than_laycan_to ,date_arrive, poi_arrive, '(on the water)' as poiname_arrive, '-' as port_arrive, '-' as country_arrive, 'see ",'"probability"',"' as region_arrive ,cat_crude.name as crude_name, probability, source",
                     " from ", tname.rec, " a,",tname.vessel,
                     " b, cat_crude, as_poi c, port d where c.port = d.code and c.poi = ",
                     water.rec[,'poi_depart'],
                     " and a.poi_arrive = ", water.rec[,'poi_arrive'],
                     " and crude_code = '", water.rec[,"grade"],
                     "' and date_arrive = date'", water.rec[,"date_arrive"],
                     "' and date_depart = date'", water.rec[,"date_depart"],
                     "' and a.vessel = b.vessel")
      full.rec[1 ,"probability"]<-paste(paste(table.water[,"region_arrive"], 
                                              table.water[,"probability"], 
                                              sep= ':'), 
                                        collapse = ',')
      full.rec<-full.rec[1,]
    }else{
      if (closest.rec[,"date_arrive"] <= csv [, "laycan_from"]) {
        notes<-NotePaste(notes, "**WARNING: laycan_from date is EVEN LATER than date_arrive. This probably is not a correct record.")
        is.dubious <- TRUE
      }else if (closest.rec[,"date_depart"] >= csv [, "laycan_to"]) {
        notes<-NotePaste(notes, "**WARNING: laycan_to date is EVEN EARLIER than date_depart. This probably is not a correct record.")
        is.dubious <- TRUE
      }else{
        days.tolerance <- 10
        diff.depart<-abs(closest.rec[, "date_depart"] - csv[,"laycan_from"])
        diff.arrive<-abs(closest.rec[, "date_arrive"] - csv[,"laycan_to"])
        if (diff.depart > days.tolerance 
            && diff.arrive > days.tolerance) {
          notes<-NotePaste(notes, "**Both arrive and depart dates are more than ", 
                           days.tolerance," days later / earlier. Probably invalid record.")
        }else if(diff.depart > days.tolerance){
          notes<-NotePaste(notes, "**Depart date is ", 
                           daystonote(closest.rec[, "date_depart"] - csv[,"laycan_from"], F),
                           " (more than ", 
                           days.tolerance," days).")
        }else if(diff.arrive > days.tolerance){
          notes<-NotePaste(notes, "**Arrive date is ", 
                           daystonote(closest.rec[, "date_arrive"] - csv[,"laycan_to"], F),
                           " (more than ", 
                           days.tolerance," days).")
        }
      }
      full.rec<-psql("select a.vessel as vessel_code, b.name, date'", csv[,"laycan_from"] ,"' - date_depart as than_laycan_from, date_depart, poi as poi_depart, cd_name as poiname_depart , d.name as port_depart, d.country as country_depart, d.area as region_depart, ",
                     " date'", csv[, "laycan_to"],"' - date_arrive as than_laycan_to , date_arrive, e.*, cat_crude.name as crude_name, probability, source",
                     " from ", tname.rec, " a,",tname.vessel,
                     " b, cat_crude, as_poi c, port d, ", 
                     tname.cjpoi , " e where c.port = d.code and c.poi = ",
                     closest.rec[,'poi_depart'], 
                     " and e.poi_arrive = ", closest.rec[,'poi_arrive'],
                     " and crude_code = '", closest.rec[,"grade"],
                     "' and date_arrive = date'", closest.rec[,"date_arrive"],
                     "' and date_depart = date'", closest.rec[,"date_depart"],
                     "' and a.vessel = b.vessel")
      if (nrow(full.rec) > 1){
        full.rec[1, "crude_name"]<-paste(full.rec[,"crude_name"], collapse = ',')
        full.rec<-full.rec[1,]
      }
    }
    if (is.dubious) {
      full.rec[1, "than_laycan_to"]<-"**"
      full.rec[1, "than_laycan_from"]<-"**"
    }else if (is.onwater){
      full.rec[1, "than_laycan_to"]<-"***"
      full.rec[1, "than_laycan_from"]<-daystonote(full.rec[1, "than_laycan_from"])
    }else{
      full.rec[1, "than_laycan_to"]<-daystonote(full.rec[1, "than_laycan_to"])
      full.rec[1, "than_laycan_from"]<-daystonote(full.rec[1, "than_laycan_from"])
    }
  }
  DropTable(tname.vessel)
  DropTable(tname.rec)
  #print(full.rec)
  #print(i)
  return (list(full.rec, notes))
}

laycaniter<-function(csv, 
                     tname.tanker = tname.tanker.global,
                     tname.vessel = tname.vessel.global,
                     tname.rec = tname.rec.global,
                     tname.cjpoi = tname.cjpoi.global){
  
  notes<-""
  full.rec<-data.frame()
  vesselname <- csv[, 'vessel']
  psql("select * into temporary ",
       tname.tanker,
       " from tanker where name ilike '", vesselname, "';")
  table.tanker<-GetTable(tname.tanker)
  if (!nrow(table.tanker)){
    vesselslice<-paste0("%", 
                        paste0(strsplit(vesselname, " ")[[1]], collapse = "%"), 
                        "%")
    table.liketanker<-psql("select * from tanker where name ilike '", 
                           vesselslice, "';")
    if (!nrow(table.liketanker)){
      notes<-NotePaste(notes, "*No vessels named (or like) ",
                    vesselname, " is found")
      if (!is.null(full.rec[1,"name"]) 
          && substr(full.rec[1,"name"] , 1,1) != "*"){
        full.rec[1,"name"] <- paste0("*", full.rec[1,"name"])
      }
    }else{
      check.result<- checkvoy(csv, 
                              tname.tanker,
                              tname.vessel,
                              tname.rec,
                              tname.cjpoi,
                              table.tanker,
                              notes,
                              full.rec,
                              vesselname)
      full.rec<-check.result[[1]]
      notes<-NotePaste(notes, "*No vessel named ",
                       vesselname, " found. Maybe ",
                       paste0(table.liketanker[,'name'], collapse = ', '),"?")
      notes<-NotePaste(notes, check.result[[2]])
    }
    if (!is.null(full.rec[1,"name"]) 
        && substr(full.rec[1,"name"] , 1,1) != "*"){
      full.rec[1,"name"] <- paste0("*", full.rec[1,"name"])
    }
  }else if (nrow(table.tanker) == 1){
    check.result<- checkvoy(csv, 
                            tname.tanker,
                            tname.vessel,
                            tname.rec,
                            tname.cjpoi,
                            table.tanker,
                            notes,
                            full.rec,
                            vesselname)
    full.rec<-check.result[[1]]
    notes<-NotePaste(notes, check.result[[2]])
  }else{
    
    #full.rec[1,"name"]<-paste0(vesselname, "(as dev.zas_lukoil_vessels provided)")
    check.result<-checkvoy(csv, 
                           tname.tanker,
                           tname.vessel,
                           tname.rec,
                           tname.cjpoi,
                           table.tanker,
                           notes,
                           full.rec,
                           vesselname)
    full.rec<-check.result[[1]]
    notes<-NotePaste(notes, "*More than one vessel named ",
                     vesselname, " are found.")
    notes<-NotePaste(notes, check.result[[2]])
    if (!is.null(full.rec[1,"name"]) 
        && substr(full.rec[1,"name"] , 1,1) != "*"){
      full.rec[1,"name"] <- paste0("*", full.rec[1,"name"])
    }
  }
  DropTable(tname.tanker)
  full.rec[1, "*notes (double click to see multiple lines)"]<-notes
  #print(full.rec)
  return (full.rec)
}

laycaniter.global<- function(csv, 
                             tname.tanker = tname.tanker.global,
                             tname.vessel = tname.vessel.global,
                             tname.rec = tname.rec.global,
                             tname.cjpoi = tname.cjpoi.global){
  notes<-""
  full.rec<-data.frame()
  vesselname <- csv[, 'vessel']
  psql("select * into temporary ",
       tname.tanker,
       " from tanker where name ilike '", vesselname, "';")
  table.tanker<-GetTable(tname.tanker)
  if (!nrow(table.tanker)){
    vesselslice<-paste0("%", 
                        paste0(strsplit(vesselname, " ")[[1]], collapse = "%"), 
                        "%")
    table.liketanker<-psql("select * from tanker where name ilike '", 
                           vesselslice, "';")
    if (!nrow(table.liketanker)){
      notes<-NotePaste(notes, "*No vessels named (or like) ",
                       vesselname, " is found")
      full.rec[1,"name"] <- paste0("*", vesselname)
    }else{
      check.result<- checkvoy(csv, 
                              tname.tanker,
                              tname.vessel,
                              tname.rec,
                              tname.cjpoi,
                              table.tanker,
                              notes,
                              full.rec,
                              vesselname)
      full.rec<-check.result[[1]]
      if (!is.null(full.rec[1,"name"]) 
          && substr(full.rec[1,"name"] , 1,1) != "*"){
        full.rec[1,"name"] <- paste0("*", full.rec[1,"name"])
      }
      notes<-NotePaste(notes, "*No vessel named ",
                       vesselname, " found. Maybe ",
                       paste0(table.liketanker[,'name'], collapse = ', '),"?")
      notes<-NotePaste(notes, check.result[[2]])
    }
    print("not found")
  }else if (nrow(table.tanker) == 1){
    check.result<- checkvoy(csv, 
                            tname.tanker,
                            tname.vessel,
                            tname.rec,
                            tname.cjpoi,
                            table.tanker,
                            notes,
                            full.rec,
                            vesselname)
    full.rec<-check.result[[1]]
    notes<-NotePaste(notes, check.result[[2]])
  }else{
    check.result<-checkvoy(csv, 
                           tname.tanker,
                           tname.vessel,
                           tname.rec,
                           tname.cjpoi,
                           table.tanker,
                           notes,
                           full.rec,
                           vesselname)
    full.rec<-check.result[[1]]
    if (!is.null(full.rec[1,"name"]) 
        && substr(full.rec[1,"name"] , 1,1) != "*"){
      full.rec[1,"name"] <- paste0("*", full.rec[1,"name"])
    }
    notes<-NotePaste(notes, "*More than one vessel named ",
                     vesselname, " are found.")
    notes<-NotePaste(notes, check.result[[2]])
  }
  DropTable(tname.tanker)
  full.rec[1, "*notes (double click to see multiple lines)"]<-notes
  #print(full.rec)
  return (full.rec)
}

walklaycan.global<-function(start = 1,
                            end = nrow(table.laycan),
                            erase = FALSE,
                            csv = table.laycan[1:end,]){
  if (erase){
    laycancheck<<-data.frame()
  }  
  for (i in start:end){
    print(i)
    laycancheck <<- rbind.fill(laycancheck, laycaniter.global(csv = csv[i,]))
  }
}

walklaycan<-function(end = nrow(table.laycan), csv = table.laycan[1:end,]){
  #return(sapply(1:nrow(csv), FUN = laycaniter, csv = csv))
  adply(csv[1:end,], .margins = 1, .id = NULL, .fun = laycaniter)
}

insertlaycan<-function(pos, 
                       start = pos[1],
                       end = pos[length(pos)], 
                       csv = table.laycan){
  ncol.csv<-ncol(csv)
  sapply(pos[pos>=start & pos<= end], function(i){
    print(i)
    good<<-laycaniter.global(csv = csv[i,])
    if (ncol(good) != ncol(csv)){
      for (name in colnames(good)){
        tmp.laycancheck[i, name] <<- good[ ,name]
      }
    }else{
      tmp.laycancheck<<-good
    }
  })
}
