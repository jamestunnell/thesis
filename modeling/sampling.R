require(timeDate)

sample.slocs.dateRange <- function(slocs, startdate, enddate, period){
  startdate <- timeDate(startdate)
  enddate <- timeDate(enddate)
  
  datebreaks <- align(c(startdate,enddate), by=paste0(period,"d"))
  period_days <- as.integer(datebreaks[2]-datebreaks[1], units="days")
  last_datebreak <- timeDate(as.Date(tail(datebreaks,1))+period_days)
  nperiods <- length(datebreaks)
  datebreaks <- append(datebreaks, last_datebreak)
  
  sloc_totals <- NULL
  for(i in 1:(nperiods-1)){
    l <- datebreaks[i]
    r <- datebreaks[i+1]
    if((i+1) < nperiods){
      inrange = slocs$date >= l & slocs$date < r
    } else {
      inrange = slocs$date >= l & slocs$date <= r
    }
    tot <- sum(slocs$sloc[inrange])
    sloc_totals <- append(sloc_totals,tot)
  }
  return(sloc_totals)
}

sample.issues.dateRange <- function(issues, startdate, enddate, period){
  startdate <- timeDate(startdate)
  enddate <- timeDate(enddate)
  
  datebreaks <- align(c(startdate,enddate), by=paste0(period,"d"))
  period_days <- as.integer(datebreaks[2]-datebreaks[1], units="days")
  last_datebreak <- timeDate(as.Date(tail(datebreaks,1))+period_days)
  nperiods <- length(datebreaks)
  datebreaks <- append(datebreaks, last_datebreak)
  
  bugs.active <- NULL
  imps.active <- NULL
  news.active <- NULL
  tsks.active <- NULL
  
  bugs.created <- NULL
  imps.resolved <- NULL
  news.resolved <- NULL
  tsks.created <- NULL
  
  dates <- NULL
  
  for(i in 1:(nperiods-1)){
    l <- datebreaks[i]
    r <- datebreaks[i+1]
    
    created <- issues$created >= l & issues$created < r
    resolved <- issues$resolved >= l & issues$resolved < r
    active <- created | (issues$created < l & issues$resolved >= l)
    
    nbugs.active <- length(which(issues$type == "bug" & active))
    nimps.active <- length(which(issues$type == "improvement" & active))
    nnews.active <- length(which(issues$type == "newfeature" & active))
    ntsks.active <- length(which(issues$type == "task" & active))
    
    bugs.active <- append(bugs.active, nbugs.active)
    imps.active <- append(imps.active, nimps.active)
    news.active <- append(news.active, nnews.active)
    tsks.active <- append(tsks.active, ntsks.active)
    
    nbugs.created <- length(which(issues$type == "bug" & created))
    nimps.resolved <- length(which(issues$type == "improvement" & resolved))
    nnews.resolved <- length(which(issues$type == "newfeature" & resolved))
    ntsks.created <- length(which(issues$type == "task" & created))
    
    bugs.created <- append(bugs.created, nbugs.created)
    imps.resolved <- append(imps.resolved, nimps.resolved)
    news.resolved <- append(news.resolved, nnews.resolved)
    tsks.created <- append(tsks.created, ntsks.created)
    
    dates <- append(dates,as.Date(l))
  }
  
  df <- data.frame(list(date=dates,
                        bugs.created=bugs.created, bugs.active=bugs.active,
                        imps.resolved=imps.resolved, imps.active=imps.active,
                        news.resolved=news.resolved, news.active=news.active,
                        tsks.created=tsks.created, tsks.active=tsks.active))
#                         bugs.effect=bugs.mulcreated, imps.effect=imps.mulresolved,
#                         news.effect=news.mulresolved))
  return(df)
}