fillMisvalue <- function(dt){
  Nas <- NaCol(dt)
  naData <- dt[,names(Nas)]
  NaClassNum <- names(sapply(naData,class)[sapply(naData,class)=="numeric"])
  NaClassChar <- names(sapply(naData,class)[sapply(naData,class)=="character"])
  for (c in NaClassNum) {
    dt[][[c]] <- if_else(is.na(dt[][[c]]),round(mean(dt[][[c]],na.rm = TRUE)),dt[][[c]])
  }
  for (d in NaClassChar) {
    dt[][[d]] <- if_else(is.na(dt[][[d]]),"none",dt[][[d]])
  }
return(dt)
}

combineData <- fillMisvalue(combineData)
View(combineData)
NaCol(combineData)

spreadOutCol <- function(dt,dcName){
  for (c in dcName) {
    dt <- dt %>% spread(key = c,value = c)
  }
  
  dt <- fillMisBin(dt)
  
  return(dt)
}


fillMisBin <- function(dt){
  Nas <- NaCol(dt)
  NaClassNum <- names(Nas)
  for (c in NaClassNum) {
    dt[][[c]] <- if_else(is.na(dt[][[c]]),1,0)
  }
  return(dt)
}



