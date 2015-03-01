library(forecast)



convert.versions_to_periods <- function(issues, version_map){
  versions <- issues$release
  relnums <- NULL
  for(i in 1:length(versions)){
    vstr <- as.character(versions[i])
    if(versions[i] %in% names(version_map)){
      relnum <- version_map[[vstr]]
    } else {
      relnum <- -1
      cat("Issue version",vstr,"not found in map. Issue will be discarded.\n")
    }
    relnums <- append(relnums, relnum)
  }
  issues$release <- relnums
  issues <- subset(issues, issues$release != -1)
  return(issues)
}