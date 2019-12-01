yadirGetReportRec <- function(ReportType        = "CUSTOM_REPORT", 
                              DateRangeType     = "LAST_30_DAYS", 
                              DateFrom          = NULL, 
                              DateTo            = NULL, 
                              FieldNames        = c("CampaignName","Impressions","Clicks","Cost"), 
                              FilterList        = NULL,
                              Goals             = NULL,
                              AttributionModels = NULL,
                              IncludeVAT        = "YES",
                              IncludeDiscount   = "NO",
                              Login             = NULL,
                              AgencyAccount     = NULL,
                              Token             = NULL,
                              TokenPath         = getwd()) {
  st <- Sys.time()
  result <- yadirGetReportFun(ReportType, 
                             DateRangeType, 
                             DateFrom, 
                             DateTo, 
                             FieldNames, 
                             FilterList,
                             Goals,
                             AttributionModels,
                             IncludeVAT,
                             IncludeDiscount,
                             Login,
                             AgencyAccount,
                             Token,
                             TokenPath
  )
  if (nrow(result) < 1000000) {
    # if no limit
    return(result)
  } else {
    packageStartupMessage("Sorry, there is some Yandex.Direct limits. Loading extra-function to fix it...")
    # if limit then recursively divide by 2 periods till get no limit
    dates <- seq.Date(from = as.Date(DateFrom), to = as.Date(DateTo), by = "day")
    if (length(dates) %% 2 == 1)
      datesspl <- split(dates, ceiling(seq_along(dates) / (length(dates) %/% 2 + 1)))
    else
      datesspl <- split(dates, ceiling(seq_along(dates) / (length(dates) %/% 2)))
    result <- data.frame()
    for (d in datesspl) {
      df <- yadirGetReport(ReportType, 
                          DateRangeType, 
                          d[1], 
                          d[length(d)], 
                          FieldNames, 
                          FilterList,
                          Goals,
                          AttributionModels,
                          IncludeVAT,
                          IncludeDiscount,
                          Login,
                          AgencyAccount,
                          Token,
                          TokenPath
      )
      result <- rbind(result,df)
    }
    packageStartupMessage()
    packageStartupMessage("Extra-function has executed successfully.")
    packageStartupMessage("Total of total time is: ", round(Sys.time() - st), " sec.")
    return(result)
  }
}
