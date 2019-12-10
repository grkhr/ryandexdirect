yadirGetCampaignList <-
function (Logins          = NULL,
          States          = c("OFF","ON","SUSPENDED","ENDED","CONVERTED","ARCHIVED"),
          Types           = c("TEXT_CAMPAIGN","MOBILE_APP_CAMPAIGN","DYNAMIC_TEXT_CAMPAIGN","CPM_BANNER_CAMPAIGN"),
          Statuses        = c("ACCEPTED","DRAFT","MODERATION","REJECTED"),
          StatusesPayment = c("DISALLOWED","ALLOWED"),
          Token           = NULL,
          AgencyAccount = NULL,
          TokenPath     = getwd()) {

# set start time
start_time  <- Sys.time()

# base data frame
result       <- data.frame(Id = character(0),
                           Name = character(0),
                           Type = character(0),
                           Status = character(0),
                           State = character(0),
                           DailyBudgetAmount = double(0),
                           DailyBudgetMode = character(0),
                           Currency = character(0),
                           StartDate = as.Date(character(0)),
                           Impressions = integer(0),
                           Clicks = integer(0),
                           ClientInfo = character(0),
                           Login = character(0),
                           stringsAsFactors=FALSE)

# checking if beta-campaigns is activated
if (Types[1] == "ALL") Types <- c("TEXT_CAMPAIGN", "MOBILE_APP_CAMPAIGN", "DYNAMIC_TEXT_CAMPAIGN", "CPM_BANNER_CAMPAIGN", "BETAS")
Betas <- Types[Types == "BETAS"]
Types <- Types[Types != "BETAS"]

# filters
States          <- paste("\"",States,"\"",collapse=", ",sep="")
Types           <- paste("\"",Types,"\"",collapse=", ",sep="")
Statuses        <- paste("\"",Statuses,"\"",collapse=", ",sep="")
StatusesPayment <- paste("\"",StatusesPayment,"\"",collapse=", ",sep="")

# offset starting
lim <- 0

# start message
packageStartupMessage("Processing", appendLF = F)

# main cycle
while(lim != "stoped"){
# forming the body of a query
queryBody <- paste0("{
  \"method\": \"get\",
  \"params\": {
    \"SelectionCriteria\": {
                      \"States\": [",States,"],
                      \"Types\": [",Types,"],
                      \"StatusesPayment\": [",StatusesPayment,"],
                      \"Statuses\": [",Statuses,"]},
    \"FieldNames\": [
                    \"Id\",
                    \"Name\",
                    \"Type\",
                    \"StartDate\",
                    \"Status\",
                    \"State\",
                    \"Statistics\",
                    \"Currency\",
                    \"DailyBudget\",
                    \"ClientInfo\"],
    \"Page\": {
      \"Limit\": 10000,
      \"Offset\": ",lim,"
    }
  }
}")



    for(l in 1:length(Logins)){
      # auth
      Token <- tech_auth(login = Logins[l], token = Token, AgencyAccount = AgencyAccount, TokenPath = TokenPath)

      answer <- POST("https://api.direct.yandex.com/json/v5/campaigns", body = queryBody, add_headers(Authorization = paste0("Bearer ",Token), 'Accept-Language' = "ru","Client-Login" = Logins[l]))
      # answer processing
      stop_for_status(answer)
      dataRaw <- content(answer, "parsed", "application/json")

        if(length(dataRaw$error) > 0){
            stop(paste0(dataRaw$error$error_string, " - ", dataRaw$error$error_detail))
           }

      # answer parsing
      for (i in 1:length(dataRaw$result$Campaigns)){

        try(result <- rbind(result,
                        data.frame(Id                 = dataRaw$result$Campaigns[[i]]$Id,
                                   Name               = dataRaw$result$Campaigns[[i]]$Name,
                                   Type               = dataRaw$result$Campaigns[[i]]$Type,
                                   Status             = dataRaw$result$Campaigns[[i]]$Status,
                                   State              = dataRaw$result$Campaigns[[i]]$State,
                                   DailyBudgetAmount  = ifelse(is.null(dataRaw$result$Campaigns[[i]]$DailyBudget$Amount), NA, dataRaw$result$Campaigns[[i]]$DailyBudget$Amount / 1000000),
                                   DailyBudgetMode    = ifelse(is.null(dataRaw$result$Campaigns[[i]]$DailyBudget$Mode), NA, dataRaw$result$Campaigns[[i]]$DailyBudget$Mode),
                                   Currency           = dataRaw$result$Campaigns[[i]]$Currency,
                                   StartDate          = dataRaw$result$Campaigns[[i]]$StartDate,
                                   Impressions        = ifelse(is.null(dataRaw$result$Campaigns[[i]]$Statistics$Impressions), NA,dataRaw$result$Campaigns[[i]]$Statistics$Impressions),
                                   Clicks             = ifelse(is.null(dataRaw$result$Campaigns[[i]]$Statistics$Clicks), NA,dataRaw$result$Campaigns[[i]]$Statistics$Clicks),
                                   ClientInfo         = dataRaw$result$Campaigns[[i]]$ClientInfo,
                                   Login              = Logins[l])), silent = T)

      }
    }

  packageStartupMessage(".", appendLF = F)
  #????????? ???????? ?? ??? ?????? ??????? ???? ???????
  lim <- ifelse(is.null(dataRaw$result$LimitedBy), "stoped",dataRaw$result$LimitedBy + 1)
}

# parsing beta-campaigns through yadirGetReport
if (length(Betas) > 0)
{
  camp_list <- c("MCBANNER_CAMPAIGN",
                 "SMART_CAMPAIGN",
                 "CONTENT_PROMOTION_CAMPAIGN",
                 "CPM_DEALS_CAMPAIGN",
                 "CPM_FRONTPAGE_CAMPAIGN",
                 "MCBANNER_CAMPAIGN")
  betacampaigns <- suppressWarnings(suppressMessages(yadirGetReport(ReportType = "CAMPAIGN_PERFORMANCE_REPORT",
                                                                    DateRangeType = "ALL_TIME",
                                                                    FieldNames = c("CampaignId","CampaignName","CampaignType","Impressions","Clicks"),
                                                                    Login = Logins[l],
                                                                    Token = Token
                                                                    ,
                                                                    FilterList = c(paste0("CampaignType IN ",paste0(camp_list, collapse = ";")))
                                                                    )))

  # merging main result with beta-campaigns
  if (nrow(betacampaigns))
  {
    colnames(betacampaigns) <- gsub("Campaign","",names(betacampaigns))
    betacampaigns$Login <- Logins[l]
    betacampaigns$ClientInfo <- dataRaw$result$Campaigns[[i]]$ClientInfo
    myList <- list(result, betacampaigns)
    result <- merge(myList[[1]], myList[[2]], all = TRUE, sort = FALSE)
  }
}

# changing column's classes to factors
result$Type <- as.factor(result$Type)
result$Status <- as.factor(result$Status)
result$State <- as.factor(result$State)
result$Currency <- as.factor(result$Currency)

# set end time
stop_time <- Sys.time()

# done message
packageStartupMessage("Done", appendLF = T)
packageStartupMessage(paste0("Number of campaigns received: ", nrow(result)), appendLF = T)
packageStartupMessage(paste0("Processing time: ", round(difftime(stop_time, start_time , units ="secs"),0), " sec."), appendLF = T)
#?????????? ?????????
return(result)
}
