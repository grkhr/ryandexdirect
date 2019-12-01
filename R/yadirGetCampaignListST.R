yadirGetCampaignListST <- function (Logins = NULL, States = c("OFF", "ON", "SUSPENDED",
                                                                "ENDED", "CONVERTED", "ARCHIVED"), Types = c("TEXT_CAMPAIGN",
                                                                                                             "MOBILE_APP_CAMPAIGN", "DYNAMIC_TEXT_CAMPAIGN", "CPM_BANNER_CAMPAIGN"), Statuses = c("ACCEPTED",
                                                                                                                                                                                                  "DRAFT", "MODERATION", "REJECTED"), StatusesPayment = c("DISALLOWED",
                                                                                                                                                                                                                                                          "ALLOWED"), Token = NULL, AgencyAccount = NULL, TokenPath = getwd())
{
  start_time <- Sys.time()
  result <- data.frame(Id = character(0), Name = character(0),
                       Type = character(0), Status = character(0), State = character(0),
                       DailyBudgetAmount = double(0), DailyBudgetMode = character(0),
                       Currency = character(0), StartDate = as.Date(character(0)),
                       Impressions = integer(0), Clicks = integer(0), ClientInfo = character(0),
                       Login = character(0), stringsAsFactors = FALSE)
  States <- paste("\"", States, "\"", collapse = ", ", sep = "")
  Types <- paste("\"", Types, "\"", collapse = ", ", sep = "")
  Statuses <- paste("\"", Statuses, "\"", collapse = ", ",
                    sep = "")
  StatusesPayment <- paste("\"", StatusesPayment, "\"", collapse = ", ",
                           sep = "")
  lim <- 0
  packageStartupMessage("Processing", appendLF = F)
  while (lim != "stoped") {
    queryBody <- paste0("{\n  \"method\": \"get\",\n  \"params\": { \n    \"SelectionCriteria\": {\n                      \"States\": [",
                        States, "],        \n                      \"Types\": [",
                        Types, "],\n                      \"StatusesPayment\": [",
                        StatusesPayment, "],\n                      \"Statuses\": [",
                        Statuses, "]},\n    \"FieldNames\": [\n                    \"Id\",\n                    \"Name\",\n                    \"Type\",\n                    \"StartDate\",\n                    \"Status\",\n                    \"State\",\n                    \"Statistics\",\n                    \"Currency\",\n                    \"DailyBudget\",\n                    \"ClientInfo\"],\n    \"Page\": {  \n      \"Limit\": 10000,\n      \"Offset\": ",
                        lim, "\n    }\n  }\n}")
    for (l in 1:length(Logins)) {
      # Token <- tech_auth(login = Logins[l], token = Token,
      #                   AgencyAccount = AgencyAccount, TokenPath = TokenPath)
      answer <- POST("https://api.direct.yandex.com/json/v5/campaigns",
                     body = queryBody, add_headers(Authorization = paste0("Bearer ",
                                                                          Token), `Accept-Language` = "ru", `Client-Login` = Logins[l]))
      stop_for_status(answer)
      dataRaw <- content(answer, "parsed", "application/json")
      if (length(dataRaw$error) > 0) {
        stop(paste0(dataRaw$error$error_string, " - ",
                    dataRaw$error$error_detail))
      }
      for (i in 1:length(dataRaw$result$Campaigns)) {
        try(result <- rbind(result, data.frame(Id = dataRaw$result$Campaigns[[i]]$Id,
                                               Name = dataRaw$result$Campaigns[[i]]$Name,
                                               Type = dataRaw$result$Campaigns[[i]]$Type,
                                               Status = dataRaw$result$Campaigns[[i]]$Status,
                                               State = dataRaw$result$Campaigns[[i]]$State,
                                               DailyBudgetAmount = ifelse(is.null(dataRaw$result$Campaigns[[i]]$DailyBudget$Amount),
                                                                          NA, dataRaw$result$Campaigns[[i]]$DailyBudget$Amount/1e+06),
                                               DailyBudgetMode = ifelse(is.null(dataRaw$result$Campaigns[[i]]$DailyBudget$Mode),
                                                                        NA, dataRaw$result$Campaigns[[i]]$DailyBudget$Mode),
                                               Currency = dataRaw$result$Campaigns[[i]]$Currency,
                                               StartDate = dataRaw$result$Campaigns[[i]]$StartDate,
                                               Impressions = ifelse(is.null(dataRaw$result$Campaigns[[i]]$Statistics$Impressions),
                                                                    NA, dataRaw$result$Campaigns[[i]]$Statistics$Impressions),
                                               Clicks = ifelse(is.null(dataRaw$result$Campaigns[[i]]$Statistics$Clicks),
                                                               NA, dataRaw$result$Campaigns[[i]]$Statistics$Clicks),
                                               ClientInfo = dataRaw$result$Campaigns[[i]]$ClientInfo,
                                               Login = Logins[l])), silent = T)
      }
    }
    packageStartupMessage(".", appendLF = F)
    lim <- ifelse(is.null(dataRaw$result$LimitedBy), "stoped",
                  dataRaw$result$LimitedBy + 1)
  }
  result$Type <- as.factor(result$Type)
  result$Status <- as.factor(result$Status)
  result$State <- as.factor(result$State)
  result$Currency <- as.factor(result$Currency)
  stop_time <- Sys.time()
  packageStartupMessage("Done", appendLF = T)
  packageStartupMessage(paste0("aefegega: ", nrow(result)), appendLF = T)
  packageStartupMessage(paste0("aegegag: ", round(difftime(stop_time, start_time , units ="secs"),0), " ???."), appendLF = T)
  return(result)
}
