yadirGetAds <- function(CampaignIds   = NULL, 
                        AdGroupIds    = NA, 
                        Ids           = NA, 
                        States        = c("OFF","ON","SUSPENDED","OFF_BY_MONITORING","ARCHIVED"), 
                        Login         = NULL,
                        Token         = NULL,
                        AgencyAccount = NULL,
                        TokenPath     = getwd()){
  
  # auth
  Token <- ryandexdirect:::tech_auth(login = Login, token = Token, AgencyAccount = AgencyAccount, TokenPath = TokenPath)
  
  # check campaign filter
  if (is.null(CampaignIds)) {
    message("You haven't chosen any ids of campaign, adgroup or ad. Loading full campaign list without beta-campaigns.")
    CampaignIds <-  yadirGetCampaignList(Login         = Login,
                                         AgencyAccount = AgencyAccount,
                                         Token         = Token,
                                         TokenPath     = TokenPath)$Id
  }
  
  # set start time
  start_time  <- Sys.time()
  
  # result frame
  result      <- data.frame(Id                  = integer(0), 
                            AdGroupId           = integer(0),
                            CampaignId          = integer(0),
                            Type                = character(0),
                            Subtype             = character(0),
                            Status              = character(0),
                            AgeLabel            = character(0),
                            State               = character(0),
                            TextAdTitle         = character(0),
                            TextAdTitle2        = character(0),
                            TextAdText          = character(0),
                            TextAdHref          = character(0),
                            TextAdDisplayDomain = character(0),
                            TextAdMobile        = character(0),
                            TextImageAdHref     = character(0),
                            TextVideoAdHref = character(0),
                            TextMediaAdHref = character(0),
                            DynamicTextAdText = character(0),
                            DynamicTextAdDomainUrl = character(0),
                            DynamicTextAdTrackingParams = character(0),
                            DynamicTextAdSitelinkSetId = integer(0),
                            TextAdSitelinkSetId = integer(0))
  
  # states to json for query
  States          <- paste("\"",States,"\"",collapse=", ",sep="")
  
  # camp queue
  camp_num     <- as.integer(length(CampaignIds))
  camp_start   <- 1
  camp_step    <- 10
  
  packageStartupMessage("Processing", appendLF = F)
  
  # getting params for dynamic text ads
  DynamicGroups <- suppressWarnings(suppressMessages(yadirGetAdGroups(CampaignIds = CampaignIds,
                                    Types = 'DYNAMIC_TEXT_AD_GROUP',
                                    Login = Login,
                                    Token = Token)))
  if (nrow(DynamicGroups) > 0)
  {
    DynamicGroups <- data.frame(DynamicGroups$Id,DynamicGroups$DynamicTextAdGroupDomainUrl,DynamicGroups$TrackingParams)
    colnames(DynamicGroups) <- c('AdGroupId','DynamicTextAdDomainUrl','DynamicTextAdTrackingParams')
  }
  # campaign cycle
  while(camp_start <= camp_num){
    
    # how ,ani campaing need processing
    camp_step   <-  if(camp_num - camp_start >= 10) camp_step else camp_num - camp_start + 1
    
    # prepare camp list for iteration
    Ids             <- ifelse(any(is.na(Ids)), NA,paste0(Ids, collapse = ","))
    AdGroupIds      <- ifelse(any(is.na(AdGroupIds)),NA,paste0(AdGroupIds, collapse = ","))
    CampaignIdsTmp  <- paste("\"",CampaignIds[camp_start:(camp_start + camp_step - 1)],"\"",collapse=", ",sep="")
    
    # lim offset
    lim <- 0
    
    while(lim != "stoped"){
      
    #   queryBody <- paste0("{
    #                       \"method\": \"get\",
    #                       \"params\": {
    #                       \"SelectionCriteria\": {
    #                       \"CampaignIds\": [",CampaignIdsTmp,"],
    #                       ",ifelse(is.na(Ids),"",paste0("\"Ids\": [",Ids,"],")),"        
    #                       ",ifelse(is.na(AdGroupIds),"",paste0("\"AdGroupIds\": [",AdGroupIds,"],")),"
    #                       \"States\": [",States,"]
    # },
    #                       
    #                       \"FieldNames\": [
    #                       \"Id\",
    #                       \"CampaignId\",
    #                       \"AdGroupId\",
    #                       \"Status\",
    #                       \"State\",
    #                       \"AgeLabel\",
    #                       \"Type\",
    #                       \"Subtype\"],
    #                       \"TextAdFieldNames\": [
    #                       \"Title\",
    #                       \"Title2\",
    #                       \"Text\",
    #                       \"Href\",
    #                       \"Mobile\",
    #                       \"SitelinkSetId\",
    #                       \"DisplayDomain\"],
    #                       \"TextImageAdFieldNames\": [
    #                       \"Href\"],
    #                       \"Page\": {  
    #                       \"Limit\": 10000,
    #                       \"Offset\": ",lim,"}
    # }
    # }")
      queryBody <- paste0("{
                          \"method\": \"get\",                     
                          \"params\": {
                          \"SelectionCriteria\": {
                          \"CampaignIds\": [", CampaignIdsTmp, "],
                          ", ifelse(is.na(Ids), "", paste0("\"Ids\": [",Ids, "],")), "                  
                          ",ifelse(is.na(AdGroupIds), "", paste0("\"AdGroupIds\": [", AdGroupIds, "],")),"
                          \"States\": [",States, "]
  },                      
                          \"FieldNames\": [                      
                          \"Id\",                      
                          \"CampaignId\",                     
                          \"AdGroupId\",                     
                          \"Status\",                     
                          \"State\",                     
                          \"AgeLabel\",                      
                          \"Type\",                    
                          \"Subtype\"],                     
                          \"TextAdFieldNames\": [                     
                          \"Title\",                      
                          \"Title2\",                    
                          \"Text\",                     
                          \"Href\",                     
                          \"Mobile\",  
                          \"SitelinkSetId\",
                          \"DisplayDomain\"],                    
                          \"TextImageAdFieldNames\": [                     
                          \"Href\"],   
                          \"DynamicTextAdFieldNames\": [                     
                          \"Text\",                      
                          \"SitelinkSetId\"],  
                          \"CpcVideoAdBuilderAdFieldNames\": [                     
                          \"Href\"],      
                          \"CpmBannerAdBuilderAdFieldNames\": [                      
                          \"Href\"],                 
                          \"Page\": {
                          \"Limit\": 10000,                      
                          \"Offset\": ", 
                          lim, "}
                          }
                          }")
      answer <- POST("https://api.direct.yandex.com/json/v5/ads", body = queryBody, add_headers(Authorization = paste0("Bearer ",Token), 'Accept-Language' = "ru",'Client-Login' = Login))
      stop_for_status(answer)
      dataRaw <- content(answer, "parsed", "application/json")

      # check erroe
      if(length(dataRaw$error) > 0){
        stop(paste0(dataRaw$error$error_string, " - ", dataRaw$error$error_detail))
      }
      
      # parse list and add to result frame
      if (length(dataRaw$result$Ads) > 0){
        for(ads_i in seq_along(1:length(dataRaw$result$Ads))){
          result      <- rbind(result,
                               data.frame(Id                  = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$Id), NA,dataRaw$result$Ads[[ads_i]]$Id), 
                                          AdGroupId           = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$AdGroupId), NA,dataRaw$result$Ads[[ads_i]]$AdGroupId),
                                          CampaignId          = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$CampaignId), NA,dataRaw$result$Ads[[ads_i]]$CampaignId),
                                          Type                = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$Type), NA,dataRaw$result$Ads[[ads_i]]$Type),
                                          Subtype             = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$Subtype), NA,dataRaw$result$Ads[[ads_i]]$Subtype),
                                          Status              = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$Status), NA,dataRaw$result$Ads[[ads_i]]$Status),
                                          AgeLabel            = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$AgeLabel), NA,dataRaw$result$Ads[[ads_i]]$AgeLabel),
                                          State               = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$State), NA,dataRaw$result$Ads[[ads_i]]$State),
                                          TextAdTitle         = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$TextAd$Title), NA,dataRaw$result$Ads[[ads_i]]$TextAd$Title),
                                          TextAdTitle2        = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$TextAd$Title2), NA,dataRaw$result$Ads[[ads_i]]$TextAd$Title2),
                                          TextAdText          = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$TextAd$Text), NA,dataRaw$result$Ads[[ads_i]]$TextAd$Text),
                                          TextAdHref          = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$TextAd$Href), NA,dataRaw$result$Ads[[ads_i]]$TextAd$Href),
                                          TextAdDisplayDomain = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$TextAd$DisplayDomain), NA,dataRaw$result$Ads[[ads_i]]$TextAd$DisplayDomain),
                                          TextAdMobile        = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$TextAd$Mobile), NA,dataRaw$result$Ads[[ads_i]]$TextAd$Mobile),
                                          TextVideoAdHref = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$CpcVideoAdBuilderAd$Href), NA, dataRaw$result$Ads[[ads_i]]$CpcVideoAdBuilderAd$Href), 
                                          TextMediaAdHref = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$CpmBannerAdBuilderAd$Href), NA, dataRaw$result$Ads[[ads_i]]$CpmBannerAdBuilderAd$Href), 
                                          DynamicTextAdText = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$DynamicTextAd$Text), NA, dataRaw$result$Ads[[ads_i]]$DynamicTextAd$Text),
                                          DynamicTextAdDomainUrl = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$DynamicTextAd$Text), NA, as.character(subset(DynamicGroups, DynamicGroups$AdGroupId == dataRaw$result$Ads[[ads_i]]$AdGroupId)$DynamicTextAdDomainUrl)),
                                          DynamicTextAdTrackingParams = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$DynamicTextAd$Text), NA, as.character(subset(DynamicGroups, DynamicGroups$AdGroupId == dataRaw$result$Ads[[ads_i]]$AdGroupId)$DynamicTextAdTrackingParams)),
                                          DynamicTextAdSitelinkSetId = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$DynamicTextAd$SitelinkSetId), NA, dataRaw$result$Ads[[ads_i]]$DynamicTextAd$SitelinkSetId),
                                          TextImageAdHref     = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$TextImageAd$Href), NA,dataRaw$result$Ads[[ads_i]]$TextImageAd$Href),
                                          TextAdSitelinkSetId = ifelse(is.null(dataRaw$result$Ads[[ads_i]]$TextAd$SitelinkSetId), NA, dataRaw$result$Ads[[ads_i]]$TextAd$SitelinkSetId)))
        }
        # add point to progress bar
        packageStartupMessage(".", appendLF = F)
      }
      # if last iteration change lim
      lim <- ifelse(is.null(dataRaw$result$LimitedBy), "stoped",dataRaw$result$LimitedBy + 1)
    }
    
    # set offset to next iteration
    camp_start <- camp_start + camp_step
  }
  
  
  # getting ads from beta-campaigns
  if (length(subset(result, !is.na(result$Id)))>0)
  {
    betaads <- suppressWarnings(suppressMessages(yadirGetReport(ReportType = "AD_PERFORMANCE_REPORT", 
                              DateRangeType = "ALL_TIME", 
                              FieldNames = c("AdId","AdGroupId","CampaignId","AdFormat"), 
                              Login = Login,
                              Token = Token,
                              FilterList = c("CampaignType IN SMART_BANNER_CAMPAIGN;MCBANNER_CAMPAIGN"))))
    if (length(betaads))
    {
      colnames(betaads) <- c("Id","AdGroupId","CampaignId","Type")
      myList <- list(result, betaads)
      result <- merge(myList[[1]], myList[[2]], all = TRUE, sort = FALSE)
    }
  }
  
  #check if beta-campaigns was selected
  result <- result[result$CampaignId %in% CampaignIds,]
  
  # delete NA rows
  result <- subset(result, !is.na(result$Id))
  
  # set finish time
  stop_time <- Sys.time()
  
  # tech messages
  packageStartupMessage("Done", appendLF = T)
  packageStartupMessage(paste0("Number of loaded ads: ", nrow(result)), appendLF = T)
  packageStartupMessage(paste0("Duration: ", round(difftime(stop_time, start_time , units ="secs"),0), " sec."), appendLF = T)
  # return of result
  return(result)}