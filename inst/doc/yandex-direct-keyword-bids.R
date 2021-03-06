## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  eval = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
#  # подключаем пакет
#  library(ryandexdirect)
#  
#  # запрашиваем список ключевых слов
#  kw <- yadirGetKeyWords()
#  
#  # запрашиваем ставки
#  bids  <- yadirGetKeyWordsBids(kw$Id)
#  
#  # запрашиваем ставки и данные торго на поиске
#  bids_search <- yadirGetKeyWordsBids(kw$Id,
#                                      AuctionBids = "search")
#  
#  # запрашиваем ставки и данные торго в сетях
#  network_bids <- yadirGetKeyWordsBids(kw$Id,
#                                       AuctionBids = "network")

## ------------------------------------------------------------------------
#  ## $`2874076041280427473`
#  ## $`2874076041280427473`[[1]]
#  ## $`2874076041280427473`[[1]]$KeywordId
#  ## [1] 6402098519
#  ##
#  ##
#  ## $`2874076041280427473`[[2]]
#  ## $`2874076041280427473`[[2]]$KeywordId
#  ## [1] 6402098520

## ------------------------------------------------------------------------
#  ## $`2874442110113212447`
#  ## $`2874442110113212447`[[1]]
#  ## $`2874442110113212447`[[1]]$Warnings
#  ## $`2874442110113212447`[[1]]$Warnings[[1]]
#  ## $`2874442110113212447`[[1]]$Warnings[[1]]$Code
#  ## [1] 10160
#  ##
#  ## $`2874442110113212447`[[1]]$Warnings[[1]]$Message
#  ## [1] "Ставка не будет применена"
#  ##
#  ## $`2874442110113212447`[[1]]$Warnings[[1]]$Details
#  ## [1] "Ставки на сети не будут изменены, так как отключены показы на сети"
#  ##
#  ##
#  ##
#  ## $`2874442110113212447`[[1]]$Errors
#  ## $`2874442110113212447`[[1]]$Errors[[1]]
#  ## $`2874442110113212447`[[1]]$Errors[[1]]$Code
#  ## [1] 9600
#  ##
#  ## $`2874442110113212447`[[1]]$Errors[[1]]$Message
#  ## [1] "Поле не соответствует установленной стратегии"
#  ##
#  ## $`2874442110113212447`[[1]]$Errors[[1]]$Details
#  ## [1] "Для стратегии с ручным управлением ставками на поиске поле SearchBid должно быть указано"
#  ##
#  ##
#  ##
#  ##
#  ## $`2874442110113212447`[[2]]
#  ## $`2874442110113212447`[[2]]$Warnings
#  ## $`2874442110113212447`[[2]]$Warnings[[1]]
#  ## $`2874442110113212447`[[2]]$Warnings[[1]]$Code
#  ## [1] 10160
#  ##
#  ## $`2874442110113212447`[[2]]$Warnings[[1]]$Message
#  ## [1] "Ставка не будет применена"
#  ##
#  ## $`2874442110113212447`[[2]]$Warnings[[1]]$Details
#  ## [1] "Ставки на сети не будут изменены, так как отключены показы на сети"
#  ##
#  ##
#  ##
#  ## $`2874442110113212447`[[2]]$Errors
#  ## $`2874442110113212447`[[2]]$Errors[[1]]
#  ## $`2874442110113212447`[[2]]$Errors[[1]]$Code
#  ## [1] 9600
#  ##
#  ## $`2874442110113212447`[[2]]$Errors[[1]]$Message
#  ## [1] "Поле не соответствует установленной стратегии"
#  ##
#  ## $`2874442110113212447`[[2]]$Errors[[1]]$Details
#  ## [1] "Для стратегии с ручным управлением ставками на поиске поле SearchBid должно быть указано"

## ------------------------------------------------------------------------
#  !..Error: Для стратегии с ручным управлением ставками на поиске поле SearchBid должно быть указано
#  !..Error: Для стратегии с ручным управлением ставками на поиске поле SearchBid должно быть указано
#  ...Warning: Ставки на сети не будут изменены, так как отключены показы на сети
#  ...Warning: Ставки на сети не будут изменены, так как отключены показы на сети

## ------------------------------------------------------------------------
#  # подключаем пакет
#  library(ryandexdirect)
#  
#  # запрашиваем список рекламных кампаний
#  camp <- yadirGetCampaign()
#  
#  # меняем ставки
#  setbid     <- yadirSetKeyWordsBids(CampaignIds = camp$Id[1],
#                                     SearchBid   = 9)

## ------------------------------------------------------------------------
#  # подключаем пакет
#  library(ryandexdirect)
#  
#  # запрашиваем список ключевых слов
#  kw <- yadirGetKeyWords()
#  
#  # меняем ставки
#  autosetbids <- yadirSetAutoKeyWordsBids(KeywordIds = kw$Id,
#                                          TargetTrafficVolume = 50)

## ------------------------------------------------------------------------
#  # подключаем пакет
#  library(ryandexdirect)
#  library(magrittr)
#  
#  # загрузкса скписка ключевых слов и ставок по ним с помощью пайплайна
#  kw_bids <- yadirGetKeyWords() %>%
#             yadirGetyadirGetKeyWordsBids()
#  
#  # загрузкса скписка групп объявлений с типом "TEXT_AD_GROUP" и ставок по ключевым словам входящим в эти группы
#  autosetbids <-  yadirGetAdGroups(Types = "TEXT_AD_GROUP")  %>%
#                  yadirGetyadirGetKeyWordsBids(AdGroupIds = .)

