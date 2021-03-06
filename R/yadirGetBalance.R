yadirGetBalance <- function(Logins        = NULL, 
                             Token         = NULL,     
                             AgencyAccount = NULL,
                             TokenPath     = getwd()){

  # �������������� �����
  result <- data.table()
  
  #�����������
  if (length(Logins) > 1 || is.null(Logins)) {
  Token <- tech_auth(login = AgencyAccount, token = Token, AgencyAccount = AgencyAccount, TokenPath = TokenPath)
  } else {
  Token <- tech_auth(login = Logins, token = Token, AgencyAccount = AgencyAccount, TokenPath = TokenPath)  
  }
 
  # ��������� �������
  start_element <- 1
  
  # ���������� ���������� ������� � ����� �������
  lim <- 50
  
  # ���������� � ��������� ��������
  ended <- FALSE
  
  while(ended == FALSE) {
  
   # �������� ������ ������ �������
   logins_temp <- head(Logins[start_element:length(Logins)], lim)
   
   #��� ����������� ������������ JSON ������� �-�� ������� � � ������ ���� ����� 1 �� ����������� ��� � list
   if(length(logins_temp)==1){
     logins_temp <- list(logins_temp)
    }
  
  #��������� ���� �������
  body_list <-  list(method = "AccountManagement",
                     param  = list(Action = "Get",
                                   SelectionCriteria = list(Logins = logins_temp)),
                     locale = "ru",
                     token = Token)

  #��������� ���� �������
  body_json <- toJSON(body_list, auto_unbox = T, pretty = TRUE)
  
  #���������� � API
  answer <- POST("https://api.direct.yandex.ru/live/v4/json/", body = body_json)
  
  #����������� ��� ������
  stop_for_status(answer)
  
  #������ ���������
  dataRaw <- content(answer, "parsed", "application/json")
  
  #��� ���� �������� �� ������
  if(!is.null(dataRaw$error_code)){
    stop(paste0("Error: code - ",dataRaw$error_code, ", message - ",dataRaw$error_str, ", detail - ",dataRaw$error_detail))
  }
  
  #����������� ���������� ��������� � �������
  result_temp <- fromJSON(content(answer, "text", "application/json", encoding = "UTF-8"),flatten = TRUE)$data$Accounts
  
  #��������� ��� �� ������ �����������
  errors_list <- fromJSON(content(answer, "text", "application/json", encoding = "UTF-8"),flatten = TRUE)$data$ActionsResult
  
  if(length(errors_list) > 0){
  error <- data.frame(login = errors_list$Login, do.call(rbind.data.frame, errors_list$Errors))
  packageStartupMessage(paste0("Next ",nrow(error)," account",ifelse(nrow(error) > 1, "s","")," get error with try get ballance:"), appendLF = T)
  
  for(err in 1:nrow(error)){
  packageStartupMessage(paste0("Login: ", error$login[err]), appendLF = T)
  packageStartupMessage(paste0("....Code: ", error$FaultCode[err]), appendLF = T)
  packageStartupMessage(paste0("....String: ", error$FaultString[err]), appendLF = T)  
  packageStartupMessage(paste0("....Detail: ", error$FaultDetail[err]), appendLF = T)
  }}
  
  # ��������� ���������� � �������������� �����
  result <- rbind(result, result_temp, fill = TRUE)
  
  # ����������� ��������� �������
  start_element <- start_element + lim
  # ��������� ���� �� ��� ���������� � ��� ��� �������� ������� �������
  if (start_element > length(Logins)) {
    ended <- TRUE
  }
 }    
  return(as.data.frame(result))}
