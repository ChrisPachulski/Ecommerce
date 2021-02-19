pacman::p_load(tidyverse,bigrquery,googlesheets4,googledrive)
gaeas_cradle <- function(email){
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "gaeas-cradle",
    dataset = "premiums",
    billing = "gaeas-cradle"
  )
  bq_auth(email = email, use_oob = TRUE)
  options(scipen = 20)
  con
}

con <- gaeas_cradle("wolfoftinstreet@gmail.com")
statement = paste("SELECT * FROM `gaeas-cradle.ensemble_forecast_results.",gsub("-","_",Sys.Date()),"` ")

binary_check = 0
binary_check = tryCatch({dbSendQuery(con, statement = statement) %>% dbFetch(., n = -1)}, error = function(e){1})

if(binary_check == 1){
  
  con                = gaeas_cradle("wolfoftinstreet@gmail.com")
  
  statement <- paste(
    'SELECT a.Key, number, Param param 
    FROM `gaeas-cradle.ck_funny_money.*` a
    LEFT JOIN `gaeas-cradle.roster.mtgjson` b on b.tcg_id = a.Param
      WHERE CK_Backing <= .40 and Param is not NULL and 
      _Table_Suffix between 
      FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 240 DAY)) AND 
      FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL -2 DAY))  ',
    sep = ""
  )
  
  kpi_query <- dbSendQuery(con, statement = statement) %>% dbFetch(., n = -1) %>% distinct() 
  kpi_query <- kpi_query %>% drop_na()
  
  Short_list_params = NULL
  for(i in unique(kpi_query$param)){
    Short_list = paste('"',i,'",',sep="")
    Short_list_params = paste(Short_list_params,Short_list,sep="")
  }
  
  Short_list_params = gsub(",$","",Short_list_params)
  
  statement <- paste(
    "Select * ",
    "FROM ",
    "(SELECT a.param, AVG(a.BL) avg_bl ",
    "FROM `gaeas-cradle.premiums.*` a ",
    'WHERE Foil_Status not like "%FOIL%" and (Rarity like "R" or Rarity like "M" or Rarity like "U") and a.Set is not NULL and param is not NULL ',
    'AND param in (',Short_list_params,') ',
    'and _TABLE_SUFFIX BETWEEN ',
    'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 240 DAY)) AND ',
    'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL -2 DAY)) ',
    'GROUP BY 1 ',
    ") b ",
    "WHERE avg_bl >= 3.50 ",
    "LIMIT 5000 ",
    sep = ""
  )
  
  raw_params <- dbSendQuery(con, statement = statement) %>% dbFetch(., n = -1) %>% distinct()
  
  Short_list_params = NULL
  for(i in unique(raw_params$param)){
    Short_list = paste('"',i,'",',sep="")
    Short_list_params = paste(Short_list_params,Short_list,sep="")
  }
  
  Short_list_params = gsub(",$","",Short_list_params)
  
  statement <- paste(
    "SELECT CONCAT(Key,(DATE)) as dated_key, a.Key,a.Rarity,a.BL,a.Date ",
    "FROM `gaeas-cradle.premiums.*` a ",
    'WHERE Foil_Status not like "%FOIL%" and (Rarity like "R" or Rarity like "M" or Rarity like "U") and a.Set is not NULL and param is not NULL ',
    'AND param in (',Short_list_params,') ',
    'and _TABLE_SUFFIX BETWEEN ',
    'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 240 DAY)) AND ',
    'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL -2 DAY)) AND ',
    'NOT regexp_contains(a.set, r"Alpha|Beta|Guild Kit") ',
    "Order By Date asc; ",
    sep = ""
  )
  
  raw_query <- dbSendQuery(con, statement = statement) %>% dbFetch(., n = -1) %>% distinct()
  
  statement             = paste("SELECT * FROM `gaeas-cradle.ensemble_raw_results.TODAYS_EAF` ")
  
  metrics                  = dbSendQuery(con, statement = statement) %>% 
    dbFetch(., n = -1)  
  
  rmse_to_keep = metrics %>% select(Key, rmse) %>% distinct() %>% arrange(rmse) %>% mutate(rank = seq(nrow(.)), rank = round(rank/nrow(.),3))%>% filter(rank <= 0.4)
  
  
  joint_historical = raw_query %>% left_join(metrics,by = c("Key"="Key"))
  
  forecasted_tbl = metrics %>% 
    select(Key,Date,forecast_value) %>% 
    mutate(dated_key = paste(Key,Date,sep="")
           ,BL = forecast_value, 
           .model_desc = "FORECAST") %>%
    select(dated_key,Key,Date,BL,.model_desc)
  
  boxplot_ranking_tbl = raw_query %>% group_by(Key) %>% summarize(iqr = IQR(BL), range = fivenum(BL), sd = sd(BL)) %>% ungroup() %>%
    left_join(forecasted_tbl %>% filter(!grepl("ACTUAL",.model_desc)) %>% group_by(Key) %>% 
                mutate(max_forecast_value = max(BL), max_date = ifelse(max_forecast_value == BL, Date,NA)) %>% #filter(Date == max(Date)) %>% 
                select(-BL) %>% drop_na() %>%
                select(Key, max_forecast_value,Date), by = c("Key"="Key")) %>% drop_na() %>%
    left_join(joint_historical %>% rename(Date = Date.x) %>% group_by(Key) %>%
                filter(Date == max(Date)) %>% select(Key,BL,mae,rmse,rsq), by = c("Key"="Key") ) %>%
    mutate(mae_impact = round(mae/BL,4)) %>% filter( (rsq >= .35) | (Key %in% rmse_to_keep$Key) ) %>% 
    mutate(current_val = BL,plus_minus = mae) %>% select(-BL,-mae) %>% 
    select(Key,current_val,iqr,range,sd,max_forecast_value,plus_minus,Date)
  
  # Boxplot Forecast Classification Ranking ---------------------------------
  
  boxplot_overview_tbl = raw_query %>% group_by(Key) %>% summarize(iqr = IQR(BL), range = fivenum(BL), sd = sd(BL)) %>% ungroup() %>%
    left_join(forecasted_tbl %>% filter(!grepl("ACTUAL",.model_desc)) %>% group_by(Key) %>% 
                mutate(max_forecast_value = max(BL), max_date = ifelse(max_forecast_value == BL, Date,NA)) %>% #filter(Date == max(Date)) %>% 
                select(-BL) %>% drop_na() %>%
                select(Key, max_forecast_value,Date), by = c("Key"="Key")) %>% drop_na() %>%
    left_join(joint_historical %>% rename(Date = Date.x) %>% group_by(Key) %>%
                filter(Date == max(Date)) %>% select(Key,BL,mae,rmse,rsq), by = c("Key"="Key") ) %>%
    mutate(current_val = BL,plus_minus = mae) %>% select(-BL,-mae) %>% 
    select(Key,current_val,iqr,range,sd,max_forecast_value,plus_minus,Date)
    
  
  outer_range_boxplot_tbl = boxplot_ranking_tbl %>% group_by(Key) %>% slice(seq(5,n(),by = 5)) %>% ungroup()
  
  boxplot_overview_tbl = boxplot_ranking_tbl %>% left_join(outer_range_boxplot_tbl %>% select(Key,range),by = c("Key"="Key")) %>%
    mutate(median_val = range.x, outer_lim = range.y) %>% select(-range.x,-range.y) %>%
    select(Key,current_val,iqr,sd,median_val,outer_lim,max_forecast_value,plus_minus,Date)
  
  grand_slam_tbl = boxplot_overview_tbl %>% group_by(Key) %>% slice(seq(3,n(),by = 5)) %>% ungroup() %>%
    mutate(Classification = 
             ifelse(   (max_forecast_value > median_val)
                       &
                         (max_forecast_value > (median_val + sd))
                       &
                         (max_forecast_value > (median_val + iqr))
                       &
                         (max_forecast_value >= (current_val * 1.3) )
                       &
                         ((max_forecast_value - plus_minus) >= outer_lim)
                       &
                         ((Date - Sys.Date()) >= 5), 
                       "S",
                       ifelse( ((max_forecast_value > median_val)
                                &
                                  (max_forecast_value > (median_val + sd))
                                &
                                  (max_forecast_value > (median_val + iqr))
                                &
                                  (max_forecast_value >= (current_val * 1.15))
                                &
                                  ((Date - Sys.Date()) >= 5))
                               |
                                 ((max_forecast_value) >= outer_lim)
                               &
                                 ((max_forecast_value - current_val) > 1.5), 
                               "A",
                               ifelse( ((max_forecast_value > median_val)
                                        &
                                          (max_forecast_value > (median_val + sd))
                                        #&
                                        #(max_forecast_value > (median_val + iqr))
                                        &
                                          (max_forecast_value >= (current_val * 1.15))
                                        &
                                          ((Date - Sys.Date()) >= 5))
                                       |
                                         ((max_forecast_value) >= outer_lim)
                                       &
                                         ((max_forecast_value - current_val) > .5), 
                                       "B",
                                       ifelse( ((max_forecast_value > median_val)
                                                #&
                                                #   (max_forecast_value > (median_val + sd))
                                                &
                                                  (max_forecast_value > (median_val + iqr))
                                                &
                                                  (max_forecast_value >= (current_val * 1.10))
                                                &
                                                  ((Date - Sys.Date()) >= 5))
                                               |
                                                 ((max_forecast_value) >= outer_lim)
                                               &
                                                 ((max_forecast_value - current_val) > .5), 
                                               "C",
                                               ifelse( ((max_forecast_value > median_val)
                                                        #&
                                                        #    (max_forecast_value > (median_val + sd))
                                                        #&
                                                        #    (max_forecast_value > (median_val + iqr))
                                                        &
                                                          (max_forecast_value >= (current_val * 1.05))
                                                        &
                                                          ((Date - Sys.Date()) >= 3))
                                                       |
                                                         ((max_forecast_value) >= outer_lim)
                                                       &
                                                         ((max_forecast_value - current_val) > .5), 
                                                       "D",
                                                       ifelse( ((max_forecast_value > median_val)
                                                                #&
                                                                #    (max_forecast_value > (median_val + sd))
                                                                #&
                                                                #    (max_forecast_value > (median_val + iqr))
                                                                &
                                                                  (max_forecast_value >= (current_val))
                                                                &
                                                                  ((Date - Sys.Date()) >= 3))
                                                               |
                                                                 ((max_forecast_value) >= outer_lim)
                                                               &
                                                                 ((max_forecast_value - current_val) > .5), 
                                                               "E",
                                                               ifelse( ((max_forecast_value <= current_val)
                                                                        |
                                                                          (max_forecast_value <= median_val)),
                                                                       #&
                                                                       #    (max_forecast_value > (median_val + sd))
                                                                       #&
                                                                       #    (max_forecast_value > (median_val + iqr))
                                                                       #&
                                                                       #    (max_forecast_value >= (current_val))
                                                                       #&
                                                                       #    ((Date - Sys.Date()) >= 3))
                                                                       #|
                                                                       #   ((max_forecast_value) >= outer_lim)
                                                                       #&
                                                                       #       ((max_forecast_value - current_val) > .5), 
                                                                       "F","Ignore"
                                                               ) # S Tier if - else completion
                                                       ) # A Tier if - else completion
                                               ) # B Tier if - else completion
                                       ) # C Tier if - else completion
                               ) # D Tier if - else completion
                       ) # E Tier if - else completion
             ) # F Tier if - else completion
    )  %>% 
    drop_na() %>% 
    arrange(Classification)
  
  #grand_slam_tbl %>% view()
  
  grand_slam_tbl %>% group_by(Key) %>% summarize(median_val = max(median_val), outer_lim = max(outer_lim), Date = max(Date))
  slimmed_sf_tbl = grand_slam_tbl %>% select(Key,current_val,iqr,sd,max_forecast_value,plus_minus,Classification) %>% distinct() %>% 
    left_join(grand_slam_tbl %>% group_by(Key) %>% summarize(median_val = max(median_val), outer_lim = max(outer_lim), Date = max(Date)),
              by = c("Key"="Key")) %>%
    select(Key,current_val,iqr,sd,median_val,outer_lim, max_forecast_value, plus_minus, Date, Classification) %>%
    mutate(iqr = round(iqr,1),
           sd = round(sd,1),
           max_forecast_value = round(max_forecast_value,1),
           plus_minus = round(plus_minus,2)) 
  
  removal = slimmed_sf_tbl %>% group_by(Key) %>% tally() %>% arrange(desc(n)) %>% filter(n > 1)
  '%!in%' <- function(x,y)!('%in%'(x,y))
  
  slimmed_sf_tbl = slimmed_sf_tbl %>% filter( Key %!in% removal$Key )
  
  
  options(httr_oob_default=TRUE) 
  options(gargle_oauth_email = "pachun95@gmail.com")
  drive_auth(email = "pachun95@gmail.com",use_oob=TRUE)
  gs4_auth(email = "pachun95@gmail.com",use_oob=TRUE)
  
  tryCatch({Updated_Tracking_Keys <- read_csv("/home/cujo253/C20_Addition.csv", col_types = cols(hasFoil = col_character())) %>%
    #rename(c("scryfall_id" = "scryfall","tcg_ID"="param","card" = "name", "set" = "Set", "rarity" = "Rarity","hasFoil" = "Foil")) %>%
    rename(c("scryfall" = "scryfall_id","param"="tcg_ID","name" = "card", "Set" = "set", "Rarity" = "rarity","Foil" = "hasFoil")) %>%
    mutate(Semi = paste(name, Set,sep=""))},error = function(e){Updated_Tracking_Keys <- read_csv("/home/cujo253/C20_Addition.csv", col_types = cols(hasFoil = col_character())) %>%
      rename(c("scryfall_id" = "scryfall","tcg_ID"="param","card" = "name", "set" = "Set", "rarity" = "Rarity","hasFoil" = "Foil")) %>%
      #rename(c("scryfall" = "scryfall_id","param"="tcg_ID","name" = "card", "Set" = "set", "Rarity" = "rarity","Foil" = "hasFoil")) %>%
      mutate(Semi = paste(name, Set,sep=""))})
  
  Updated_Tracking_Keys = Updated_Tracking_Keys %>% replace_na(list(Foil = "")) %>%mutate(name = gsub("\\s\\/\\/.*","",name),
                                                                                          Key = trimws(paste(name,Set,Rarity," ",Foil,sep="")),
                                                                                          Semi = paste(name,Set,sep="")) 
  
  
  
  export_slim_sf_tbl = slimmed_sf_tbl %>% left_join(Updated_Tracking_Keys %>% select(Key, name, Set, Rarity), by =c("Key"="Key")) %>%
    select(Key, name, Set, Rarity, everything()) %>% mutate(Safety = ifelse(sd > iqr, "Volatile","Not Volatile")) %>%
    #select(-iqr,-sd) %>% 
    distinct() #%>% filter(Classification == "S")
  
  ss <- drive_get("Ensemble_Time_Series")
  
  sheet_write(
    export_slim_sf_tbl,
    ss = ss,
    sheet = "Todays_Forecast"
  )
  
  con <- gaeas_cradle("wolfoftinstreet@gmail.com")
  currentDate <- Sys.Date()
  mybq <- bq_table(project = "gaeas-cradle", dataset = "ensemble_forecast_results", table = paste(gsub("-","_",currentDate),"_ENSEMBLE",sep=""))
  bq_table_upload(x=mybq, values = export_slim_sf_tbl, fields=as_bq_fields(export_slim_sf_tbl),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
  print("BQ Forecast Upload Successful!")

} else {print("Today's Forecast Went Through! No Problemo Boss!!!")}
