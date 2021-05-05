pacman::p_load(tidyverse,httr,jsonlite,ranger,timetk,lubridate,bigrquery,modeltime,modeltime.ensemble,modeltime.gluonts,recipes,rsample,kernlab,glmnet,kknn,earth,tidymodels,rules,doFuture,future,tune,plotly,googlesheets4,googledrive)
invisible(right <- function(text, num_char) {
    substr(text, nchar(text) - (num_char-1), nchar(text))
})
options(httr_oob_default=TRUE) 
options(gargle_oauth_email = "pachun95@gmail.com")
drive_auth(email = "pachun95@gmail.com",use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com",use_oob=TRUE)

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

Ensemble_By_Range = function(recombined_tbl,days){
    results = recombined_tbl %>%
        filter(Date == Sys.Date() | Date == (Sys.Date()+days) ) %>%
        select(Key, .value, Date,mae) %>%
        arrange(Key,Date) %>%
        mutate(lagged = round(lag(.value, 1),1) ,
               .value = round(.value,1)) %>%
        mutate(diff = round(.value - lagged,1)) %>%
        filter(Date == (Sys.Date()+days))%>% 
        filter(diff > 0) %>% 
        mutate(Current_BL = lagged,
               Forecast_BL = .value,
               Forecasted_Gains_Worst = round(diff - round(mae,1),2),
               Forecasted_Gains = round(diff,2),
               Forecasted_Gains_Best = round(diff + round(mae,1),2),
               Forecasted_Growth = round(diff/lagged,2)) %>%
        select(-.value,-mae,-diff,-lagged) %>%
        filter(Forecasted_Growth >= .15) %>%
        arrange(desc(Forecasted_Growth),desc(Forecasted_Gains)) #%>% 
    #filter((Current_BL * 5) >= Forecast_BL)
    
    return(results)
}

registerDoFuture()
n_cores = parallel::detectCores()
plan(strategy = cluster,
     workers  = parallel::makeCluster(n_cores-1) )

# mtgjson update for Xregs ------------------------------------------------
library(jsonlite)
content <- fromJSON("https://mtgjson.com/api/v5/AllPrintings.json")
library(tidyjson)

sets_of_interest <- content$data %>% names() %>% as.list()
sets_of_interest <- sets_of_interest[sets_of_interest != "AMH1"]
sets_of_interest <- sets_of_interest[sets_of_interest != "F18"]
sets_of_interest <- sets_of_interest[sets_of_interest != "L12"]
sets_of_interest <- sets_of_interest[sets_of_interest != "L13"]
sets_of_interest <- sets_of_interest[sets_of_interest != "L14"]
sets_of_interest <- sets_of_interest[sets_of_interest != "L15"]
sets_of_interest <- sets_of_interest[sets_of_interest != "L16"]
sets_of_interest <- sets_of_interest[sets_of_interest != "L17"]
sets_of_interest <- sets_of_interest[sets_of_interest != "PLNY"]
sets_of_interest <- sets_of_interest[sets_of_interest != "PR2"]
sets_of_interest <- sets_of_interest[sets_of_interest != "AZNR"]
sets_of_interest <- sets_of_interest[sets_of_interest != "MZNR"]
sets_of_interest <- sets_of_interest[sets_of_interest != "SZNR"]
sets_of_interest <- sets_of_interest[sets_of_interest != "TBTH"]
sets_of_interest <- sets_of_interest[sets_of_interest != "TDAG"]
sets_of_interest <- sets_of_interest[sets_of_interest != "TFTH"]
sets_of_interest <- sets_of_interest[sets_of_interest != "FJMP"]

#sets_of_interest %>% unlist()
#temp$data$cards$leadershipSkills$commander

Card_Dictionary <- NULL
for(set in sets_of_interest){
    tryCatch({
        temp <- fromJSON(paste("https://mtgjson.com/api/v5/",set,".json",sep=""))
        if(temp$data$isOnlineOnly == F){
            rdate <- temp$data$releaseDate
            if(is.null(rdate)==T){rdate = temp$data$releaseDate}
            uuid <- temp$data$cards$uuid
            if(is.null(uuid)==T){uuid = NA}
            scryfall_id <- temp$data$cards$identifiers$scryfallId
            if(is.null(scryfall_id)==T){scryfall_id = NA}
            mcmid <- temp$data$cards$identifiers$mcmId
            if(is.null(mcmid)==T){mcmid = NA}
            tcg_ID <- temp$data$cards$identifiers$tcgplayerProductId
            if(is.null(tcg_ID)==T){tcg_ID = NA}
            card <- temp$data$cards$name
            if(is.null(card)==T){card = NA}
            set <- temp$data$name
            if(is.null(set)==T){set = NA}
            abbr <- temp$data$code
            if(is.null(abbr)==T){abbr = NA}
            rarity <- temp$data$cards$rarity
            if(is.null(rarity)==T){rarity = NA}
            number <- temp$data$cards$number
            if(is.null(number)==T){number = NA}
            types <- temp$data$cards$types
            if(is.null(types)==T){types = NA}
            manaCost <- temp$data$cards$convertedManaCost
            if(is.null(manaCost)==T){manaCost = NA}
            colors <- unlist(temp$data$cards$colors)
            if(is.null(colors)==T){colors = NA}
            keywords <- temp$data$cards$keywords
            if(is.null(keywords)==T){keywords = NA}
            hasFoil <- temp$data$cards$hasFoil
            if(is.null(hasFoil)==T){hasFoil = NA}
            hasNonFoil <- temp$data$cards$hasNonFoil
            if(is.null(hasNonFoil)==T){hasNonFoil = NA}
            isAlternative <- temp$data$cards$isAlternative
            if(is.null(isAlternative)==T){isAlternative = NA}
            # variations <- temp$cards$variations
            # if(is.null(variations)==T){variations = NA}
            standard <- temp$cards$legalities$standard
            if(is.null(standard)==T){standard = NA}
            pioneer <- temp$data$cards$legalities$pioneer
            if(is.null(pioneer)==T){pioneer = NA}
            modern <- temp$data$cards$legalities$modern
            if(is.null(modern)==T){modern = NA}
            legacy <- temp$data$cards$legalities$legacy
            if(is.null(legacy)==T){legacy = NA}
            commander <- temp$data$cards$legalities$commander
            if(is.null(commander)==T){commander = NA}
            pauper <- temp$data$cards$legalities$pauper
            if(is.null(pauper)==T){pauper = NA}
            ckid <- temp$data$cards$identifiers$cardKingdomId
            if(is.null(ckid)==T){ckid = NA}
            ckid_f <- temp$data$cards$identifiers$cardKingdomFoilId
            if(is.null(ckid_f)==T){ckid_f = NA}
            edhrecRank <- temp$data$cards$edhrecRank
            if(is.null(edhrecRank)==T){edhrecRank = NA}
            Printings <- str_count(temp$data$cards$printings,'"')/2
            if(is.null(Printings)==T){Printings = NA}
            isPromo = temp$data$cards$isPromo
            if(is.null(isPromo)==T){isPromo = NA}
            isReserved = temp$data$cards$isReserved
            if(is.null(isReserved)==T){isReserved = NA}
            commander <- temp$data$cards$leadershipSkills$commander
            if(is.null(commander)==T){commander = NA}
            info <- cbind(rdate,uuid,scryfall_id,mcmid,tcg_ID,card,set,abbr,rarity,number,types,manaCost,colors,hasFoil,hasNonFoil,isAlternative,standard,pioneer,modern,legacy,commander,pauper,ckid,ckid_f,edhrecRank,Printings,isPromo,isReserved,commander)
            Card_Dictionary <- rbind(Card_Dictionary,info)
        }
    },error=function(e){print(paste("Error with set:",set))})
}

Card_Dictionary_backup <- Card_Dictionary
Card_Dictionary <- Card_Dictionary_backup
Card_Dictionary <- as.data.frame(Card_Dictionary)
Card_Dictionary$rdate <- unlist(Card_Dictionary[1])
Card_Dictionary$uuid <- unlist(Card_Dictionary[2])
Card_Dictionary$scryfall_id <- unlist(Card_Dictionary[3])
Card_Dictionary$mcmid <- unlist(Card_Dictionary[4])
Card_Dictionary$tcg_ID<- unlist(Card_Dictionary[5])
Card_Dictionary$card <- unlist(Card_Dictionary[6])
Card_Dictionary$set <- unlist(Card_Dictionary[7])
Card_Dictionary$abbr <- unlist(Card_Dictionary[8])
Card_Dictionary$rarity <- unlist(Card_Dictionary[9])
Card_Dictionary$number <- unlist(Card_Dictionary[10])
Card_Dictionary$types <- unlist(ifelse(str_count(Card_Dictionary$types,'"') >=3,"Multiple",Card_Dictionary$types))

Card_Dictionary$manaCost <- unlist(Card_Dictionary[12])
Card_Dictionary$colors <- unlist(Card_Dictionary$colors)
#Card_Dictionary$colors <- unlist(ifelse(identical(unlist(Card_Dictionary$colors),character(0))==T,NA,unlist(Card_Dictionary$colors)))
Card_Dictionary$hasFoil <- unlist(Card_Dictionary[14])
Card_Dictionary$hasNonFoil <- unlist(Card_Dictionary[15])
Card_Dictionary$isAlternative <- unlist(Card_Dictionary[16])
#Card_Dictionary$variations <- unlist(Card_Dictionary[15])
Card_Dictionary$standard <- as.character(unlist(Card_Dictionary[17]))
Card_Dictionary$pioneer <- unlist(Card_Dictionary[18])
Card_Dictionary$modern <- unlist(Card_Dictionary[19])
Card_Dictionary$legacy <- unlist(Card_Dictionary[20])
Card_Dictionary$commander <- as.character(unlist(Card_Dictionary[21]))
Card_Dictionary$pauper <- unlist(Card_Dictionary[22])
Card_Dictionary$ckid <- unlist(Card_Dictionary[23])
Card_Dictionary$ckid_f <- unlist(Card_Dictionary[24])
Card_Dictionary$edhrecRank <- unlist(Card_Dictionary[25])
Card_Dictionary$Printings <- unlist(Card_Dictionary[26])
Card_Dictionary$isPromo <- as.character(unlist(Card_Dictionary[27]))
Card_Dictionary$isReserved <- as.character(unlist(Card_Dictionary[28]))
Card_Dictionary$legendary_commander <- as.character(unlist(Card_Dictionary[29]))
#Card_Dictionary <- Card_Dictionary[-11]
#Card_Dictionary <- Card_Dictionary[-13]
Card_Dictionary$rarity <- ifelse(Card_Dictionary$rarity == "mythic","M",
                                 ifelse(Card_Dictionary$rarity == "rare","R",
                                        ifelse(Card_Dictionary$rarity == "uncommon","U",
                                               ifelse(Card_Dictionary$rarity == "common","C", Card_Dictionary$rarity))))

Special_Card_Dictionary <- Card_Dictionary[grepl("\\★",Card_Dictionary$number),]
Nonfoil_Card_Dictionary <- Card_Dictionary[!grepl("\\★",Card_Dictionary$number),]

Nonfoil_Only <- Nonfoil_Card_Dictionary[which(Nonfoil_Card_Dictionary$hasNonFoil == T & Nonfoil_Card_Dictionary$hasFoil == F),]
Nonfoil_Only$hasFoil <- ""
Nonfoil_Only$hasNonFoil <- ""
Foil_Only <- Nonfoil_Card_Dictionary[which(Nonfoil_Card_Dictionary$hasNonFoil == F & Nonfoil_Card_Dictionary$hasFoil == T),]
Foil_Only$hasFoil <- " FOIL"
Foil_Only$hasNonFoil <- ""
Nonfoil_Halfs <- Nonfoil_Card_Dictionary[which(Nonfoil_Card_Dictionary$hasNonFoil == T & Nonfoil_Card_Dictionary$hasFoil == T),]
Nonfoil_Halfs$hasFoil <- ""
Nonfoil_Halfs$hasNonFoil <- ""
Foil_Halfs <- Nonfoil_Card_Dictionary[which(Nonfoil_Card_Dictionary$hasNonFoil == T & Nonfoil_Card_Dictionary$hasFoil == T),]
Foil_Halfs$hasFoil <- " FOIL"
Foil_Halfs$hasNonFoil <- ""

Entire_Dictionary <- rbind(Nonfoil_Only, Foil_Only)
Entire_Dictionary <- rbind(Entire_Dictionary,Nonfoil_Halfs)
Entire_Dictionary <- rbind(Entire_Dictionary,Foil_Halfs)
Entire_Dictionary$Key <- paste(Entire_Dictionary$card,Entire_Dictionary$set,Entire_Dictionary$rarity,Entire_Dictionary$hasFoil,Entire_Dictionary$number,sep="")
Entire_Dictionary$Working_Key <- paste(Entire_Dictionary$card,Entire_Dictionary$set,Entire_Dictionary$rarity,Entire_Dictionary$hasFoil,sep="")
names(Entire_Dictionary)[21] <- "commander_legal"
colnames(Entire_Dictionary)

# Database Interaction & Local Uploads ------------------------------------

Sets                  = read.csv("/home/cujo253/Sets.csv",stringsAsFactors = TRUE)
ck_conversion         = read_csv("/home/cujo253/mtgjson_ck_sets.csv")
Updated_Tracking_Keys = Entire_Dictionary                                                     %>% 
                        replace_na(list(Foil = ""))                                           %>%
                        mutate(card          = gsub("\\s\\/\\/.*","",card)                      ,
                               Key           = trimws(paste(card,set,rarity," ",hasFoil,sep="")),
                               Semi          = paste(card,set,sep="")) 

#

con                   = gaeas_cradle("wolfoftinstreet@gmail.com")
statement             = paste("SELECT DISTINCT rdate, a.set FROM `gaeas-cradle.roster.mtgjson`a ORDER BY rdate desc")

set_dates_xreg        = dbSendQuery(con, statement = statement) %>% 
                        dbFetch(., n = -1)                      %>% 
                        distinct()                              %>% 
                        mutate(set_release = 1, 
                               rdate = ymd(rdate))                   %>% 
                                       pad_by_time(.date_var =rdate) %>%
                        select(-set)                            %>% 
                        replace(is.na(.),0)

#

# raw_shortlist_tbl = as.data.frame(range_read(drive_get("Wolfs_Buylist_Review"),"Current_BuyList"))          %>% 
#                     select(data.name,data.edition,data.is_foil)                                             %>% 
#                     dplyr::slice(1:1250)                                                                    %>%
#                     mutate(data.is_foil = as.character(data.is_foil)) %>% replace(is.na(.), "")             %>%
#                     mutate(data.edition = ifelse(data.edition == "Promotional",data.variation,data.edition),
#                            data.edition = gsub("\\/The List","",data.edition))                              %>%
#                     mutate(data.edition = ck_conversion$Standardized[match(data.edition,ck_conversion$CK)])       %>%
#                     mutate(Semi         = paste(data.name,data.edition, sep=""))                                  %>%
#                     mutate(rarity       = Updated_Tracking_Keys$rarity[match(Semi, Updated_Tracking_Keys$Semi)])  %>%
#                     mutate(number       = Updated_Tracking_Keys$number[match(Semi, Updated_Tracking_Keys$Semi)])  %>%
#                     replace(is.na(.), "")                                                                         %>%
#                     mutate(Key          = trimws(paste(data.name, data.edition, rarity," ",data.is_foil, sep="")))%>% 
#                     mutate(param        = Updated_Tracking_Keys$tcg_ID[match(Key, Updated_Tracking_Keys$Key)])    %>%
#                     na.omit()                                                                                     %>%
#                     select(Key,param)


statement <- paste(
  'SELECT a.Key, number, Param param 
  FROM `gaeas-cradle.ck_funny_money.*` a
  LEFT JOIN `gaeas-cradle.roster.mtgjson` b on b.tcg_id = a.Param
    WHERE CK_Backing <= .40 and Param is not NULL and 
    _Table_Suffix between 
    FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 188 DAY)) AND 
    FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL -1 DAY))  ',
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
    'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 188 DAY)) AND ',
    'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL -1 DAY)) ',
    'GROUP BY 1 ',
    ") b ",
    "WHERE avg_bl >= 2.50 ",
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
    'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 188 DAY)) AND ',
    'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL -1 DAY)) AND ',
    'NOT regexp_contains(a.set, r"Alpha|Beta|Guild Kit") ',
    "Order By Date asc; ",
    sep = ""
)

raw_query <- dbSendQuery(con, statement = statement) %>% dbFetch(., n = -1) %>% distinct()

loop_limit_raw = raw_query %>% select(Key) %>% distinct()

statement <- paste(
    "SELECT CONCAT(Key,DATE) as dated_key, Key, BL_QTY, MKT, Sellers, TCG_Rank, CK_ADJ_Rank ",
    "FROM `gaeas-cradle.premiums.*` a ",
    'WHERE Foil_Status not like "%FOIL%" and (Rarity like "R" or Rarity like "M" or Rarity like "U") and a.Set is not NULL and param is not NULL ',
    'AND param in (',Short_list_params,') ',
    'and _TABLE_SUFFIX BETWEEN ',
    'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 188 DAY)) AND ',
    'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL -1 DAY)) ',
    "Order By Date asc; ",
    sep = ""
)

lagged_raw_query <- dbSendQuery(con, statement = statement) %>% dbFetch(., n = -1) %>% distinct() %>% select(-Key)

# Forecast Loop -----------------------------------------------------------


Start_Time = Sys.time()
loop_limit = round(nrow(loop_limit_raw)/50,0)

one_week_combined_tbl   = NULL
two_week_combined_tbl   = NULL
three_week_combined_tbl = NULL
four_week_combined_tbl  = NULL
expanded_all_forecasts  = NULL
boxplot_ranking_tbl     = NULL
a = 1
b = 50

for(i in 1:loop_limit){
tryCatch({
this_round = (raw_query %>% select(Key) %>% distinct() %>% dplyr::slice(a:b))                                                 

mythic_add = raw_query %>% filter(grepl(".*M$",Key)) %>% select(Key) %>% head(1)
rare_add = raw_query %>% filter(grepl(".*R$",Key)) %>% select(Key) %>% head(1)

if(right(this_round$Key,1) %>% unique() %>% length() == 1){this_round = rbind(this_round,mythic_add )}
if(right(this_round$Key,1) %>% unique() %>% length() == 1){this_round = rbind(this_round,rare_add)}

raw_list = raw_query %>% filter(Key %in% this_round$Key)                                                

a = a + 50
b = b + 50
if(b > 1000){b = 1000}

unique_card = raw_list %>% #filter(Key == ukey) %>% 
    mutate(BL = ifelse(is.na(BL), (0), BL )) %>%
    mutate(manaCost = Updated_Tracking_Keys$manaCost[match(Key,Updated_Tracking_Keys$Working_Key)],
           types = Updated_Tracking_Keys$types[match(Key,Updated_Tracking_Keys$Working_Key)],
           colors = Updated_Tracking_Keys$colors[match(Key,Updated_Tracking_Keys$Working_Key)],
           printings = Updated_Tracking_Keys$Printings[match(Key,Updated_Tracking_Keys$Working_Key)],
           edhrecRank = round(Updated_Tracking_Keys$edhrecRank[match(Key,Updated_Tracking_Keys$Working_Key)],-2),
           ) 

key_count = unique_card  %>% group_by(Key) %>% tally() %>% as.data.frame()
unique_card = unique_card %>% left_join(key_count, by = c("Key"="Key")) %>% filter(n > 10) %>% select(-n)
individual_keys = unique_card %>% select(Key) %>% distinct()

#unique_card %>% filter(grepl("Conquer",Key)) %>% arrange(desc(Date))
#for(i in 1:nrow(individual_keys)){
#    individual_card = unique_card %>% filter(Key == as.character(individual_keys[i,]))
# Training & Future Data Preparation ---------------------------------------------------------------------------------
full_tbl = unique_card                                   %>%
    #select(Date,Key,BL)                                  %>% 
    group_by(Key)                                        %>% 
    pad_by_time(Date, .by = "day", .pad_value = NA)      %>%
    fill(BL, .direction = "down")                        %>%
    ungroup()                                            %>%
    group_by(Key)                                        %>%
    future_frame(Date, .length_out = 32, .bind_data = T) %>%
    ungroup()                                            %>%
    left_join(set_dates_xreg, by = c("Date"="rdate"))    %>%
    left_join(lagged_raw_query, by = c("dated_key"="dated_key")) %>%
    distinct()                                           %>%
    fill(Rarity,manaCost,types,colors,printings,edhrecRank,
         BL_QTY,MKT,Sellers, TCG_Rank, CK_ADJ_Rank, .direction = "down")               %>%
    ungroup()                                            %>%
    mutate(BL          = log1p(BL),
           BL_QTY      = log1p(BL_QTY),
           MKT         = log1p(MKT),
           Sellers     = log1p(Sellers),
           TCG_Rank    = log1p(TCG_Rank),
           CK_ADJ_Rank = log1p(CK_ADJ_Rank)
    )                                                    %>%
    select(-dated_key)                                   %>%
    mutate(gap  = Date - lag(Date))                      %>% 
    filter(gap != 0)                                     %>% 
    mutate(Key  = as.factor(Key))                        %>%
    select(-gap)                                         %>%
    group_by(Key)                                        %>% 
    group_split()                                        %>% 
    map(.f = function(df){
        df %>% 
            arrange(Date)                                             %>% 
            
            tk_augment_fourier(Date    ,.period = c(5,10,14,21,30,45,90))       %>%
            
            tk_augment_lags(BL         , .lags = c(3,7,14,32,47))   %>%
            
            tk_augment_lags(BL_QTY     , .lags = c(3,7,14,32,47))   %>%
            
            tk_augment_lags(MKT        , .lags = c(3,7,14,32,47))   %>%
            
            tk_augment_lags(Sellers    , .lags = c(3,7,14,32,47))   %>%
            
            tk_augment_lags(TCG_Rank   , .lags = c(3,7,14,32,47))   %>%
            
            tk_augment_lags(CK_ADJ_Rank, .lags = c(3,7,14,32,47))   %>%
            
            #Slidify BL Lags
            
            tk_augment_slidify(BL_lag3              ,
                               .f       = ~mean(.x,na.rm=T),
                               .period  = c(3,7,14)  ,
                               .partial = T          ,
                               .align   = "right")          %>%
            
            tk_augment_slidify(BL_lag7              ,
                               .f       = ~mean(.x,na.rm=T),
                               .period  = c(3,7,14)  ,
                               .partial = T          ,
                               .align   = "right")          %>%
            tk_augment_slidify(BL_lag14              ,
                               .f       = ~mean(.x,na.rm=T),
                               .period  = c(3,7,14)  ,
                               .partial = T          ,
                               .align   = "right")          %>%
            
            #Slidify BL QTY Lags
            
            tk_augment_slidify(BL_QTY_lag3              ,
                               .f       = ~mean(.x,na.rm=T),
                               .period  = c(3,7,14)  ,
                               .partial = T          ,
                               .align   = "right")          %>%
            
            tk_augment_slidify(BL_QTY_lag7              ,
                               .f       = ~mean(.x,na.rm=T),
                               .period  = c(3,7,14)  ,
                               .partial = T          ,
                               .align   = "right")          %>%
            tk_augment_slidify(BL_QTY_lag14              ,
                               .f       = ~mean(.x,na.rm=T),
                               .period  = c(3,7,14)  ,
                               .partial = T          ,
                               .align   = "right")          %>% 
            #Slidify MKT Lags
            
            tk_augment_slidify(MKT_lag3              ,
                               .f       = ~mean(.x,na.rm=T),
                               .period  = c(3,7,14)  ,
                               .partial = T          ,
                               .align   = "right")          %>%
            
            tk_augment_slidify(MKT_lag7              ,
                               .f       = ~mean(.x,na.rm=T),
                               .period  = c(3,7,14)  ,
                               .partial = T          ,
                               .align   = "right")          %>%
            tk_augment_slidify(MKT_lag14              ,
                               .f       = ~mean(.x,na.rm=T),
                               .period  = c(3,7,14)  ,
                               .partial = T          ,
                               .align   = "right")          %>%
            
            #Slidify Sellers Lags
            
            tk_augment_slidify(Sellers_lag3              ,
                               .f       = ~mean(.x,na.rm=T),
                               .period  = c(3,7,14)  ,
                               .partial = T          ,
                               .align   = "right")          %>%
            
            tk_augment_slidify(Sellers_lag7              ,
                               .f       = ~mean(.x,na.rm=T),
                               .period  = c(3,7,14)  ,
                               .partial = T          ,
                               .align   = "right")          %>%
            tk_augment_slidify(Sellers_lag14              ,
                               .f       = ~mean(.x,na.rm=T),
                               .period  = c(3,7,14)  ,
                               .partial = T          ,
                               .align   = "right")          %>%
            
            #Slidify TCG_Rank Lags
            
            tk_augment_slidify(TCG_Rank_lag3              ,
                               .f       = ~mean(.x,na.rm=T),
                               .period  = c(3,7,14)  ,
                               .partial = T          ,
                               .align   = "right")          %>%
            
            tk_augment_slidify(TCG_Rank_lag7              ,
                               .f       = ~mean(.x,na.rm=T),
                               .period  = c(3,7,14)  ,
                               .partial = T          ,
                               .align   = "right")          %>%
            tk_augment_slidify(TCG_Rank_lag14              ,
                               .f       = ~mean(.x,na.rm=T),
                               .period  = c(3,7,14)  ,
                               .partial = T          ,
                               .align   = "right")          %>%
            
            #Slidify CK_ADJ_Rank Lags
            
            tk_augment_slidify(CK_ADJ_Rank_lag3              ,
                               .f       = ~mean(.x,na.rm=T),
                               .period  = c(3,7,14)  ,
                               .partial = T          ,
                               .align   = "right")          %>%
            
            tk_augment_slidify(CK_ADJ_Rank_lag7              ,
                               .f       = ~mean(.x,na.rm=T),
                               .period  = c(3,7,14)  ,
                               .partial = T          ,
                               .align   = "right")          %>%
            tk_augment_slidify(CK_ADJ_Rank_lag14              ,
                               .f       = ~mean(.x,na.rm=T),
                               .period  = c(3,7,14)  ,
                               .partial = T          ,
                               .align   = "right")          
    })                                                  %>% 
    bind_rows()                                         %>% 
    rowid_to_column(var = "rowid") %>%
  select(-BL_QTY,-MKT,-Sellers,-TCG_Rank,-CK_ADJ_Rank)


# Data preparation, Splits, & Recipe --------------------------------------

data_prepared_tbl = full_tbl %>% 
    filter(!is.na(BL))       %>% 
    drop_na()
#data_prepared_tbl %>% select(Rarity) %>% distinct()

future_tbl =  full_tbl                                                          %>%
    filter(is.na(BL))                                                           %>%
    mutate(across(.cols = contains("_lag"), .fns = ~ ifelse(is.nan(.x),NA,.x))) %>%
    fill(contains("_lag"), .direction = "down")

splits = data_prepared_tbl %>% time_series_split(Date, assess = 32, cumulative = T)

train_cleaned = training(splits)              %>%
    group_by(Key)                             %>%
    mutate(BL = ts_clean_vec(BL, period = 7)) %>%
    ungroup()

recipe_spec = recipe(BL ~., data = train_cleaned)                                    %>%
    update_role(rowid, new_role = "indicator")                                       %>%
    step_timeseries_signature(Date)                                                  %>%
    step_rm(matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(Date_year)")) %>%
    step_normalize(Date_index.num)                                                   %>%
    step_other(Key)                                                                  %>%
    step_dummy(all_nominal(), one_hot = T)                                           %>%
    step_nzv(all_predictors())

recipe_spec_dl = recipe(BL ~ ., data = train_cleaned)                                    %>%
  update_role(rowid, new_role = "indicator")                                       %>%
  step_timeseries_signature(Date)                                                  %>%
  step_rm(matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(am.pm)|(Date_year)")) %>%
  step_normalize(Date_index.num)                                                   %>%
  step_dummy(Date_month.lbl, one_hot = T)                                          %>%
  step_dummy(Date_wday.lbl, one_hot = T)                                           %>%
  step_nzv(all_predictors())
# Models ------------------------------------------------------------------

# wflw 1 - Prophet 
# wflw_fit_prophet = workflow()                                                   %>%
#     add_model(spec = prophet_reg()                                              %>% 
#                   set_engine("prophet"))                                        %>%
#     add_recipe(recipe_spec)                                                     %>%
#     fit(train_cleaned)

# wflw 2 - XGBoost
# wflw_fit_xgboost = workflow()                                                   %>%
#     add_model(spec = boost_tree(mode = "regression") %>% set_engine("xgboost")) %>%
#     add_recipe(recipe_spec %>% update_role(Date, new_role = "indicator"))       %>%
#     fit(train_cleaned)

# wflw 3 - Prophet Boost
wflw_fit_prophet_boost = workflow()                                             %>%
    add_model(spec = prophet_boost(seasonality_daily  = F,
                                   seasonality_weekly = F,
                                   seasonality_yearly = F)                      %>% 
                  set_engine("prophet_xgboost"))                                %>%
    add_recipe(recipe_spec)                                                     %>%
    fit(train_cleaned)

# wflw 4 - Random Forest
wflw_fit_rf = workflow()                                                        %>%
    add_model(spec = rand_forest(mode = "regression") %>% set_engine("ranger")) %>%
    add_recipe(recipe_spec %>% update_role(Date, new_role = "indicator"))       %>%
    fit(train_cleaned)

# wflw 5 - MARS (Invaders!) 
wflw_fit_mars = workflow()                                                      %>%
    add_model(spec = mars(mode = "regression") %>% set_engine("earth"))         %>%
    add_recipe(recipe_spec %>% update_role(Date, new_role = "indicator"))       %>%
    fit(train_cleaned)
gc()
wkflw_deepar_lstm = workflow() %>% add_model(
  deep_ar(
    id                    = "Key",
    freq                  = "D",
    num_layers            = 1,
    prediction_length     = 32,
    epochs                = 3,
    num_batches_per_epoch = 21,
    cell_type             = "lstm",
    #clip_gradient         = 1,
    #penalty               = 5,
    #learn_rate            = .9,
    scale                 = T
    )       %>%
    
    set_engine("gluonts_deepar"))    %>%
  add_recipe(recipe_spec_dl)         %>%
  fit(train_cleaned)

# wflw 7 - Deep AR - GRU  
wkflw_deepar_gru = workflow() %>% add_model(
  nbeats(
    id                    = "Key",
    freq                  = "D",
    #num_layers            = 3,
    prediction_length     = 32,
    epochs                = 3,
    num_batches_per_epoch = 21,
    #cell_type             = "gru",
    #clip_gradient         = 1,
    #penalty               = .5,
    #learn_rate            = .9,
    scale                 = T)       %>%
    
    set_engine("gluonts_nbeats"))    %>%
  add_recipe(recipe_spec_dl)         %>%
  fit(train_cleaned) 



all_models_tbl = modeltime_table(wflw_fit_prophet_boost,
                                 wflw_fit_rf           ,
                                 wflw_fit_mars) # %>%
  # update_model_description(.model_id = 5,.new_model_desc ="DEEPAR - GRU") %>% 
  # update_model_description(.model_id = 4,.new_model_desc ="DEEPAR - LSTM")

deep_ar_tbl = modeltime_table(wkflw_deepar_lstm,wkflw_deepar_gru) %>%
  update_model_description(.model_id = 2,.new_model_desc ="NBEATS") %>% 
  update_model_description(.model_id = 1,.new_model_desc ="DEEPAR - LSTM")

deep_ar_tbl %>% modeltime_accuracy(testing(splits)) %>% arrange(mae)


#Accuracy Check for the curious before we tune
#all_models_tbl %>% modeltime_accuracy(testing(splits)) %>% arrange(mae)


# Resampling & Tuning -----------------------------------------------------

# Set seed for reproducibility & assign 10 folds across 3 grids
# These folds and grids are entirely dependent upon my VM's restrictions

set.seed(253)
resampled_kfolds = train_cleaned %>% vfold_cv(v = 7)    

# XGBOOST hyper parameters
# Create a Model with tune() instead of values
# Allows R/Machine to zero in on best performing metrics

model_spec_prophet_xgboost_tune = prophet_boost(
  mode = "regression",
  seasonality_daily  = F,
  seasonality_weekly = F,
  seasonality_yearly = F,
  mtry = tune(),
  trees = tune(),
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune()
) %>%
    set_engine("prophet_xgboost")

# Same recipe as earlier (see data preparation)
# Slight alteration for xgboost is converting Date
# as an indicator column
wflw_spec_prophet_xgboost_tune = workflow()    %>%
    add_model(model_spec_prophet_xgboost_tune) %>%
    add_recipe(recipe_spec)#         %>% 
    #update_role(Date, new_role = "indicator")) 

set.seed(253)
tune_results_prophet_xgboost = wflw_spec_prophet_xgboost_tune %>% 
    tune_grid(
        resamples  = resampled_kfolds,
        param_info = parameters(wflw_spec_prophet_xgboost_tune) %>% 
            update( mtry       = learn_rate(range = c(15,50), trans = NULL)     ,
                    trees      = trees(range = c(1,2000), trans = NULL)         ,
                    min_n      = min_n(range(1,50))                             ,
                    learn_rate = learn_rate(range = c(0.01,0.30), trans = NULL),
                    tree_depth = tree_depth(range = c(1,50), trans = NULL))    ,
                    grid       = 3                                              ,
                    control    = control_grid(verbose = T, allow_par = T))

# Metrics for the curious to visualize what has been chosen
# by tune(). I generally like to peek.

#tune_results_prophet_xgboost %>% show_best("rmse", n = Inf)

#Finalize the workflow with the tune() values from our best model
# "Best" is defined by lowest rmse.
wflw_fit_prophet_xgboost_tune = wflw_spec_prophet_xgboost_tune           %>% 
    finalize_workflow(select_best(tune_results_prophet_xgboost, "rmse")) %>%
    fit(train_cleaned)

# Random Forest -----------------------------------------------------------
# Rinse Repeat for Random Forest ...
model_spec_rf_tune = rand_forest(
    mode           = "regression",
    mtry           = tune(),
    trees          = tune(),
    min_n          = tune()) %>%
    set_engine("ranger")

wflw_spec_rf_tune = workflow()    %>%
    add_model(model_spec_rf_tune) %>%
    add_recipe(recipe_spec %>% 
               update_role(Date, 
                           new_role = "indicator"))

set.seed(253)
tune_results_rf = wflw_spec_rf_tune %>% 
    tune_grid(
        resamples  = resampled_kfolds             ,
        param_info = parameters(wflw_spec_rf_tune),
        grid       = 3                            ,
        control    = control_grid(verbose = T     , 
                                  allow_par = T)
    )

#tune_results_rf %>% show_best("rmse", n = Inf)

wflw_fit_rf_tune = wflw_spec_rf_tune                        %>% 
    finalize_workflow(select_best(tune_results_rf, "rmse")) %>%
    fit(train_cleaned)


# Earth Tuning ------------------------------------------------------------

model_spec_mars_tune = mars(
    mode           = "regression",
    num_terms      = tune()      ,
    prod_degree    = tune())     %>%
    set_engine("earth")

wflw_spec_mars_tune = workflow()    %>%
    add_model(model_spec_mars_tune) %>%
    add_recipe(recipe_spec %>% 
               update_role(Date, 
                           new_role = "indicator"))

set.seed(253)
tune_results_mars = wflw_spec_mars_tune %>% 
    tune_grid(
        resamples  = resampled_kfolds               ,
        param_info = parameters(wflw_spec_mars_tune),
        grid       = 3                              ,
        control    = control_grid(verbose = F       , 
                                  allow_par = T)
    )

#tune_results_mars %>% show_best("rmse", n = Inf)

wflw_fit_mars_tune = wflw_spec_mars_tune                      %>% 
    finalize_workflow(select_best(tune_results_mars, "rmse")) %>%
    fit(train_cleaned)

# Evaluate Panel Forecasts ------------------------------------------------

# Pull our new tuned models into the fray.
# Only the three most consistently performing models
# were chosen due to time constraints.
all_models_and_tuned_tbl = modeltime_table(
    wflw_fit_prophet_xgboost_tune,
    wflw_fit_rf_tune     ,
    wflw_fit_mars_tune
) %>% 
    update_model_description(1, "PROPHET W/ XGB - Tuned") %>%
    update_model_description(2, "RANGER - Tuned")  %>%
    update_model_description(3, "EARTH - Tuned")   %>%
    combine_modeltime_tables(all_models_tbl)

#calibration_tbl = all_models_and_tuned_tbl %>% 
#                  modeltime_calibrate(testing(splits))

#You know I need to sneak a peek at that measures...
#calibration_tbl %>% modeltime_accuracy() %>% arrange(mae)

# If running less than 25, below allows a nice visual.
# *Remember, on the visuals, we took the log1 of all our
# *numeric/dbl columns. You will have to reverse via expm()
# *in order to see actual values again.

# calibration_tbl %>% 
#  modeltime_forecast(new_data = testing(splits), actual_data = data_prepared_tbl, keep_data = T) %>% group_by(Key) %>% 
#  plot_modeltime_forecast(.facet_ncol = 4 )

# Resampling --------------------------------------------------------------
resamples_tscv = train_cleaned %>% ungroup() %>%
    time_series_cv(
        assess      = 32,
        skip        = 17,
        cumulative  = T,
        slice_limit = 7
    )

model_tuned_resample_tbl = all_models_and_tuned_tbl %>%
    modeltime_fit_resamples(
        resamples        = resamples_tscv,
        control          = control_resamples(verbose = T, 
                                             allow_par = T))


combined_model_accuracy = rbind(model_tuned_resample_tbl %>% modeltime_resample_accuracy() %>% select(-n) ,
                                deep_ar_tbl %>% modeltime_accuracy(testing(splits))) %>% mutate(.model_id = seq(nrow(.)))

# I have a problem, I NEED to see.
model_tuned_resample_tbl %>% modeltime_resample_accuracy() %>% arrange(mae)
deep_ar_tbl %>% modeltime_accuracy(testing(splits)) %>% arrange(mae)

# Ensemble Average --------------------------------------------------------

# Selecting the top 5 of our 8 models for the current batch of forecasts
models_to_keep_ensemble = combined_model_accuracy               %>% 
                          arrange(mae)                          %>% 
                          select(.model_id)                     %>% 
                          head(n=5)
# Interesting element here that can drastically change forecasts results
# There are three options:
# Ensemble Average  - Prone to over fitting, but, if we can get away with it ...
# Ensemble Median   - My 'gold`i`locks' choice. 
# Ensemble Weighted - Manually assign weights to the various models. Hard to
#                     to gauge batch to batch given my machine's restrictions 

ensemble_fit = all_models_and_tuned_tbl                      %>%
  combine_modeltime_tables(deep_ar_tbl) %>%
    filter(.model_id %in% models_to_keep_ensemble$.model_id) %>%
    ensemble_average(type = "median")

#ensemble_weighted(loadings = c(12,6,2,1,1))
#ensemble_average()


model_ensemble_tbl = modeltime_table(ensemble_fit) 

model_ensemble_tbl %>% modeltime_accuracy(testing(splits))

forecast_ensemble_tbl = model_ensemble_tbl              %>% 
    modeltime_forecast(new_data    = testing(splits)    ,
                       actual_data = data_prepared_tbl  ,
                       keep_data   = T)                 %>%
    mutate(across(.cols = c(.value, BL), .fns = expm1)) %>% 
    group_by(Key)                                       %>% 
    arrange(Key,Date) 

# Visualizing Option, again:

#forecast_ensemble_tbl %>%  group_by(Key) %>% plot_modeltime_forecast(.facet_ncol = 4)

# Individual metrics for each element in the forecast:

metrics  = forecast_ensemble_tbl %>% 
    filter(.key == "prediction") %>%
    select(Key, .value, BL)      %>%
    group_by(Key)                %>%
    summarize_accuracy_metrics(
        truth      = BL,
        estimate   = .value,
        metric_set = metric_set(mae, rmse, rsq))


rmse_to_keep = metrics %>% arrange(rmse) %>% mutate(rank = seq(nrow(.))) %>% filter(rank <= 20)


# Sneak a peak:
#metrics

#Refit ----------------------------------------------------------------
# Remember, we smoothed the BL data in our training split, so lets ensure
# we remember to do so again here for the overall data to refit the models
# on.
data_prepared_tbl_cleaned = data_prepared_tbl                        %>% 
                            group_by(Key)                            %>% 
                            mutate(BL = ts_clean_vec(BL,period = 7)) %>% 
                            ungroup()

model_ensemble_refit_tbl = model_ensemble_tbl %>% 
                           modeltime_refit(data_prepared_tbl_cleaned)

# Visualization Option:

#model_ensemble_refit_tbl                %>% 
#    modeltime_forecast(new_data    = future_tbl        ,
#                       actual_data =  data_prepared_tbl,
#                       keep_data   = T) %>% 
#    mutate(.value = expm1(.value),
#           BL     = expm1(BL))          %>%
#    group_by(Key)                       %>% 
#    plot_modeltime_forecast(.smooth = T, .facet_ncol = 4)

recombined_tbl = NULL

recombined_tbl = model_ensemble_refit_tbl %>% 
    modeltime_forecast(new_data    = future_tbl       ,
                       actual_data = data_prepared_tbl,
                       keep_data   = T)   %>%
    mutate(.value = expm1(.value), 
           BL     = expm1(BL))            %>% 
    left_join(
        metrics                                       , 
        by        = c("Key"="Key"))    


# Arbitrary Breakout by week. As we're forecasting 28 days out.
# 4 tibbles for review and compilation 
one_week_results_tbl   = Ensemble_By_Range(recombined_tbl,7)
two_week_results_tbl   = Ensemble_By_Range(recombined_tbl,14)
three_week_results_tbl = Ensemble_By_Range(recombined_tbl,21)
four_week_results_tbl  = Ensemble_By_Range(recombined_tbl,28)

one_week_combined_tbl   = rbind(one_week_combined_tbl,one_week_results_tbl)
two_week_combined_tbl   = rbind(two_week_combined_tbl,two_week_results_tbl)
three_week_combined_tbl = rbind(three_week_combined_tbl,three_week_results_tbl)
four_week_combined_tbl  = rbind(four_week_combined_tbl,four_week_results_tbl)


boxplot_tbl = raw_query %>% group_by(Key) %>% summarize(iqr = IQR(BL), range = fivenum(BL), sd = sd(BL)) %>% ungroup() %>%
    left_join(recombined_tbl %>% filter(!grepl("ACTUAL",.model_desc)) %>% group_by(Key) %>% 
                  mutate(max_forecast_value = max(.value), max_date = ifelse(max_forecast_value == .value, Date,NA)) %>% #filter(Date == max(Date)) %>% 
                  select(-BL) %>% drop_na() %>%
                  select(Key, max_forecast_value,Date), by = c("Key"="Key")) %>% drop_na() %>%
    left_join(recombined_tbl %>% filter(grepl("ACTUAL",.model_desc)) %>% group_by(Key) %>%
                  filter(Date == max(Date)) %>% select(Key,BL,mae,rmse,rsq), by = c("Key"="Key") ) %>%
    mutate(mae_impact = round(mae/BL,4)) %>% filter( (rsq >= .35) | (Key %in% rmse_to_keep$Key) ) %>% 
    mutate(current_val = BL,plus_minus = mae) %>% select(-BL,-mae) %>% 
    select(Key,current_val,iqr,range,sd,max_forecast_value,plus_minus,Date)

boxplot_ranking_tbl = rbind(boxplot_ranking_tbl,boxplot_tbl)

# recombined_tbl %>% filter(!grepl("ACTUAL",.model_desc)) %>% group_by(Key) %>% 
#     mutate(max_forecast_value = max(.value), max_date = ifelse(max_forecast_value == .value, Date,NA)) %>% #filter(Date == max(Date)) %>% 
#     select(-BL) %>% view()

# That said... since we have all this forecasting data, lets not waste it...
# Below will be the ticker tape for ALL elements that were forecasted.
# In case inspiration strikes for a new way to review.

expanded_all_forecasts = rbind(expanded_all_forecasts,recombined_tbl)

Round_End = Sys.time()

eaf = expanded_all_forecasts %>% 
  filter(.key == "prediction") %>% 
  select(Date,.value,Key,mae,rmse,rsq) %>% 
  rename(forecast_value = .value) %>%
  select(Key,everything())

print(paste("Forecasts 1 Through",if(b != 2000){b-50}else{2000},"Took",Round_End - Start_Time))

con <- gaeas_cradle("wolfoftinstreet@gmail.com")
mybq <- bq_table(project = "gaeas-cradle", dataset = "ensemble_raw_results", table = "TODAYS_EAF")
bq_table_upload(x=mybq, values = eaf, fields=as_bq_fields(eaf),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
print("BQ Temporary Raw Results Upload Successful!")


}, error = function(e){print("Error in loop")})
}
Final_Time = Sys.time()
print(Final_Time - Start_Time)

eaf = expanded_all_forecasts %>% 
  filter(.key == "prediction") %>% 
  select(Date,.value,Key,mae,rmse,rsq) %>% 
  rename(forecast_value = .value) %>%
  select(Key,everything())
  

con <- gaeas_cradle("wolfoftinstreet@gmail.com")
currentDate <- Sys.Date()
mybq <- bq_table(project = "gaeas-cradle", dataset = "ensemble_raw_results", table = paste(gsub("-","_",currentDate),"_EAF",sep=""))
bq_table_upload(x=mybq, values = eaf, fields=as_bq_fields(eaf),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
print("BQ Raw Results Upload Successful!")

one_week_final_tbl   = one_week_combined_tbl   %>% arrange(desc(Forecasted_Growth),desc(Forecasted_Gains))
two_week_final_tbl   = two_week_combined_tbl   %>% arrange(desc(Forecasted_Growth),desc(Forecasted_Gains))
three_week_final_tbl = three_week_combined_tbl %>% arrange(desc(Forecasted_Growth),desc(Forecasted_Gains))
four_week_final_tbl  = four_week_combined_tbl  %>% arrange(desc(Forecasted_Growth),desc(Forecasted_Gains))

one_four_combined_tbls = rbind(one_week_final_tbl  ,
                               two_week_final_tbl  ,
                               three_week_final_tbl,
                               four_week_final_tbl )


# Tier breakdown by Individual Weeks:

# Tier One: Appears as a gainer all 4 weeks:
all_four_week_keys  = one_four_combined_tbls %>% 
                      filter(Forecasted_Gains_Worst/Current_BL >= .10) %>%
                      group_by(Key)          %>% 
                      tally()                %>% 
                      arrange(desc(n))       %>% 
                      filter(n == max(n))    %>% 
                      select(Key)

todays_tier_one     = one_four_combined_tbls           %>% 
                      filter(one_four_combined_tbls$Key 
                             %in% 
                             all_four_week_keys$Key)   %>% 
                      arrange(Key, desc(Date))

tier_1_keys_cs = todays_tier_one %>% group_by(Key) %>% summarize(max_growth = max(Forecasted_Growth),
                                                Current_BL = Current_BL) %>% 
    distinct() %>% 
    ungroup() %>% 
    arrange(desc(max_growth)) %>%
    filter(max_growth >= .20) %>%
    mutate(tier = 1)


# Tier Two: Appears as a gainer for 3/4 weeks:
three_week_keys     =  one_four_combined_tbls %>% 
                       filter(Forecasted_Gains_Worst/Current_BL >= .10) %>%
                       group_by(Key)          %>% 
                       tally()                %>% 
                       arrange(desc(n))       %>% 
                       filter(n == (max(n)-1))%>% 
                       select(Key)

todays_tier_two     = one_four_combined_tbls           %>% 
                      filter(one_four_combined_tbls$Key 
                             %in% 
                             three_week_keys$Key)      %>% 
                      arrange(Key, desc(Date)) 

tier_2_keys_cs = todays_tier_two %>% group_by(Key) %>% summarize(max_growth = max(Forecasted_Growth),
                                                Current_BL = Current_BL) %>% 
    distinct() %>% 
    ungroup() %>% 
    arrange(desc(max_growth)) %>%
    filter(max_growth >= .35) %>%
    mutate(tier = 2)


# Tier Three: Appears as a gainer for 2/4 weeks:
two_week_keys       = one_four_combined_tbls %>%
                      filter(Forecasted_Gains_Worst/Current_BL >= .10) %>%
                      group_by(Key)          %>% 
                      tally()                %>% 
                      arrange(desc(n))       %>% 
                      filter(n == (max(n)-2))%>% 
                      select(Key)

todays_tier_three   = one_four_combined_tbls           %>% 
                      filter(one_four_combined_tbls$Key 
                             %in% 
                             two_week_keys$Key)        %>% 
                      arrange(Key, desc(Date)) 

tier_3_keys_cs = todays_tier_three %>% group_by(Key) %>% summarize(max_growth = max(Forecasted_Growth),
                                                                 Current_BL = Current_BL) %>% 
    distinct() %>% 
    ungroup() %>% 
    arrange(desc(max_growth)) %>%
    filter(max_growth >= .45) %>%
    mutate(tier = 3)

tiers_for_cs_puchasing = rbind(tier_1_keys_cs,tier_2_keys_cs,tier_3_keys_cs)

con <- gaeas_cradle("wolfoftinstreet@gmail.com")
currentDate <- Sys.Date()
mybq <- bq_table(project = "gaeas-cradle", dataset = "cs_tiers_ensemble", table = paste(gsub("-","_",currentDate),"_CS_TIERS",sep=""))
bq_table_upload(x=mybq, values = tiers_for_cs_puchasing, fields=as_bq_fields(tiers_for_cs_puchasing),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_TRUNCATE")
print("BQ CS Tiers Upload Successful!")


Key_List <- paste('"',unique(one_four_combined_tbls$Key), '"', sep = "") %>% unlist() %>% map_chr(paste(sep="")) %>% toString() %>% str_replace("^\\,\\s+","") %>% str_replace("\\,\\s+$","") 

statement <- paste('SELECT * 
                    FROM ( 
                    SELECT *, (BL_AVG - lead_col) as bl_diff 
                    FROM ( 
                    SELECT *, LEAD(BL_AVG) OVER (PARTITION BY Card ORDER BY Card desc,B.Set, Week desc) AS lead_col 
                    FROM ( 
                    SELECT Key,Card,a.Set,rarity,Foil_status, Date_Trunc(Date,Week) as Week, ROUND(AVG(BL),0) as BL_AVG 
                    FROM `gaeas-cradle.premiums.*` a 
                    WHERE  _TABLE_SUFFIX BETWEEN 
                      FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 4 Week)) AND 
                      FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 0 Week)) 
                    and Foil_Status like "" and Key in (',Key_List,') 
                    GROUP BY 1,2,3,4,5,6 
                    Order By Card desc,a.Set, Week desc) B 
                     
                    ORDER BY Card desc, b.Set, Week desc) C 
                    WHERE lead_col is not null) D 
                     
                    ORDER BY Card desc, D.Set, Week desc ',
                    sep="")

today_extra <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1) %>%
               mutate(Week      = Week + ((min(one_four_combined_tbls$Date)-7)-max(Week)),
                      Dated_Key = paste(Key,Week,sep="")) 

todays_tier_one_hit_tbl = todays_tier_one %>% mutate(Dated_Key = paste(Key,Date, sep="" )) %>%
    left_join(today_extra %>% select(Dated_Key, BL_AVG,bl_diff), by = c("Dated_Key"="Dated_Key")) %>%
    arrange(Key,desc(Date)) %>% mutate(Four_Wk_Lag_BL = BL_AVG, BL_Movement = bl_diff) %>% 
    select(-Dated_Key,BL_AVG,bl_diff)

todays_tier_two_hit_tbl = todays_tier_two %>% mutate(Dated_Key = paste(Key,Date, sep="" )) %>%
    left_join(today_extra %>% select(Dated_Key, BL_AVG,bl_diff), by = c("Dated_Key"="Dated_Key")) %>%
    arrange(Key,desc(Date)) %>% mutate(Four_Wk_Lag_BL = BL_AVG, BL_Movement = bl_diff) %>% 
    select(-Dated_Key,BL_AVG,bl_diff)

todays_tier_three_hit_tbl = todays_tier_three %>% mutate(Dated_Key = paste(Key,Date, sep="" )) %>%
    left_join(today_extra %>% select(Dated_Key, BL_AVG,bl_diff), by = c("Dated_Key"="Dated_Key")) %>%
    arrange(Key,desc(Date)) %>% mutate(Four_Wk_Lag_BL = BL_AVG, BL_Movement = bl_diff) %>% 
    select(-Dated_Key,BL_AVG,bl_diff)


# Boxplot Forecast Classification Ranking ---------------------------------

boxplot_overview_tbl = raw_query %>% group_by(Key) %>% summarize(iqr = IQR(BL), range = fivenum(BL), sd = sd(BL)) %>% ungroup() %>%
    left_join(expanded_all_forecasts %>% filter(!grepl("ACTUAL",.model_desc)) %>% group_by(Key) %>% 
                  mutate(max_forecast_value = max(.value), max_date = ifelse(max_forecast_value == .value, Date,NA)) %>% #filter(Date == max(Date)) %>% 
                  select(-BL) %>% drop_na() %>%
                  select(Key, max_forecast_value,Date), by = c("Key"="Key")) %>% drop_na() %>%
    left_join(expanded_all_forecasts %>% filter(grepl("ACTUAL",.model_desc)) %>% group_by(Key) %>%
                  filter(Date == max(Date)) %>% select(Key,BL,mae), by = c("Key"="Key") ) %>%
    mutate(current_val = BL,plus_minus = mae) %>% select(-BL,-mae) %>% 
    select(Key,current_val,iqr,range,sd,max_forecast_value,plus_minus,Date)

outer_range_boxplot_tbl = boxplot_ranking_tbl %>% group_by(Key) %>% slice(seq(5,n(),by = 5)) %>% ungroup()

boxplot_overview_tbl = boxplot_ranking_tbl %>% left_join(outer_range_boxplot_tbl %>% select(Key,range) %>% distinct(),by = c("Key"="Key")) %>%
  group_by(Key) %>%
  mutate(median_val = range.x, outer_lim = max(range.y)) %>% select(-range.x,-range.y) %>%
  select(Key,current_val,iqr,sd,median_val,outer_lim,max_forecast_value,plus_minus,Date) %>% ungroup() %>% distinct()


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
    distinct()  %>% mutate(change = (max_forecast_value + (plus_minus * .5)) - current_val, rate_chg = round(change/current_val,3)) %>% filter(rate_chg >= .05) %>% select(-change,-rate_chg)

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

