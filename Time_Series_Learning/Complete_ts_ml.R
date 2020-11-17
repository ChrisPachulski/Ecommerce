pacman::p_load(tidyverse,httr,jsonlite,timetk,lubridate,bigrquery,modeltime,recipes,rsample,kernlab,glmnet,kknn,earth,tidymodels,rules,doFuture,future,tune,plotly)

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

tcg_sr_rr <- function(email){
    con <- dbConnect(
        bigrquery::bigquery(),
        project = "tcg-sr-rr",
        dataset = "aether_revolt",
        billing = "tcg-sr-rr"
    )
    bq_auth(email = email, use_oob = TRUE)
    options(scipen = 20)
    con
}
con <- gaeas_cradle("wolfoftinstreet@gmail.com")
statement <- paste("SELECT DISTINCT rdate, a.set FROM `gaeas-cradle.roster.mtgjson`a ORDER BY rdate desc")
set_dates_xreg = dbSendQuery(con, statement = statement) %>% dbFetch(., n = -1) %>% distinct() %>% 
    mutate(set_release = 1, rdate = ymd(rdate)) %>% pad_by_time(.date_var =rdate) %>%
    select(-set) %>% replace(is.na(.),0) 

statement <- paste(
    "SELECT a.Key,a.Card,a.MKT,a.Sellers,a.BL,a.BL_QTY,a.Arb,a.TCG_Rank,a.CK_ADJ_Rank,a.Date ",
    "FROM `gaeas-cradle.premiums.*` a ",
    'WHERE Foil_Status not like "%FOIL%" and (Rarity like "R" or Rarity like "M" or Rarity like "U") and a.Set is not NULL and param is not NULL ',
    'and _TABLE_SUFFIX BETWEEN ',
    'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 300 DAY)) AND ',
    'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL -1 DAY)) ',
    "Order By Date asc; ",
    sep = ""
)

raw_query <- dbSendQuery(con, statement = statement) %>% dbFetch(., n = -1) %>% distinct()


unique_card = raw_query%>% filter(grepl("M$",Key)) %>% mutate(Arb = ifelse(is.na(Arb), (MKT*(-1)),Arb ),
                                                              BL = ifelse(is.na(BL), (0), BL ),
                                                              BL_QTY = ifelse(is.na(BL_QTY), (0), BL_QTY ))
cards_listed = NULL
for(i in unique(unique_card$Card)){
    card_list = paste('"',i,'",',sep="")
    cards_listed = paste(cards_listed,card_list,sep="")
}

cards_listed = gsub(",$","",cards_listed)

statement <- paste(
    "SELECT DISTINCT rdate,types,manaCost, card, count(*) added_printings FROM `gaeas-cradle.roster.mtgjson`a WHERE card in (",cards_listed,") GROUP BY 1,2,3,4  ORDER BY rdate desc ",
    sep = ""
)
printings_xreg = dbSendQuery(con, statement = statement) %>% dbFetch(., n = -1) %>% distinct() %>% mutate(rdate = ymd(rdate))


# mtgjson xregs -----------------------------------------------------------

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


sets_of_interest %>% unlist()
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
    },error=function(e){next})
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
Entire_Dictionary$Key <- paste(Entire_Dictionary$abbr,"-",Entire_Dictionary$number,sep="")
Entire_Dictionary$Working_Key <- paste(Entire_Dictionary$card,Entire_Dictionary$set,Entire_Dictionary$rarity,Entire_Dictionary$hasFoil,sep="")
Entire_Dictionary = Entire_Dictionary %>% as.data.frame() %>% replace(is.na(.),0)
# Implementing mtgjson xregs ----------------------------------------------
Entire_Dictionary %>% select(Working_Key,rarity,colors,hasFoil,hasNonFoil,standard,pioneer,modern,legacy,pauper,edhrecRank,Printings,isPromo,isReserved,legendary_commander) %>% filter(grepl("M$",Working_Key))

printings_xreg <- cbind(printings_xreg, combined_attributes %>% as.data.frame()) %>% select(-scryfall_id)

printings_xreg = printings_xreg %>% group_by(card,types,manaCost,final_color,commander,rarity) %>% summarize(printings = sum(added_printings)) %>% arrange(card)


#uc %>% select(-Card) %>% select(Date,everything()) %>% group_by(Key) %>% plot_time_series(Date, BL, .facet_ncol = 4,.smooth = F,.interactive = F)

uc = unique_card %>% 
    left_join(set_dates_xreg, by = c("Date"="rdate")) %>%
    left_join(Entire_Dictionary %>% select(Working_Key,rarity,colors,hasFoil,hasNonFoil,standard,pioneer,modern,legacy,pauper,edhrecRank,Printings,isPromo,isReserved,legendary_commander), by = c("Key"="Working_Key")) %>% 
    distinct() %>% arrange(Key,Date) %>% fill(Sellers, TCG_Rank, .direction = "downup") %>% 
    pad_by_time(Date,.by = "day",.pad_value = 0)

# modeltime ---------------------------------------------------------------

unique_card_bl <- unique_card %>% select(Date,BL) %>% pivot_longer(-Date) %>% select(-name) %>% na.omit()

unique_card_external_analytics_tbl = unique_card %>% select(-Arb,-Card) %>%
    fill(., .direction = "up")%>% summarise_by_time(Date,.by = "day", across(MKT:CK_ADJ_Rank,.fns =sum)) %>%
    mutate(across(MKT:CK_ADJ_Rank, .fns = log1p)) %>% fill(MKT,Sellers,BL,BL_QTY,TCG_Rank,CK_ADJ_Rank) %>% 
    mutate(across(MKT:CK_ADJ_Rank, .fns = standardize_vec)) %>% select(-BL)

#unique_card_bl %>% View()
bl_prepared_tbl = unique_card_bl %>% summarise_by_time(.date_var = Date, .by = "day",value = sum(value)
) %>% pad_by_time(.date_var = Date,.pad_value = 0
) %>% mutate(bl_trans = log_interval_vec(value,limit_lower = 0, offset = 1)
) %>% mutate(bl_trans = standardize_vec(bl_trans)
) %>% mutate(bl_trans_clean = ts_clean_vec(bl_trans, period = 7)
) %>% select(-value, -bl_trans
) %>% pivot_longer(contains("trans")
) %>% select(-name
) 

# bl_prepared_tbl %>%
# plot_acf_diagnostics(Date,value,.ccf_vars = MKT:CK_ADJ_Rank,.show_ccf_vars_only=T)


lower_limit = 0
upper_limit = 27.4
offset = 1
std_mean = 0.0238097218810935
standard_deviation =  1.100113898328

horizon = 30
lag_period = 30
rolling_periods = c(7,14,21)

#data_prepared_full_tbl %>% view()
data_prepared_full_tbl = bl_prepared_tbl                       %>%
    bind_rows(
        future_frame(.data = .,.date_var = Date,.length_out = horizon)
    )                                                          %>% 
    # tk_augment_fourier(Date,.periods = c(15,40,60,130), .K =2) %>%
    tk_augment_lags(value,.lags = lag_period)                  %>%
    tk_augment_slidify(.value   = value_lag30,
                       .f       = median, 
                       .period  = rolling_periods,
                       .align   = "center",
                       .partial = T) %>% 
    left_join(set_dates_xreg, by = c("Date"="rdate")) %>%
    left_join(printings_xreg %>% select(-card), by = c("Date"="rdate")) %>% 
    distinct() %>% 
    mutate(printings = ifelse(min(printings_xreg$rdate)<Date[1], 
                              sum(added_printings[1], sum(printings_xreg %>% 
                                                              filter(rdate<min(bl_prepared_tbl$Date)) %>% 
                                                              select(added_printings)),na.rm=T))) 
data_prepared_full_tbl = data_prepared_full_tbl %>% 
    mutate(added_printings = ifelse(is.na(added_printings),0,added_printings),
           new_printings = ifelse( (added_printings==0) & (printings > added_printings), printings, added_printings + printings) ) %>%
    select(-added_printings,-printings)

for(i in 1:nrow(data_prepared_full_tbl)){
    if(data_prepared_full_tbl$new_printings[i+1]<data_prepared_full_tbl$new_printings[i]){data_prepared_full_tbl$new_printings[i+1]=data_prepared_full_tbl$new_printings[i]}
    if(i+1 == nrow(data_prepared_full_tbl)){break}
}



data_prepared_tbl = data_prepared_full_tbl %>% filter(!is.na(value))
forecast_tbl = data_prepared_full_tbl %>% filter(is.na(value))

splits = time_series_split(data_prepared_tbl, assess = horizon, cumulative = T)

registerDoFuture()
n_cores = parallel::detectCores()
plan(strategy = cluster,workers = parallel::makeCluster(n_cores))
