pacman::p_load(tidyverse,httr,jsonlite,ranger,timetk,lubridate,bigrquery,modeltime,modeltime.ensemble,recipes,rsample,kernlab,glmnet,kknn,earth,tidymodels,rules,doFuture,future,tune,plotly,googlesheets4,googledrive)

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

# mtgjson update for Xregs ------------------------------------------------
url = "https://mtgjson.com/api/v5/AllPrintings.json"
content <- fromJSON(url)
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
Entire_Dictionary$Key <- paste(Entire_Dictionary$card,Entire_Dictionary$set,Entire_Dictionary$rarity,Entire_Dictionary$hasFoil,Entire_Dictionary$number,sep="")
Entire_Dictionary$Working_Key <- paste(Entire_Dictionary$card,Entire_Dictionary$set,Entire_Dictionary$rarity,Entire_Dictionary$hasFoil,sep="")
names(Entire_Dictionary)[21] <- "commander_legal"
colnames(Entire_Dictionary)
# Database Interactions ---------------------------------------------------
Sets <- read.csv("/home/cujo253/Sets.csv",stringsAsFactors = TRUE)
#View(Sets)
ck_conversion <- read_csv("~/mtgjson_ck_sets.csv")

Updated_Tracking_Keys = Entire_Dictionary %>% replace_na(list(Foil = "")) %>%mutate(card = gsub("\\s\\/\\/.*","",card),
                                                                                    Key = trimws(paste(card,set,rarity," ",hasFoil,sep="")),
                                                                                    Semi = paste(card,set,sep="")) 

Final_Forecast_Results = NULL

con <- gaeas_cradle("wolfoftinstreet@gmail.com")
statement <- paste("SELECT DISTINCT rdate, a.set FROM `gaeas-cradle.roster.mtgjson`a ORDER BY rdate desc")
set_dates_xreg = dbSendQuery(con, statement = statement) %>% dbFetch(., n = -1) %>% distinct() %>% 
    mutate(set_release = 1, rdate = ymd(rdate)) %>% pad_by_time(.date_var =rdate) %>%
    select(-set) %>% replace(is.na(.),0)
gc()
registerDoFuture()
n_cores = parallel::detectCores()
plan(
  strategy = cluster,
  workers = parallel::makeCluster(n_cores -1)
)

A = 1
B = 100
for(i in 1:20){
shortlist_tbl = range_read(drive_get("Wolfs_Buylist_Review"),"Current_BuyList") %>% select(data.name,data.edition,data.is_foil) %>% slice(A:B) %>%
    mutate(data.is_foil = as.character(data.is_foil)) %>% replace_na(list(data.is_foil = "")) %>%
    mutate(data.edition = ifelse(data.edition == "Promotional",data.variation,data.edition),
           data.edition = gsub("\\/The List","",data.edition) ) %>%
    mutate(data.edition = ck_conversion$Standardized[match(data.edition,ck_conversion$CK)]) %>%
    mutate(Semi = paste(data.name,data.edition, sep="")) %>%
    mutate(rarity = Updated_Tracking_Keys$rarity[match(Semi, Updated_Tracking_Keys$Semi)]) %>%
    mutate(number = Updated_Tracking_Keys$number[match(Semi, Updated_Tracking_Keys$Semi)]) %>%
    mutate(CK_Key = trimws(paste(data.name, data.edition, rarity," ",data.is_foil, sep=""))) %>% 
    mutate(param = Updated_Tracking_Keys$tcg_ID[match(CK_Key, Updated_Tracking_Keys$Key)]) %>%
    na.omit()

A = A + 100
B = B + 100
if(B > 2000){B=2000}

Short_list_params = NULL
for(i in unique(shortlist_tbl$param)){
    Short_list = paste('"',i,'",',sep="")
    Short_list_params = paste(Short_list_params,Short_list,sep="")
}

Short_list_params = gsub(",$","",Short_list_params)


statement <- paste(
    "SELECT a.Key,a.Card,a.Set,a.Rarity,a.MKT,a.Sellers,a.BL,a.BL_QTY,a.Arb,a.TCG_Rank,a.CK_ADJ_Rank,a.Date ",
    "FROM `gaeas-cradle.premiums.*` a ",
    'WHERE Foil_Status not like "%FOIL%" and (Rarity like "R" or Rarity like "M" or Rarity like "U") and a.Set is not NULL and param is not NULL ',
    'AND param in (',Short_list_params,') ',
    'and _TABLE_SUFFIX BETWEEN ',
    'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 300 DAY)) AND ',
    'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL -1 DAY)) ',
    "Order By Date asc; ",
    sep = ""
)

raw_query <- dbSendQuery(con, statement = statement) %>% dbFetch(., n = -1) %>% distinct()

unique_card = raw_query%>% #filter(Key == ukey) %>% 
    mutate(Arb = ifelse(is.na(Arb), (MKT*(-1)),Arb ),
           BL = ifelse(is.na(BL), (0), BL ),
           BL_QTY = ifelse(is.na(BL_QTY), (0), BL_QTY ))
nrow((unique_card))
min(unique_card$Date)

key_count = unique_card  %>% group_by(Key) %>% tally() %>% as.data.frame()
unique_card = unique_card %>% left_join(key_count, by = c("Key"="Key")) %>% filter(n > 10) %>% select(-n)

full_data_tbl = unique_card %>% 
    left_join(set_dates_xreg, by = c("Date"="rdate")) %>%
    left_join(Entire_Dictionary %>% select(Working_Key,rarity,hasFoil,hasNonFoil,standard,pioneer,modern,legacy,pauper,edhrecRank,Printings,isPromo,isReserved,legendary_commander), by = c("Key"="Working_Key")) %>% 
    distinct() %>% select(-MKT,-Sellers,-Rarity, - BL_QTY, -Arb, -TCG_Rank, -CK_ADJ_Rank) %>%
    arrange(Key,Date) %>% group_by(Key) %>% pad_by_time(Date,.by = "day",.pad_value = NA)  %>%
    fill(Card,Set,BL,set_release, rarity,hasFoil,hasNonFoil,standard,pioneer,modern,legacy,pauper,edhrecRank,Printings,isPromo,isReserved,legendary_commander, .direction = "downup") %>% 
    ungroup() %>% #mutate(TCG_Rank = log1p(TCG_Rank), CK_ADJ_Rank = log1p(CK_ADJ_Rank)) %>%
    group_by(Key) %>% future_frame(Date, .length_out = 45, .bind_data = T) %>%ungroup() %>%
    fill( Card, Set, set_release, rarity,hasFoil,hasNonFoil,standard,pioneer,modern,legacy,pauper,edhrecRank,Printings,isPromo,isReserved,legendary_commander, .direction = "down") %>%
    arrange(Key, Date) %>% group_by(Key) %>% mutate(gap = Date - lag(Date)) %>% filter(gap != 0) %>% select(-gap) %>%
    mutate(Key = as.factor(Key)) %>% group_by(Key)  %>% group_split() %>% map(.f = function(df){
        df %>% arrange(Date) %>% tk_augment_fourier(Date,.period = c(7,28)) %>%
            tk_augment_lags(BL, .lags = c(5,14,28)) %>%
            tk_augment_slidify(BL_lag5,
                               .f = ~mean(.x,na.rm=T),
                               .period = c(5,14,28),
                               .partial = T,
                               .align = "center") %>%
            tk_augment_slidify(BL_lag14,
                               .f = ~mean(.x,na.rm=T),
                               .period = c(5,14,28),
                               .partial = T,
                               .align = "center") %>%
            tk_augment_slidify(BL_lag28,
                               .f = ~mean(.x,na.rm=T),
                               .period = c(5,14,28),
                               .partial = T,
                               .align = "center")
    }) %>% bind_rows() %>% rowid_to_column(var = "rowid") %>%
    replace_na(list(pauper_Banned       = 0)) %>%
    replace_na(list(pauper_Legal        = 0)) %>%
    replace_na(list(standard            = 0)) %>%
    replace_na(list(pauper              = 0)) %>%
    replace_na(list(isPromo             = 0)) %>%
    replace_na(list(isReserved          = 0))%>%
    replace_na(list(pioneer             = 0))%>%
    replace_na(list(legendary_commander = 0))


# Splitting the Data ------------------------------------------------------

data_prepared_tbl = full_data_tbl %>% filter(!is.na(BL)) %>% drop_na()

future_data_tbl = full_data_tbl %>% filter(is.na(BL)) %>% 
    mutate(across(.cols = contains("_lag"), .fns = ~ ifelse(is.nan(.x),NA,.x))) %>%
    mutate(across(.cols = contains("_lag"), .fns = ~ replace_na(.x,0) ))

splits = data_prepared_tbl %>% time_series_split(Date,assess = 45, cumulative = T)

#splits %>% tk_time_series_cv_plan() %>% plot_time_series_cv_plan(Date,BL)



# pre-processing ----------------------------------------------------------

train_cleaned = training(splits) %>% group_by(BL) %>% mutate(BL = ts_clean_vec(BL,period = 7))

summary( train_cleaned %>% mutate(across(.cols = rarity:pauper, .fns = as.factor)))

recipe_spec = recipe(BL ~ ., data = train_cleaned) %>%
    update_role(rowid, new_role = "indicator") %>%
    step_timeseries_signature(Date) %>%
    step_rm(matches("(.xts$)|(.iso$)|(hour)|(Key)|(Card)|(Set)|(minute)|(rarity)|(pioneer)|(modern)|(pauper)|(legacy)|(second)|(am.pm)|(Date_year)|(hasFoil)|(hasNonFoil)|(standard)|(isPromo)|(isReserved)|(legendary_commander)")) %>% #|(rarity)|(hasFoil)|(hasNonFoil)|(modern)|(pauper)|(legacy)|(Set)|(standard)|(isPromo)|(isReserved)")) %>% #|(hasFoil)|(hasNonFoil)|(legendary_commander)|(pauper)|(standard)|(modern)|(legacy)|(pioneer)|(Card)|(Key)|(isPromo)|(isReserved)")) %>%
    step_normalize(Date_index.num) %>%
    step_dummy(all_nominal(), one_hot = T) %>%
    replace_na(list(legendary_commander_FALSE = 0)) %>%
    replace_na(list(legendary_commander_TRUE = 0))

#memory.limit(size = 1500000)

#recipe_spec %>% prep() %>% juice() %>% glimpse()
gc()
# wflw 1 - Prophet --------------------------------------------------------
wflw_fit_prophet = workflow() %>%
    add_model(
        spec = prophet_reg() %>% set_engine("prophet")
    ) %>%
    add_recipe(recipe_spec) %>%
    fit(train_cleaned)
gc()
# wflw 2 - XGBoost --------------------------------------------------------
wflw_fit_xgboost = workflow() %>%
    add_model(
        spec = boost_tree(mode = "regression") %>% set_engine("xgboost")
    ) %>%
    add_recipe(recipe_spec %>% update_role(Date, new_role = "indicator")) %>%
    fit(train_cleaned)
gc()
# wflw 3 - Prophet Boost --------------------------------------------------
wflw_fit_prophet_boost = workflow() %>%
    add_model(
        spec = prophet_boost(
            seasonality_daily  = F,
            seasonality_weekly = F,
            seasonality_yearly = F
        ) %>% set_engine("prophet_xgboost")
    ) %>%
    add_recipe(recipe_spec) %>%
    fit(train_cleaned)
gc()
# wflw 4 - Arima ---------------------------------------------------
# wflw_fit_arima = workflow() %>%
#     add_model(
#         spec = arima_boost(mode = "regression") %>% set_engine("auto_arima_xgboost")
#     ) %>%
#     add_recipe(recipe_spec) %>%
#     fit(train_cleaned)
# gc()
# wflw 5 - Random Forest --------------------------------------------------
wflw_fit_rf = workflow() %>%
    add_model(
        spec = rand_forest(mode = "regression") %>% set_engine("ranger")
    ) %>%
    add_recipe(recipe_spec %>% update_role(Date, new_role = "indicator")) %>%
    fit(train_cleaned)
gc()
# wflw 6 - Neural Net -----------------------------------------------------
wflw_fit_nnet = workflow() %>%
    add_model(
        spec = mlp(mode = "regression") %>% set_engine("nnet")
    ) %>%
    add_recipe(recipe_spec %>% update_role(Date, new_role = "indicator")) %>%
    fit(train_cleaned)
gc()
# wflw 7 - MARS (Invaders!) -----------------------------------------------
wflw_fit_mars = workflow() %>%
    add_model(
        spec = mars(mode = "regression") %>% set_engine("earth")
    ) %>%
    add_recipe(recipe_spec %>% update_role(Date, new_role = "indicator")) %>%
    fit(train_cleaned)
gc()
# wflw 8 - GLM -----------------------------------------------
wflw_fit_glm = workflow() %>%
    add_model(
        spec = linear_reg(penalty = .75, mixture = .15) %>% set_engine("glmnet")
    ) %>%
    add_recipe(recipe_spec %>% update_role(Date, new_role = "indicator")) %>%
    fit(train_cleaned)
gc()
# Accuracy for wflws ------------------------------------------------------

all_models_tbl = modeltime_table(wflw_fit_prophet,
                                 wflw_fit_xgboost,
                                 wflw_fit_prophet_boost,
                                 #wflw_fit_arima,
                                 wflw_fit_rf,
                                 wflw_fit_nnet,
                                 wflw_fit_mars,
                                 wflw_fit_glm
                                 )
all_models_tbl %>% modeltime_accuracy(testing(splits)) %>% arrange(rmse)


# # Hyper Parameter Tuning --------------------------------------------------
# set.seed(253)
# resampled_kfolds = train_cleaned %>% vfold_cv(v = 10)                                                           
# 
# 
# # XGBOOST hyperparams -----------------------------------------------------
# model_spec_xgboost_tune = boost_tree(
#     mode           = "regression",
#     mtry           = tune(),
#     trees          = tune(),
#     min_n          = tune(),
#     tree_depth     = tune(),
#     learn_rate     = tune(),
#     loss_reduction = tune()
# ) %>%
#     set_engine("xgboost")
# 
# wflw_spec_xgboost_tune = workflow()%>%
#     add_model(model_spec_xgboost_tune) %>%
#     add_recipe(recipe_spec %>% update_role(Date, new_role = "indicator"))
# #Skip the grid with tune_grid
# registerDoFuture()
# n_cores = parallel::detectCores()
# plan(
#     strategy = cluster,
#     workers = parallel::makeCluster(n_cores-1)
# )
# 
# set.seed(253)
# tune_results_xgboost = wflw_spec_xgboost_tune %>% 
#     tune_grid(
#         resamples  = resampled_kfolds,
#         param_info = parameters(wflw_spec_xgboost_tune) %>% update(mtry = learn_rate(range = c(15,50), trans = NULL),
#                                                                    trees = trees(range = c(1,2000), trans = NULL),
#                                                                    min_n = min_n(range(1,50)),
#                                                                    learn_rate = learn_rate(range = c(0.01,0.400), trans = NULL),
#                                                                    tree_depth = tree_depth(range = c(1,100), trans = NULL) ),
#         grid       = 10,
#         control = control_grid(verbose = T, allow_par = T)
#     )
# 
# tune_results_xgboost %>% show_best("rmse", n = Inf)
# 
# wflw_fit_xgboost_tune = wflw_spec_xgboost_tune %>% 
#     finalize_workflow(select_best(tune_results_xgboost, "rmse")) %>%
#     fit(train_cleaned)
# 
# # Random Forest -----------------------------------------------------------
# 
# model_spec_rf_tune = rand_forest(
#     mode           = "regression",
#     mtry           = tune(),
#     trees          = tune(),
#     min_n          = tune()) %>%
#     set_engine("ranger")
# 
# wflw_spec_rf_tune = workflow()%>%
#     add_model(model_spec_rf_tune) %>%
#     add_recipe(recipe_spec %>% update_role(Date, new_role = "indicator"))
# #Skip the grid with tune_grid
# registerDoFuture()
# n_cores = parallel::detectCores()
# plan(
#     strategy = cluster,
#     workers = parallel::makeCluster(n_cores-1)
# )
# 
# set.seed(253)
# tune_results_rf = wflw_spec_rf_tune %>% 
#     tune_grid(
#         resamples  = resampled_kfolds,
#         param_info = parameters(wflw_spec_rf_tune),
#         grid       = 10,
#         control = control_grid(verbose = T, allow_par = T)
#     )
# 
# tune_results_rf %>% show_best("rmse", n = Inf)
# 
# wflw_fit_rf_tune = wflw_spec_rf_tune %>% 
#     finalize_workflow(select_best(tune_results_rf, "rmse")) %>%
#     fit(train_cleaned)
# 
# 
# # Earth Tuning ------------------------------------------------------------
# 
# model_spec_mars_tune = mars(
#     mode           = "regression",
#     num_terms      = tune(),
#     prod_degree    = tune()) %>%
#     set_engine("earth")
# 
# wflw_spec_mars_tune = workflow()%>%
#     add_model(model_spec_mars_tune) %>%
#     add_recipe(recipe_spec %>% update_role(Date, new_role = "indicator"))
# #Skip the grid with tune_grid
# registerDoFuture()
# n_cores = parallel::detectCores()
# plan(
#     strategy = cluster,
#     workers = parallel::makeCluster(n_cores-1)
# )
# 
# set.seed(253)
# tune_results_mars = wflw_spec_mars_tune %>% 
#     tune_grid(
#         resamples  = resampled_kfolds,
#         param_info = parameters(wflw_spec_mars_tune),
#         grid       = 10,
#         control = control_grid(verbose = T, allow_par = T)
#     )
# 
# tune_results_mars %>% show_best("rmse", n = Inf)
# 
# wflw_fit_mars_tune = wflw_spec_mars_tune %>% 
#     finalize_workflow(select_best(tune_results_mars, "rmse")) %>%
#     fit(train_cleaned)
# 
# 
# # Evaluate Panel Forecasts ------------------------------------------------
# all_models_and_tuned_tbl = modeltime_table(
#     wflw_fit_xgboost_tune,
#     wflw_fit_rf_tune,
#     wflw_fit_mars_tune
# ) %>% 
#     update_model_description(1, "XGBOOST - Tuned") %>%
#     update_model_description(2, "RANGER - Tuned") %>%
#     update_model_description(3, "EARTH - Tuned") %>%
#     combine_modeltime_tables(all_models_tbl)
# 
# calibration_tbl = all_models_and_tuned_tbl %>% modeltime_calibrate(testing(splits))
# 
# calibration_tbl %>% modeltime_accuracy() %>% arrange(rmse)
# 
# #calibration_tbl %>% 
# #  modeltime_forecast(new_data = testing(splits), actual_data = data_prepared_tbl, keep_data = T) %>%
# #  group_by(Key) %>% plot_modeltime_forecast(.facet_ncol = 4 )
# 
# 
# Resampling --------------------------------------------------------------
resamples_tscv = train_cleaned %>% ungroup() %>%
    time_series_cv(
        assess      = 45,
        skip        = 45,
        cumulative  = T,
        slice_limit = 3
    )

resamples_tscv %>% tk_time_series_cv_plan() %>% plot_time_series_cv_plan(Date, Key)


model_tuned_resample_tbl = all_models_tbl %>%
    modeltime_fit_resamples(
        resamples = resamples_tscv,
        control   = control_resamples(verbose = T, allow_par = T)
    )

model_tuned_resample_tbl %>% modeltime_resample_accuracy() %>% arrange(mae)


# Ensemble Average --------------------------------------------------------

models_to_keep_ensemble = model_tuned_resample_tbl %>% modeltime_resample_accuracy() %>% arrange(mae) %>% select(.model_id) %>% slice(1:3)

ensemble_fit = all_models_tbl %>%
    filter(.model_id %in% models_to_keep_ensemble$.model_id) %>%
    ensemble_average()#type = "median")


model_ensemble_tbl = modeltime_table(ensemble_fit)

model_ensemble_tbl %>% modeltime_accuracy(testing(splits))

forecast_ensemble_tbl = model_ensemble_tbl %>% modeltime_forecast(new_data    = testing(splits),
                                                                  actual_data = data_prepared_tbl,
                                                                  keep_data   = T) 

#forecast_ensemble_tbl %>% group_by(Key) %>% plot_modeltime_forecast(.facet_ncol = 4)

metrics  = forecast_ensemble_tbl %>% 
    filter(.key == "prediction") %>%
    select(Key, .value, BL) %>%
    group_by(Key) %>%
    summarize_accuracy_metrics(
        truth = BL,
        estimate = .value,
        metric_set = metric_set(mae, rmse, rsq)
    )

forecast_ensemble_tbl

data_prepared_tbl_cleaned = data_prepared_tbl%>% group_by(BL) %>% mutate(BL = ts_clean_vec(BL,period = 7))



model_ensemble_refit_tbl = model_ensemble_tbl %>% 
    modeltime_refit(data_prepared_tbl_cleaned)

#model_ensemble_refit_tbl %>% 
#  modeltime_forecast(new_data = future_data_tbl,
#                     actual_data =  data_prepared_tbl,
#                     keep_data = T) %>% 
#  group_by(Key) %>% plot_modeltime_forecast(.facet_ncol = 4)


recombined_tbl = model_ensemble_refit_tbl %>% 
  modeltime_forecast(new_data = future_data_tbl,
                     actual_data =  data_prepared_tbl,
                     keep_data = T) 

hopeful_results = recombined_tbl %>%
  filter(Date == Sys.Date() | Date == max(Date)) %>%
  select(Key, Card, Set, rarity, .value, Date) %>%
  mutate(lagged = round(lag(.value, 1),1) ,
         .value = round(.value,1)) %>%
  mutate(diff = round(.value - lagged,1)) %>%
  filter(Date == "2020-12-31") %>% 
  filter(diff > 0) %>% 
  left_join(metrics %>% select(Key, mae), by = c("Key"="Key")) %>%
  mutate(diff = diff - round(mae,1)) %>%
  mutate(Current_BL = lagged,
         Forecast_BL = .value,
         Forecasted_Gains = diff,
         Forecasted_Growth = round(diff/lagged,1)) %>%
  select(-.value,-diff,-mae,-lagged) %>%
  filter(Forecasted_Growth <= 10 & Forecasted_Growth >= .1) %>%
  left_join(Sets %>% select(Set_Excl,Excl_Excl), by = c("Set"="Set_Excl")) %>%
  filter(Excl_Excl != "Exclude") %>%
  select(-Excl_Excl)

Final_Forecast_Results = rbind(Final_Forecast_Results,hopeful_results)
}
