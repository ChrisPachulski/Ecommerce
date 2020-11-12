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
    "SELECT DISTINCT rdate,scryfall_id,types,manaCost, card, count(*) added_printings FROM `gaeas-cradle.roster.mtgjson`a WHERE card in (",cards_listed,") GROUP BY 1,2,3,4,5  ORDER BY rdate desc ",
    sep = ""
)
printings_xreg = dbSendQuery(con, statement = statement) %>% dbFetch(., n = -1) %>% distinct() %>% mutate(rdate = ymd(rdate))

printings_xreg = printings_xreg %>% group_by(scryfall_id,card,types,manaCost) %>% summarize(printings = sum(added_printings)) %>% arrange(card)


Character_Check <- character(0)
combined_attributes = NULL
for(i in 1:nrow(printings_xreg)){
    scryfall_link <- paste("https://api.scryfall.com/cards/",printings_xreg$scryfall_id[i],sep="")
    scryfall <- GET(scryfall_link)
    card <- (content(scryfall,"parsed")$name)
    if(is.null(card) == T){card = NA}
    commander <- gsub("legal","E",(content(scryfall,"parsed")$legalities$commander))
    if(identical(commander,Character_Check)==T){commander <- "?"}
    if(is.null(commander) == T){commander = NA}
    color_1 <- tryCatch(expr = {content(scryfall,"parsed")$colors[[1]]}, error = function(e){color_1 = "NA"})
    color_2 <- tryCatch(expr = {content(scryfall,"parsed")$colors[[2]]}, error = function(e){color_2 = "NA"})
    color_3 <- tryCatch(expr = {content(scryfall,"parsed")$colors[[3]]}, error = function(e){color_3 = "NA"})
    color_4 <- tryCatch(expr = {content(scryfall,"parsed")$colors[[4]]}, error = function(e){color_4 = "NA"})
    color_5 <- tryCatch(expr = {content(scryfall,"parsed")$colors[[5]]}, error = function(e){color_5 = "NA"})
    color_6 <- tryCatch(expr = {content(scryfall,"parsed")$colors[[6]]}, error = function(e){color_6 = "NA"})
    if(is.null(color_1) == T){color_1 = "NA"}
    if(is.null(color_2) == T){color_2 = "NA"}
    if(is.null(color_3) == T){color_3 = "NA"}
    if(is.null(color_4) == T){color_4 = "NA"}
    if(is.null(color_5) == T){color_5 = "NA"}
    if(is.null(color_6) == T){color_6 = "NA"}
    if(color_2 != "NA"){final_color <- "Multi"}else if(color_1 == "G"){final_color <- "Green"}else if(color_1 == "R"){final_color <- "Red"}else if(color_1 == "W"){final_color <- "White"}else if(color_1 == "B"){final_color <- "Black"}else if(color_1 == "U"){final_color <- "Blue"}else{final_color <- "Brown"}
    if(rarity == "mythic"){rarity <- "M"}else if(rarity == "rare"){rarity <- "R"}else if(rarity == "uncommon"){rarity <- "U"}else if(rarity == "common"){rarity <- "C"}else{rarity <- "S"}
    attributes <- cbind(final_color,commander)
    combined_attributes <- rbind(combined_attributes,attributes)
    Sys.sleep(.12)
}
printings_xreg <- cbind(printings_xreg, combined_attributes %>% as.data.frame())


#uc %>% select(-Card) %>% select(Date,everything()) %>% group_by(Key) %>% plot_time_series(Date, BL, .facet_ncol = 4,.smooth = F,.interactive = F)

uc = unique_card %>% 
    left_join(set_dates_xreg, by = c("Date"="rdate")) %>%
    left_join(printings_xreg, by = c("Card"="card")) %>% 
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
