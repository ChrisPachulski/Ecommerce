#Packages & Functions####
library(tidyverse)
library(devtools)
library(googlesheets4)
library(googledrive)
library(googlesheets)
library(gargle)
library(httr)
library(RMySQL)
library(bigrquery)
delayed_newspaper <- function(ip){
    con = dbConnect(MySQL(), user='HerculesMulligan', password='AlexandreDumasMakesItRain', dbname ='three_day_newspaper',host=ip)
    con
}
current_newspaper <- function(ip){
    con = dbConnect(MySQL(), user='HerculesMulligan', password='AlexandreDumasMakesItRain', dbname ='newspaper',host=ip)
    con
}
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
#3 Day lag####
con <- gaeas_cradle("wolfoftinstreet@gmail.com")
Sets <- read.csv("/home/cujo253/Essential_Referential_CSVS/Sets.csv",stringsAsFactors = TRUE)

ck_conversion <- read_csv("~/Essential_Referential_CSVS/mtgjson_ck_sets.csv")

Updated_Tracking_Keys <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(hasFoil = col_character())) %>%
    #rename(c("scryfall_id" = "scryfall","tcg_ID"="param","card" = "name", "set" = "Set", "rarity" = "Rarity","hasFoil" = "Foil")) %>%
    rename(c("scryfall" = "scryfall_id","param"="tcg_ID","name" = "card", "Set" = "set", "Rarity" = "rarity","Foil" = "hasFoil")) %>%
    mutate(Semi = paste(name, Set,sep=""))

Three_Day_Lag <- gsub("-","_",Sys.Date()-3)

statement <- paste("SELECT * ","FROM `gaeas-cradle.premiums.",Three_Day_Lag,"*` a ",sep = "")
Premium <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% select(!Date)

statement <- paste("SELECT a.Key as Unique_Keys, r.card as Name,r.set as `Set`,r.rarity as Rarity, a.Foil_Status as `F_NF`, a.BL_QTY,a.BL,a.TCG_MKT,a.CK_MKT,a.MKT_Diff,a.Sellers,a.CK_Backing,a.TCG_Backing,a.Group,p.TCG_Rank,p.CK_ADJ_Rank ",
                   "FROM `gaeas-cradle.ck_funny_money.",Three_Day_Lag,"*` a ",
                   " LEFT JOIN roster.mtgjson r on r.Key = a.Key",
                   " LEFT JOIN `gaeas-cradle.premiums.",Three_Day_Lag,"*` p  on r.Key = p.Key",
                   sep = "")
Funny_Money <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)

statement <- paste("SELECT a.Unique_Keys as Unique_Keys, r.card as Name,r.set as `Set`,r.rarity as Rarity,r.hasFoil as Foil, a.Todays_BL,a.Yesterday_BL,a.Week_Ago_BL,a.Month_Ago_BL,a.Yesterday_BL_Chg,a.Week_Ago_BL_Chg,a.Month_Ago_BL_Chg,a.Buylist_Backing", 
                   " FROM `gaeas-cradle.buylist_growth.",Three_Day_Lag,"*` a ",
                   " LEFT JOIN roster.mtgjson r on r.Key = a.Unique_Keys",
                   " WHERE Todays_BL is not NULL and Yesterday_BL is not NULL and Week_Ago_BL is not NULL and Month_Ago_BL is not NULL and Yesterday_BL_Chg is not NULL and Week_Ago_BL_Chg is not NULL and Month_Ago_BL_Chg is not NULL",
                   sep = "")
Buylist <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)

statement <- paste("SELECT a.Unique_Keys as Unique_Keys, r.card as Name,r.set as `Set`,r.rarity as Rarity, a.Todays_TCG,a.Yesterday_TCG,a.Week_Ago_TCG,a.Month_Ago_TCG,a.Yesterday_TCG_Chg,a.Week_Ago_TCG_Chg,a.Month_Ago_TCG_Chg ", 
                   "FROM `gaeas-cradle.demand_growth.",Three_Day_Lag,"*` a ",
                   " LEFT JOIN roster.mtgjson r on r.Key = a.Unique_Keys",
                   sep = "")
Demand <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)

statement <- paste("SELECT a.Unique_Keys as Unique_Keys, r.card as Name,r.set as `Set`,r.rarity as Rarity, a.Todays_Sellers,a.Yesterday_Sellers,a.Week_Ago_Sellers,a.Month_Ago_Sellers,a.Yesterday_Sellers_Chg,a.Week_Ago_Sellers_Chg,a.Month_Ago_Sellers_Chg ", 
                   "FROM `gaeas-cradle.vendor_growth.",Three_Day_Lag,"*` a ",
                   " LEFT JOIN roster.mtgjson r on r.Key = a.Unique_Keys",
                   sep = "")
Vendor <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)

statement <- paste("SELECT a.Key as Key, r.card as Name,r.set as `Set`,r.rarity as Rarity,r.hasFoil as `F_NF`,a.Ranking, p.MKT as Retail,p.BL as Buylist,p.Sellers as Vendors ", 
                   "FROM `gaeas-cradle.kpi.",Three_Day_Lag,"*` a ", 
                   " LEFT JOIN roster.mtgjson r on r.Key = a.Key",
                   " LEFT JOIN `gaeas-cradle.premiums.",Three_Day_Lag,"*` p  on r.Key = p.Key",
                   " ORDER BY Ranking", sep = "")
KPI_Master <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% rename(c("F_NF"="F/NF"))

statement <- paste("SELECT * ", "FROM `gaeas-cradle.ck_velocity.",Three_Day_Lag,"*` a ", sep = "")
CK_Velocity <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% select(!meta_created_at)


drive_auth(email = "pachun95@gmail.com", use_oob = T)
gs4_auth(email = "pachun95@gmail.com", use_oob = T)
ss <- drive_get("Three_Day_Lag")
sheet_write(Premium,
            ss = ss,
            sheet = "Premium")
sheet_write(Funny_Money,
            ss = ss,
            sheet = "Funny_Money")
sheet_write(Buylist,
            ss = ss,
            sheet = "Buylist")
sheet_write(Demand,
            ss = ss,
            sheet = "Demand")
sheet_write(Vendor,
            ss = ss,
            sheet = "Vendor")
sheet_write(KPI_Master,
            ss = ss,
            sheet = "KPI_Master")
sheet_write(data.frame(CK_Velocity),
            ss = ss,
            sheet = "CK_Velocity")

#BAN Server Upload for Lagged Data####
con <- delayed_newspaper("ip")
dbListTables(con)
dbGetInfo(con)

drive_auth(email = "pachun95@gmail.com", use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com", use_oob=TRUE)
ss <- drive_get("Three_Day_Lag")
KPI_Table <- range_read(ss,"KPI_Master") %>% filter(is.na(`F/NF`)) %>% mutate(Ranking = seq(nrow(KPI_Table)),
    uuid = Updated_Tracking_Keys$uuid[match(Key,Updated_Tracking_Keys$Key)]) %>%
    select(uuid,Ranking,Retail,Buylist,Vendors)
dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = KPI_Table, name ='top_25')


vendor_levels <- range_read(ss,"Vendor") %>% mutate(uuid = Updated_Tracking_Keys$uuid[match(Unique_Keys,Updated_Tracking_Keys$Key)]) %>%
    select(uuid,Todays_Sellers,Yesterday_Sellers,Week_Ago_Sellers, Month_Ago_Sellers,Yesterday_Sellers_Chg,Week_Ago_Sellers_Chg,Month_Ago_Sellers_Chg)
vendor_levels$uuid <- Updated_Tracking_Keys$uuid[match(vendor_levels$Unique_Keys,Updated_Tracking_Keys$Key)]
dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = vendor_levels, name ='vendor_levels')


BL_levels <- range_read(ss,"Buylist") %>% filter(is.na(Foil)) %>% mutate(uuid = Updated_Tracking_Keys$uuid[match(Unique_Keys,Updated_Tracking_Keys$Key)]) %>%
    select(!c("Unique_Keys","Name","Set","Rarity","Foil","Buylist_Backing")) %>% select(uuid,everything())
dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = BL_levels, name ='buylist_levels')


ss <- drive_get("3_TS_Delay")
vendor_decline_forecast <- range_read(ss, sheet = "Vendor")
vendor_decline_forecast$`F/NF`[is.na(vendor_decline_forecast$`F/NF`)] <- ""
vendor_decline_forecast <- vendor_decline_forecast[which(vendor_decline_forecast$`F/NF` == ""),]
vendor_decline_forecast$uuid <- Updated_Tracking_Keys$uuid[match(vendor_decline_forecast$Key,Updated_Tracking_Keys$Key)]
vendor_decline_forecast <- vendor_decline_forecast[-c(1:5)]
vendor_decline_forecast <- vendor_decline_forecast[c(15,1,2,3,4,5,6,7,8,9,10,11,12,13,14)]
dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = vendor_decline_forecast, name ='vendor_decline_forecast')


ss <- drive_get("3_TS_Delay")
bl_decline_forecast <- range_read(ss, sheet = "BL")
bl_decline_forecast$`F/NF`[is.na(bl_decline_forecast$`F/NF`)] <- ""
bl_decline_forecast <- bl_decline_forecast[which(bl_decline_forecast$`F/NF` == ""),]
bl_decline_forecast$uuid <- Updated_Tracking_Keys$uuid[match(bl_decline_forecast$Key,Updated_Tracking_Keys$Key)]
bl_decline_forecast <- bl_decline_forecast[-c(1:5)]
bl_decline_forecast <- bl_decline_forecast[c(15,1,2,3,4,5,6,7,8,9,10,11,12,13,14)]
dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = bl_decline_forecast, name ='bl_growth_forecast')


ss <- drive_get("3_TS_Delay")
vendor_perform <- range_read(ss, sheet = "Vendor_List")
vendor_perform$`F/NF`[is.na(vendor_perform$`F/NF`)] <- ""
vendor_perform <- vendor_perform[which(vendor_perform$`F/NF` == ""),]
vendor_perform$uuid <- Updated_Tracking_Keys$uuid[match(vendor_perform$Key,Updated_Tracking_Keys$Key)]
vendor_perform <- vendor_perform[-c(1:5)]
vendor_perform <- vendor_perform[c(8,1,2,3,4,5,6,7)]
dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = vendor_perform, name ='vendor_forecast_perform')


ss <- drive_get("3_TS_Delay")
bl_perform <- range_read(ss, sheet = "BL_List")
bl_perform$`F/NF`[is.na(bl_perform$`F/NF`)] <- ""
bl_perform <- bl_perform[which(bl_perform$`F/NF` == ""),]
bl_perform$uuid <- Updated_Tracking_Keys$uuid[match(bl_perform$Key,Updated_Tracking_Keys$Key)]
bl_perform <- bl_perform[-c(1:5)]
bl_perform <- bl_perform[c(8,1,2,3,4,5,6,7)]
dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = bl_perform, name ='bl_forecast_perform')
#Current Data Upload####
con <- gaeas_cradle("wolfoftinstreet@gmail.com")
Sets <- read.csv("/home/cujo253/Essential_Referential_CSVS/Sets.csv",stringsAsFactors = TRUE)

ck_conversion <- read_csv("~/Essential_Referential_CSVS/mtgjson_ck_sets.csv")

Updated_Tracking_Keys <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(hasFoil = col_character())) %>%
    rename(c("scryfall_id" = "scryfall","tcg_ID"="param","card" = "name", "set" = "Set", "rarity" = "Rarity","hasFoil" = "Foil")) %>%
    #rename(c("scryfall" = "scryfall_id","param"="tcg_ID","name" = "card", "Set" = "set", "Rarity" = "rarity","Foil" = "hasFoil")) %>%
    mutate(Semi = paste(name, Set,sep=""))

currentDate <- gsub("-","_",Sys.Date()-3)

statement <- paste("SELECT * ","FROM `gaeas-cradle.premiums.",currentDate,"*` a ",sep = "")
Premium <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% select(!Date)

statement <- paste("SELECT a.Key as Unique_Keys, r.card as Name,r.set as `Set`,r.rarity as Rarity, a.Foil_Status as `F_NF`, a.BL_QTY,a.BL,a.TCG_MKT,a.CK_MKT,a.MKT_Diff,a.Sellers,a.CK_Backing,a.TCG_Backing,a.Group,p.TCG_Rank,p.CK_ADJ_Rank ",
                   "FROM `gaeas-cradle.ck_funny_money.",currentDate,"*` a ",
                   " LEFT JOIN roster.mtgjson r on r.Key = a.Key",
                   " LEFT JOIN `gaeas-cradle.premiums.",currentDate,"*` p  on r.Key = p.Key",
                   sep = "")
Funny_Money <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)

statement <- paste("SELECT a.Unique_Keys as Unique_Keys, r.card as Name,r.set as `Set`,r.rarity as Rarity,r.hasFoil as Foil, a.Todays_BL,a.Yesterday_BL,a.Week_Ago_BL,a.Month_Ago_BL,a.Yesterday_BL_Chg,a.Week_Ago_BL_Chg,a.Month_Ago_BL_Chg,a.Buylist_Backing", 
                   " FROM `gaeas-cradle.buylist_growth.",currentDate,"*` a ",
                   " LEFT JOIN roster.mtgjson r on r.Key = a.Unique_Keys",
                   " WHERE Todays_BL is not NULL and Yesterday_BL is not NULL and Week_Ago_BL is not NULL and Month_Ago_BL is not NULL and Yesterday_BL_Chg is not NULL and Week_Ago_BL_Chg is not NULL and Month_Ago_BL_Chg is not NULL",
                   sep = "")
Buylist <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)

statement <- paste("SELECT a.Unique_Keys as Unique_Keys, r.card as Name,r.set as `Set`,r.rarity as Rarity, a.Todays_TCG,a.Yesterday_TCG,a.Week_Ago_TCG,a.Month_Ago_TCG,a.Yesterday_TCG_Chg,a.Week_Ago_TCG_Chg,a.Month_Ago_TCG_Chg ", 
                   "FROM `gaeas-cradle.demand_growth.",currentDate,"*` a ",
                   " LEFT JOIN roster.mtgjson r on r.Key = a.Unique_Keys",
                   sep = "")
Demand <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)

statement <- paste("SELECT a.Unique_Keys as Unique_Keys, r.card as Name,r.set as `Set`,r.rarity as Rarity, a.Todays_Sellers,a.Yesterday_Sellers,a.Week_Ago_Sellers,a.Month_Ago_Sellers,a.Yesterday_Sellers_Chg,a.Week_Ago_Sellers_Chg,a.Month_Ago_Sellers_Chg ", 
                   "FROM `gaeas-cradle.vendor_growth.",currentDate,"*` a ",
                   " LEFT JOIN roster.mtgjson r on r.Key = a.Unique_Keys",
                   sep = "")
Vendor <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1)

statement <- paste("SELECT a.Key as Key, r.card as Name,r.set as `Set`,r.rarity as Rarity,r.hasFoil as `F_NF`,a.Ranking, p.MKT as Retail,p.BL as Buylist,p.Sellers as Vendors ", 
                   "FROM `gaeas-cradle.kpi.",currentDate,"*` a ", 
                   " LEFT JOIN roster.mtgjson r on r.Key = a.Key",
                   " LEFT JOIN `gaeas-cradle.premiums.",currentDate,"*` p  on r.Key = p.Key",
                   " ORDER BY Ranking", sep = "")
KPI_Master <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% rename(c("F_NF"="F/NF"))

statement <- paste("SELECT * ", "FROM `gaeas-cradle.ck_velocity.",currentDate,"*` a ", sep = "")
CK_Velocity <- dbSendQuery(con, statement = statement) %>% dbFetch( n = -1) %>% select(!meta_created_at)


drive_auth(email = "pachun95@gmail.com", use_oob = T)
gs4_auth(email = "pachun95@gmail.com", use_oob = T)
ss <- drive_get("currentDate")
sheet_write(Premium,
            ss = ss,
            sheet = "Premium")
sheet_write(Funny_Money,
            ss = ss,
            sheet = "Funny_Money")
sheet_write(Buylist,
            ss = ss,
            sheet = "Buylist")
sheet_write(Demand,
            ss = ss,
            sheet = "Demand")
sheet_write(Vendor,
            ss = ss,
            sheet = "Vendor")
sheet_write(KPI_Master,
            ss = ss,
            sheet = "KPI_Master")
sheet_write(data.frame(CK_Velocity),
            ss = ss,
            sheet = "CK_Velocity")

#BAN Server Upload for Current Data####
library(RMySQL)

con <- current_newspaper("ip")
dbListTables(con)
dbGetInfo(con)

drive_auth(email = "pachun95@gmail.com", use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com", use_oob=TRUE)
ss <- drive_get("currentDate")
KPI_Table <- range_read(ss,"KPI_Master") %>% filter(is.na(`F/NF`)) %>% mutate(Ranking = seq(nrow(KPI_Table)),
                                                                              uuid = Updated_Tracking_Keys$uuid[match(Key,Updated_Tracking_Keys$Key)]) %>%
    select(uuid,Ranking,Retail,Buylist,Vendors)
dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = KPI_Table, name ='top_25')


vendor_levels <- range_read(ss,"Vendor") %>% mutate(uuid = Updated_Tracking_Keys$uuid[match(Unique_Keys,Updated_Tracking_Keys$Key)]) %>%
    select(uuid,Todays_Sellers,Yesterday_Sellers,Week_Ago_Sellers, Month_Ago_Sellers,Yesterday_Sellers_Chg,Week_Ago_Sellers_Chg,Month_Ago_Sellers_Chg)
vendor_levels$uuid <- Updated_Tracking_Keys$uuid[match(vendor_levels$Unique_Keys,Updated_Tracking_Keys$Key)]
dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = vendor_levels, name ='vendor_levels')


BL_levels <- range_read(ss,"Buylist") %>% filter(is.na(Foil)) %>% mutate(uuid = Updated_Tracking_Keys$uuid[match(Unique_Keys,Updated_Tracking_Keys$Key)]) %>%
    select(!c("Unique_Keys","Name","Set","Rarity","Foil","Buylist_Backing")) %>% select(uuid,everything())
dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = BL_levels, name ='buylist_levels')


ss <- drive_get("Vendor_Time_Series_Results")
vendor_decline_forecast <- range_read(ss, sheet = "Decline")
vendor_decline_forecast$`F/NF`[is.na(vendor_decline_forecast$`F/NF`)] <- ""
vendor_decline_forecast <- vendor_decline_forecast[which(vendor_decline_forecast$`F/NF` == ""),]
vendor_decline_forecast$uuid <- Updated_Tracking_Keys$uuid[match(vendor_decline_forecast$Key,Updated_Tracking_Keys$Key)]
vendor_decline_forecast <- vendor_decline_forecast[-c(1:5)]
vendor_decline_forecast <- vendor_decline_forecast[c(15,1,2,3,4,5,6,7,8,9,10,11,12,13,14)]
dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = vendor_decline_forecast, name ='vendor_decline_forecast')


ss <- drive_get("Time_Series_Results")
bl_decline_forecast <- range_read(ss, sheet = "Growth")
bl_decline_forecast$`F/NF`[is.na(bl_decline_forecast$`F/NF`)] <- ""
bl_decline_forecast <- bl_decline_forecast[which(bl_decline_forecast$`F/NF` == ""),]
bl_decline_forecast$uuid <- Updated_Tracking_Keys$uuid[match(bl_decline_forecast$Key,Updated_Tracking_Keys$Key)]
bl_decline_forecast <- bl_decline_forecast[-c(1:5)]
bl_decline_forecast <- bl_decline_forecast[c(15,1,2,3,4,5,6,7,8,9,10,11,12,13,14)]
dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = bl_decline_forecast, name ='bl_growth_forecast')


ss <- drive_get("TS_Delay")
vendor_perform <- range_read(ss, sheet = "Vendor_List")
vendor_perform$`F/NF`[is.na(vendor_perform$`F/NF`)] <- ""
vendor_perform <- vendor_perform[which(vendor_perform$`F/NF` == ""),]
vendor_perform$uuid <- Updated_Tracking_Keys$uuid[match(vendor_perform$Key,Updated_Tracking_Keys$Key)]
vendor_perform <- vendor_perform[-c(1:5)]
vendor_perform <- vendor_perform[c(8,1,2,3,4,5,6,7)]
dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = vendor_perform, name ='vendor_forecast_perform')


ss <- drive_get("TS_Delay")
bl_perform <- range_read(ss, sheet = "BL_List")
bl_perform$`F/NF`[is.na(bl_perform$`F/NF`)] <- ""
bl_perform <- bl_perform[which(bl_perform$`F/NF` == ""),]
bl_perform$uuid <- Updated_Tracking_Keys$uuid[match(bl_perform$Key,Updated_Tracking_Keys$Key)]
bl_perform <- bl_perform[-c(1:5)]
bl_perform <- bl_perform[c(8,1,2,3,4,5,6,7)]
dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = bl_perform, name ='bl_forecast_perform')
