library(tidyverse)
library(devtools)
#devtools::install_github("tidyverse/googlesheets4", force = TRUE)
library(googlesheets4)
library(googledrive)
library(googlesheets)
library(gargle)
library(httr)
Three_Day_Lag <- Sys.Date()-3
setwd("/home/cujo253/Reports/High Confidence Reps")
Premium <- paste(Three_Day_Lag,"_Premium",".csv",sep="")
Premium <- read_csv(Premium)


setwd("/home/cujo253/Funny Money")

Funny_Money <- paste(Three_Day_Lag,"_CK_Credit_Data",".csv",sep="")
Funny_Money <- read_csv(Funny_Money)


setwd("/home/cujo253/Reports/Growth Reports/Buylist_Growth")
Buylist <- paste(Three_Day_Lag,"_C_BuyList_Growth",".csv",sep="")
Buylist <- read_csv(Buylist)


setwd("/home/cujo253/Reports/Growth Reports/Demand_Growth")
Demand <- paste(Three_Day_Lag,"_C_Demand_Growth",".csv",sep="")
Demand <- as.data.frame(read_csv(Demand))
Three_Day_Lag <- Sys.Date()-3
setwd("/home/cujo253/Reports/Growth Reports/Vendor_Growth")
Vendor <- paste(Three_Day_Lag,"_C_Vendor_Growth",".csv",sep="")
Vendor <- read_csv(Vendor, col_types = cols (.default = "c"))
Vendor$Todays_Sellers <- as.numeric(Vendor$Todays_Sellers)
Vendor$Yesterday_Sellers <- as.numeric(Vendor$Yesterday_Sellers)
Vendor$Week_Ago_Sellers <- as.numeric(Vendor$Week_Ago_Sellers)
Vendor$Month_Ago_Sellers <- as.numeric(Vendor$Month_Ago_Sellers)
Vendor$Yesterday_Sellers_Chg <- as.numeric(Vendor$Yesterday_Sellers_Chg)
Vendor$Week_Ago_Sellers_Chg <- as.numeric(Vendor$Week_Ago_Sellers_Chg)
Vendor$Month_Ago_Sellers_Chg <- as.numeric(Vendor$Month_Ago_Sellers_Chg )

na.omit(Vendor)



Three_Day_Lag <- Sys.Date()-3
setwd("/home/cujo253/Reports/KPI/Master")
KPI_Master <- paste(Three_Day_Lag,"_Master_KPI",".csv",sep="")
KPI_Master <- read_csv(KPI_Master)


setwd("/home/cujo253/Metrics/Daily_Velocity_Trackers")
CK_Velocity <- paste(Three_Day_Lag,"_Velocity.csv",sep="")
CK_Velocity <- read_csv(CK_Velocity, col_types = cols(data.is_foil = col_character()))
CK_Velocity$Printings[is.na(CK_Velocity$Printings)==T] <- 1
CK_Velocity$Printing_Status <- ifelse(CK_Velocity$data.edition == "Commander 2020", "In Print", CK_Velocity$Printing_Status)
CK_Velocity <- CK_Velocity[-2]

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

#BAN Server Upload####
library(RMySQL)

con = dbConnect(MySQL(), user='remote', password='zachIsTheBest404!!', dbname ='newspaper',host='157.245.255.185')

dbListTables(con)
dbGetInfo(con)
# initialquerycheck <- "show databases;"
# dbGetQuery(con, initialquerycheck)
# 
# rs = dbSendQuery(con, "select * from newspaper")

Updated_Tracking_Keys <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv",col_types = cols(hasFoil = col_character()))

Updated_Tracking_Keys <- Updated_Tracking_Keys[c(2,3,5,6,8,9,10,11,12)]
colnames(Updated_Tracking_Keys) <- c("uuid","scryfall","param","abbr","Key","name","Set","Rarity","Foil")
ck_conversion <- read_csv("~/Essential_Referential_CSVS/mtgjson_ck_sets.csv")



drive_auth(email = "pachun95@gmail.com", use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com", use_oob=TRUE)
ss <- drive_get("Three_Day_Lag")
KPI_Table <- range_read(ss,"KPI_Master")
KPI_Table$`F/NF`[is.na(KPI_Table$`F/NF`)] <- ""
KPI_Table <- KPI_Table[which(KPI_Table$`F/NF` == ""),]
KPI_Table$Ranking <- seq(nrow(KPI_Table))
KPI_Table$uuid <- Updated_Tracking_Keys$uuid[match(KPI_Table$Key,Updated_Tracking_Keys$Key)]
KPI_Table <- KPI_Table[-c(1:5)]
KPI_Table <- KPI_Table[c(5,1,2,3,4)]
dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = KPI_Table, name ='top_25')

ss <- drive_get("Three_Day_Lag")
vendor_levels <- range_read(ss,"Vendor")
vendor_levels$uuid <- Updated_Tracking_Keys$uuid[match(vendor_levels$Unique_Keys,Updated_Tracking_Keys$Key)]
vendor_levels <- vendor_levels[-c(1:4)]
vendor_levels <- vendor_levels[c(8,1,2,3,4,5,6,7)]
dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = vendor_levels, name ='vendor_levels')

BL_levels <- range_read(ss,"Buylist")
glimpse(BL_levels)
BL_levels$Foil[is.na(BL_levels$Foil)] <- ""
BL_levels <- BL_levels[which(BL_levels$Foil == ""),]
BL_levels$uuid <- Updated_Tracking_Keys$uuid[match(BL_levels$Unique_Keys,Updated_Tracking_Keys$Key)]
BL_levels <- BL_levels[-c(1:5)]
BL_levels <- BL_levels[c(8,1,2,3,4,5,6,7)]
dbWriteTable(conn=con, overwrite = TRUE, append= FALSE, value = BL_levels, name ='buylist_levels')

ss <- drive_get("3_TS_Delay")
vendor_decline_forecast <- range_read(ss, sheet = "Vendor")
glimpse(vendor_decline_forecast)
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
