library(tidyverse)
library(bigrquery)
library(googlesheets4)
library(googledrive)
library(RSelenium)
library(rvest)
library(readr)
clean_names <- function(.data, unique = FALSE) {
  n <- if (is.data.frame(.data)) colnames(.data) else .data
  
  n <- gsub("%+", "_pct_", n)
  n <- gsub("\\$+", "_dollars_", n)
  n <- gsub("\\++", "_plus_", n)
  n <- gsub("-+", "_minus_", n)
  n <- gsub("\\*+", "_star_", n)
  n <- gsub("#+", "_cnt_", n)
  n <- gsub("&+", "_and_", n)
  n <- gsub("@+", "_at_", n)
  
  n <- gsub("[^a-zA-Z0-9_]+", "_", n)
  n <- gsub("([A-Z][a-z])", "_\\1", n)
  n <- tolower(trimws(n))
  
  n <- gsub("(^_+|_+$)", "", n)
  
  n <- gsub("_+", "_", n)
  
  if (unique) n <- make.unique(n, sep = "_")
  
  if (is.data.frame(.data)) {
    colnames(.data) <- n
    .data
  } else {
    n
  }
}
#install.packages("dplyr")
bq_auth(email = "wolfoftinstreet@gmail.com", use_oob = TRUE)
con <- dbConnect(
  bigrquery::bigquery(),
  project = "gaeas-cradle",
  dataset = "premiums",
  billing = "gaeas-cradle"
)

currentDate <- Sys.Date()
Title_Date <- gsub("\\-","\\_",currentDate)

Updated_Tracking_Keys <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv",col_types = cols(hasFoil = col_character()))

# mybq <- bq_table(project = "gaeas-cradle", dataset = "roster", table = paste("mtgjson",sep=""))
# bq_table_upload(x=mybq, values = Updated_Tracking_Keys, fields=as_bq_fields(Updated_Tracking_Keys),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")


Updated_Tracking_Keys <- Updated_Tracking_Keys[c(3,5,6,8,9,10,11,12)]
colnames(Updated_Tracking_Keys) <- c("scryfall","param","abbr","Key","name","Set","Rarity","Foil")
ck_conversion <- read_csv("~/Essential_Referential_CSVS/mtgjson_ck_sets.csv")

setwd("/home/cujo253/Reports/High Confidence Reps")
temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp) - 1
start_date <- currentDate #- Number_Of_Files
Title_Date <- gsub("\\-","\\_",start_date)
combined_file <- NULL

for (i in 1:Number_Of_Files){
combined_file <- NULL
tmp <- read_csv(paste("/home/cujo253/Reports/High Confidence Reps/",start_date,"_Premium.csv",sep=""),col_types = cols(.default = "c"))
tmp <- tmp[1:12]
colnames(tmp)[5] <- c("Foil_Status")
colnames(tmp)[12] <- c("CK_ADJ_Rank")
tmp$Date <- start_date
tmp$Foil_Status <- ifelse(is.na(tmp$Foil_Status)==T,"",tmp$Foil_Status)
tmp$Set <- gsub("Ikoria: Lair of Behemoths Variants","Ikoria: Lair of Behemoths",gsub("Theros Beyond Death Variants","Theros Beyond Death",gsub("Vanguard","Vanguard Series",gsub("Deckmaster","Deckmasters",gsub("Promo Pack","M20 Promo Packs",gsub("Throne of Eldraine Variants","Throne of Eldraine",gsub("War of the Spark JPN Planeswalkers","War of the Spark",gsub("Collectors Ed.*","Intl. Collectorsâ€™ Edition",gsub("Duel Decks: Merfolk Vs. Goblins","Duel Decks: Merfolk vs. Goblins",gsub("Ravnica Allegiance: Guild Kits","RNA Guild Kit",gsub("Beatdown","Beatdown Box Set",gsub("Battle Royale","Battle Royale Box Set",gsub("Timeshifted","Time Spiral Timeshifted",gsub("Beta","Limited Edition Beta",gsub("Alpha","Limited Edition Alpha",gsub("3rd Edition","Revised Edition",gsub("Archenemy - Nicol Bolas","Archenemy",gsub("Modern Event Deck","Modern Event Deck 2014", tmp$Set))))))))))))))))))
tmp$Set <- ck_conversion$Standardized[match(tmp$Set,ck_conversion$CK)]
tmp$Key <- trimws(paste(tmp$Card,tmp$Set,tmp$Rarity," ",tmp$Foil_Status,sep=""))
tmp$BL_QTY <- as.numeric(as.character(tmp$BL_QTY))
tmp$BL <- as.numeric(as.character(tmp$BL))
tmp$MKT <- as.numeric(as.character(tmp$MKT))
tmp$Arb <- as.numeric(as.character(tmp$Arb))
tmp$TCG_Rank <- as.numeric(as.character(tmp$TCG_Rank))
tmp$CK_ADJ_Rank <- as.numeric(as.character(tmp$CK_ADJ_Rank))
tmp$Sellers <- as.numeric(as.character(tmp$Sellers))
tmp$param <- as.character(Updated_Tracking_Keys$param)[match(tmp$Key,Updated_Tracking_Keys$Key)]
tmp$scryfall <- Updated_Tracking_Keys$scryfall[match(tmp$Key,Updated_Tracking_Keys$Key)]
#summary(tmp)
# colnames(tmp)
# colnames(combined_file)
combined_file <- rbind(combined_file,tmp)

Title_Date <- gsub("\\-","\\_",start_date)
mybq <- bq_table(project = "gaeas-cradle", dataset = "premiums", table = paste(Title_Date,"_TCG_CK_Data",sep=""))
bq_table_upload(x=mybq, values = combined_file, fields=as_bq_fields(combined_file),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")
start_date <- start_date + 1
}

setwd("/home/cujo253/Reports/TCG Vendor")
temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp) - 3
start_date <- currentDate - 1 #Number_Of_Files
Title_Date <- gsub("\\-","\\_",start_date)
combined_file <- NULL
i = 1
for (i in 8:Number_Of_Files){
tmp <- tryCatch(expr = {read_csv(paste("/home/cujo253/Reports/TCG Vendor/",start_date,"_TCG.csv",sep=""),col_types = cols(.default = "c")) %>% 
    mutate(Direct_Listings = X10) %>% mutate(Potential_Direct_Copies = X11) %>% mutate(Total_Copies = X12)}, error = function(e){
      read_csv(paste("/home/cujo253/Reports/TCG Vendor/",start_date,"_TCG.csv",sep=""),col_types = cols(.default = "c")) %>% 
          mutate(Direct_Listings = 0) %>% mutate(Potential_Direct_Copies = 0) %>% mutate(Total_Copies = 0)
    })
if(is.null(tmp$MKT) == T){tmp$MKT = 0}
colnames(tmp)[1] = "Primary_Key"
colnames(tmp)[6] = "Vendor Listings"
tmp <- tmp %>% filter(Rarity == "M" |Rarity == "R" |Rarity == "U" ) %>%
        mutate(Product_ID = Updated_Tracking_Keys$param[match(Primary_Key, Updated_Tracking_Keys$Key)]) %>% 
        select(Product_ID, MKT_EST, `Vendor Listings`, MKT, Rank, Direct_Listings, Potential_Direct_Copies, Total_Copies) %>% 
        rename(c(Vendors = `Vendor Listings`)) %>%
        mutate(MKT_EST = as.numeric(MKT_EST)) %>% mutate(Vendors = as.numeric(Vendors)) %>% mutate(MKT = as.numeric(MKT)) %>% mutate(Rank = as.numeric(Rank))

Title_Date <- gsub("\\-","\\_",start_date)
mybq <- bq_table(project = "gaeas-cradle", dataset = "tcgplayer", table = paste(Title_Date,"_TCGPLAYER",sep=""))
bq_table_upload(x=mybq, values = tmp, fields=as_bq_fields(tmp),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")
start_date <- start_date + 1
}





setwd("/home/cujo253/Reports/KPI/Master")
temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp) - 1
start_date <- currentDate - 1
Title_Date <- gsub("\\-","\\_",start_date)
combined_file <- NULL

for (i in 1:Number_Of_Files){
setwd("/home/cujo253/Reports/KPI/Master")
tmp <- read_csv(paste("/home/cujo253/Reports/KPI/Master/",start_date,"_Master_KPI.csv",sep=""),col_types = cols(`F/NF` = col_character())) %>%
  mutate(Param = Updated_Tracking_Keys$param[match(Key,Updated_Tracking_Keys$Key)]) %>% select(Key, Param,Ranking)


Title_Date <- gsub("\\-","\\_",start_date)
mybq <- bq_table(project = "gaeas-cradle", dataset = "kpi", table = paste(Title_Date,"_kpi",sep=""))
bq_table_upload(x=mybq, values = tmp, fields=as_bq_fields(tmp),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")
start_date <- start_date + 1
}








setwd("/home/cujo253/Metrics/Time_Series_Results")
tmp <- read_csv(paste("/home/cujo253/Metrics/Time_Series_Results/",currentDate,"_Times_Series.csv",sep=""),col_types = cols(`F/NF` = col_character()))
#tmp  <- read_csv(tmp, col_types = cols(.default = "c"))
tmp <- tmp[c(1:15)]
colnames(tmp) <- c("Key","Name","Set","Rarity","Foil_Status","Day_1","Day_2","Day_3","Day_4","Day_5","Day_6","Day_7","RMSE","MAPES","Diff")
mybq <- bq_table(project = "gaeas-cradle", dataset = "ck_time_series", table = paste(Title_Date,"_ck_ts",sep=""))
bq_table_upload(x=mybq, values = tmp, fields=as_bq_fields(tmp),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")

setwd("/home/cujo253/Funny Money")
tmp <- read_csv(paste("/home/cujo253/Funny Money/",currentDate,"_CK_Credit_Data.csv",sep=""))
#tmp  <- read_csv(tmp, col_types = cols(.default = "c"))
colnames(tmp)[5] <- c("Foil_Status")
colnames(tmp)[12] <-c("CK_Mkt_Perc")
colnames(tmp)[13] <- c("TCG_Mkt_Perc")
mybq <- bq_table(project = "gaeas-cradle", dataset = "funny_money", table = paste(Title_Date,"_funny_money",sep=""))
bq_table_upload(x=mybq, values = tmp, fields=as_bq_fields(tmp),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")



currentDate <- Sys.Date()
Title_Date <- gsub("\\-","\\_",currentDate)

Updated_Tracking_Keys <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv",col_types = cols(hasFoil = col_character()))
Updated_Tracking_Keys <- Updated_Tracking_Keys[c(3,5,6,8,9,10,11,12)]
colnames(Updated_Tracking_Keys) <- c("scryfall","param","abbr","Key","name","Set","Rarity","Foil")
ck_conversion <- read_csv("~/Essential_Referential_CSVS/mtgjson_ck_sets.csv")

setwd("/home/cujo253/Funny Money")
temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp)

start_date <- (currentDate - Number_Of_Files)
start_date <- start_date + 1
Title_Date <- gsub("\\-","\\_",start_date)
combined_file <- NULL

for (i in 1:Number_Of_Files){
  combined_file <- NULL
  tmp <- read_csv(paste("/home/cujo253/Funny Money/",start_date,"_CK_Credit_Data.csv",sep=""),col_types = cols(.default = "c"))
  tmp <- tmp[c(1:14)]
  colnames(tmp)[5] <- c("Foil_Status")
  colnames(tmp)[12] <- c("CK_Backing")
  colnames(tmp)[13] <- c("TCG_Backing")
  tmp$Date <- start_date
  tmp$Foil_Status <- ifelse(is.na(tmp$Foil_Status)==T,"",tmp$Foil_Status)
  tmp$Set <- gsub("Ikoria: Lair of Behemoths Variants","Ikoria: Lair of Behemoths",gsub("Theros Beyond Death Variants","Theros Beyond Death",gsub("Vanguard","Vanguard Series",gsub("Deckmaster","Deckmasters",gsub("Promo Pack","M20 Promo Packs",gsub("Throne of Eldraine Variants","Throne of Eldraine",gsub("War of the Spark JPN Planeswalkers","War of the Spark",gsub("Collectors Ed.*","Intl. Collectorsâ€™ Edition",gsub("Duel Decks: Merfolk Vs. Goblins","Duel Decks: Merfolk vs. Goblins",gsub("Ravnica Allegiance: Guild Kits","RNA Guild Kit",gsub("Beatdown","Beatdown Box Set",gsub("Battle Royale","Battle Royale Box Set",gsub("Timeshifted","Time Spiral Timeshifted",gsub("Beta","Limited Edition Beta",gsub("Alpha","Limited Edition Alpha",gsub("3rd Edition","Revised Edition",gsub("Archenemy - Nicol Bolas","Archenemy",gsub("Modern Event Deck","Modern Event Deck 2014", tmp$Set))))))))))))))))))
  tmp$Set <- ck_conversion$Standardized[match(tmp$Set,ck_conversion$CK)]
  tmp$Key <- trimws(paste(tmp$Card,tmp$Set,tmp$Rarity," ",tmp$Foil_Status,sep=""))
  tmp$BL_QTY <- as.numeric(as.character(tmp$BL_QTY))
  tmp$BL <- as.numeric(as.character(tmp$BL))
  tmp$TCG_MKT <- as.numeric(as.character(tmp$TCG_MKT))
  tmp$CK_MKT <- as.numeric(as.character(tmp$CK_MKT))
  tmp$MKT_Diff <- as.numeric(as.character(tmp$MKT_Diff))
  tmp$CK_Backing <- as.numeric(as.character(tmp$CK_Backing))
  tmp$TCG_Backing <- as.numeric(as.character(tmp$TCG_Backing))
  tmp$Sellers <- as.numeric(as.character(tmp$Sellers))
  tmp$param <- as.character(Updated_Tracking_Keys$param)[match(tmp$Key,Updated_Tracking_Keys$Key)]
  tmp$scryfall <- Updated_Tracking_Keys$scryfall[match(tmp$Key,Updated_Tracking_Keys$Key)]
  # colnames(tmp)
  # colnames(combined_file)
  combined_file <- rbind(combined_file,tmp)
  
  Title_Date <- gsub("\\-","\\_",start_date)
  mybq <- bq_table(project = "gaeas-cradle", dataset = "ck_funny_money", table = paste(Title_Date,"_CK_Credit",sep=""))
  bq_table_upload(x=mybq, values = combined_file, fields=as_bq_fields(combined_file),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")
  start_date <- start_date + 1
  
}

mybq <- bq_table(project = "gaeas-cradle", dataset = "premiums", table = paste("CK_Credit",sep=""))
bq_table_upload(x=mybq, values = combined_file, fields=as_bq_fields(combined_file),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")



currentDate <- Sys.Date()
setwd("/home/cujo253/Reports/Growth Reports/Buylist_Growth")
temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp) - 1
start_date <- currentDate - Number_Of_Files
Title_Date <- gsub("\\-","\\_",start_date)
combined_file <- NULL

for (i in 1:Number_Of_Files){
  tmp <- read_csv(paste("/home/cujo253/Reports/Growth Reports/Buylist_Growth/",start_date,"_C_BuyList_Growth.csv",sep="")) %>%
    mutate(Param = Updated_Tracking_Keys$param[match(Unique_Keys,Updated_Tracking_Keys$Key)]) %>% select(!c(Name,Set,Rarity)) 
  
  Title_Date <- gsub("\\-","\\_",start_date)
  mybq <- bq_table(project = "gaeas-cradle", dataset = "buylist_growth", table = paste(Title_Date,"_buylist_growth",sep=""))
  bq_table_upload(x=mybq, values = tmp, fields=as_bq_fields(tmp),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")
  start_date <- start_date + 1
}

setwd("/home/cujo253/Reports/Growth Reports/Demand_Growth")
temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp) - 1
start_date <- currentDate - (Number_Of_Files -1)
Title_Date <- gsub("\\-","\\_",start_date)
combined_file <- NULL

for (i in 1:Number_Of_Files){
  tmp <- read_csv(paste("/home/cujo253/Reports/Growth Reports/Demand_Growth/",start_date,"_C_Demand_Growth.csv",sep="")) %>%
    mutate(Param = Updated_Tracking_Keys$param[match(Key,Updated_Tracking_Keys$Key)]) %>% select(!c(Name,Set,Rarity)) %>% select(Param, everything())

  Title_Date <- gsub("\\-","\\_",start_date)
  mybq <- bq_table(project = "gaeas-cradle", dataset = "vendor_growth", table = paste(Title_Date,"_vendor_grwoth",sep=""))
  bq_table_upload(x=mybq, values = tmp, fields=as_bq_fields(tmp),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")
  start_date <- start_date + 1
}

currentDate <- Sys.Date()
setwd("/home/cujo253/Reports/Growth Reports/Vendor_Growth")
temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp) - 1
start_date <- currentDate - (Number_Of_Files -1)
Title_Date <- gsub("\\-","\\_",start_date)
combined_file <- NULL
for (i in 1:Number_Of_Files){
  tmp <- read_csv(paste("/home/cujo253/Reports/Growth Reports/Vendor_Growth/",start_date,"_C_Vendor_Growth.csv",sep=""), 
                  col_types = cols(Todays_Sellers = col_number(), 
                                   Yesterday_Sellers = col_number(), 
                                   Week_Ago_Sellers = col_number(), 
                                   Month_Ago_Sellers = col_number(), 
                                   Yesterday_Sellers_Chg = col_number(), 
                                   Week_Ago_Sellers_Chg = col_number(), 
                                   Month_Ago_Sellers_Chg = col_number())) %>%
    mutate(Param = Updated_Tracking_Keys$param[match(Unique_Keys,Updated_Tracking_Keys$Key)]) %>% select(!c(Name,Set,Rarity)) %>% select(Param, everything())
  
  
  na_count = NULL
for( i in 1:nrow(tmp)){
  count <- sum(is.na(tmp[i,]))
  na_count <- rbind(na_count, count) 
}
na_count <- as.data.frame(na_count)
colnames(na_count) <- "count"
tmp <- tmp %>% mutate(na_count = na_count$count) %>% filter(na_count <= 5) 


Title_Date <- gsub("\\-","\\_",start_date)
mybq <- bq_table(project = "gaeas-cradle", dataset = "vendor_growth", table = paste(Title_Date,"_vendor_growth",sep=""))
bq_table_upload(x=mybq, values = tmp, fields=as_bq_fields(tmp),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")
start_date <- start_date + 1
}


currentDate <- Sys.Date()
setwd("/home/cujo253/Metrics/Daily_Velocity_Trackers")
temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp) - 1
start_date <- currentDate - 60
Title_Date <- gsub("\\-","\\_",start_date)
combined_file <- NULL

for (i in 1:Number_Of_Files){
  tmp <- read_csv(paste("/home/cujo253/Metrics/Daily_Velocity_Trackers/",start_date,"_Velocity.csv",sep=""), 
                  col_types = cols(data.is_foil = col_character())) %>% mutate(Semi = paste(data.name,data.edition,sep=""),
                                                                               Rarity = Updated_Tracking_Keys$Rarity[match(Semi,Updated_Tracking_Keys$Semi)],
                                                                               Unique_Keys = trimws(paste(data.name,data.edition,Rarity,data.is_foil,sep=""))) %>%
                  mutate(Param = Updated_Tracking_Keys$param[match(Unique_Keys,Updated_Tracking_Keys$Key)]) %>% select(!c("Semi"))
  tmp <- clean_names(tmp)
  Title_Date <- gsub("\\-","\\_",start_date)
  mybq <- bq_table(project = "gaeas-cradle", dataset = "ck_velocity", table = paste(Title_Date,"_CK_VELOCITY",sep=""))
  bq_table_upload(x=mybq, values = tmp, fields=as_bq_fields(tmp),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")
  start_date <- start_date + 1
}

currentDate <- Sys.Date()
setwd("/home/cujo253/Metrics/Time_Series_Results/Vendor")
temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp) - 1
start_date <- currentDate #- 4
Title_Date <- gsub("\\-","\\_",start_date)
combined_file <- NULL

for (i in 1:1){
  tmp <- read_csv(paste("/home/cujo253/Metrics/Time_Series_Results/Vendor/",start_date,"_Vendor_Times_Series.csv",sep=""), 
                  col_types = cols(`F/NF` = col_character())) %>% rename(c("Foil" = `F/NF`)) %>% 
    mutate(uuid = Updated_Tracking_Keys$uuid[match(Key,Updated_Tracking_Keys$Key)], Date = start_date) %>% select(uuid,everything())
   
  
  Title_Date <- gsub("\\-","\\_",start_date)
  mybq <- bq_table(project = "gaeas-cradle", dataset = "vendor_time_series", table = paste(Title_Date,"_VENDOR_FORECAST",sep=""))
  bq_table_upload(x=mybq, values = tmp, fields=as_bq_fields(tmp),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")
  start_date <- start_date + 1
}
