library(tidyverse)
library(bigrquery)
library(googlesheets4)
library(googledrive)
library(RSelenium)
library(rvest)
library(readr)

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
Updated_Tracking_Keys <- Updated_Tracking_Keys[c(3,5,6,8,9,10,11,12)]
colnames(Updated_Tracking_Keys) <- c("scryfall","param","abbr","Key","name","Set","Rarity","Foil")
ck_conversion <- read_csv("~/Essential_Referential_CSVS/mtgjson_ck_sets.csv")

setwd("/home/cujo253/Reports/High Confidence Reps")
tmp <- read_csv(paste("/home/cujo253/Reports/High Confidence Reps/",currentDate,"_Premium.csv",sep=""),col_types = cols(.default = "c"))
tmp <- tmp[1:12]
colnames(tmp)[5] <- c("Foil_Status")
colnames(tmp)[12] <- c("CK_ADJ_Rank")
tmp$Date <- Title_Date
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
start_date <- start_date + 1
Title_Date <- gsub("\\-","\\_",start_date)
mybq <- bq_table(project = "gaeas-cradle", dataset = "premiums", table = paste("TCG_CK_Data",sep=""))
bq_table_upload(x=mybq, values = combined_file, fields=as_bq_fields(combined_file),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")
