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

currentDate <- Sys.Date()-1
Title_Date <- gsub("\\-","\\_",currentDate)

setwd("/home/cujo253/Metrics/Daily_Velocity_Trackers")
tmp <- read_csv(paste("/home/cujo253/Metrics/Daily_Velocity_Trackers/",currentDate,"_Velocity.csv",sep=""),col_types = cols(`F/NF` = col_character()))
colnames(tmp) <- c("Rank","meta_date","name","edition","is_foil","Listings","qty_retail","price_buy","qty_buying","qty_diff","price_diff","Tier","Ratio_Rank","QTY_Rank","Sell_Rank","Velocity","Printings","Set_Bucket","Printing_Status","Velocity_Adjusted")
mybq <- bq_table(project = "gaeas-cradle", dataset = "ck_velocity", table = paste(Title_Date,"_ck_velocity",sep=""))
bq_table_upload(x=mybq, values = tmp, fields=as_bq_fields(tmp),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")

setwd("/home/cujo253/Reports/A_Game/Strict")
tmp <- read_csv(paste("/home/cujo253/Reports/A_Game/Strict/",currentDate,"_Strict.csv",sep=""),col_types = cols(`F/NF` = col_character()))
colnames(tmp)[5] <- c("Foil_Status")
mybq <- bq_table(project = "gaeas-cradle", dataset = "ck_strict", table = paste(Title_Date,"_ck_strict",sep=""))
bq_table_upload(x=mybq, values = tmp, fields=as_bq_fields(tmp),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")

setwd("/home/cujo253/Reports/A_Game/Goldilocks")
tmp <- read_csv(paste("/home/cujo253/Reports/A_Game/Goldilocks/",currentDate,"_Goldilocks.csv",sep=""),col_types = cols(`F/NF` = col_character()))
colnames(tmp)[5] <- c("Foil_Status")
mybq <- bq_table(project = "gaeas-cradle", dataset = "ck_goldilocks", table = paste(Title_Date,"_ck_velocity",sep=""))
bq_table_upload(x=mybq, values = tmp, fields=as_bq_fields(tmp),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")


setwd("/home/cujo253/Reports/A_Game/Relaxed")
tmp <- read_csv(paste("/home/cujo253/Reports/A_Game/Relaxed/",currentDate,"_Relaxed.csv",sep=""),col_types = cols(`F/NF` = col_character()))
colnames(tmp)[5] <- c("Foil_Status")
mybq <- bq_table(project = "gaeas-cradle", dataset = "ck_relaxed", table = paste(Title_Date,"_ck_velocity",sep=""))
bq_table_upload(x=mybq, values = tmp, fields=as_bq_fields(tmp),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")


setwd("/home/cujo253/Reports/High Confidence Reps")
tmp <- read_csv(paste("/home/cujo253/Reports/High Confidence Reps",currentDate,"_Premium.csv",sep=""),col_types = cols(`F/NF` = col_character()))
tmp <- tmp[1:12]
colnames(tmp)[5] <- c("Foil_Status")
mybq <- bq_table(project = "gaeas-cradle", dataset = "premiums", table = paste(Title_Date,"premiums",sep=""))
bq_table_upload(x=mybq, values = tmp, fields=as_bq_fields(tmp),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")

setwd("/home/cujo253/Reports/TCG Vendor")
tmp <- read_csv(paste("/home/cujo253/Reports/TCG Vendor/",currentDate,"_TCG.csv",sep=""),col_types = cols(`F/NF` = col_character()))
colnames(tmp)[6] <- c("Listings")
mybq <- bq_table(project = "gaeas-cradle", dataset = "tcg_vendor", table = paste(Title_Date,"_tcg_vendor",sep=""))
bq_table_upload(x=mybq, values = tmp, fields=as_bq_fields(tmp),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")


setwd("/home/cujo253/Reports/KPI/Master")
tmp <- read_csv(paste("/home/cujo253/Reports/KPI/Master/",currentDate,"_Master_KPI.csv",sep=""),col_types = cols(`F/NF` = col_character()))
colnames(tmp)[5] <- c("Foil_Status")
mybq <- bq_table(project = "gaeas-cradle", dataset = "master_kpi", table = paste(Title_Date,"master_kpi",sep=""))
bq_table_upload(x=mybq, values = tmp, fields=as_bq_fields(tmp),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")

setwd("/home/cujo253/Reports/Tokyo")
tmp <- read_csv(paste("/home/cujo253/Reports/Tokyo/",currentDate,"_Tokyo.csv",sep=""))
#tmp  <- read_csv(tmp, col_types = cols(.default = "c"))
#colnames(tmp)[5] <- c("Foil_Status")
mybq <- bq_table(project = "gaeas-cradle", dataset = "tokyo", table = paste(Title_Date,"_Tokyo",sep=""))
bq_table_upload(x=mybq, values = tmp, fields=as_bq_fields(tmp),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")

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
