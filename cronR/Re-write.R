#Functions & packages####
invisible(right <- function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}) #Recreating the right function from Excel 
invisible(left <- function(text, num_char) {
  substr(text, 1, num_char)
}) #Recreating the left function from Excel 
invisible(funk <- function(t){
  ifelse(nchar(t) <= 10, right(t,1),ifelse(nchar(t)<=190, right(t,2),ifelse(nchar(t)>=191, right((t),3),0)))
}) #Character Count utilization of 'left'&'right' functions for quantity breakdown
invisible(moveme <- function (invec, movecommand) {
  movecommand <- lapply(strsplit(strsplit(movecommand, ";")[[1]], 
                                 ",|\\s+"), function(x) x[x != ""])
  movelist <- lapply(movecommand, function(x) {
    Where <- x[which(x %in% c("before", "after", "first", 
                              "last")):length(x)]
    ToMove <- setdiff(x, Where)
    list(ToMove, Where)
  })
  myVec <- invec
  for (i in seq_along(movelist)) {
    temp <- setdiff(myVec, movelist[[i]][[1]])
    A <- movelist[[i]][[2]][1]
    if (A %in% c("before", "after")) {
      ba <- movelist[[i]][[2]][2]
      if (A == "before") {
        after <- match(ba, temp) - 1
      }
      else if (A == "after") {
        after <- match(ba, temp)
      }
    }
    else if (A == "first") {
      after <- 0
    }
    else if (A == "last") {
      after <- length(myVec)
    }
    myVec <- append(temp, values = movelist[[i]][[1]], after = after)
  }
  myVec
})
invisible(gaeas_cradle <- function(email){
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "gaeas-cradle",
    dataset = "premiums",
    billing = "gaeas-cradle"
  )
  bq_auth(email = email, use_oob = TRUE)
  options(scipen = 20)
  con
})
invisible(tracker_creation <- function(tracker){
  tracker = tracker %>% `colnames<-` (c("Key",format(seq(from = (currentDate - 21), to = currentDate, by = 'day'),format = "%Y-%m-%d"))) %>%
            mutate(Name = Updated_Tracking_Keys$name[match(Key,Updated_Tracking_Keys$Key)]) %>%
            mutate(Set = Updated_Tracking_Keys$Set[match(Key,Updated_Tracking_Keys$Key)]) %>%
            mutate(Rarity = Updated_Tracking_Keys$Rarity[match(Key,Updated_Tracking_Keys$Key)]) %>%
            mutate(Foil = Updated_Tracking_Keys$Foil[match(Key,Updated_Tracking_Keys$Key)]) %>% as.data.frame() %>%
            relocate(any_of(c("Key", "Name", "Set", "Rarity", "Foil"))) %>% replace_na(list(Foil = ""))
  tracker
})
invisible(prior_3_weeks <- function(tracker){
  MBT <- tracker[,-c(1:5)]
  ncol(MBT)
  MBT <- sapply(MBT,as.numeric)
  New <- Buylist_Tracker$Key
  for (i in 1:ncol(MBT)-1){
    New_Element <- MBT[,(i+1)] - MBT[,i]
    New_Element <- as.data.frame(New_Element)
    New <- cbind(New, New_Element)
  }
  
  New <- as.data.frame(New)
  New[is.na(New)] <- 0
  ncol(New)
  Binary_Form <- ifelse(New[,c(2:ncol(New))]>0,1,ifelse(New[,c(2:ncol(New))]<0,-1,0))
  
  Binary_Form <- as.data.frame(Binary_Form)
  Binary_Form <- sapply(Binary_Form, as.numeric)
  Binary_Form <- as.data.frame(Binary_Form)
  Binary_Form$Key <- Buylist_Tracker$Key
  
  Binary_Form <- Binary_Form[,-ncol(Binary_Form)]
  Three_Weeks_Binary <- Binary_Form[,c((ncol(Binary_Form)-21):ncol(Binary_Form))]
  SPS_21 <- rowSums(Three_Weeks_Binary)
  SPS_21 <- as.data.frame(SPS_21)
  
  Fifteen_Days_Binary <- Binary_Form[,c((ncol(Binary_Form)-15):ncol(Binary_Form))]
  SPS_15 <- rowSums(Fifteen_Days_Binary)
  SPS_15 <- as.data.frame(SPS_15)
  
  Seven_Day_Binary <- Binary_Form[,c((ncol(Binary_Form)-7):ncol(Binary_Form))]
  SPS_7 <- rowSums(Seven_Day_Binary)
  SPS_7 <- as.data.frame(SPS_7)
  bl_metrics <- data.frame(SPS_21$SPS_21,SPS_15$SPS_15,SPS_7$SPS_7)
  bl_metrics$Key <- Buylist_Tracker$Key
  bl_metrics <- bl_metrics[moveme(names(bl_metrics), "Key first")]
  tbl_metrics <- t(bl_metrics)
  tbl_metrics <- as.data.frame(tbl_metrics)
  
  tbl_metrics <- as.data.frame(tbl_metrics)
  tbl_numbers <- (tbl_metrics[-1,])
  tbl_numbers <- lapply(tbl_numbers,as.character)
  tbl_numbers <- lapply(tbl_numbers,as.numeric)
  tbl_numbers <- data.frame(tbl_numbers)
  why <- colSums(tbl_numbers)
  
  Final <- data.frame(Buylist_Tracker[,1:5],bl_metrics$SPS_21.SPS_21,bl_metrics$SPS_15.SPS_15,bl_metrics$SPS_7.SPS_7,why)
  colnames(Final) <- c("Key","Name","Set","Rarity","N/NF","21_Day_Rank","15_Day_Rank","7_Day_Rank","Rank_Sums")
  Final
})
invisible(ck_tcg_comparison <- function(CK_Retail_Comparison){
  MBT <- CK_Retail_Comparison[,-c(1)]
  MBT <- sapply(MBT,as.numeric)
  New <- CK_Retail_Comparison$Key
  for (i in 1:ncol(MBT)-1){
    New_Element <- MBT[,(i+1)] - MBT[,i]
    New_Element <- as.data.frame(New_Element)
    New <- cbind(New, New_Element)
  }
  
  New <- as.data.frame(New)
  New[is.na(New)] <- 0
  New <- New[-1]
  New <- sapply(New, as.numeric)
  
  
  Three_Weeks_Sum <- New[,c((ncol(New)-60):ncol(New))]
  Up_Down <- rowSums(Three_Weeks_Sum) %>% data.frame() %>% `colnames<-` (c("sumup"))
  summary(Up_Down)
  
  Fifteen_Days_Sum <- New[,c((ncol(New)-21):ncol(New))]
  Up_Down_Three_Weeks <- rowSums(Fifteen_Days_Sum) %>% as.data.frame() %>% `colnames<-` (c("sumup"))
  
  Seven_Day_Sum <- New[,c((ncol(New)-7):ncol(New))]
  Up_Down_Seven_Days <- rowSums(Seven_Day_Sum) %>% as.data.frame() %>% `colnames<-` (c("sumup"))

  
  CK_Retail_Comparison$All_Time <- Up_Down$sumup
  CK_Retail_Comparison$Three_Weeks <- Up_Down_Three_Weeks$sumup
  CK_Retail_Comparison$One_Week <- Up_Down_Seven_Days$sumup
  CK_Retail_Comparison_AT <- CK_Retail_Comparison[order(CK_Retail_Comparison$All_Time),]
  CK_Retail_Comparison_AT
})
invisible(chrome <-function(ip){
  remDr = remoteDriver(remoteServerAddr = ip, port = 4445L, browser = "chrome")
  remDr$open()
  remDr$maxWindowSize()
  remDr
})
pacman::p_load(tidyverse,rvest,jsonlite,devtools,googlesheets4,googledrive,googlesheets,readr,dplyr,gargle,httr,bigrquery,RSelenium)

#Refresh Our Master Roster####
content <- fromJSON("https://mtgjson.com/api/v5/AllPrintings.json")
library(tidyjson)
sets_of_interest <- content$data %>% names()

Card_Dictionary <- NULL
for(set in sets_of_interest){
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
    colors <- temp$data$cards$colors
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
    info <- cbind(rdate,uuid,scryfall_id,mcmid,tcg_ID,card,set,abbr,rarity,number,types,manaCost,colors,hasFoil,hasNonFoil,isAlternative,standard,pioneer,modern,legacy,commander,pauper,ckid,ckid_f)
    Card_Dictionary <- rbind(Card_Dictionary,info)
  }
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
#Card_Dictionary$colors <- unlist(ifelse(identical(Card_Dictionary$colors,character(0))==T,NA,Card_Dictionary$colors))
Card_Dictionary$hasFoil <- unlist(Card_Dictionary[14])
Card_Dictionary$hasNonFoil <- unlist(Card_Dictionary[15])
Card_Dictionary$isAlternative <- unlist(Card_Dictionary[16])
#Card_Dictionary$variations <- unlist(Card_Dictionary[15])
Card_Dictionary$standard <- unlist(Card_Dictionary[17])
Card_Dictionary$pioneer <- unlist(Card_Dictionary[18])
Card_Dictionary$modern <- unlist(Card_Dictionary[19])
Card_Dictionary$legacy <- unlist(Card_Dictionary[20])
Card_Dictionary$commander <- unlist(Card_Dictionary[21])
Card_Dictionary$pauper <- unlist(Card_Dictionary[22])
Card_Dictionary$ckid <- unlist(Card_Dictionary[23])
Card_Dictionary$ckid_f <- unlist(Card_Dictionary[24])
#Card_Dictionary <- Card_Dictionary[-11]
Card_Dictionary <- Card_Dictionary[-13]
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

Shortened_Dictionary <- Entire_Dictionary[c(1,2,3,4,5,8,10,24,6,7,9,13,11,12,22,23)]
Shortened_Dictionary$Key <- paste(Shortened_Dictionary$card,Shortened_Dictionary$set,Shortened_Dictionary$rarity,Shortened_Dictionary$hasFoil,sep="")

setwd("/home/cujo253/Essential_Referential_CSVS/")
csvFileName <- paste("C20_Addition",".csv",sep="")
write.csv(Shortened_Dictionary, file=csvFileName, row.names = FALSE)

con <- gaeas_cradle("wolfoftinstreet@gmail.com")
mybq <- bq_table(project = "gaeas-cradle", dataset = "roster", table = paste("mtgjson",sep=""))
bq_table_upload(x=mybq, values = Shortened_Dictionary, fields=as_bq_fields(Shortened_Dictionary),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")
print("BQ Premium Upload Successful!")


#CK Buylist####
#Call full buylist via "Raw_CK_Buylist" data frame
#Call functional (/slimmed) buylist via "CK_Buylist_Retrieved" data frame
Sets <- read.csv("/home/cujo253/Essential_Referential_CSVS/Sets.csv",stringsAsFactors = TRUE)

ck_conversion <- read_csv("~/Essential_Referential_CSVS/mtgjson_ck_sets.csv")

tryCatch({Updated_Tracking_Keys <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(hasFoil = col_character())) %>%
  #rename(c("scryfall_id" = "scryfall","tcg_ID"="param","card" = "name", "set" = "Set", "rarity" = "Rarity","hasFoil" = "Foil")) %>%
  rename(c("scryfall" = "scryfall_id","param"="tcg_ID","name" = "card", "Set" = "set", "Rarity" = "rarity","Foil" = "hasFoil")) %>%
  mutate(Semi = paste(name, Set,sep=""))},error = function(e){Updated_Tracking_Keys <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(hasFoil = col_character())) %>%
    rename(c("scryfall_id" = "scryfall","tcg_ID"="param","card" = "name", "set" = "Set", "rarity" = "Rarity","hasFoil" = "Foil")) %>%
    #rename(c("scryfall" = "scryfall_id","param"="tcg_ID","name" = "card", "Set" = "set", "Rarity" = "rarity","Foil" = "hasFoil")) %>%
    mutate(Semi = paste(name, Set,sep=""))})

CK_Smaller_List <- fromJSON("https://api.cardkingdom.com/api/pricelist") %>% 
                as.data.frame() %>%
                mutate(data.edition = ifelse(data.edition == "Promotional",data.variation,data.edition)) %>%
                mutate(data.edition = ck_conversion$Standardized[match(data.edition,ck_conversion$CK)]) %>%
                mutate(Semi = paste(data.name,data.edition, sep="")) %>%
                mutate(data.is_foil = ifelse(data.is_foil == "false", "", "FOIL")) %>%
                mutate(rarity = Updated_Tracking_Keys$Rarity[match(Semi, Updated_Tracking_Keys$Semi)]) %>%
                mutate(CK_Key = trimws(paste(data.name, data.edition, rarity," ",data.is_foil, sep=""))) %>%
                select(data.id, CK_Key,data.name,data.edition,rarity, data.is_foil,data.price_buy,data.qty_buying) %>%
                `colnames<-` (c("ckid","CK_Key","Card Name","Set","Rarity","NF/F","BL_Value","Qty_Des")) %>%
                mutate(Gold_Merge = Sets$GF_Abbr[match(Set,Sets$CK_BL_Scrape_Sets)]) %>%
                mutate(MTG_Gold_Key = paste(`Card Name`,Gold_Merge, sep ="")) %>%
                na.omit()

# View(CK_Smaller_List)
#Goldfish Pricing Retrieval#### 
sets = Sets %>% select(sets) %>% filter(sets != "") #%>% as.list()
total <- nrow(sets)

Gold_Market <-NULL #Create null value - note each scrape is given it's own unique null value so that we have each scrapes original source material to prevent excessive scrapping
for(i in 1:total){
  Gold <- paste("https://www.mtggoldfish.com/index/",sets[i,],"#paper",sep="") %>% read_html()%>%html_nodes("table") %>% .[2] %>% html_table(fill = TRUE) %>% as.data.frame() 
  Gold_Market <- rbind(Gold_Market,Gold)
  Sys.sleep(.05)
}
Back_Up_Gold <- Gold_Market
#Gold_Market <- Back_Up_Gold
#View(Gold_Market)
sets_Foil = Sets %>% select(sets_Foil) %>% filter(sets_Foil != "") #%>% as.list() %>% as.character()
total <- nrow(sets_Foil)

Gold_Foil_Market <-NULL #Create null value - note each scrape is given it's own unique null value so that we have each scrapes original source material to prevent excessive scrapping
for(i in 1:total){
  try(Shiny_Gold <- paste("https://www.mtggoldfish.com/index/",sets_Foil[i,],"#paper",sep="") %>% read_html()%>%html_nodes("table") %>% .[2] %>% html_table(fill = TRUE) %>% as.data.frame())
  Gold_Foil_Market <- rbind(Gold_Foil_Market,Shiny_Gold)
  Sys.sleep(.05)
}
End_Time <- Sys.time()
Foil_Market <- Gold_Foil_Market

Gold_Market <- Gold_Market %>% select(Daily, Card, Set,Price) %>% mutate(Price = suppressWarnings(round(as.numeric(Price) * .845,2))) %>% mutate(Daily = paste(Card,Set, sep=""))
Foil_Market <- Foil_Market %>% select(Daily, Card, Set,Price) %>% mutate(Price = suppressWarnings(round(as.numeric(Price) * .845,2))) %>% mutate(Daily = paste(Card,Set, sep=""))

CK_Smaller_List$Gold_Market <- ifelse(CK_Smaller_List$`NF/F` == " FOIL", Foil_Market$Price[match(CK_Smaller_List$MTG_Gold_Key,Foil_Market$Daily)] ,Gold_Market$Price[match(CK_Smaller_List$MTG_Gold_Key,Gold_Market$Daily)])

Basic_Market_Review <- CK_Smaller_List %>% 
                      select(ckid,CK_Key,`Card Name`, Set, Rarity, `NF/F`, Gold_Market, Qty_Des, BL_Value, Gold_Market) %>%
                      replace_na(list(Gold_Market = 0)) %>%
                      `colnames<-` (c("ckid","Key","Card","Set","Rarity","F/NF","MKT_Est","BL","BL Qty"))

#CK Best Sellers####
library(foreach)
library(doParallel)
Limit <- data.frame(raw_elements = read_html("https://www.cardkingdom.com/catalog/view?filter%5Bipp%5D=60&filter%5Bsort%5D=most_popular&filter%5Bsearch%5D=mtg_advanced&filter%5Bcategory_id%5D=0&filter%5Bmulti%5D%5B0%5D=1&filter%5Btype_mode%5D=any&filter%5Bmanaprod_select%5D=any&page=1") %>% html_nodes("a") %>% html_attr("href")) %>%  filter(grepl("page=",raw_elements)) %>%as.data.frame() %>% slice(4:20) %>% lapply(as.character) %>% as.data.frame() %>% mutate(pages = parse_number(str_sub(raw_elements,-5,-1))) %>% mutate(pages = as.numeric(pages)) %>% as.data.frame() %>% select(pages) %>% max()    
cl <- makeCluster(2, type = "FORK")
registerDoParallel(cl)

Start_Time <- Sys.time()

CK_Prices_df <- foreach(i = 1:Limit, .packages = c("rvest","httr"),.combine=rbind)%dopar% {
  Sys.sleep(.25)
  CK_Results <- GET(paste("https://www.cardkingdom.com/catalog/view?filter%5Bipp%5D=60&filter%5Bsort%5D=most_popular&filter%5Bsearch%5D=mtg_advanced&filter%5Bcategory_id%5D=0&filter%5Bmulti%5D%5B0%5D=1&filter%5Btype_mode%5D=any&filter%5Bmanaprod_select%5D=any&page=",i,sep=""))#, body = body)
  Card <- content(CK_Results,"text") %>% read_html %>% html_nodes(".productDetailTitle") %>% html_text()
  Set <- gsub(" \\([A-Z]\\)$","",trimws(content(CK_Results,"text") %>% read_html%>% html_nodes(".productDetailSet") %>% html_text())) 
  Rarity <- gsub("\\)","",gsub("^.*\\s\\(","",trimws(content(CK_Results,"text") %>% read_html%>% html_nodes(".productDetailSet") %>% html_text())))
  Price <- as.numeric(gsub("\\$","",trimws(content(CK_Results,"text") %>% read_html %>% html_nodes(".stylePrice") %>% html_text()))[seq(1, length(gsub("\\$","",trimws(content(CK_Results,"text") %>% read_html %>% html_nodes(".stylePrice") %>% html_text()))),4)])
  key <- paste(Card, Set, Rarity,sep="")
  Results <- data.frame(key,Card,Set,Rarity,Price,i)
}
End_Time <- Sys.time()
stopCluster(cl)
colnames(CK_Prices_df)[6] <- "Rank"
CK_Prices_df <- CK_Prices_df %>% arrange((Rank)) %>% mutate(Rank = seq(nrow(CK_Prices_df))) %>% select(Card, Set, Rarity, Price, key, Rank)
print(paste("CK Best Selling Data Acquisition Lasted:",round(End_Time - Start_Time,2),"Minutes"))

CK_Smaller_List$CK_Rank <- CK_Prices_df$Rank[match(CK_Smaller_List$CK_Key,trimws(CK_Prices_df$key))] #Merge the CK best selling rankings with our original CK Buylist scrape
Low_Confidence_Report <- CK_Smaller_List
#TCG- Market####
Start_Time <- Sys.time()
A <- 0
B <- 100
C <- 100
TCG__Best_Sellers <- NULL
body <- paste('{
    "algorithm": "salesrel",
    "context": {
          "cart": {},
          "shippingCountry": "US"
              },
    "from": "',A,'",
    "size": "',B,'",
    "filters": {
        "range": {},
        "term": {
            "productLineName": [
                "magic"
            ],
            "productTypeName": [
                "Cards"
            ],
            "rarityName": [
                "Mythic",
                "Uncommon",
                "Rare"
            ]
        }
    }
}',
              sep="")
A <- B 
B <- 200
TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body)
TCG_Results_1 <- (content(TCG_Results,"parsed"))$results
if(length(TCG_Results_1[[1]]$results) == 0){Sys.sleep(10);TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body); TCG_Results_1 <- (content(TCG_Results,"parsed"))$results}
i = 1
for(i in 1:C){
  Name <- TCG_Results_1[[1]]$results[[i]]$productName
  Set <- TCG_Results_1[[1]]$results[[i]]$setName
  Rarity <- TCG_Results_1[[1]]$results[[i]]$rarityName
  MKT_EST <- TCG_Results_1[[1]]$results[[i]]$marketPrice
  Listings <-TCG_Results_1[[1]]$results[[i]]$totalListings
  MKT <- ifelse((is.null(TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping) == T), 0, TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping)
  Product_ID <- TCG_Results_1[[1]]$results[[i]]$productId
  Direct_Listings <- 0
  Total_Copies <- NULL
  Potential_Direct_Copies <- NULL
  limit <- if(Listings < 3){Listings}else{3}
  if(limit >0){
    for(j in 1:limit){
      if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$sellerRating > 0){
        Direct_Listings <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directInventory
        if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directSeller == T){
          dcopies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
          Potential_Direct_Copies <- rbind(Potential_Direct_Copies,dcopies)}
        else{
          Copies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
          Total_Copies <- rbind(Total_Copies,Copies)
        }
      }
    }
  }
  Potential_Direct_Copies <- sum(Potential_Direct_Copies)
  Total_Copies <- sum(Total_Copies)
  if(Direct_Listings == 0){Total_Copies <- Total_Copies + Potential_Direct_Copies}
  Line_Item <- cbind(Name,Set,Rarity,MKT_EST,Listings,MKT,Product_ID,Direct_Listings,Potential_Direct_Copies,Total_Copies)
  TCG__Best_Sellers <- rbind(TCG__Best_Sellers, Line_Item)
}

for(i in 1:97){
  body <- paste('{
        "algorithm": "salesrel",
            "context": {
              "cart": {},
              "shippingCountry": "US"
                  },
        "from": "',A,'",
        "size": "',B,'",
        "filters": {
            "range": {},
            "term": {
                "productLineName": [
                    "magic"
                ],
                "productTypeName": [
                    "Cards"
                ],
                "rarityName": [
                    "Mythic",
                    "Uncommon",
                    "Rare"
                ]
            }
        }
    }',
                sep="")
  A <- A + 100
  B <- 100
  C <- 100
  TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body)
  TCG_Results_1 <- (content(TCG_Results,"parsed"))$results
  repeat{
    if(length(TCG_Results_1[[1]]$results) == 0){Sys.sleep(1);TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body); TCG_Results_1 <- (content(TCG_Results,"parsed"))$results}
    if((length(TCG_Results_1[[1]]$results) != 0)) break
  }
  for(i in 1:C){
    Name <- TCG_Results_1[[1]]$results[[i]]$productName
    Set <- TCG_Results_1[[1]]$results[[i]]$setName
    Rarity <- TCG_Results_1[[1]]$results[[i]]$rarityName
    MKT_EST <- ifelse( is.null(TCG_Results_1[[1]]$results[[i]]$marketPrice) == T, 0, TCG_Results_1[[1]]$results[[i]]$marketPrice)
    Listings <-TCG_Results_1[[1]]$results[[i]]$totalListings
    MKT <- ifelse((is.null(TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping) == T), 0, TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping)
    Product_ID <- TCG_Results_1[[1]]$results[[i]]$productId
    Direct_Listings <- 0
    Total_Copies <- NULL
    Potential_Direct_Copies <- NULL
    limit <- if(Listings < 3){Listings}else{3}
    if(limit >0){
      for(j in 1:limit){
        if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$sellerRating > 0){
          Direct_Listings <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directInventory
          if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directSeller == T){
            dcopies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
            Potential_Direct_Copies <- rbind(Potential_Direct_Copies,dcopies)}
          else{
            Copies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
            Total_Copies <- rbind(Total_Copies,Copies)
          }
        }
      }
    }
    Potential_Direct_Copies <- sum(Potential_Direct_Copies)
    Total_Copies <- sum(Total_Copies)
    if(Direct_Listings == 0){Total_Copies <- Total_Copies + Potential_Direct_Copies}
    Line_Item <- cbind(Name,Set,Rarity,MKT_EST,Listings,MKT,Product_ID,Direct_Listings,Potential_Direct_Copies,Total_Copies)
    TCG__Best_Sellers <- rbind(TCG__Best_Sellers, Line_Item)
    colnames(TCG__Best_Sellers)
    colnames(Line_Item)    
  }
  #if(A >= 9990) break
  Sys.sleep(.25)
}
TCG__Best_Sellers <- unique(TCG__Best_Sellers)
TCG__Best_Sellers <- TCG__Best_Sellers %>% as.data.frame() %>% mutate(Rank = seq(nrow(TCG__Best_Sellers))) %>%
                      mutate(Rarity = ifelse(Rarity == "Mythic","M", ifelse(Rarity == "Rare", "R", ifelse(Rarity == "Uncommon", "U", ifelse(Rarity == "Common", "C", Rarity))))) %>%
                      mutate(Key = paste(Name,Set,Rarity,sep="")) %>% relocate(Key, .before = Name)

#sheets_deauth()
gs4_auth(email = "pachun95@gmail.com", use_oob = T)
sheet_write(
  TCG__Best_Sellers,
  ss = "/d/1Ef2FgpR-bOg28a8JetHTTIXFH4FeR3eSEj5wpIAVjlU",
  sheet = "TCG_Real_View"
)

remDr <- chrome("138.68.229.207")
remDr$navigate("https://www.tcgplayer.com/search/magic/product?productLineName=magic&page=1")
Sys.sleep(4)
remDr$findElement('xpath','//*[@id="app"]/div/section[2]/div/div[1]/button')$clickElement()
Sys.sleep(4)
remDr$findElement('xpath','//*[@id="app"]/div/section[2]/section/div[1]/div[2]/div[2]/div[2]/div[2]')$clickElement()
# webElem <- remDr$findElements("css", "iframe")
# remDr$switchToFrame(webElem[[1]])
stacked_text <- NULL
stacked_qty <- NULL
numbers <- remDr$findElements('css','.search-filter__option-count')
for(i in 1:length(numbers)){
  text <- numbers[[i]]$getElementText()
  stacked_qty <- rbind(stacked_qty,text)}

options <- remDr$findElements('css','.checkbox__option-value')
for(i in 1:length(options)){
  text <- options[[i]]$getElementText()
  stacked_text <- rbind(stacked_text,text)}

stacked_text <- cbind(stacked_text,stacked_qty)
stacked_backup <- stacked_text
#stacked_text <- stacked_backup

stacked_text <- stacked_text %>% as.data.frame() %>% slice(-c(1:45))
rownames(stacked_text) <- seq(nrow(stacked_text))
cutoff <- which(grepl("^Cards$",stacked_text$V1))
stacked_text <- tryCatch({stacked_text %>% unnest(cols = c(V1,V2)) %>% slice(-c(cutoff:nrow(stacked_text)),) %>% rename(c("V1" = "editions", "V2" = "qty"))},error = function(e){stacked_text %>% unnest(cols = c(V1,V2)) %>% slice(-c(cutoff:nrow(stacked_text)),) %>% rename(c("editions" = "V1", "qty" = "V2"))}) %>% 
  mutate(editions  = gsub(" ","-",gsub("\\'","",gsub("\\(","",gsub("\\)","",gsub(": ","-",tolower(editions))))))) %>% mutate(qty = as.numeric(qty))

groupings <- NULL
groupings <- as.numeric(stacked_text$qty[1])
groupings <- rbind(groupings,(stacked_text$qty[1] + stacked_text$qty[2]))
for(i in 3:nrow(stacked_text)){
  groupings <- rbind(groupings,(groupings[i-1] + stacked_text$qty[i]))
  if(groupings[i-1] >= 9000){groupings[i] = stacked_text$qty[i]}
}
groupings <- as.data.frame(groupings) %>% mutate(groups = NA)
ID = 1
for(i in 1:nrow(groupings)){
  if(groupings$V1[i+1]>=groupings$V1[i]){groupings$groups[i] = ID}else{groupings$groups[i] = ID}
  if(groupings$V1[i+1]<=groupings$V1[i]){ID = ID + 1}
  }
groupings[nrow(groupings),2] <- max(groupings$groups, na.rm = T)

stacked_text <- stacked_text %>% mutate(groups = groupings$groups,
                                        editions = paste('"',editions,'", ',sep=""))
bb <- stacked_text
library(plyr)
library(dplyr)
json_body_contents <- ddply(stacked_text,.(groups),summarise,paste0(unique(unlist(strsplit(editions,split=' '))),collapse=" "))[2]
library(dplyr)
colnames(json_body_contents) <- "stringeroonies"
json_body_contents$stringeroonies <- gsub(",$","",json_body_contents$stringeroonies)
stop_points <- tryCatch({stacked_text %>% group_by(groups) %>% summarise(grp_one = sum(qty[groups == 1]),
                                                                   grp_two = sum(qty[groups == 2]),
                                                                   grp_three = sum(qty[groups == 3]),
                                                                   grp_four = sum(qty[groups == 4]),
                                                                   grp_five = sum(qty[groups == 5]),
                                                                   grp_six = sum(qty[groups == 6])) %>% t() %>% as.data.frame() %>%
  `rownames<-`(seq(c(1:6))) %>% rename(c("V1" = "grp_amts"))}, error = function(e){stacked_text %>% 
    group_by(groups) %>% summarise(grp_one = sum(qty[groups == 1]),
    grp_two = sum(qty[groups == 2]),
    grp_three = sum(qty[groups == 3]),
    grp_four = sum(qty[groups == 4]),
    grp_five = sum(qty[groups == 5]),
    grp_six = sum(qty[groups == 6])) %>% t() %>% as.data.frame() %>%
      `rownames<-`(seq(c(1:6))) %>% rename(c("grp_amts" = "V1"))})
q = 1
All_TCG_Sets <- NULL
for(q in 1:max(groupings$groups)){
  A <- 0
  B <- 100
  C <- 100
  All_TCG <- NULL
  body <- paste('{
      "algorithm": "salesrel",
      "context": {
            "cart": {},
            "shippingCountry": "US"
                },
      "from": "',A,'",
      "size": "',B,'",
      "filters": {
          "range": {},
          "term": {
              "productLineName": [
                  "magic"
              ],
              "productTypeName": [
                  "Cards"
              ],
              "setName": [
                  ',json_body_contents$stringeroonies[q],'
              ]
          }
      }
  }',
                sep="")
  A <- B 
  B <- 200
  TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body)
  TCG_Results_1 <- (content(TCG_Results,"parsed"))$results
  if(length(TCG_Results_1[[1]]$results) == 0){Sys.sleep(10);TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body); TCG_Results_1 <- (content(TCG_Results,"parsed"))$results}
  i = 1
  for(i in 1:C){
    Name <- TCG_Results_1[[1]]$results[[i]]$productName
    Set <- TCG_Results_1[[1]]$results[[i]]$setName
    Rarity <- TCG_Results_1[[1]]$results[[i]]$rarityName
    MKT_EST <- TCG_Results_1[[1]]$results[[i]]$marketPrice
    Listings <-TCG_Results_1[[1]]$results[[i]]$totalListings
    MKT <- ifelse((is.null(TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping) == T), 0, TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping)
    Product_ID <- TCG_Results_1[[1]]$results[[i]]$productId
    Direct_Listings <- 0
    Total_Copies <- NULL
    Potential_Direct_Copies <- NULL
    limit <- if(Listings < 3){Listings}else{3}
    if(limit >0){
      for(j in 1:limit){
        if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$sellerRating > 0){
          Direct_Listings <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directInventory
          if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directSeller == T){
            dcopies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
            Potential_Direct_Copies <- rbind(Potential_Direct_Copies,dcopies)}
          else{
            Copies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
            Total_Copies <- rbind(Total_Copies,Copies)
          }
        }
      }
    }
    Potential_Direct_Copies <- sum(Potential_Direct_Copies)
    Total_Copies <- sum(Total_Copies)
    if(Direct_Listings == 0){Total_Copies <- Total_Copies + Potential_Direct_Copies}
    Line_Item <- cbind(Name,Set,Rarity,MKT_EST,Listings,MKT,Product_ID,Direct_Listings,Potential_Direct_Copies,Total_Copies)
    All_TCG <- rbind(All_TCG, Line_Item)
  }
  
  for(i in 1:(round_any(stop_points$grp_amts[q],100,floor)/100)-3) {
    body <- paste('{
          "algorithm": "salesrel",
              "context": {
                "cart": {},
                "shippingCountry": "US"
                    },
          "from": "',A,'",
          "size": "',B,'",
          "filters": {
              "range": {},
              "term": {
                  "productLineName": [
                      "magic"
                  ],
                  "productTypeName": [
                      "Cards"
                  ],
                  "setName": [
                  ',json_body_contents$stringeroonies[q],'
              ]
              }
          }
      }',
                  sep="")
    A <- A + 100
    B <- 100
    TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body)
    TCG_Results_1 <- (content(TCG_Results,"parsed"))$results
    # repeat{
    #   if(length(TCG_Results_1[[1]]$results) == 0){Sys.sleep(1);TCG_Results <- POST("https://mpapi.tcgplayer.com/v2/search/request?q=&isList=false", content_type_json(), body = body); TCG_Results_1 <- (content(TCG_Results,"parsed"))$results}
    #   if((length(TCG_Results_1[[1]]$results) != 0)) break
    # }
    C = length(TCG_Results_1[[1]]$results)
    if(C == 0)break
    for(i in 1:C){
      Name <- TCG_Results_1[[1]]$results[[i]]$productName
      Set <- TCG_Results_1[[1]]$results[[i]]$setName
      Rarity <- TCG_Results_1[[1]]$results[[i]]$rarityName
      MKT_EST <- ifelse( is.null(TCG_Results_1[[1]]$results[[i]]$marketPrice) == T, 0, TCG_Results_1[[1]]$results[[i]]$marketPrice)
      Listings <-length(TCG_Results_1[[1]]$results[[i]]$listings)
      MKT <- ifelse((is.null(TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping) == T), 0, TCG_Results_1[[1]]$results[[i]]$lowestPriceWithShipping)
      Product_ID <- TCG_Results_1[[1]]$results[[i]]$productId
      Direct_Listings <- 0
      Total_Copies <- NULL
      Potential_Direct_Copies <- NULL
      limit <- if(Listings < 3){Listings}else{3}
      if(limit >0){
        for(j in 1:limit){
          if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$sellerRating > 0){
            Direct_Listings <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directInventory
            if(TCG_Results_1[[1]]$results[[i]]$listings[[j]]$directSeller == T){
              dcopies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
              Potential_Direct_Copies <- rbind(Potential_Direct_Copies,dcopies)}
            else{
              Copies <- TCG_Results_1[[1]]$results[[i]]$listings[[j]]$quantity
              Total_Copies <- rbind(Total_Copies,Copies)
            }
          }
        }
      }
      Potential_Direct_Copies <- sum(Potential_Direct_Copies)
      Total_Copies <- sum(Total_Copies)
      if(Direct_Listings == 0){Total_Copies <- Total_Copies + Potential_Direct_Copies}
      Line_Item <- cbind(Name,Set,Rarity,MKT_EST,Listings,MKT,Product_ID,Direct_Listings,Potential_Direct_Copies,Total_Copies)
      All_TCG <- rbind(All_TCG, Line_Item)
      if(A >= round_any(stop_points$grp_amts[q],100,floor)) break
    }
    #if(A >= 9990) break
    Sys.sleep(.25)
  }
  All_TCG_Sets <- rbind(All_TCG_Sets,All_TCG)
}
TCG_Sets <- as.data.frame(All_TCG_Sets) %>% 
  mutate(Rarity = ifelse(Rarity == "Mythic","M", ifelse(Rarity == "Rare", "R", ifelse(Rarity == "Uncommon", "U", ifelse(Rarity == "Common", "C", ifelse(Rarity == "Land","L",ifelse(Rarity == "Promo","P",ifelse(Rarity == "Special","S",ifelse(Rarity == "Token","T",Rarity))))))))) %>% 
  mutate(Key = paste(Name,Set,Rarity,sep=""), 
         Rank = ifelse(!is.na(TCG__Best_Sellers$Rank[match(Key,TCG__Best_Sellers$Key)]),TCG__Best_Sellers$Rank[match(Key,TCG__Best_Sellers$Key)],NA )) %>%
  select(Key,everything())
TCG_Sets[5:ncol(TCG_Sets)] <- sapply(TCG_Sets[5:ncol(TCG_Sets)],as.numeric)

TCG <- TCG__Best_Sellers
TCG <- unique(rbind(TCG__Best_Sellers,TCG_Sets))
End_Time <- Sys.time()
print(paste("TCG Best Sellers Lasted:",round(End_Time - Start_Time,2)))
#TCG Formatting####
colnames(TCG) <- c("Primary_Key","Card_Name","Set","Rarity","MKT_EST","Vendor Listings","MKT","Product_ID","Direct_Listings","Potential_Direct_Copies","Total_Copies","Rank")
TCG$Primary_Key <- gsub(" \\([A-Z]\\d{2}\\)","",TCG$Primary_Key)
TCG$Set <- gsub(" \\([A-Z]\\d{2}\\)","",TCG$Set)

TCG$Primary_Key <- gsub("10th Edition","Tenth Edition",TCG$Primary_Key)
TCG$Set <- gsub("10th Edition","Tenth Edition",TCG$Set)

TCG$Primary_Key <- gsub("9th Edition","Ninth Edition",TCG$Primary_Key)
TCG$Set <- gsub("9th Edition","Ninth Edition",TCG$Set)

TCG$Primary_Key <- gsub("8th Edition","Eighth Edition",TCG$Primary_Key)
TCG$Set <- gsub("8th Edition","Eighth Edition",TCG$Set)

TCG$Primary_Key <- gsub("7th Edition","Seventh Edition",TCG$Primary_Key)
TCG$Set <- gsub("7th Edition","Seventh Edition",TCG$Set)

TCG$Primary_Key <- gsub("6th Edition","Sixth Edition",TCG$Primary_Key)
TCG$Set <- gsub("6th Edition","Sixth Edition",TCG$Set)

TCG$Primary_Key <- gsub("5th Edition","Fifth Edition",TCG$Primary_Key)
TCG$Set <- gsub("5th Edition","Fifth Edition",TCG$Set)

TCG$Primary_Key <- gsub("4th Edition","Fourth Edition",TCG$Primary_Key)
TCG$Set <- gsub("4th Edition","Fourth Edition",TCG$Set)

TCG <- TCG %>% mutate(MKT_EST = as.numeric(MKT_EST)) %>% mutate(`Vendor Listings` = as.numeric(`Vendor Listings`)) %>% mutate(MKT = as.numeric(MKT)) %>%
        mutate(Direct_Listings = as.numeric(Direct_Listings)) %>% mutate(Potential_Direct_Copies = as.numeric(Potential_Direct_Copies)) %>% mutate(Total_Copies = as.numeric(Total_Copies))
Vendor <- as.data.frame(TCG)
TCG_Vendor <- Vendor
Middle_Confidence_Report <- TCG_Vendor
TCG_Export = TCG

Sets_V2 <- Sets[c(1:5)] %>% `colnames<-` (c("CK_BL_Scrape_Sets","MTG_Goldfish_Sets","GF_Abbr","GF_Abbr_Foil","TCG_Key"))

TCG_Export <- TCG_Export %>% mutate(Set = Sets_V2$CK_BL_Scrape_Sets[match(Set,Sets_V2$TCG_Key)]) %>% mutate(Primary_Key = paste(Card_Name,Set, Rarity, sep = ""))

CK_Smaller_List <- CK_Smaller_List %>% mutate(CK_Key = trimws(CK_Key)) %>% mutate(TCG_Rank = TCG_Export$Rank[match(CK_Key,TCG_Export$Primary_Key)]) %>%
                    mutate(TCG_Price = TCG_Export$MKT_EST[match(CK_Key,TCG_Export$Primary_Key)]) %>% replace_na(list(Gold_Market = 0)) 

Pricing <- CK_Smaller_List %>% mutate(MKT_Est = ifelse(`NF/F` == " FOIL", Gold_Market,ifelse((is.na(TCG_Price)==T),Gold_Market,TCG_Price))) %>%
                  mutate(BL_Value = as.numeric(BL_Value)) %>% mutate(Arbit = round(ifelse(is.na(MKT_Est) != TRUE,(BL_Value - MKT_Est),0),2)) %>% replace_na(list(Arbit = 0)) %>%
                  mutate(CK_MKT = CK_Prices_df$Price[match(as.factor(trimws(CK_Key)),as.factor(trimws(CK_Prices_df$key)))]) %>% mutate(CK_Rank = as.numeric(CK_Rank)) %>% mutate(TCG_Rank = as.numeric(TCG_Rank))

Anchor_CK_price <- Pricing %>% arrange(CK_Rank) %>% select(CK_MKT) %>% slice(1) %>% as.numeric()
Worst_CK_Rank <- Pricing %>% mutate(CK_Rank = round(((CK_MKT/Anchor_CK_price)*CK_Rank),5)) %>%arrange((CK_Rank))%>% mutate(CK_Rank = seq(nrow(Pricing)))  %>%arrange(desc(CK_Rank)) %>% select(CK_Rank) %>% slice(1) %>% as.numeric() + 1
Worst_TCG_Rank <- Pricing %>% arrange(desc(TCG_Rank)) %>% select(TCG_Rank) %>% slice(1) %>% as.numeric() + 1


Pricing <- Pricing %>% mutate(CK_Rank = ifelse(CK_Rank == 0,Worst_CK_Rank,CK_Rank)) %>% arrange(CK_Rank) %>% mutate(CK_Rank = seq(nrow(Pricing)))

Ranking <- Pricing %>% mutate(CK_Rank = round(((CK_MKT/Anchor_CK_price)*CK_Rank),5)) %>% mutate(CK_Rank = ifelse(CK_Rank == 0,NA,CK_Rank)) %>% replace_na(list(CK_Rank = Worst_CK_Rank)) %>% arrange(CK_Rank) %>%
            mutate(CK_Rank = seq(nrow(Pricing))) %>% replace_na(list(TCG_Rank = Worst_CK_Rank)) %>% mutate(Weighted_Rank = round(ifelse(((is.na(CK_Rank) != TRUE) & (is.na(TCG_Rank) != TRUE)), (((CK_Rank*.0818)+(TCG_Rank*.5568)/(.5568+.0818))),ifelse(((is.na(CK_Rank) != TRUE) & (is.na(TCG_Rank) = TRUE)),CK_Rank,ifelse(((is.na(CK_Rank) = TRUE) & (is.na(TCG_Rank) != TRUE)),TCG_Rank,40001))),2)) %>%
            arrange(Weighted_Rank) %>% mutate(Demand_Pct_Conf = round(ifelse(((round(MKT_Est,2) == round(Pricing$TCG_Price,2)) & (is.na(TCG_Rank) != TRUE) & (CK_Rank < Worst_CK_Rank)), 64,ifelse(((MKT_Est = Pricing$TCG_Price) & (is.na(TCG_Rank) != TRUE)),56,ifelse((CK_Rank < Worst_CK_Rank),8,0))),0)) %>%
            arrange(Weighted_Rank) %>% mutate(Weighted_Rank = seq(nrow(Pricing))) %>% mutate(Vendor = TCG_Export$`Vendor Listings`[match(as.factor(CK_Key),as.factor(trimws(TCG_Export$Primary_Key)))]) %>% 
            mutate(TCG_Rank = ifelse(TCG_Rank == Worst_CK_Rank, NA,TCG_Rank ))
#Ranking %>% arrange(Weighted_Rank)

Final_Export <- Pricing %>% mutate(Vendor = Ranking$Vendor[match(CK_Key,Ranking$CK_Key)]) %>% mutate(Weighted_Rank = Ranking$Weighted_Rank[match(CK_Key,Ranking$CK_Key)]) %>%
                mutate(Adj_CK_Ranking = Ranking$CK_Rank[match(CK_Key,Ranking$CK_Key)]) 


Final_Export$Demand_PCT_Conf <-  ifelse(((Final_Export$Adj_CK_Rank >= Worst_CK_Rank)&(Final_Export$TCG_Rank == "")),0,ifelse(((Final_Export$Adj_CK_Rank < Worst_CK_Rank)&(Final_Export$TCG_Rank == "")),8,ifelse(((Final_Export$Adj_CK_Rank >= Worst_CK_Rank)&(Final_Export$TCG_Rank != "")),56,ifelse(((Final_Export$Adj_CK_Rank < Worst_CK_Rank)&(Final_Export$TCG_Rank != "")),64,0))))

Exclusion <- data.frame(Sets$Set_Excl,Sets$Excl_Excl) %>% `colnames<-`  (c("Set_Excl","Excl_Excl"))

Final_Export <- Final_Export %>% mutate(CK_MKT = Ranking$CK_MKT[match(CK_Key,Ranking$CK_Key)]) %>% mutate(MKT_Est = ifelse(MKT_Est == 0.00, CK_MKT, MKT_Est)) %>%
                select(CK_Key,`Card Name`,Set,Rarity, `NF/F`,Qty_Des,BL_Value,MKT_Est,Arbit,Vendor,TCG_Rank,Adj_CK_Ranking,Weighted_Rank,Demand_PCT_Conf) %>%
                `colnames<-` (c("Key","Card","Set","Rarity","F/NF","BL_QTY","BL","MKT","Arb","Sellers","TCG_Rank","CK_ADJ_Rank","OVR_Rank","%_of_Market")) %>%
                arrange(CK_ADJ_Rank) %>% mutate(CK_ADJ_Rank = seq(nrow(Final_Export))) %>% arrange(OVR_Rank) %>%
                mutate(MKT_TS_Single = round((MKT * 1.08875)+.78,2)) %>% mutate(MKT_TS_Set = round(((MKT * 4)* 1.08875)+.78,2)) %>%
                mutate(`Single_Arb_%` = round((BL - MKT_TS_Single)/MKT_TS_Single,2)) %>% mutate(`Set_Arb_%` = round(((BL*4) - MKT_TS_Set)/MKT_TS_Set,2)) %>%
                arrange(desc(`Single_Arb_%`)) %>% filter(MKT!=0) %>% mutate(Exclusion = Exclusion$Excl_Excl[match(Set,Exclusion$Set_Excl)]) %>%
                mutate(Sellers = as.numeric(Sellers))
#Personal Recommended View
#Final_Export %>% filter(`F/NF` == "") %>% filter(Exclusion != "Exclude") %>% filter(Sellers != "") %>% arrange(desc(`Set_Arb_%`)) %>% filter(Rarity == "M" |Rarity == "R") %>% filter(BL > 5.00)
#Export Premium & TCG####
con <- gaeas_cradle("wolfoftinstreet@gmail.com")
currentDate <- Sys.Date()
premium_bq_export <- tryCatch({Final_Export[c(1:12)] %>% rename(c("Foil_Status" = "F/NF")) %>% mutate(Date = currentDate)}, error = function(e){Final_Export[c(1:12)] %>% rename(c("F/NF" = "Foil_Status")) %>% mutate(Date = currentDate)})
if(colnames(premium_bq_export)[5] == "F/NF" ){colnames(premium_bq_export)[5] = c("Foil_Status")}

premium_bq_export$Set <- gsub("Ikoria: Lair of Behemoths Variants","Ikoria: Lair of Behemoths",gsub("Theros Beyond Death Variants","Theros Beyond Death",gsub("Vanguard","Vanguard Series",gsub("Deckmaster","Deckmasters",gsub("Promo Pack","M20 Promo Packs",gsub("Throne of Eldraine Variants","Throne of Eldraine",gsub("War of the Spark JPN Planeswalkers","War of the Spark",gsub("Collectors Ed.*","Intl. Collectorsâ€™ Edition",gsub("Duel Decks: Merfolk Vs. Goblins","Duel Decks: Merfolk vs. Goblins",gsub("Ravnica Allegiance: Guild Kits","RNA Guild Kit",gsub("Beatdown","Beatdown Box Set",gsub("Battle Royale","Battle Royale Box Set",gsub("Timeshifted","Time Spiral Timeshifted",gsub("Beta","Limited Edition Beta",gsub("Alpha","Limited Edition Alpha",gsub("3rd Edition","Revised Edition",gsub("Archenemy - Nicol Bolas","Archenemy",gsub("Modern Event Deck","Modern Event Deck 2014", premium_bq_export$Set))))))))))))))))))
premium_bq_export <- premium_bq_export %>% mutate(Set = ck_conversion$Standardized[match(Set,ck_conversion$CK)]) %>% 
                     mutate(Key = trimws(paste(Card,Set,Rarity," ",Foil_Status,sep=""))) %>% mutate(BL_QTY = as.numeric(as.character(BL_QTY))) %>%
                     mutate(TCG_Rank = as.numeric(as.character(TCG_Rank))) %>% mutate(CK_ADJ_Rank = as.numeric(as.character(CK_ADJ_Rank))) %>%
                     mutate(Sellers = as.numeric(as.character(Sellers))) %>% mutate(param = as.character(Updated_Tracking_Keys$param)[match(Key,Updated_Tracking_Keys$Key)]) %>%
                     mutate(scryfall = Updated_Tracking_Keys$scryfall[match(Key,Updated_Tracking_Keys$Key)])
mybq <- bq_table(project = "gaeas-cradle", dataset = "premiums", table = paste(gsub("-","_",currentDate),"_TCG_CK_Data",sep=""))
bq_table_upload(x=mybq, values = premium_bq_export, fields=as_bq_fields(premium_bq_export),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")
print("BQ Premium Upload Successful!")

tcgplayer_bq_export <- tryCatch({TCG_Vendor %>% rename(c("Vendors" = "Vendor Listings")) %>% mutate(Vendors = as.numeric(Vendors)) %>% select(Product_ID,MKT_EST,Vendors,MKT,Rank,Direct_Listings,Potential_Direct_Copies,Total_Copies)},error = function(e){TCG_Vendor %>% rename(c("Vendor Listings" = "Vendors")) %>% mutate(Vendors = as.numeric(Vendors)) %>% select(Product_ID,MKT_EST,Vendors,MKT,Rank,Direct_Listings,Potential_Direct_Copies,Total_Copies)})
mybq <- bq_table(project = "gaeas-cradle", dataset = "tcgplayer", table = paste(gsub("-","_",currentDate),"_TCGPLAYER",sep=""))
bq_table_upload(x=mybq, values = tcgplayer_bq_export, fields=as_bq_fields(tcgplayer_bq_export),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")
print("BQ TCG Upload Successful!")


#setwd("/home/cujo253/Reports/Low Confidence Reps")
#Basic_Market_Review <- Basic_Market_Review[c(1,2,3,4,5,8,7,6)]
#names(Basic_Market_Review)<- c("Key","Card","Set","Rarity","F/NF","BL_QTY","BL","MKT")
#csvFileName <- paste(currentDate,"_Basic",".csv",sep="")
#write.csv(Basic_Market_Review, file=csvFileName, row.names = FALSE)
#Funny Money Report####
CK_Price_Comparison <- CK_Prices_df %>% mutate(key = trimws(key)) %>% mutate(TCG_Price = as.numeric(Final_Export$MKT[match(key,Final_Export$Key)])) %>%
                       mutate(CK_TCG_PCT_DIFF = round(as.numeric(as.character(Price))/TCG_Price,4)) %>% 
                       select(key, Card, Set,Rarity,TCG_Price,Price) %>% `colnames<-` (c("Key","Card_Name","Set","Rarity","TCG_Price","CK_Price")) %>%
                       mutate(TCG_Price = ifelse(TCG_Price == 0.00,NA, TCG_Price)) %>%
                       na.omit() %>% mutate(Price_Diff = round((CK_Price-TCG_Price),2)) %>%
                       mutate(Price_Diff =  ifelse(Price_Diff == CK_Price, "Not Captured", Price_Diff)) %>%
                       filter(Price_Diff != "Not Captured") %>%
                       arrange(desc(Price_Diff)) %>% mutate(Set_Group = Exclusion$Excl_Excl[match(Set,Exclusion$Set_Excl)]) %>%
                       mutate(CK_BL = as.numeric(Final_Export$BL)[match(Key,Final_Export$Key)]) %>%
                       mutate(CK_BL_Backing = round((CK_BL/CK_Price),2)) %>%
                       mutate(TCG_BL_Backing = round((CK_BL/TCG_Price),2)) %>%
                       mutate(TCG_Vendors = Final_Export$Sellers[match(Key,Final_Export$Key)]) %>%
                       mutate(BL_Desired_Amt = Final_Export$BL_QTY[match(Key,Final_Export$Key)]) %>%
                       mutate(`F/NF` = "") %>% select(Key, Card_Name, Set, Rarity, `F/NF`,BL_Desired_Amt,CK_BL,TCG_Price,CK_Price,Price_Diff,TCG_Vendors,CK_BL_Backing,TCG_BL_Backing,Set_Group) %>%
                       `colnames<-` (c("Key","Card","Set","Rarity","F/NF","BL_QTY","BL","TCG_MKT","CK_MKT","MKT_Diff","Sellers","CK_MKT_%","TCG_MKT_%","Group"))

Funny_Money_Analysis <- CK_Price_Comparison %>% filter(Group != "Exclude") %>% mutate(TCG_Rank = Final_Export$TCG_Rank[match(Key,Final_Export$Key)]) %>%
                          mutate(CK_Rank = Final_Export$CK_ADJ_Rank[match(Key,Final_Export$Key)]) %>% replace_na(list(BL = ""))%>% filter(TCG_MKT != 1 & BL != "") %>%
                        arrange(desc(`TCG_MKT_%`))

FM <- Funny_Money_Analysis[c(1:14)]
colnames(FM)[5] <- c("Foil_Status")
colnames(FM)[12] <- c("CK_Backing")
colnames(FM)[13] <- c("TCG_Backing")
FM <- FM %>% mutate(Date = currentDate) %>% mutate(Foil_Status = ifelse(is.na(Foil_Status)==T,"",Foil_Status))
FM$Set <- gsub("Ikoria: Lair of Behemoths Variants","Ikoria: Lair of Behemoths",gsub("Theros Beyond Death Variants","Theros Beyond Death",gsub("Vanguard","Vanguard Series",gsub("Deckmaster","Deckmasters",gsub("Promo Pack","M20 Promo Packs",gsub("Throne of Eldraine Variants","Throne of Eldraine",gsub("War of the Spark JPN Planeswalkers","War of the Spark",gsub("Collectors Ed.*","Intl. Collectorsâ€™ Edition",gsub("Duel Decks: Merfolk Vs. Goblins","Duel Decks: Merfolk vs. Goblins",gsub("Ravnica Allegiance: Guild Kits","RNA Guild Kit",gsub("Beatdown","Beatdown Box Set",gsub("Battle Royale","Battle Royale Box Set",gsub("Timeshifted","Time Spiral Timeshifted",gsub("Beta","Limited Edition Beta",gsub("Alpha","Limited Edition Alpha",gsub("3rd Edition","Revised Edition",gsub("Archenemy - Nicol Bolas","Archenemy",gsub("Modern Event Deck","Modern Event Deck 2014", FM$Set))))))))))))))))))
FM <- FM %>% mutate(Set = ck_conversion$Standardized[match(Set,ck_conversion$CK)]) %>% mutate(Key = trimws(paste(Card,Set,Rarity," ",Foil_Status,sep=""))) %>%
  mutate(BL_QTY = as.numeric(as.character(BL_QTY)),
         BL = as.numeric(as.character(BL)),
         TCG_MKT = as.numeric(as.character(TCG_MKT)),
         CK_MKT = as.numeric(as.character(CK_MKT)),
         MKT_Diff = as.numeric(as.character(MKT_Diff)),
         CK_Backing = as.numeric(as.character(CK_Backing)),
         TCG_Backing = as.numeric(as.character(TCG_Backing)),
         Sellers = as.numeric(as.character(Sellers)),
         param = as.character(Updated_Tracking_Keys$param)[match(Key,Updated_Tracking_Keys$Key)],
         scryfall =  Updated_Tracking_Keys$scryfall[match(Key,Updated_Tracking_Keys$Key)])

mybq <- bq_table(project = "gaeas-cradle", dataset = "ck_funny_money", table = paste(gsub("-","_",currentDate),"_CK_Credit",sep=""))
bq_table_upload(x=mybq, values = FM, fields=as_bq_fields(FM),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")
print("BQ CK_Funny_Money Upload Successful!")

#Load in Dated Premium Reports####
#View(Final_Export)
currentDate <- Sys.Date()
con <- gaeas_cradle("wolfoftinstreet@gmail.com")

statement <- paste(
  "SELECT Key, Card, a.Set, Rarity, BL_QTY, BL, MKT, Arb, Sellers, TCG_Rank, CK_ADJ_Rank   ",
  "FROM `gaeas-cradle.premiums.",gsub("-","_",currentDate),"_TCG_CK_Data` a ",
  sep = ""
)
today_final_export <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)

statement <- paste(
  "SELECT Key, Card, a.Set, Rarity, BL_QTY, BL, MKT, Arb, Sellers, TCG_Rank, CK_ADJ_Rank   ",
  "FROM `gaeas-cradle.premiums.",gsub("-","_",(currentDate-1)),"_TCG_CK_Data` a ",
  sep = ""
)
Yesterday <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)

statement <- paste(
  "SELECT Key, Card, a.Set, Rarity, BL_QTY, BL, MKT, Arb, Sellers, TCG_Rank, CK_ADJ_Rank   ",
  "FROM `gaeas-cradle.premiums.",gsub("-","_",(currentDate-7)),"_TCG_CK_Data` a ",
  sep = ""
)
Week_Ago <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)

statement <- paste(
  "SELECT Key, Card, a.Set, Rarity, BL_QTY, BL, MKT, Arb, Sellers, TCG_Rank, CK_ADJ_Rank   ",
  "FROM `gaeas-cradle.premiums.",gsub("-","_",(currentDate-30)),"_TCG_CK_Data` a ",
  sep = ""
)
Month_Ago <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)



Unique_Keys <- dbSendQuery(con, statement = paste("SELECT DISTINCT Key, Card, a.Set,Rarity, Foil_Status ","FROM `gaeas-cradle.premiums.*` a ",'WHERE _TABLE_SUFFIX BETWEEN ',
                                                  'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 90 DAY)) AND ',
                                                  'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL -1 DAY)) ',sep = "")) %>% dbFetch(n= -1) %>%
                                                  `colnames<-` (c("Key","Name","Set","Rarity","Foil"))

#View(Unique_Keys)
#Growth Reports####
BuyList_Growth <- Unique_Keys %>% mutate(Todays_BL = as.numeric(today_final_export$BL[match(Key,today_final_export$Key)])) %>%
                  mutate(Yesterday_BL = Yesterday$BL[match(Key,Yesterday$Key)]) %>%
                  mutate(Week_Ago_BL = Week_Ago$BL[match(Key,Week_Ago$Key)]) %>%
                  mutate(Month_Ago_BL = Month_Ago$BL[match(Key,Month_Ago$Key)]) %>%
                  mutate(Yesterday_BL_Chg = round((Todays_BL - Yesterday_BL)/Yesterday_BL,4)) %>%
                  mutate(Week_Ago_BL_Chg = round((Todays_BL - Week_Ago_BL)/Week_Ago_BL,4)) %>%
                  mutate(Month_Ago_BL_Chg = round((Todays_BL - Month_Ago_BL)/Month_Ago_BL,4)) %>%
                  mutate(BuyList_Backing = Funny_Money_Analysis$CK_MKT[match(Key, Funny_Money_Analysis$Key)]) %>%
                  mutate(BuyList_Backing = (1 - round((BuyList_Backing - Todays_BL)/BuyList_Backing,4))) %>%
                  filter_at(vars(Todays_BL,Yesterday_BL,Week_Ago_BL,Month_Ago_BL), any_vars(!is.na(.)))


BuyList_Growth[BuyList_Growth == "Inf"] <- ""

Consistent_BuyLists <- BuyList_Growth

#View(Consistent_BuyLists)
#View(BuyList_Growth)
con <- gaeas_cradle("wolfoftinstreet@gmail.com")

statement <- paste(
  "SELECT *   ",
  "FROM `gaeas-cradle.tcgplayer.",gsub("-","_",currentDate),"_TCGPLAYER` a ",
  sep = ""
)
Today_Vendor <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)

statement <- paste(
  "SELECT *   ",
  "FROM `gaeas-cradle.tcgplayer.",gsub("-","_",(currentDate-1)),"_TCGPLAYER` a ",
  sep = ""
)
Yesterday_Vendor <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)

statement <- paste(
  "SELECT *   ",
  "FROM `gaeas-cradle.tcgplayer.",gsub("-","_",(currentDate-7)),"_TCGPLAYER` a ",
  sep = ""
)
Week_Ago_Vendor <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)

statement <- paste(
  "SELECT *   ",
  "FROM `gaeas-cradle.tcgplayer.",gsub("-","_",(currentDate-30)),"_TCGPLAYER` a ",
  sep = ""
)
Month_Ago_Vendor <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)

Vendor_Growth <- Unique_Keys %>% mutate(Product_ID = Updated_Tracking_Keys$param[match(Key,Updated_Tracking_Keys$Key)]) %>% 
                 mutate(Todays_Sellers = as.numeric(as.character(Today_Vendor$Vendors[match(Product_ID,Today_Vendor$Product_ID)]))) %>%
                 mutate(Yesterday_Sellers = as.numeric(as.character(Yesterday_Vendor$Vendors[match(Product_ID,Yesterday_Vendor$Product_ID)]))) %>%
                 mutate(Week_Ago_Sellers = as.numeric(as.character(Week_Ago_Vendor$Vendors[match(Product_ID,Week_Ago_Vendor$Product_ID)]))) %>%
                 mutate(Month_Ago_Sellers =as.numeric(as.character(Month_Ago_Vendor$Vendors[match(Product_ID,Month_Ago_Vendor$Product_ID)]))) %>%
                 filter(is.na(Product_ID) != T) %>% filter(Rarity != "C") %>% replace_na(list(Foil = ""))%>%filter(Foil == "") %>%
                 mutate(Yesterday_Sellers_Chg = round((Todays_Sellers - Yesterday_Sellers)/Yesterday_Sellers,4)*(-1)) %>%
                 mutate(Week_Ago_Sellers_Chg = round((Todays_Sellers - Week_Ago_Sellers)/Week_Ago_Sellers,4)*(-1)) %>%
                 mutate(Month_Ago_Sellers_Chg = round((Todays_Sellers - Month_Ago_Sellers)/Month_Ago_Sellers,4)*(-1)) %>%
                 mutate(Month_Ago_Sellers_Chg = ifelse(nchar(Month_Ago_Sellers)>2 & (Month_Ago_Sellers_Chg > .50 | Month_Ago_Sellers_Chg > .50), NA, Month_Ago_Sellers_Chg)) %>%
                 filter_at(vars(Todays_Sellers,Yesterday_Sellers,Week_Ago_Sellers,Month_Ago_Sellers), any_vars(!is.na(.)))

#View(Vendor_Growth)

Consistent_Vendors <- Vendor_Growth %>% select(-one_of("Product_ID","Foil")) %>% arrange(desc(Yesterday_Sellers_Chg))
#View(Consistent_Vendors)
#Consistent_Vendors <- na.omit(Consistent_Vendors)
TCG_Growth <- Unique_Keys %>%mutate(Todays_TCG = as.numeric(today_final_export$TCG_Rank[match(Key,today_final_export$Key)])) %>%
              mutate(Yesterday_TCG = Yesterday$TCG_Rank[match(Key,Yesterday$Key)]) %>%
              mutate(Week_Ago_TCG = Week_Ago$TCG_Rank[match(Key,Week_Ago$Key)]) %>%
              mutate(Month_Ago_TCG = Month_Ago$TCG_Rank[match(Key,Month_Ago$Key)]) %>%
              mutate(Yesterday_TCG_Chg =  round((Todays_TCG - Yesterday_TCG)/Yesterday_TCG,4)*(-1)) %>%
              mutate(Week_Ago_TCG_Chg = round((Todays_TCG - Week_Ago_TCG)/Week_Ago_TCG,4)*(-1)) %>%
              mutate(Month_Ago_TCG_Chg = round((Todays_TCG - Month_Ago_TCG)/Month_Ago_TCG,4)*(-1)) %>%
              mutate(BuyList_Backing = Funny_Money_Analysis$CK_MKT[match(Key, Funny_Money_Analysis$Key)]) %>%
              filter(Rarity != "C") %>%
              filter_at(vars(Todays_TCG,Yesterday_TCG,Week_Ago_TCG,Month_Ago_TCG), any_vars(!is.na(.)))
  
Consistent_Sellers <- TCG_Growth %>% arrange(desc(Yesterday_TCG_Chg))
#View(TCG_Growth)
#View(Consistent_Vendors)

Consistent_BuyLists_Export <- Consistent_BuyLists %>%
  mutate(Param = Updated_Tracking_Keys$param[match(Key,Updated_Tracking_Keys$Key)]) %>% select(Param, everything()) 

colnames(Consistent_BuyLists_Export)[2] <- "Unique_Keys"

Title_Date <- gsub("\\-","\\_",currentDate)
mybq <- bq_table(project = "gaeas-cradle", dataset = "buylist_growth", table = paste(Title_Date,"_buylist_growth",sep=""))
bq_table_upload(x=mybq, values = Consistent_BuyLists_Export, fields=as_bq_fields(Consistent_BuyLists_Export),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")

Consistent_Sellers_Export <- Consistent_Sellers %>%
  mutate(Param = Updated_Tracking_Keys$param[match(Key,Updated_Tracking_Keys$Key)]) %>% select(!c(Name,Set,Rarity)) %>% select(Param, everything()) %>%
  mutate(Todays_TCG = as.numeric(Todays_TCG),
         Yesterday_TCG = as.numeric(Yesterday_TCG),
         Week_Ago_TCG = as.numeric(Week_Ago_TCG),
         Month_Ago_TCG = as.numeric(Month_Ago_TCG),
         Yesterday_TCG_Chg = as.numeric(Yesterday_TCG_Chg),
         Week_Ago_TCG_Chg = as.numeric(Week_Ago_TCG_Chg),
         Month_Ago_TCG_Chg = as.numeric(Month_Ago_TCG_Chg))

Title_Date <- gsub("\\-","\\_",Sys.Date())
mybq <- bq_table(project = "gaeas-cradle", dataset = "demand_growth", table = paste(Title_Date,"_demand_growth",sep=""))
bq_table_upload(x=mybq, values = Consistent_Sellers_Export, fields=as_bq_fields(Consistent_Sellers_Export),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")

Consistent_Vendors_Export <- Consistent_Vendors %>%
  mutate(Param = Updated_Tracking_Keys$param[match(Key,Updated_Tracking_Keys$Key)]) %>% select(!c(Name,Set,Rarity)) %>% select(Param, everything())
colnames(Consistent_Vendors_Export)[2] <- "Unique_Keys"

na_count = NULL
for( i in 1:nrow(Consistent_Vendors)){
  count <- sum(is.na(Consistent_Vendors[i,]))
  na_count <- rbind(na_count, count) 
}
na_count <- as.data.frame(na_count)
colnames(na_count) <- "count"
Consistent_Vendors_Export <- Consistent_Vendors_Export %>% mutate(na_count = na_count$count) %>% filter(na_count <= 5)


Title_Date <- gsub("\\-","\\_",currentDate)
mybq <- bq_table(project = "gaeas-cradle", dataset = "vendor_growth", table = paste(Title_Date,"_vendor_growth",sep=""))
bq_table_upload(x=mybq, values = Consistent_Vendors_Export, fields=as_bq_fields(Consistent_Vendors_Export),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")


#Today's Buy List Movers####
#View(Consistent_BuyLists)

CB_Short <- Consistent_BuyLists %>% filter(Yesterday_BL>= 1.25) %>% filter(Todays_BL >= 1.25) %>% filter(Name != "") %>% arrange(desc(Yesterday_BL_Chg))

#View(CB_Short)
CB_Short_F <- CB_Short %>% filter(CB_Short$Foil == "FOIL") %>%
              filter(!is.na(Week_Ago_BL_Chg)) %>%
              filter(Week_Ago_BL_Chg > 0) %>%
              filter(!is.na(Month_Ago_BL_Chg)) %>%
              filter(Month_Ago_BL_Chg > 0) %>%
              arrange(desc(Yesterday_BL_Chg)) %>%
              mutate(Growth = ifelse((Todays_BL > Yesterday_BL & Yesterday_BL > Week_Ago_BL & Week_Ago_BL > Month_Ago_BL), "Yes", ifelse(((Todays_BL < Yesterday_BL & Yesterday_BL < Week_Ago_BL & Week_Ago_BL < Month_Ago_BL)), "No", "Mixed"))) %>%
              mutate(Highest = ifelse(Todays_BL >= Yesterday_BL & Todays_BL >= Week_Ago_BL & Todays_BL >= Month_Ago_BL , "Today", ifelse(Yesterday_BL >= Todays_BL & Yesterday_BL >= Week_Ago_BL & Yesterday_BL >= Month_Ago_BL, "Yesterday",ifelse(Week_Ago_BL >= Todays_BL & Week_Ago_BL >= Yesterday_BL & Week_Ago_BL >= Month_Ago_BL, "Week_Ago",ifelse(Month_Ago_BL >= Todays_BL & Month_Ago_BL >= Yesterday_BL & Month_Ago_BL >= Week_Ago_BL,"Month_Ago","")))) )



CB_Short_NF <- CB_Short %>% filter(CB_Short$Foil != "FOIL") %>% 
               filter(!is.na(BuyList_Backing)) %>%
               filter(BuyList_Backing >= .55) %>%
               filter(!is.na(Week_Ago_BL_Chg)) %>%
               filter(Week_Ago_BL_Chg > 0) %>%
               filter(!is.na(Month_Ago_BL_Chg)) %>%
               filter(Month_Ago_BL_Chg > 0) %>%
               arrange(desc(Yesterday_BL_Chg)) %>%
               mutate(Growth = ifelse((Todays_BL > Yesterday_BL & Yesterday_BL > Week_Ago_BL & Week_Ago_BL > Month_Ago_BL), "Yes", ifelse(((Todays_BL < Yesterday_BL & Yesterday_BL < Week_Ago_BL & Week_Ago_BL < Month_Ago_BL)), "No", "Mixed"))) %>%
               mutate(Highest = ifelse(Todays_BL >= Yesterday_BL & Todays_BL >= Week_Ago_BL & Todays_BL >= Month_Ago_BL , "Today", ifelse(Yesterday_BL >= Todays_BL & Yesterday_BL >= Week_Ago_BL & Yesterday_BL >= Month_Ago_BL, "Yesterday",ifelse(Week_Ago_BL >= Todays_BL & Week_Ago_BL >= Yesterday_BL & Week_Ago_BL >= Month_Ago_BL, "Week_Ago",ifelse(Month_Ago_BL >= Todays_BL & Month_Ago_BL >= Yesterday_BL & Month_Ago_BL >= Week_Ago_BL,"Month_Ago","")))) )


#CB_Short_NF$Growth <- ifelse((CB_Short_NF$Yesterday_BL > CB_Short_NF$Week_Ago_BL & CB_Short_NF$Week_Ago_BL > CB_Short_NF$Month_Ago_BL), "Yes", ifelse(((CB_Short_NF$Yesterday_BL > CB_Short_NF$Week_Ago_BL & CB_Short_NF$Week_Ago_BL < CB_Short_NF$Month_Ago_BL)|(CB_Short_NF$Yesterday_BL < CB_Short_NF$Week_Ago_BL & CB_Short_NF$Week_Ago_BL > CB_Short_NF$Month_Ago_BL)), "Mixed", "No"))
#View(CB_Short_NF)
con <- gaeas_cradle("wolfoftinstreet@gmail.com")

statement <- paste(
  "SELECT *   ",
  "FROM `gaeas-cradle.ck_funny_money.",gsub("-","_",currentDate),"_CK_Credit` a ",
  sep = ""
)
Today <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)

statement <- paste(
  "SELECT *   ",
  "FROM `gaeas-cradle.ck_funny_money.",gsub("-","_",(currentDate-1)),"_CK_Credit` a ",
  sep = ""
)
Yesterday <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)
  
statement <- paste(
  "SELECT *   ",
  "FROM `gaeas-cradle.ck_funny_money.",gsub("-","_",(currentDate-7)),"_CK_Credit` a ",
  sep = ""
)
Week_Ago <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)

statement <- paste(
  "SELECT *   ",
  "FROM `gaeas-cradle.ck_funny_money.",gsub("-","_",(currentDate-30)),"_CK_Credit` a ",
  sep = ""
)
Month_Ago <- dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)

CB_Short_NF <- CB_Short_NF %>% mutate(Todays_BL_Accel= round((BuyList_Backing - Yesterday$CK_Backing[match(Key,Yesterday$Key)])/Yesterday$CK_Backing[match(Key,Yesterday$Key)],2)) %>%
                mutate(Yesterday_BL_Accel = round((Yesterday$CK_Backing[match(Key,Yesterday$Key)] - Week_Ago$CK_Backing[match(Key,Week_Ago$Key)])/Week_Ago$CK_Backing[match(Key,Week_Ago$Key)],2)) %>%
                mutate(Week_Ago_BL_Accel = round((Week_Ago$CK_Backing[match(Key,Week_Ago$Key)] - Month_Ago$CK_Backing[match(Key,Month_Ago$Key)])/Month_Ago$CK_Backing[match(Key,Month_Ago$Key)],2)) %>%
                mutate(CK_MKT = Funny_Money_Analysis$CK_MKT[match(Key,Funny_Money_Analysis$Key)]) %>%
                mutate(Yest_CK_MKT = Yesterday$CK_MKT[match(Key,Yesterday$Key)]) %>%
                mutate(Week_CK_MKT = Week_Ago$CK_MKT[match(Key,Week_Ago$Key)]) %>%
                mutate(Month_CK_MKT =  Month_Ago$CK_MKT[match(Key,Month_Ago$Key)]) %>%
                mutate(Todays_MKT_Accel = round((CK_MKT - Yesterday$CK_MKT[match(Key,Yesterday$Key)])/Yesterday$CK_MKT[match(Key,Yesterday$Key)],2)) %>%
                mutate(Yesterday_MKT_Accel = round((Yesterday$CK_MKT[match(Key,Yesterday$Key)] - Week_Ago$CK_MKT[match(Key,Week_Ago$Key)])/Week_Ago$CK_MKT[match(Key,Week_Ago$Key)],2)) %>%
                mutate(Week_Ago_MKT_Accel = round((Week_Ago$CK_MKT[match(Key,Week_Ago$Key)] - Month_Ago$CK_MKT[match(Key,Month_Ago$Key)])/Month_Ago$CK_MKT[match(Key,Month_Ago$Key)],2))

CB_CK_Final <- CB_Short_NF %>% filter(Growth != "Mixed" & Highest == "Today") %>%
               mutate(Growth = ifelse((CK_MKT > Yest_CK_MKT & Yest_CK_MKT > Week_CK_MKT & Week_CK_MKT > Month_CK_MKT), "Yes", ifelse(((CK_MKT < Yest_CK_MKT & Yest_CK_MKT < Week_CK_MKT & Week_CK_MKT < Month_CK_MKT)), "No", "Mixed")))

Stringent_CB_CK_Final <- CB_CK_Final[which(CB_CK_Final$Yesterday_BL_Accel > 0 & CB_CK_Final$Week_Ago_BL_Accel > 0 & CB_CK_Final$Growth == "Yes"),]
Goldilocks_CB_CK_Final <- CB_CK_Final[which(CB_CK_Final$Yesterday_BL_Accel > 0 & CB_CK_Final$Growth == "Yes"),]
Relaxed_CB_CK_Final <- CB_CK_Final[which(CB_CK_Final$Yesterday_BL_Accel > 0),]

#Google Sheets For Easy Communication####
options(httr_oob_default=TRUE) 
options(gargle_oauth_email = "pachun95@gmail.com")
drive_auth(email = "pachun95@gmail.com",use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com",use_oob=TRUE)

ss <- drive_get("Market_Review")

sheet_write(
  Final_Export,
  ss = ss,
  sheet = "TCG_Market_Angle"
)

sheet_write(
  Funny_Money_Analysis,
  ss = ss,
  sheet = "Funny_Money_Conversion"
)



ss <- drive_get("Growth_Reports")
#sheets_deauth()
sheet_write(
  as.data.frame(CB_Short),
  ss = ss,
  sheet = "BL"
)

sheet_write(
  as.data.frame(Consistent_Sellers),
  ss = ss,
  sheet = "Demand"
)

sheet_write(
  as.data.frame(Consistent_Vendors),
  ss = ss,
  sheet = "Vendors"
)

ss <- drive_get("Wolfs_Warrens")
#sheets_deauth()
sheet_write(
  Relaxed_CB_CK_Final,
  ss = ss,
  sheet = "Relaxed"
)
sheet_write(
  Goldilocks_CB_CK_Final,
  ss = ss,
  sheet = "Goldilocks"
)
sheet_write(
  Stringent_CB_CK_Final,
  ss = ss,
  sheet = "Stringent"
)

ss <- drive_get("Bills & MTG 2020")
sheet_write(
  Final_Export,
  ss = ss,
  sheet = "BL"
)

#Metric Aggregation####
con <- gaeas_cradle("wolfoftinstreet@gmail.com")
currentDate <- Sys.Date()
ThreeWeekDate <- Sys.Date() - 22
statement <- paste("SELECT DISTINCT Key ","FROM `gaeas-cradle.premiums.*` a ",'WHERE _TABLE_SUFFIX BETWEEN ',
                   'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 22 DAY)) AND ',
                   'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL -1 DAY)) ',sep = "")
unique_keys = dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)
overwrite_file = unique_keys
Buylist_Tracker = unique_keys
statement <- paste("SELECT Key, BL, Sellers, TCG_Rank, CK_ADJ_Rank, Date ","FROM `gaeas-cradle.premiums.*` a ",'WHERE _TABLE_SUFFIX BETWEEN ',
                    'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 22 DAY)) AND ',
                    'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL -1 DAY)) ',
                    'ORDER BY DATE', sep = "")
temporary_data_hub = dbSendQuery(con, statement = statement) %>% dbFetch(n = -1)
for(i in 1:22){
  specified_date <- temporary_data_hub %>% filter(Date == ThreeWeekDate)
  overwrite_file$new <- specified_date$BL[match(overwrite_file$Key,specified_date$Key)]
  Buylist_Tracker <- cbind(Buylist_Tracker,overwrite_file$new )
  ThreeWeekDate = ThreeWeekDate + 1
}
ThreeWeekDate = ThreeWeekDate - 22
Buylist_Tracker = tracker_creation(Buylist_Tracker)


overwrite_file = unique_keys
Vendor_Tracker = unique_keys
for(i in 1:22){
  specified_date <- temporary_data_hub %>% filter(Date == ThreeWeekDate)
  overwrite_file$new <- specified_date$Sellers[match(overwrite_file$Key,specified_date$Key)]
  Vendor_Tracker <- cbind(Vendor_Tracker,overwrite_file$new )
  ThreeWeekDate = ThreeWeekDate + 1
}
ThreeWeekDate = ThreeWeekDate - 22
Vendor_Tracker = tracker_creation(Vendor_Tracker)

overwrite_file = unique_keys
TCG_Ranks = unique_keys
for(i in 1:22){
  specified_date <- temporary_data_hub %>% filter(Date == ThreeWeekDate)
  overwrite_file$new <- specified_date$TCG_Rank[match(overwrite_file$Key,specified_date$Key)]
  TCG_Ranks <- cbind(TCG_Ranks,overwrite_file$new )
  ThreeWeekDate = ThreeWeekDate + 1
}
ThreeWeekDate = ThreeWeekDate - 22
TCG_Ranks = tracker_creation(TCG_Ranks)

overwrite_file = unique_keys
CK_Ranks = unique_keys
for(i in 1:22){
  specified_date <- temporary_data_hub %>% filter(Date == ThreeWeekDate)
  overwrite_file$new <- specified_date$CK_ADJ_Rank[match(overwrite_file$Key,specified_date$Key)]
  CK_Ranks <- cbind(CK_Ranks,overwrite_file$new )
  ThreeWeekDate = ThreeWeekDate + 1
}
ThreeWeekDate = ThreeWeekDate - 22
CK_Ranks = tracker_creation(CK_Ranks)


BL_Final <- prior_3_weeks(Buylist_Tracker) %>% arrange(desc(Rank_Sums)) %>% mutate(Rank_Groups = as.numeric(as.factor(Rank_Sums)))
VEN_Final <- prior_3_weeks(Vendor_Tracker) %>% arrange(desc(Rank_Sums)) %>% mutate(Rank_Groups = as.numeric(as.factor(Rank_Sums)))
TCG_Final <- prior_3_weeks(TCG_Ranks) %>% arrange(desc(Rank_Sums)) %>% mutate(Rank_Groups = as.numeric(as.factor(Rank_Sums)))
CK_Final <- prior_3_weeks(CK_Ranks) %>% arrange(desc(Rank_Sums)) %>% mutate(Rank_Groups = as.numeric(as.factor(Rank_Sums)))




BLUE <- BL_Final[which(BL_Final$Rank_Groups >= (max(BL_Final$Rank_Groups)-9)),] %>% group_by(Key) %>% add_tally()
VENUE <- VEN_Final[which(VEN_Final$Rank_Groups >= (max(VEN_Final$Rank_Groups)-9)),] %>% group_by(Key) %>% add_tally()
TCGUE <- TCG_Final[which(TCG_Final$Rank_Groups >= (max(TCG_Final$Rank_Groups)-9)),] %>% group_by(Key) %>% add_tally()
CKUE <- CK_Final[which(CK_Final$Rank_Groups >= (max(CK_Final$Rank_Groups)-9)),] %>%  group_by(Key) %>% add_tally()


BL_Upper_Esch <- BL_Final[which(BL_Final$Rank_Groups >= (max(BL_Final$Rank_Groups)-9)),]
VEN_Upper_Esch <- VEN_Final[which(VEN_Final$Rank_Groups >= (max(VEN_Final$Rank_Groups)-9)),]
TCG_Upper_Esch <- TCG_Final[which(TCG_Final$Rank_Groups >= (max(TCG_Final$Rank_Groups)-9)),] 
CK_Upper_Esch <- CK_Final[which(CK_Final$Rank_Groups >= (max(CK_Final$Rank_Groups)-9)),]


Combined_Upper_Esch <- rbind(BL_Upper_Esch[,1:5], VEN_Upper_Esch[,1:5], TCG_Upper_Esch[,1:5], CK_Upper_Esch[,1:5])
Combined_Upper_Esch[,5][is.na(Combined_Upper_Esch[,5])] <- ""
Unique_Combined_Upper_Esch <- unique(Combined_Upper_Esch)

CUE <- Combined_Upper_Esch %>% group_by(Key) %>% add_tally()

Unique_Combined_Upper_Esch <- Unique_Combined_Upper_Esch %>% mutate(Total_KPI_CT = CUE$n[match(Key,CUE$Key)]) %>%
  mutate(BL_KPI = BLUE$n[match(Key,BLUE$Key)]) %>%
  mutate(VEN_KPI = VENUE$n[match(Key,VENUE$Key)]) %>%
  mutate(TCG_KPI = TCGUE$n[match(Key,TCGUE$Key)]) %>%
  mutate(CK_KPI = CKUE$n[match(Key,CKUE$Key)]) %>%
  mutate(BL_Bracket = as.numeric(BL_Upper_Esch$Rank_Groups[match(Key,BL_Upper_Esch$Key)])) %>%
  mutate(VEN_Bracket = VEN_Upper_Esch$Rank_Groups[match(Key,VEN_Upper_Esch$Key)]) %>%
  mutate(TCG_Bracket = TCG_Upper_Esch$Rank_Groups[match(Key,TCG_Upper_Esch$Key)]) %>%
  mutate(CK_Bracket = CK_Upper_Esch$Rank_Groups[match(Key,CK_Upper_Esch$Key)]) %>%
  mutate(BL_Bracket = as.numeric(ifelse(BL_Bracket == max(BL_Final$Rank_Groups),1,ifelse(BL_Bracket == (max(BL_Final$Rank_Groups)-1),2,ifelse(BL_Bracket == (max(BL_Final$Rank_Groups)-3),3,ifelse(BL_Bracket == (max(BL_Final$Rank_Groups)-4),4,ifelse(BL_Bracket == (max(BL_Final$Rank_Groups)-5),5,ifelse(BL_Bracket == (max(BL_Final$Rank_Groups)-6),6,ifelse(BL_Bracket == (max(BL_Final$Rank_Groups)-7),7,ifelse(BL_Bracket == (max(BL_Final$Rank_Groups)-8),8,ifelse(BL_Bracket == (max(BL_Final$Rank_Groups)-9),9,ifelse(BL_Bracket == (max(BL_Final$Rank_Groups)-10),10,"")))))))))))) %>%
  mutate(VEN_Bracket = as.numeric(ifelse(VEN_Bracket == max(VEN_Final$Rank_Groups),1,ifelse(VEN_Bracket == (max(VEN_Final$Rank_Groups)-1),2,ifelse(VEN_Bracket == (max(VEN_Final$Rank_Groups)-3),3,ifelse(VEN_Bracket == (max(VEN_Final$Rank_Groups)-4),4,ifelse(VEN_Bracket == (max(VEN_Final$Rank_Groups)-5),5,ifelse(VEN_Bracket == (max(VEN_Final$Rank_Groups)-6),6,ifelse(VEN_Bracket == (max(VEN_Final$Rank_Groups)-7),7,ifelse(VEN_Bracket == (max(VEN_Final$Rank_Groups)-8),8,ifelse(VEN_Bracket == (max(VEN_Final$Rank_Groups)-9),9,ifelse(VEN_Bracket == (max(VEN_Final$Rank_Groups)-10),10,"")))))))))))) %>%
  mutate(TCG_Bracket = as.numeric(ifelse(TCG_Bracket == max(TCG_Final$Rank_Groups),1,ifelse(TCG_Bracket == (max(TCG_Final$Rank_Groups)-1),2,ifelse(TCG_Bracket == (max(TCG_Final$Rank_Groups)-3),3,ifelse(TCG_Bracket == (max(TCG_Final$Rank_Groups)-4),4,ifelse(TCG_Bracket == (max(TCG_Final$Rank_Groups)-5),5,ifelse(TCG_Bracket == (max(TCG_Final$Rank_Groups)-6),6,ifelse(TCG_Bracket == (max(TCG_Final$Rank_Groups)-7),7,ifelse(TCG_Bracket == (max(TCG_Final$Rank_Groups)-8),8,ifelse(TCG_Bracket == (max(TCG_Final$Rank_Groups)-9),9,ifelse(TCG_Bracket == (max(TCG_Final$Rank_Groups)-10),10,"")))))))))))) %>%
  mutate(CK_Bracket = as.numeric(ifelse(CK_Bracket == max(CK_Final$Rank_Groups),1,ifelse(CK_Bracket == (max(CK_Final$Rank_Groups)-1),2,ifelse(CK_Bracket == (max(CK_Final$Rank_Groups)-3),3,ifelse(CK_Bracket == (max(CK_Final$Rank_Groups)-4),4,ifelse(CK_Bracket == (max(CK_Final$Rank_Groups)-5),5,ifelse(CK_Bracket == (max(CK_Final$Rank_Groups)-6),6,ifelse(CK_Bracket == (max(CK_Final$Rank_Groups)-7),7,ifelse(CK_Bracket == (max(CK_Final$Rank_Groups)-8),8,ifelse(CK_Bracket == (max(CK_Final$Rank_Groups)-9),9,ifelse(CK_Bracket == (max(CK_Final$Rank_Groups)-10),10,"")))))))))))) %>% 
  replace_na(list(BL_Bracket = 10, VEN_Bracket = 10, TCG_Bracket = 10, CK_Bracket = 10))

library(magrittr)
OVR_KPI_DF <- Unique_Combined_Upper_Esch %>%
  mutate(WMS = Unique_Combined_Upper_Esch[,c(11:14)] %>%  rowwise() %>% do(data.frame(WMS=weighted.mean( x=c(as.numeric(.$BL_Bracket),as.numeric(.$VEN_Bracket),as.numeric(.$TCG_Bracket),as.numeric(.$CK_Bracket)), w=c(.35,.57,.05,.03))))  %>% ungroup() %>% use_series("WMS")) %>%
  arrange(WMS) %>% mutate(Ranking = seq(nrow(Unique_Combined_Upper_Esch))) %>% select(Key, Name, Set, Rarity, `N/NF`, Ranking) %>% `colnames<-` (c("Key","Name","Set","Rarity","F/NF","Ranking")) %>%
  mutate(Retail = Final_Export$MKT[match(Key,Final_Export$Key)],
         Buylist = Final_Export$BL[match(Key,Final_Export$Key)],
         Vendors = Final_Export$Sellers[match(Key,Final_Export$Key)])
  
  


#Export
drive_auth(email = "pachun95@gmail.com", use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com", use_oob=TRUE)

ss <- drive_get("Master_KPI_Review")

gs4_auth(email = "pachun95@gmail.com")
sheet_write(
  OVR_KPI_DF,
  ss = ss,
  sheet = "Master"
)

KPI_Export <- OVR_KPI_DF %>% mutate(Param = Updated_Tracking_Keys$param[match(Key, Updated_Tracking_Keys$Key)]) %>% select(Key, Param, Ranking)
mybq <- bq_table(project = "gaeas-cradle", dataset = "kpi", table = paste(gsub("-","_",Sys.Date()),"_kpi",sep=""))
bq_table_upload(x=mybq, values = KPI_Export, fields=as_bq_fields(KPI_Export),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")
print("BQ_KPI_TRANSFER_COMPLETE!!")

#CK vs TCG Dollar Differences####
setwd("/home/cujo253/Funny Money/")
currentDate <- Sys.Date()
con <- gaeas_cradle("wolfoftinstreet@gmail.com")

CK_Market__Tracker <- Updated_Tracking_Keys %>% select(Key)
TCG_Market_Tracker <- Updated_Tracking_Keys %>% select(Key)
Temporary_Keys <- Updated_Tracking_Keys %>% select(Key)
Two_Months_Data <- dbSendQuery(con, statement = paste("SELECT DISTINCT Date, Key, TCG_MKT, CK_MKT ","FROM `gaeas-cradle.ck_funny_money.*` a ",'WHERE _TABLE_SUFFIX BETWEEN ',
                                   'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 61 DAY)) AND ',
                                   'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL -1 DAY)) ',sep = "")) %>% dbFetch(n= -1) %>% arrange(Date)

for(i in 61:0){
  date = currentDate - i
  date_data = Two_Months_Data %>% filter(Date == date)
  Temporary_Keys$ck_matches <- date_data$CK_MKT[match(Temporary_Keys$Key,date_data$Key)]
  Temporary_Keys$tcg_matches <- date_data$TCG_MKT[match(Temporary_Keys$Key,date_data$Key)]
  CK_Market__Tracker <- cbind(CK_Market__Tracker, Temporary_Keys$ck_matches)
  TCG_Market_Tracker <- cbind(TCG_Market_Tracker, Temporary_Keys$tcg_matches)
  }

CK_Market__Tracker <- CK_Market__Tracker %>% 
  `colnames<-` (c("Key", format(seq(from = (currentDate - 61), to = currentDate, by = 'day'),format = "%Y-%m-%d"))) %>% replace(is.na(.), 0)
TCG_Market_Tracker <- TCG_Market_Tracker %>% 
  `colnames<-` (c("Key", format(seq(from = (currentDate - 61), to = currentDate, by = 'day'),format = "%Y-%m-%d"))) %>% replace(is.na(.), 0)

range = ncol(TCG_Market_Tracker)
CK_Retail_Comparison <- Updated_Tracking_Keys %>% select(Key) %>% replace_na(list(Foil = "")) %>% as.data.frame()

for (i in 2:range){CK_Retail_Comparison[i] <- round(CK_Market__Tracker[i] -  TCG_Market_Tracker[i],2)}

for (i in 3:range){CK_Retail_Comparison[i] <- round(CK_Retail_Comparison[i] -  CK_Retail_Comparison[i-1],2)}
CK_Retail_Comparison <- CK_Retail_Comparison[-2]

CK_Retail_Comparison_AT <- ck_tcg_comparison(CK_Retail_Comparison) 

CK_Retail_Comparison_Three_Weeks <- CK_Retail_Comparison_AT %>% arrange(Three_Weeks) %>% slice(1:500)

CK_Retail_Comparison_One_Weeks <- CK_Retail_Comparison_AT %>% arrange(One_Week) %>% slice(1:500)

CK_Retail_Comparison_AT <- CK_Retail_Comparison_AT %>% slice(1:500)


AT_CK <- data.frame(CK_Retail_Comparison_AT[1],CK_Retail_Comparison_AT[ncol(CK_Retail_Comparison_AT)-2])
Three_Week_CK <- data.frame(CK_Retail_Comparison_Three_Weeks[1],CK_Retail_Comparison_Three_Weeks[ncol(CK_Retail_Comparison_Three_Weeks)-1])
One_Week_CK <- data.frame(CK_Retail_Comparison_One_Weeks[1],CK_Retail_Comparison_One_Weeks[ncol(CK_Retail_Comparison_One_Weeks)])

AT_CK$Rank <- seq(nrow(AT_CK))
Three_Week_CK$Rank <-seq(nrow(Three_Week_CK))
One_Week_CK$Rank <- seq(nrow(One_Week_CK))

ncol(CK_Market__Tracker)
AT_CK$CK_Retail <- CK_Market__Tracker[,ncol(CK_Market__Tracker)][match(AT_CK$Key,CK_Market__Tracker$Key)]
AT_CK$TCG_Retail <- TCG_Market_Tracker[,ncol(CK_Market__Tracker)][match(AT_CK$Key, TCG_Market_Tracker$Key)]

Three_Week_CK$CK_Retail<- CK_Market__Tracker[,ncol(CK_Market__Tracker)][match(Three_Week_CK$Key,CK_Market__Tracker$Key)]
Three_Week_CK$TCG_Retail<- TCG_Market_Tracker[,ncol(CK_Market__Tracker)][match(Three_Week_CK$Key, TCG_Market_Tracker$Key)]

One_Week_CK$CK_Retail<- CK_Market__Tracker[,ncol(CK_Market__Tracker)][match(One_Week_CK$Key,CK_Market__Tracker$Key)]
One_Week_CK$TCG_Retail<- TCG_Market_Tracker[,ncol(CK_Market__Tracker)][match(One_Week_CK$Key, TCG_Market_Tracker$Key)]
Ignore_Most_Recent_Set <- Sets[nrow(Sets),]$Set_Excl
Cards_In_Most_Recent_Set <- Updated_Tracking_Keys[which(Updated_Tracking_Keys$Set == Ignore_Most_Recent_Set),]
library(reshape2)
CK_Inv <- fromJSON("https://api.cardkingdom.com/api/pricelist") %>% 
  as.data.frame() %>%
  mutate(data.edition = ifelse(data.edition == "Promotional",data.variation,data.edition)) %>%
  mutate(data.edition = ck_conversion$Standardized[match(data.edition,ck_conversion$CK)]) %>%
  mutate(Semi = paste(data.name,data.edition, sep="")) %>%
  mutate(data.is_foil = ifelse(data.is_foil == "false", "", "FOIL")) %>%
  mutate(rarity = Updated_Tracking_Keys$Rarity[match(Semi, Updated_Tracking_Keys$Semi)]) %>%
  mutate(CK_Key = trimws(paste(data.name, data.edition, rarity," ",data.is_foil, sep=""))) %>%
  select(CK_Key, data.qty_retail, data.qty_buying, data.price_buy)

Market_Comparison <- tryCatch({data.frame(All = AT_CK$Key,Three_Week = Three_Week_CK$Key,One_Week = One_Week_CK$Key) %>% gather() %>% select(value) %>% distinct() %>% rename(c("Key" = "value")) %>%
  mutate(Two_Month_Change =  as.numeric(AT_CK$All_Time[match(Key,AT_CK$Key)]),
         Two_Month_Rank =  as.numeric(AT_CK$Rank[match(Key,AT_CK$Key)]),
         Three_Week_Change = as.numeric(Three_Week_CK$Three_Weeks[match(Key,Three_Week_CK$Key)]),
         Three_Week_Rank =  as.numeric(Three_Week_CK$Rank[match(Key,Three_Week_CK$Key)]),
         One_Week_Change = as.numeric(One_Week_CK$One_Week[match(Key,One_Week_CK$Key)]),
         One_Week_Rank =  as.numeric(One_Week_CK$Rank[match(Key,One_Week_CK$Key)]),
         CK_Retail = as.numeric(CK_Market__Tracker[,ncol(CK_Market__Tracker)][match(Key,CK_Market__Tracker$Key)]),
         TCG_Retail = as.numeric(TCG_Market_Tracker[,ncol(CK_Market__Tracker)][match(Key, TCG_Market_Tracker$Key)]),
         Name = Updated_Tracking_Keys$name[match(Key,Updated_Tracking_Keys$Key)],
         Set = Updated_Tracking_Keys$Set[match(Key,Updated_Tracking_Keys$Key)],
         Rarity = Updated_Tracking_Keys$Rarity[match(Key,Updated_Tracking_Keys$Key)],
         Most_Recent_Set = Cards_In_Most_Recent_Set$abbr[match(Name,Cards_In_Most_Recent_Set$name)]) %>%
  filter(is.na(Most_Recent_Set)) %>% select(!Most_Recent_Set) %>% select(Key, Name, Set, Rarity,everything()) %>%
  mutate(CK_Retail_Qty = as.numeric(CK_Inv$data.qty_retail[match(Key,CK_Inv$CK_Key)]),
         CK_Buylist_Qty = as.numeric(CK_Inv$data.qty_buying[match(Key,CK_Inv$CK_Key)]),
         CK_Buylist_Offer = as.numeric(CK_Inv$data.price_buy[match(Key,CK_Inv$CK_Key)]))}, error = function(e){
           data.frame(All = AT_CK$Key,Three_Week = Three_Week_CK$Key,One_Week = One_Week_CK$Key) %>% gather() %>% select(value) %>% distinct() %>% rename(c("value" = "Key")) %>%
             mutate(Two_Month_Change =  as.numeric(AT_CK$All_Time[match(Key,AT_CK$Key)]),
                    Two_Month_Rank =  as.numeric(AT_CK$Rank[match(Key,AT_CK$Key)]),
                    Three_Week_Change = as.numeric(Three_Week_CK$Three_Weeks[match(Key,Three_Week_CK$Key)]),
                    Three_Week_Rank =  as.numeric(Three_Week_CK$Rank[match(Key,Three_Week_CK$Key)]),
                    One_Week_Change = as.numeric(One_Week_CK$One_Week[match(Key,One_Week_CK$Key)]),
                    One_Week_Rank =  as.numeric(One_Week_CK$Rank[match(Key,One_Week_CK$Key)]),
                    CK_Retail = as.numeric(CK_Market__Tracker[,ncol(CK_Market__Tracker)][match(Key,CK_Market__Tracker$Key)]),
                    TCG_Retail = as.numeric(TCG_Market_Tracker[,ncol(CK_Market__Tracker)][match(Key, TCG_Market_Tracker$Key)]),
                    Name = Updated_Tracking_Keys$name[match(Key,Updated_Tracking_Keys$Key)],
                    Set = Updated_Tracking_Keys$Set[match(Key,Updated_Tracking_Keys$Key)],
                    Rarity = Updated_Tracking_Keys$Rarity[match(Key,Updated_Tracking_Keys$Key)],
                    Most_Recent_Set = Cards_In_Most_Recent_Set$abbr[match(Name,Cards_In_Most_Recent_Set$name)]) %>%
             filter(is.na(Most_Recent_Set)) %>% select(!Most_Recent_Set) %>% select(Key, Name, Set, Rarity,everything()) %>%
             mutate(CK_Retail_Qty = as.numeric(CK_Inv$data.qty_retail[match(Key,CK_Inv$CK_Key)]),
                    CK_Buylist_Qty = as.numeric(CK_Inv$data.qty_buying[match(Key,CK_Inv$CK_Key)]),
                    CK_Buylist_Offer = as.numeric(CK_Inv$data.price_buy[match(Key,CK_Inv$CK_Key)]))
         })

gs4_auth(email = "pachun95@gmail.com")
drive_auth(email = "pachun95@gmail.com")
ss <- drive_get("CK_VS_TCG_Review")
sheet_write(Market_Comparison,ss = ss,sheet = "Market_Comparisons")
 
Comparison_Export <- Market_Comparison %>% mutate(Param = Updated_Tracking_Keys$param[match(Key, Updated_Tracking_Keys$Key)]) %>% select(Param,everything()) %>% select(!Name) %>% select(!Set) %>% select(!Rarity)
mybq <- bq_table(project = "gaeas-cradle", dataset = "ck_retail_review", table = paste(gsub("-","_",currentDate),"_CK_VS_MARKET",sep=""))
bq_table_upload(x=mybq, values = Comparison_Export, fields=as_bq_fields(Comparison_Export),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")
print("BQ TCG Upload Successful!")



