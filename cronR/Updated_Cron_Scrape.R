#Functions####
right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
} #Recreating the right function from Excel 
left = function(text, num_char) {
  substr(text, 1, num_char)
} #Recreating the left function from Excel 
funk <- function(t){
  ifelse(nchar(t) <= 10, right(t,1),ifelse(nchar(t)<=190, right(t,2),ifelse(nchar(t)>=191, right((t),3),0)))
} #Character Count utilization of 'left'&'right' functions for quantity breakdown
moveme <- function (invec, movecommand) {
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
}

#Packages####
library("tidyverse")
library("rvest")
library("jsonlite")
library(devtools)
library(googlesheets4)
library(googledrive)
library(googlesheets)
library(readr)
#install.packages("readr", INSTALL_opts = '--no-lock',force = TRUE)
library(gargle)
library(httr)

Total_Start_Time <- Sys.time()
#CK Buylist####
#Call full buylist via "Raw_CK_Buylist" data frame
#Call functional (/slimmed) buylist via "CK_Buylist_Retrieved" data frame
Updated_Tracking_Keys <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(hasFoil = col_character()))
Updated_Tracking_Keys <- Updated_Tracking_Keys[c(8:12)]
colnames(Updated_Tracking_Keys) <- c("Key","name","Set","Rarity","Foil")
Updated_Tracking_Keys$Semi <- paste(Updated_Tracking_Keys$name, Updated_Tracking_Keys$Set,sep="")
CK_Buylist <- fromJSON("https://api.cardkingdom.com/api/pricelist")
CK_Buylist <- as.data.frame(CK_Buylist)
CK_Buylist$data.edition <- ifelse(CK_Buylist$data.edition == "Promotional",CK_Buylist$data.variation,CK_Buylist$data.edition)
ck_conversion <- read_csv("~/Essential_Referential_CSVS/mtgjson_ck_sets.csv")
CK_Buylist$data.edition <- ck_conversion$Standardized[match(CK_Buylist$data.edition,ck_conversion$CK)]
CK_Buylist$Semi <- paste(CK_Buylist$data.name,CK_Buylist$data.edition, sep="")
CK_Buylist$rarity <- Updated_Tracking_Keys$Rarity[match(CK_Buylist$Semi, Updated_Tracking_Keys$Semi)]
summary(as.factor(CK_Buylist$rarity))
CK_Buylist$data.is_foil <- ifelse(CK_Buylist$data.is_foil == "false", "", "FOIL")
CK_Buylist$CK_Key <- paste(CK_Buylist$data.name, CK_Buylist$data.edition, CK_Buylist$rarity," ",CK_Buylist$data.is_foil, sep="")

CK_Smaller_List <- data.frame(CK_Buylist$CK_Key,CK_Buylist$data.name, CK_Buylist$data.edition, CK_Buylist$rarity,CK_Buylist$data.is_foil, CK_Buylist$data.price_buy, CK_Buylist$data.qty_buying)
names(CK_Smaller_List) <- c("CK_Key","Card Name","Set","Rarity","NF/F","BL_Value","Qty_Des") #rename columns to be a tad easier to track
Sets <- read.csv("/home/cujo253/Essential_Referential_CSVS/Sets.csv",stringsAsFactors = TRUE)

CK_Smaller_List$Gold_Merge <- Sets$GF_Abbr[match(CK_Smaller_List$Set,Sets$CK_BL_Scrape_Sets)]
CK_Smaller_List$MTG_Gold_Key <- paste(CK_Smaller_List$`Card Name`,CK_Smaller_List$Gold_Merge, sep ="")

Raw_CK_Buylist <- CK_Buylist
CK_Buylist_Retrieved <- CK_Smaller_List
CK_Smaller_List <- na.omit(CK_Smaller_List)
CK_Smaller_List$CK_Key <- trimws(CK_Smaller_List$CK_Key)
# View(CK_Smaller_List)
#Goldfish Pricing Retrieval#### 
sets = Sets$sets
sets = sets[which(sets != "")]
total = nrow(as.data.frame(sets)) #238 sets on the list above, take my word for it ;)
sets = as.list(as.character(sets))
pb <- txtProgressBar(min=0, max = total, style = 3) # loading bar formatting
Gold_Market <-NULL #Create null value - note each scrape is given it's own unique null value so that we have each scrapes original source material to prevent excessive scrapping
Start_Time <- Sys.time()
for(i in sets){
  url <- paste0("https://www.mtggoldfish.com/index/",print(i),"#paper")
  html <- read_html(url)
  tbls_ls <- html %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE)
  tbls_ls <-data.frame(tbls_ls)
  Gold_Market <-rbind(Gold_Market,tbls_ls)
  setTxtProgressBar(pb,i)
}
Back_Up_Gold <- Gold_Market
#Gold_Market <- Back_Up_Gold
#
sets_Foil = Sets$sets_Foil
sets_Foil = sets_Foil[which(sets_Foil != "")]
total = nrow(as.data.frame(sets_Foil)) #238 sets on the list above, take my word for it ;)
sets_Foil = as.list(as.character(sets_Foil))
pb <- txtProgressBar(min=0, max = total, style = 3) # loading bar formatting
Gold_Foil_Market <-NULL #Create null value - note each scrape is given it's own unique null value so that we have each scrapes original source material to prevent excessive scrapping
for(i in sets_Foil){
  try(url <- paste0("https://www.mtggoldfish.com/index/",print(i),"#paper"))
  html <- read_html(url)
  tbls_fs <- html %>%
    html_nodes("table") %>%
    .[2] %>%
    html_table(fill = TRUE)
  tbls_ls <-data.frame(tbls_fs)
  Gold_Foil_Market <-rbind(Gold_Foil_Market,tbls_ls)
  setTxtProgressBar(pb,i)
}
End_Time <- Sys.time()
print(paste("MTG Goldfish Data Acquisition Lasted:",round(End_Time - Start_Time,2),"Minutes"))
Foil_Market <- Gold_Foil_Market
Gold_Market <- Gold_Market[c(5,1,2,4)] 
#Snag only the desired columns, we are leaving plenty of good material out here, just not my focus now
Gold_Market$Price <- as.numeric(Gold_Market$Price) 
#Convert to numeric from characters
Gold_Market$Price <- round(Gold_Market$Price * 0.845,2) 
#ARBITRARY mkt adjustment factor to serve as estimate for actual value. Thank you mtg goldfish but you also kinda suck here.
Gold_Market$Daily <- paste(Gold_Market$Card,Gold_Market$Set, sep="") 
#Create secondary goldfish key to act as merger column
Foil_Market <- Foil_Market[c(5,1,2,4)] 
#Snag only the desired columns, we are leaving plenty of good material out here, just not my focus now
Foil_Market$Price <- as.numeric(Foil_Market$Price) 
#Convert to numeric from characters
Foil_Market$Price <- round(Foil_Market$Price * 0.845,2) 
#ARBITRARY mkt adjustment factor to serve as estimate for actual value. Thank you mtg Foilfish but you also kinda suck here.
Foil_Market$Daily <- paste(Foil_Market$Card,Foil_Market$Set, sep="")
CK_Smaller_List$Gold_Market <- ifelse(CK_Smaller_List$`NF/F` == " FOIL", Foil_Market$Price[match(CK_Smaller_List$MTG_Gold_Key,Foil_Market$Daily)] ,Gold_Market$Price[match(CK_Smaller_List$MTG_Gold_Key,Gold_Market$Daily)])
Basic_Market_Review <- CK_Smaller_List
Basic_Market_Review <- Basic_Market_Review[c(1,2,3,4,5,10,7,6)]
Basic_Market_Review$Gold_Market[is.na(Basic_Market_Review$Gold_Market)] <- 0
names(Basic_Market_Review) = c("Key","Card","Set","Rarity","F/NF","MKT_Est","BL","BL Qty")
#CK Best Sellers####
library(foreach)
library(doParallel)
Limit <- 770 
  #as.numeric(trimws(unlist(as.list(read_html("https://www.cardkingdom.com/catalog/view?filter%5Bipp%5D=60&filter%5Bsort%5D=most_popular&filter%5Bsearch%5D=mtg_advanced&filter%5Bcategory_id%5D=0&filter%5Bmulti%5D%5B0%5D=1&filter%5Btype_mode%5D=any&filter%5Bmanaprod_select%5D=any") %>% html_nodes("li") %>% html_text())[853])))
Start_Time <- Sys.time() 
cl <- makeCluster(3, type = "FORK")
registerDoParallel(cl)

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
CK_Prices_df <- CK_Prices_df[order(CK_Prices_df$i),]
CK_Prices_df$i <- seq(nrow(CK_Prices_df))
colnames(CK_Prices_df)[6] <- "Rank"
CK_Prices_df <- CK_Prices_df[c(2,3,4,5,1,6)]
print(paste("CK Best Selling Data Acquisition Lasted:",round(End_Time - Start_Time,2),"Minutes"))
#CK Market Formatting####
CK_Smaller_List$CK_Rank <- CK_Prices_df$Rank[match(CK_Smaller_List$CK_Key,trimws(CK_Prices_df$key))] #Merge the CK best selling rankings with our original CK Buylist scrape
Low_Confidence_Report <- CK_Smaller_List
#TCG- Market####
Updated_Tracking_Keys <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(hasFoil = col_character()))
Updated_Tracking_Keys <- Updated_Tracking_Keys[c(8:12)]
colnames(Updated_Tracking_Keys) <- c("Key","name","Set","Rarity","Foil")
Updated_Tracking_Keys$Semi <- paste(Updated_Tracking_Keys$name,Updated_Tracking_Keys$Set,sep="")

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
  Potential_Direct_Copies <- sum(Potential_Direct_Copies)
  Total_Copies <- sum(Total_Copies)
  if(Direct_Listings == 0){Total_Copies <- Total_Copies + Potential_Direct_Copies}
  Line_Item <- cbind(Name,Set,Rarity,MKT_EST,Listings,MKT,Product_ID,Direct_Listings,Potential_Direct_Copies,Total_Copies)
  TCG__Best_Sellers <- rbind(TCG__Best_Sellers, Line_Item)
}

for(Q in 1:97){
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
TCG__Best_Sellers <- as.data.frame(TCG__Best_Sellers)
TCG__Best_Sellers$Rank <- seq(nrow(TCG__Best_Sellers))
TCG__Best_Sellers$Rarity <- ifelse(TCG__Best_Sellers$Rarity == "Mythic","M", ifelse(TCG__Best_Sellers$Rarity == "Rare", "R", ifelse(TCG__Best_Sellers$Rarity == "Uncommon", "U", ifelse(TCG__Best_Sellers$Rarity == "Common", "C", TCG__Best_Sellers$Rarity))))
TCG__Best_Sellers$Key <- paste(TCG__Best_Sellers$Name,TCG__Best_Sellers$Set,TCG__Best_Sellers$Rarity,sep="")
TCG__Best_Sellers <- TCG__Best_Sellers[moveme(names(TCG__Best_Sellers), "Key first")]
End_Time <- Sys.time()
print(paste("TCG Best Sellers Lasted:",round(End_Time - Start_Time,2)))

#sheets_deauth()
gs4_auth(email = "pachun95@gmail.com", use_oob = T)
sheet_write(
  TCG__Best_Sellers,
  ss = "/d/1Ef2FgpR-bOg28a8JetHTTIXFH4FeR3eSEj5wpIAVjlU",
  sheet = "TCG_Real_View"
)


TCG <- TCG__Best_Sellers
#TCG Formatting####
colnames(TCG) <- c("Primary_Key","Card_Name","Set","Rarity","MKT_EST","Vendor Listings","MKT","Product_ID","Rank")
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

#unique(TCG$Set)
#TCG <- Results[c(7,2,1,6,4,3,5)]
TCG$MKT_EST <- as.numeric(as.character(TCG$MKT_EST))
#Line 388 is CRUCIAL####
#TCG$MKT <- gsub("[^0-9.-]", 0,TCG$MKT)
TCG$MKT <- as.numeric(as.character(TCG$MKT))
Vendor <- TCG
TCG_Vendor <- as.data.frame(Vendor)
TCG_Vendor$Rank <- seq.int(nrow(TCG_Vendor))
Middle_Confidence_Report <- TCG_Vendor

TCG_Export = TCG
Sets_V2 <- Sets[c(1:5)]
colnames(Sets_V2) <- c("CK_BL_Scrape_Sets","MTG_Goldfish_Sets","GF_Abbr","GF_Abbr_Foil","TCG_Key")
TCG_Export$Set <- Sets_V2$CK_BL_Scrape_Sets[match(TCG$Set,Sets_V2$TCG_Key)]
TCG_Export$Primary_Key <- paste(TCG_Export$Card_Name,TCG_Export$Set, TCG_Export$Rarity, sep = "")
TCG_Export$TCG_Rank <- TCG_Vendor$Rank


CK_Smaller_List$CK_Key <- trimws(CK_Smaller_List$CK_Key)
CK_Smaller_List$TCG_Rank <-TCG_Export$TCG_Rank[match(CK_Smaller_List$CK_Key,TCG_Export$Primary_Key)]
CK_Smaller_List$TCG_Price <- TCG_Export$MKT_EST[match(CK_Smaller_List$CK_Key,TCG_Export$Primary_Key)]
CK_Smaller_List$Gold_Market[is.na(CK_Smaller_List$Gold_Market)] <- 0
CK_Smaller_List$CK_Rank[is.na(CK_Smaller_List$CK_Rank)] <- ""
CK_Smaller_List$TCG_Rank[is.na(CK_Smaller_List$TCG_Rank)] <- ""

Pricing = CK_Smaller_List[c(1,2,3,4,5,6,7,8,9,10,11,12,13)]
Pricing = as.data.frame(Pricing)
#Pricing$TCG_Price <- Price_Trim(as.numeric(Pricing$TCG_Price))
Pricing$TCG_Price <- as.numeric(as.character(Pricing$TCG_Price))
Pricing$Gold_Market <- as.numeric(as.character(Pricing$Gold_Market))
Pricing$MKT_Est <- ifelse(Pricing$`NF/F` == " FOIL", Pricing$Gold_Market,ifelse((is.na(Pricing$TCG_Price)==T),Pricing$Gold_Market,Pricing$TCG_Price))
Pricing1 = Pricing
Pricing1$MKT_Est = as.numeric(as.character(Pricing1$MKT_Est))
Pricing1$BL_Value = as.numeric(as.character(Pricing1$BL_Value))
Pricing$Arbit <- ifelse(is.na(Pricing1$MKT_Est) != TRUE,(Pricing1$BL_Value - Pricing1$MKT_Est),0)
Pricing$Arbit = round(Pricing$Arbit,2)
Pricing$BL_Value <- as.numeric(as.character(Pricing$BL_Value))
Pricing$BL_Value[is.na(Pricing$BL_Value)==T] <- 0
Pricing$Arbit[is.na(Pricing$Arbit)] <- 0
Pricing_Export <- Pricing

CK_Prices_df$Price <- as.numeric(as.character(CK_Prices_df$Price))
Pricing_Export$CK_MKT <- CK_Prices_df$Price[match(as.factor(trimws(Pricing_Export$CK_Key)),as.factor(trimws(CK_Prices_df$key)))]

#
Ranking <- Pricing_Export
#Ranking <- Ranking[which(Ranking$CK_MKT != 0.01),]
Ranking$CK_Rank = as.numeric(Ranking$CK_Rank)
Ranking$TCG_Rank = as.numeric(Ranking$TCG_Rank)
Ranking <- Ranking[order(Ranking$CK_Rank),]
#If Weighted and Adj are "Inf" <- change Line 1596 from c(1) to c(2)####

Anchor_CK_price <- Ranking[1,ncol(Ranking)]
#View(Ranking[,(9)])
Ranking$CK_Rank <- round(((Ranking$CK_MKT/Anchor_CK_price)*Ranking$CK_Rank),5)
Ranking <- Ranking[order(-Ranking$CK_Rank),]
Worst_CK_Rank <- (max(Ranking$CK_Rank,na.rm=T))+1
Ranking$CK_Rank <- ifelse(Ranking$CK_Rank == 0,Worst_CK_Rank,Ranking$CK_Rank)
Ranking$CK_Rank[is.na(Ranking$CK_Rank)] <- Worst_CK_Rank
Ranking <- Ranking[order(Ranking$CK_Rank),]
Absolute_CK_Ranking <- Ranking$CK_Rank
Absolute_CK_Ranking <- seq.int(nrow(Ranking))
Ranking$Abs_CK_Rank <- Absolute_CK_Ranking
Ranking1 <- Ranking

Ranking1$TCG_Rank[is.na(Ranking1$TCG_Rank)==T] <- ""
Ranking1$CK_Rank <- as.numeric(Ranking1$CK_Rank)
Ranking1$TCG_Rank <- as.numeric(Ranking1$TCG_Rank)
Ranking1$Weighted_Rank = round(ifelse(((is.na(Ranking1$CK_Rank) != TRUE) & (is.na(Ranking1$TCG_Rank) != TRUE)), (((Ranking1$CK_Rank*.0818)+(Ranking1$TCG_Rank*.5568)/(.5568+.0818))),ifelse(((is.na(Ranking1$CK_Rank) != TRUE) & (is.na(Ranking1$TCG_Rank) = TRUE)),Ranking1$CK_Rank,ifelse(((is.na(Ranking1$CK_Rank) = TRUE) & (is.na(Ranking1$TCG_Rank) != TRUE)),Ranking1$TCG_Rank,40001))),2)

Ranking1 <- Ranking1[order(Ranking1$Weighted_Rank),]

Ranking1$CK_Rank <- Ranking$CK_Rank
Ranking1$TCG_Rank <- Ranking$TCG_Rank
Ranking1 = as.data.frame(Ranking1)
Ranking2 = Ranking1
Ranking2$CK_Rank = as.numeric(Ranking1$CK_Rank)
Ranking2$TCG_Rank = as.numeric(Ranking1$TCG_Rank)
Ranking2$Demand_Pct_Conf = round(ifelse(((Ranking2$MKT_Est = Pricing1$TCG_Price) & (is.na(Ranking2$TCG_Rank) != TRUE) & (Ranking2$CK_Rank < Worst_CK_Rank)), 64,ifelse(((Ranking2$MKT_Est = Pricing1$TCG_Price) & (is.na(Ranking2$TCG_Rank) != TRUE)),56,ifelse((Ranking2$CK_Rank < Worst_CK_Rank),8,0))),0)
Ranking2$CK_Rank <- Ranking$CK_Rank
Ranking2$TCG_Rank <- Ranking$TCG_Rank
Ranking2 <- Ranking2[order(Ranking2$CK_Rank),]

Ranking_Export <- Ranking2
Ranking_Export$Weighted_Rank <- seq.int(nrow(Ranking_Export))
Ranking_Export$Vendor <- TCG_Export$`Vendor Listings`[match(as.factor(Ranking_Export$CK_Key),as.factor(trimws(TCG_Export$Primary_Key)))]
Ranking_Export$TCG_Rank[is.na(Ranking_Export$TCG_Rank)] <- ""
Ranking_Export$Vendor <- as.character(Ranking_Export$Vendor)
Ranking_Export$Vendor[is.na(Ranking_Export$Vendor)] <- ""

Final_Export <- Pricing_Export
Final_Export$Vendor <- Ranking_Export$Vendor[match(Final_Export$CK_Key,Ranking_Export$CK_Key)]
Final_Export$Weighted_Rank <- Ranking_Export$Weighted_Rank[match(Final_Export$CK_Key,Ranking_Export$CK_Key)]
Final_Export$Adj_CK_Ranking <- Ranking_Export$CK_Rank[match(Final_Export$CK_Key,Ranking_Export$CK_Key)]
Final_Export$Demand_PCT_Conf <-  ifelse(((Final_Export$Adj_CK_Rank >= Worst_CK_Rank)&(Final_Export$TCG_Rank == "")),0,ifelse(((Final_Export$Adj_CK_Rank < Worst_CK_Rank)&(Final_Export$TCG_Rank == "")),8,ifelse(((Final_Export$Adj_CK_Rank >= Worst_CK_Rank)&(Final_Export$TCG_Rank != "")),56,ifelse(((Final_Export$Adj_CK_Rank < Worst_CK_Rank)&(Final_Export$TCG_Rank != "")),64,0))))
Final_Export$CK_MKT <-  Ranking_Export$CK_MKT[match(Final_Export$CK_Key,Ranking_Export$CK_Key)]
Final_Export$MKT_Est <- ifelse(Final_Export$MKT_Est == 0.00, Final_Export$CK_MKT, Final_Export$MKT_Est)
Final_Export <- Final_Export[c(1,2,3,4,5,7,6,14,15,17,12,19,18,20)]

names(Final_Export)<- c("Key","Card","Set","Rarity","F/NF","BL_QTY","BL","MKT","Arb","Sellers","TCG_Rank","CK_ADJ_Rank","OVR_Rank","%_of_Market")
Final_Export$CK_ADJ_Rank <- as.numeric(as.character(Final_Export$CK_ADJ_Rank))
Final_Export$CK_ADJ_Rank <- round(Final_Export$CK_ADJ_Rank,2)
Final_Export <- Final_Export[order(Final_Export$CK_ADJ_Rank),]
Final_Export$CK_ADJ_Rank <- seq.int(nrow(Final_Export))
Final_Export <- Final_Export[order(Final_Export$OVR_Rank),]
Final_Export$OVR_Rank <- seq.int(nrow(Final_Export))
Final_Export$MKT_TS_Single <- round((Final_Export$MKT * 1.08875)+.78,2)
Final_Export$MKT_TS_Set <- round(((Final_Export$MKT * 4)* 1.08875)+.78,2)
Final_Export$`Single_Arb_%` <- round((Final_Export$BL - Final_Export$MKT_TS_Single)/Final_Export$MKT_TS_Single,2)
Final_Export$`Set_Arb_%` <- round(((Final_Export$BL*4) - Final_Export$MKT_TS_Set)/Final_Export$MKT_TS_Set,2)

Final_Export_1 <- Final_Export
Final_Export_1$MKT <- as.numeric(Final_Export_1$MKT)

Final_Export_1 <- Final_Export[c(1,2,3,4,5,6,7,8,9,10,11,12,15,16,17,18,7)]
#seq.int(nrow(CK_MKT))
#Final_Export$BL_Value <- CK_Smaller_List$BL_Value[match(Final_Export$CK_Key,Ranking_Export$CK_Key)]
Final_Export_1 <- Final_Export_1[order(-Final_Export_1$`Single_Arb_%`),]
Final_Export_1<-subset(Final_Export_1, Final_Export_1$MKT!=0)
Final_Export_1<-subset(Final_Export_1, Final_Export_1$MKT!=0)

#Export My Masterpiece####

setwd("/home/cujo253/Reports/High Confidence Reps")
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"_Premium",".csv",sep="")
write.csv(Final_Export_1, file=csvFileName, row.names = FALSE)
print("FINAL EXPORT SUCCEEDED ONCE")

setwd("/home/cujo253/Reports/High Confidence Reps")
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"_Premium",".csv",sep="")
write.csv(Final_Export_1, file=csvFileName, row.names = FALSE) 
print("FINAL EXPORT SUCCEEDED TWICE")

setwd("/home/cujo253/Reports/TCG Vendor")
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"_TCG",".csv",sep="")
write.csv(TCG_Vendor, file=csvFileName, row.names = FALSE) 
print("TCG VENDOR EXPORT SUCCEEDED")


#setwd("/home/cujo253/Reports/Low Confidence Reps")
#Basic_Market_Review <- Basic_Market_Review[c(1,2,3,4,5,8,7,6)]
#names(Basic_Market_Review)<- c("Key","Card","Set","Rarity","F/NF","BL_QTY","BL","MKT")
#csvFileName <- paste(currentDate,"_Basic",".csv",sep="")
#write.csv(Basic_Market_Review, file=csvFileName, row.names = FALSE)
#Funny Money Report####

CK_Prices_df$key <- trimws(CK_Prices_df$key)
CK_Prices_df$TCG_Price <- Final_Export$MKT[match(CK_Prices_df$key,Final_Export$Key)]
CK_Prices_df$TCG_Price <- as.numeric(CK_Prices_df$TCG_Price)
CK_Prices_df$CK_TCG_PCT_DIFF <- as.numeric(as.character(CK_Prices_df$Price))/CK_Prices_df$TCG_Price
CK_Price_Comparison <- CK_Prices_df[c(5,1,2,3,7,4)]

names(CK_Price_Comparison) <- c("Key","Card_Name","Set","Rarity","TCG_Price","CK_Price")

#CK_Price_Comparison$TCG_Price[is.na(CK_Price_Comparison$TCG_Price)] <- 1
CK_Price_Comparison$TCG_Price <- ifelse(CK_Price_Comparison$TCG_Price == 0.00,NA, CK_Price_Comparison$TCG_Price)
CK_Price_Comparison <- na.omit(CK_Price_Comparison)
CK_Price_Comparison$CK_Price <- as.numeric(as.character(CK_Price_Comparison$CK_Price))
CK_Price_Comparison$Price_Diff <- round((CK_Price_Comparison$CK_Price-CK_Price_Comparison$TCG_Price),2)
CK_Price_Comparison$Price_Diff <- ifelse(CK_Price_Comparison$Price_Diff == CK_Price_Comparison$CK_Price, "Not Captured", CK_Price_Comparison$Price_Diff)
CK_Price_Comparison <- CK_Price_Comparison[which(CK_Price_Comparison$Price_Diff != "Not Captured"),]
CK_Price_Comparison <- CK_Price_Comparison[order(-as.numeric(CK_Price_Comparison$Price_Diff)),]
Sets <- read.csv("/home/cujo253/Essential_Referential_CSVS/Sets.csv",stringsAsFactors = TRUE)
Exclusion <- data.frame(Sets$Set_Excl,Sets$Excl_Excl)
colnames(Exclusion) <- c("Set_Excl","Excl_Excl")
CK_Price_Comparison$Set_Group <- Exclusion$Excl_Excl[match(CK_Price_Comparison$Set,Exclusion$Set_Excl)]
CK_Price_Comparison$CK_BL <- as.numeric(Final_Export$BL)[match(CK_Price_Comparison$Key,Final_Export$Key)]
CK_Price_Comparison$CK_BL_Backing <- round((CK_Price_Comparison$CK_BL/CK_Price_Comparison$CK_Price),2)
CK_Price_Comparison$TCG_BL_Backing <- round((CK_Price_Comparison$CK_BL/CK_Price_Comparison$TCG_Price),2)
CK_Price_Comparison$TCG_Vendors <- Final_Export$Sellers[match(CK_Price_Comparison$Key,Final_Export$Key)]
CK_Price_Comparison$BL_Desired_Amt <- Final_Export$BL_QTY[match(CK_Price_Comparison$Key,Final_Export$Key)]
CK_Price_Comparison$`F/NF` <- ""
CK_Price_Comparison <- CK_Price_Comparison[c(1,2,3,4,14,13,9,5,6,7,12,10,11,8)]
names(CK_Price_Comparison) <- c("Key","Card","Set","Rarity","F/NF","BL_QTY","BL","TCG_MKT","CK_MKT","MKT_Diff","Sellers","CK_MKT_%","TCG_MKT_%","Group")
CK_Price_Excluded_Sets <- CK_Price_Comparison[which(CK_Price_Comparison$Group != "Exclude"),]
CK_Price_Excluded_Sets$TCG_Rank <- Final_Export$TCG_Rank[match(CK_Price_Excluded_Sets$Key,Final_Export$Key)]
CK_Price_Excluded_Sets$CK_Rank <- Final_Export$CK_ADJ_Rank[match(CK_Price_Excluded_Sets$Key,Final_Export$Key)]
#View(CK_Price_Excluded_Sets)
#Final Preparations For Funny Money####
currentDate <- Sys.Date()
CK_Price_Excluded_Sets[is.na(CK_Price_Excluded_Sets)==TRUE]<-""
CK_Price_Excluded_Sets <- CK_Price_Excluded_Sets[which( (CK_Price_Excluded_Sets$TCG_MKT != 1) & (CK_Price_Excluded_Sets$BL != "")),]
Funny_Money_Analysis <- CK_Price_Excluded_Sets

setwd("/home/cujo253/Funny Money")

csvFileName <- paste(currentDate,"_CK_Credit_Data",".csv",sep="")
write.csv(Funny_Money_Analysis, file=csvFileName, row.names = FALSE) 

setwd("/home/cujo253/Reports/High Confidence Reps")
Final_Export_1$Exclusion <- Exclusion$Excl_Excl[match(Final_Export_1$Set,Exclusion$Set_Excl)]

currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"_Premium",".csv",sep="")
write.csv(Final_Export_1, file=csvFileName, row.names = FALSE)

#Load in Dated Premium Reports####
#View(Final_Export)
currentDate <- Sys.Date()
today_final_export <- paste("/home/cujo253/Reports/High Confidence Reps/",currentDate,"_Premium.csv", sep = "")
FE <- read_csv(today_final_export, 
               col_types = cols(`F/NF` = col_character(), 
                                Sellers = col_character(), TCG_Rank = col_character(), BL_QTY = col_character()))
Special_Rep <- FE[c(1,2,3,4,7,15,17,18,10,11,12)]
#View(Special_Rep$Sellers)
YEST <- Sys.Date()-1
WK <- Sys.Date()-7
MTH <- Sys.Date()-30
YEST_BL_Import <- paste("/home/cujo253/Reports/High Confidence Reps/",YEST,"_Premium.csv", sep = "")
WK_BL_Import <- paste("/home/cujo253/Reports/High Confidence Reps/",WK,"_Premium.csv", sep = "")
MTH_BL_Import <- paste("/home/cujo253/Reports/High Confidence Reps/",MTH,"_Premium.csv", sep = "")
Yesterday <- read_csv(YEST_BL_Import, 
                      col_types = cols(`F/NF` = col_character(), 
                                       Sellers = col_character(), TCG_Rank = col_character(), BL_QTY = col_character()))
Week_Ago <- read_csv(WK_BL_Import, 
                     col_types = cols(`F/NF` = col_character(), 
                                      Sellers = col_character(), TCG_Rank = col_character(), BL_QTY = col_character()))
Month_Ago <-read_csv(MTH_BL_Import, 
                     col_types = cols(`F/NF` = col_character(), 
                                      Sellers = col_character(), TCG_Rank = col_character(), BL_QTY = col_character()))
#View(Week_Ago)
Yesterday$Sellers <- gsub("[^0-9.-]", 0,Yesterday$Sellers)
Yesterday$Sellers <- as.numeric(Yesterday$Sellers)
Special_Rep$Sellers <- gsub("[^0-9.-]", 0,Special_Rep$Sellers)
Special_Rep$Sellers<- as.numeric(Special_Rep$Sellers)
Week_Ago$Sellers <- gsub("[^0-9.-]", 0,Week_Ago$Sellers)
Week_Ago$Sellers <- as.numeric(Week_Ago$Sellers)
Month_Ago$Sellers <- gsub("[^0-9.-]", 0,Month_Ago$Sellers)
Month_Ago$Sellers <- as.numeric(Month_Ago$Sellers)
Key_Amalgamation <- NULL
Key_Amalgamation <- cbind(Special_Rep$Key, Yesterday$Key)
Key_Amalgamation <- cbind(Key_Amalgamation, Week_Ago$Key)
Key_Amalgamation <- cbind(Key_Amalgamation, Month_Ago$Key)
Key_Amalgamation <- as.data.frame(Key_Amalgamation)
Key_Amalgamation <- reshape2::melt(Key_Amalgamation, id.vars=c(),var='Key')
Unique_Keys <- unique(Key_Amalgamation$value)
Unique_Keys <- as.data.frame(Unique_Keys)

Names <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(`hasFoil` = col_character()))
Names <- Names[c(8:12)]
colnames(Names) <- c("Key","name","Set","Rarity","F/NF")
# Names <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
# Names <- as.data.frame(Names)

Unique_Keys$Name <- Names$name[match(Unique_Keys$Unique_Keys,Names$Key)]
Unique_Keys$Set <- Names$Set[match(Unique_Keys$Unique_Keys,Names$Key)]
Unique_Keys$Rarity <- Names$Rarity[match(Unique_Keys$Unique_Keys,Names$Key)]
Unique_Keys$Foil <- Names$`F/NF`[match(Unique_Keys$Unique_Keys,Names$Key)]
#View(Unique_Keys)
#Growth Reports####
BuyList_Growth <- Unique_Keys
BuyList_Growth$Todays_BL <- Special_Rep$BL[match(BuyList_Growth$Unique_Keys,Special_Rep$Key)]
BuyList_Growth$Yesterday_BL <- Yesterday$BL[match(BuyList_Growth$Unique_Keys,Yesterday$Key)]
BuyList_Growth$Week_Ago_BL <- Week_Ago$BL[match(BuyList_Growth$Unique_Keys,Week_Ago$Key)]
BuyList_Growth$Month_Ago_BL <- Month_Ago$BL[match(BuyList_Growth$Unique_Keys,Month_Ago$Key)]

BuyList_Growth$Yesterday_BL_Chg <- round((BuyList_Growth$Todays_BL - BuyList_Growth$Yesterday_BL)/BuyList_Growth$Yesterday_BL,4)
BuyList_Growth$Week_Ago_BL_Chg <- round((BuyList_Growth$Todays_BL - BuyList_Growth$Week_Ago_BL)/BuyList_Growth$Week_Ago_BL,4)
BuyList_Growth$Month_Ago_BL_Chg <- round((BuyList_Growth$Todays_BL - BuyList_Growth$Month_Ago_BL)/BuyList_Growth$Month_Ago_BL,4)

BuyList_Growth$BuyList_Backing <- Funny_Money_Analysis$CK_MKT[match(BuyList_Growth$Unique_Keys, Funny_Money_Analysis$Key)]
BuyList_Growth$BuyList_Backing <- 1 - round((BuyList_Growth$BuyList_Backing - BuyList_Growth$Todays_BL)/BuyList_Growth$BuyList_Backing,4)

BuyList_Growth[is.na(BuyList_Growth)] <- ""
BuyList_Growth[BuyList_Growth == "Inf"] <- ""

Consistent_BuyLists <- subset(BuyList_Growth, is.na(BuyList_Growth$Todays_BL) != TRUE & is.na(BuyList_Growth$Yesterday_BL) != TRUE & is.na(BuyList_Growth$Week_Ago_BL) != TRUE & is.na(BuyList_Growth$Month_Ago_BL) != TRUE & is.na(BuyList_Growth$Yesterday_BL_Chg) != TRUE & is.na(BuyList_Growth$Week_Ago_BL_Chg) != TRUE & is.na(BuyList_Growth$Month_Ago_BL_Chg) != TRUE)

#View(Consistent_BuyLists)
#View(BuyList_Growth)

YEST <- Sys.Date()-1
WK <- Sys.Date()-7
MTH <- Sys.Date()-29
Todays_BL_Import <- paste("/home/cujo253/Reports/TCG Vendor/",currentDate,"_TCG.csv", sep = "")
YEST_BL_Import <- paste("/home/cujo253/Reports/TCG Vendor/",YEST,"_TCG.csv", sep = "")
WK_BL_Import <- paste("/home/cujo253/Reports/TCG Vendor/",WK,"_TCG.csv", sep = "")
MTH_BL_Import <- paste("/home/cujo253/Reports/TCG Vendor/",MTH,"_TCG.csv", sep = "")
TCG_Vendor <- read_csv(Todays_BL_Import,col_types = cols(`Vendor Listings` = col_number()))
Yesterday <- read_csv(YEST_BL_Import,col_types = cols(`Vendor Listings` = col_number())) 
Week_Ago <- read_csv(WK_BL_Import,col_types = cols(`Vendor Listings` = col_number()))
Month_Ago <-read_csv(MTH_BL_Import,col_types = cols(`Vendor Listings` = col_number()))


Vendor_Growth <- Unique_Keys
TCG_Vendor$`Vendor Listings` <- gsub("[^0-9.-]", 0,TCG_Vendor$`Vendor Listings`)
Yesterday$`Vendor Listings` <- gsub("[^0-9.-]", 0,Yesterday$`Vendor Listings`)
Week_Ago$`Vendor Listings` <- gsub("[^0-9.-]", 0,Week_Ago$`Vendor Listings`)
Month_Ago$`Vendor Listings` <- gsub("[^0-9.-]", 0,Month_Ago$`Vendor Listings`)

#View(Special_Rep)
Vendor_Growth$Todays_Sellers <- as.numeric(as.character(TCG_Vendor$`Vendor Listings`[match(Vendor_Growth$Unique_Keys,TCG_Vendor$Primary_Key)]))
Vendor_Growth$Yesterday_Sellers <- as.numeric(as.character(Yesterday$`Vendor Listings`[match(Vendor_Growth$Unique_Keys,Yesterday$Primary_Key)]))
Vendor_Growth$Week_Ago_Sellers <- as.numeric(as.character(Week_Ago$`Vendor Listings`[match(Vendor_Growth$Unique_Keys,Week_Ago$Primary_Key)]))
Vendor_Growth$Month_Ago_Sellers <- as.numeric(as.character(Month_Ago$`Vendor Listings`[match(Vendor_Growth$Unique_Keys,Month_Ago$Primary_Key)]))

# Vendor_Growth$Todays_Sellers <- ifelse((Vendor_Growth$Todays_Sellers >= Vendor_Growth$Yesterday_Sellers * 1.2) | (Vendor_Growth$Todays_Sellers <= Vendor_Growth$Yesterday_Sellers * .8), NA, Vendor_Growth$Todays_Sellers)
# Vendor_Growth$Week_Ago_Sellers <- ifelse( (Vendor_Growth$Week_Ago_Sellers <= Vendor_Growth$Month_Ago_Sellers * 1.50 | Vendor_Growth$Week_Ago_Sellers >= Vendor_Growth$Month_Ago_Sellers * .50 ) & (nchar(Vendor_Growth$Week_Ago_Sellers)>2) , Vendor_Growth$Month_Ago_Sellers, Vendor_Growth$Week_Ago_Sellers)
# Vendor_Growth$Month_Ago_Sellers <- ifelse(nchar(Vendor_Growth$Month_Ago_Sellers) < nchar(Vendor_Growth$Week_Ago_Sellers)-2 | nchar(Vendor_Growth$Month_Ago_Sellers) > nchar(Vendor_Growth$Week_Ago_Sellers)+2, NA, Vendor_Growth$Month_Ago_Sellers)
# Vendor_Growth$Month_Ago_Sellers <- ifelse((is.na(Vendor_Growth$Week_Ago_Sellers) == T) & (nchar(Vendor_Growth$Month_Ago_Sellers)+1 == nchar(Vendor_Growth$Yesterday_Sellers) | nchar(Vendor_Growth$Month_Ago_Sellers)-1 == nchar(Vendor_Growth$Yesterday_Sellers ) | nchar(Vendor_Growth$Month_Ago_Sellers) == nchar(Vendor_Growth$Yesterday_Sellers )),Vendor_Growth$Month_Ago_Sellers,NA)
# Vendor_Growth$Month_Ago_Sellers <- ifelse(Vendor_Growth$Month_Ago_Sellers <= .80 * Vendor_Growth$Yesterday_Sellers, NA, Vendor_Growth$Month_Ago_Sellers)


Vendor_Growth$Yesterday_Sellers_Chg <- round((Vendor_Growth$Todays_Sellers - Vendor_Growth$Yesterday_Sellers)/Vendor_Growth$Yesterday_Sellers,4)*(-1)
Vendor_Growth$Week_Ago_Sellers_Chg <- round((Vendor_Growth$Todays_Sellers - Vendor_Growth$Week_Ago_Sellers)/Vendor_Growth$Week_Ago_Sellers,4)*(-1)
Vendor_Growth$Month_Ago_Sellers_Chg <- round((Vendor_Growth$Todays_Sellers - Vendor_Growth$Month_Ago_Sellers)/Vendor_Growth$Month_Ago_Sellers,4)*(-1)

Vendor_Growth$Month_Ago_Sellers_Chg <- ifelse(nchar(Vendor_Growth$Month_Ago_Sellers)>2 & (Vendor_Growth$Month_Ago_Sellers_Chg > .50 | Vendor_Growth$Month_Ago_Sellers_Chg > .50), NA, Vendor_Growth$Month_Ago_Sellers_Chg)
Consistent_Vendors <- Vendor_Growth[c(-5)]
#View(Consistent_Vendors)
#Consistent_Vendors <- na.omit(Consistent_Vendors)
YEST <- Sys.Date()-1
WK <- Sys.Date()-7
MTH <- Sys.Date()-30
YEST_BL_Import <- paste("/home/cujo253/Reports/High Confidence Reps/",YEST,"_Premium.csv", sep = "")
WK_BL_Import <- paste("/home/cujo253/Reports/High Confidence Reps/",WK,"_Premium.csv", sep = "")
MTH_BL_Import <- paste("/home/cujo253/Reports/High Confidence Reps/",MTH,"_Premium.csv", sep = "")
Yesterday <- read_csv(YEST_BL_Import, 
                      col_types = cols(`F/NF` = col_character(), 
                                       Sellers = col_character(), TCG_Rank = col_character(), BL_QTY = col_character()))
Week_Ago <- read_csv(WK_BL_Import, 
                     col_types = cols(`F/NF` = col_character(), 
                                      Sellers = col_character(), TCG_Rank = col_character(), BL_QTY = col_character()))
Month_Ago <-read_csv(MTH_BL_Import, 
                     col_types = cols(`F/NF` = col_character(), 
                                      Sellers = col_character(), TCG_Rank = col_character(), BL_QTY = col_character()))

TCG_Growth <- Unique_Keys
TCG_Growth$Todays_TCG <- Special_Rep$TCG_Rank[match(TCG_Growth$Unique_Keys,Special_Rep$Key)]
TCG_Growth$Yesterday_TCG <- Yesterday$TCG_Rank[match(TCG_Growth$Unique_Keys,Yesterday$Key)]
TCG_Growth$Week_Ago_TCG <- Week_Ago$TCG_Rank[match(TCG_Growth$Unique_Keys,Week_Ago$Key)]
TCG_Growth$Month_Ago_TCG <- Month_Ago$TCG_Rank[match(TCG_Growth$Unique_Keys,Month_Ago$Key)]

TCG_Growth$Todays_TCG <- as.numeric(TCG_Growth$Todays_TCG)
TCG_Growth$Yesterday_TCG <- as.numeric(TCG_Growth$Yesterday_TCG)
TCG_Growth$Week_Ago_TCG <- as.numeric(TCG_Growth$Week_Ago_TCG)
TCG_Growth$Month_Ago_TCG <- as.numeric(TCG_Growth$Month_Ago_TCG)

TCG_Growth$Yesterday_TCG_Chg <- round((TCG_Growth$Todays_TCG - TCG_Growth$Yesterday_TCG)/TCG_Growth$Yesterday_TCG,4)*(-1)
TCG_Growth$Week_Ago_TCG_Chg <- round((TCG_Growth$Todays_TCG - TCG_Growth$Week_Ago_TCG)/TCG_Growth$Week_Ago_TCG,4)*(-1)
TCG_Growth$Month_Ago_TCG_Chg <- round((TCG_Growth$Todays_TCG - TCG_Growth$Month_Ago_TCG)/TCG_Growth$Month_Ago_TCG,4)*(-1)
Consistent_Sellers <- subset(TCG_Growth, is.na(TCG_Growth$Todays_TCG) != TRUE & is.na(TCG_Growth$Yesterday_TCG) != TRUE & is.na(TCG_Growth$Week_Ago_TCG) != TRUE & is.na(TCG_Growth$Month_Ago_TCG) != TRUE & is.na(TCG_Growth$Yesterday_TCG_Chg) != TRUE & is.na(TCG_Growth$Week_Ago_TCG_Chg) != TRUE & is.na(TCG_Growth$Month_Ago_TCG_Chg) != TRUE)
Consistent_Sellers <- Consistent_Sellers[c(-5)]
#View(TCG_Growth)
#View(Consistent_Vendors)

setwd("/home/cujo253/Reports/Growth Reports/Buylist_Growth")
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"_C_BuyList_Growth",".csv",sep="")
write.csv(Consistent_BuyLists, file=csvFileName, row.names = FALSE)
setwd("/home/cujo253/Reports/Growth Reports/Demand_Growth")
csvFileName <- paste(currentDate,"_C_Demand_Growth",".csv",sep="")
write.csv(Consistent_Sellers, file=csvFileName, row.names = FALSE)
setwd("/home/cujo253/Reports/Growth Reports/Vendor_Growth")
csvFileName <- paste(currentDate,"_C_Vendor_Growth",".csv",sep="")
write.csv(Consistent_Vendors, file=csvFileName, row.names = FALSE)
#Today's Buy List Movers####
#View(Consistent_BuyLists)
Consistent_BuyLists$Yesterday_BL_Chg <- as.numeric(Consistent_BuyLists$Yesterday_BL_Chg)
Consistent_BuyLists$Week_Ago_BL_Chg <- as.numeric(Consistent_BuyLists$Week_Ago_BL_Chg)
Consistent_BuyLists$Month_Ago_BL_Chg <- as.numeric(Consistent_BuyLists$Month_Ago_BL_Chg)
Consistent_BuyLists$Todays_BL <- as.numeric(Consistent_BuyLists$Todays_BL)
Consistent_BuyLists$Yesterday_BL <- as.numeric(Consistent_BuyLists$Yesterday_BL )
Consistent_BuyLists$Week_Ago_BL <- as.numeric(Consistent_BuyLists$Week_Ago_BL)
Consistent_BuyLists$Month_Ago_BL <- as.numeric(Consistent_BuyLists$Month_Ago_BL)
Consistent_BuyLists$Foil <- as.factor(Consistent_BuyLists$Foil)

CB_Short <- Consistent_BuyLists[which(Consistent_BuyLists$Yesterday_BL >= 1.25),]
CB_Short <- CB_Short[which(CB_Short$Todays_BL >= 1.25),]
#View(CB_Short)
CB_Short_F <- CB_Short[which(CB_Short$Foil == "FOIL"),]
CB_Short_NF <- CB_Short[which(CB_Short$Foil != "FOIL"),]
CB_Short_NF <- CB_Short_NF[which(CB_Short_NF$BuyList_Backing != "" & CB_Short_NF$BuyList_Backing >= .55 & is.na(CB_Short_NF$Week_Ago_BL_Chg) != TRUE & CB_Short_NF$Week_Ago_BL_Chg > 0 & is.na(CB_Short_NF$Month_Ago_BL_Chg) != TRUE & CB_Short_NF$Month_Ago_BL_Chg > 0),]
CB_Short_F <- CB_Short_F[which(is.na(CB_Short_F$Week_Ago_BL_Chg) != TRUE & CB_Short_F$Week_Ago_BL_Chg > 0 & is.na(CB_Short_F$Month_Ago_BL_Chg) != TRUE & CB_Short_F$Month_Ago_BL_Chg > 0),]
CB_Short_F <- CB_Short_F[order(-CB_Short_F$Yesterday_BL_Chg),]
CB_Short_NF <- CB_Short_NF[order(-CB_Short_NF$Yesterday_BL_Chg),]
#CB_Short_NF$Growth <- ifelse((CB_Short_NF$Yesterday_BL > CB_Short_NF$Week_Ago_BL & CB_Short_NF$Week_Ago_BL > CB_Short_NF$Month_Ago_BL), "Yes", ifelse(((CB_Short_NF$Yesterday_BL > CB_Short_NF$Week_Ago_BL & CB_Short_NF$Week_Ago_BL < CB_Short_NF$Month_Ago_BL)|(CB_Short_NF$Yesterday_BL < CB_Short_NF$Week_Ago_BL & CB_Short_NF$Week_Ago_BL > CB_Short_NF$Month_Ago_BL)), "Mixed", "No"))
CB_Short_NF$Growth <- ifelse((CB_Short_NF$Todays_BL > CB_Short_NF$Yesterday_BL & CB_Short_NF$Yesterday_BL > CB_Short_NF$Week_Ago_BL & CB_Short_NF$Week_Ago_BL > CB_Short_NF$Month_Ago_BL), "Yes", ifelse(((CB_Short_NF$Todays_BL < CB_Short_NF$Yesterday_BL & CB_Short_NF$Yesterday_BL < CB_Short_NF$Week_Ago_BL & CB_Short_NF$Week_Ago_BL < CB_Short_NF$Month_Ago_BL)), "No", "Mixed"))
CB_Short_NF$Highest <- ifelse(CB_Short_NF$Todays_BL >= CB_Short_NF$Yesterday_BL & CB_Short_NF$Todays_BL >= CB_Short_NF$Week_Ago_BL & CB_Short_NF$Todays_BL >= CB_Short_NF$Month_Ago_BL , "Today", ifelse(CB_Short_NF$Yesterday_BL >= CB_Short_NF$Todays_BL & CB_Short_NF$Yesterday_BL >= CB_Short_NF$Week_Ago_BL & CB_Short_NF$Yesterday_BL >= CB_Short_NF$Month_Ago_BL, "Yesterday",ifelse(CB_Short_NF$Week_Ago_BL >= CB_Short_NF$Todays_BL & CB_Short_NF$Week_Ago_BL >= CB_Short_NF$Yesterday_BL & CB_Short_NF$Week_Ago_BL >= CB_Short_NF$Month_Ago_BL, "Week_Ago",ifelse(CB_Short_NF$Month_Ago_BL >= CB_Short_NF$Todays_BL & CB_Short_NF$Month_Ago_BL >= CB_Short_NF$Yesterday_BL & CB_Short_NF$Month_Ago_BL >= CB_Short_NF$Week_Ago_BL,"Month_Ago","")))) 
#View(CB_Short_NF)
setwd("/home/cujo253/Reports/Growth Reports/Refined_BL")
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"_Refined_BuyList_Growth",".csv",sep="")
write.csv(CB_Short_NF, file=csvFileName, row.names = FALSE)

CB_Short_F$Growth <- ifelse((CB_Short_F$Yesterday_BL > CB_Short_F$Week_Ago_BL & CB_Short_F$Week_Ago_BL > CB_Short_F$Month_Ago_BL), "Yes", ifelse(((CB_Short_F$Yesterday_BL > CB_Short_F$Week_Ago_BL & CB_Short_F$Week_Ago_BL < CB_Short_F$Month_Ago_BL)|(CB_Short_F$Yesterday_BL < CB_Short_F$Week_Ago_BL & CB_Short_F$Week_Ago_BL > CB_Short_F$Month_Ago_BL)), "Mixed", "No"))
CB_Short_F$Growth <- ifelse((CB_Short_F$Todays_BL > CB_Short_F$Yesterday_BL & CB_Short_F$Yesterday_BL > CB_Short_F$Week_Ago_BL & CB_Short_F$Week_Ago_BL > CB_Short_F$Month_Ago_BL), "Yes", ifelse(((CB_Short_F$Todays_BL < CB_Short_F$Yesterday_BL & CB_Short_F$Yesterday_BL < CB_Short_F$Week_Ago_BL & CB_Short_F$Week_Ago_BL < CB_Short_F$Month_Ago_BL)), "No", "Mixed"))
CB_Short_F$Highest <- ifelse(CB_Short_F$Todays_BL >= CB_Short_F$Yesterday_BL & CB_Short_F$Todays_BL >= CB_Short_F$Week_Ago_BL & CB_Short_F$Todays_BL >= CB_Short_F$Month_Ago_BL , "Today", ifelse(CB_Short_F$Yesterday_BL >= CB_Short_F$Todays_BL & CB_Short_F$Yesterday_BL >= CB_Short_F$Week_Ago_BL & CB_Short_F$Yesterday_BL >= CB_Short_F$Month_Ago_BL, "Yesterday",ifelse(CB_Short_F$Week_Ago_BL >= CB_Short_F$Todays_BL & CB_Short_F$Week_Ago_BL >= CB_Short_F$Yesterday_BL & CB_Short_F$Week_Ago_BL >= CB_Short_F$Month_Ago_BL, "Week_Ago",ifelse(CB_Short_F$Month_Ago_BL >= CB_Short_F$Todays_BL & CB_Short_F$Month_Ago_BL >= CB_Short_F$Yesterday_BL & CB_Short_F$Month_Ago_BL >= CB_Short_F$Week_Ago_BL,"Month_Ago","")))) 
YEST <- Sys.Date()-1
WK <- Sys.Date()-7
MTH <- Sys.Date()-30

YEST_BL_Import <- paste("/home/cujo253/Funny Money/",YEST,"_CK_Credit_Data.csv", sep = "")
WK_BL_Import <- paste("/home/cujo253/Funny Money/",WK,"_CK_Credit_Data.csv", sep = "")
MTH_BL_Import <- paste("/home/cujo253/Funny Money/",MTH,"_CK_Credit_Data.csv", sep = "")

Yesterday <- read_csv(YEST_BL_Import, col_types = cols(BL = col_number(), BL_QTY = col_character(), CK_MKT = col_number(), `CK_MKT_%` = col_number(), `F/NF` = col_character(), MKT_Diff = col_number(), Sellers = col_number(), TCG_MKT = col_number(), `TCG_MKT_%` = col_number()))
Week_Ago <- read_csv(WK_BL_Import, col_types = cols(BL = col_number(), BL_QTY = col_character(), CK_MKT = col_number(), `CK_MKT_%` = col_number(), `F/NF` = col_character(), MKT_Diff = col_number(), Sellers = col_number(), TCG_MKT = col_number(), `TCG_MKT_%` = col_number()))
Month_Ago <- read_csv(MTH_BL_Import, col_types = cols(BL = col_number(), BL_QTY = col_character(), CK_MKT = col_number(), `CK_MKT_%` = col_number(), `F/NF` = col_character(), MKT_Diff = col_number(), Sellers = col_number(), TCG_MKT = col_number(), `TCG_MKT_%` = col_number()))

CB_Short_NF$BuyList_Backing <- as.numeric(CB_Short_NF$BuyList_Backing)
CB_Short_NF$Todays_BL_Accel <- round((CB_Short_NF$BuyList_Backing - Yesterday$`CK_MKT_%`[match(CB_Short_NF$Unique_Keys,Yesterday$Key)])/Yesterday$`CK_MKT_%`[match(CB_Short_NF$Unique_Keys,Yesterday$Key)],2)
CB_Short_NF$Yesterday_BL_Accel <- round((Yesterday$`CK_MKT_%`[match(CB_Short_NF$Unique_Keys,Yesterday$Key)] - Week_Ago$`CK_MKT_%`[match(CB_Short_NF$Unique_Keys,Week_Ago$Key)])/Week_Ago$`CK_MKT_%`[match(CB_Short_NF$Unique_Keys,Week_Ago$Key)],2)
CB_Short_NF$Week_Ago_BL_Accel <- round((Week_Ago$`CK_MKT_%`[match(CB_Short_NF$Unique_Keys,Week_Ago$Key)] - Month_Ago$`CK_MKT_%`[match(CB_Short_NF$Unique_Keys,Month_Ago$Key)])/Month_Ago$`CK_MKT_%`[match(CB_Short_NF$Unique_Keys,Month_Ago$Key)],2)

CB_Short_NF$CK_MKT <- Funny_Money_Analysis$CK_MKT[match(CB_Short_NF$Unique_Keys,Funny_Money_Analysis$Key)]
CB_Short_NF$Yest_CK_MKT <- Yesterday$CK_MKT[match(CB_Short_NF$Unique_Keys,Yesterday$Key)]
CB_Short_NF$Week_CK_MKT <- Week_Ago$CK_MKT[match(CB_Short_NF$Unique_Keys,Week_Ago$Key)]
CB_Short_NF$Month_CK_MKT <- Month_Ago$CK_MKT[match(CB_Short_NF$Unique_Keys,Month_Ago$Key)]

CB_Short_NF$Todays_MKT_Accel <- round((CB_Short_NF$CK_MKT - Yesterday$CK_MKT[match(CB_Short_NF$Unique_Keys,Yesterday$Key)])/Yesterday$CK_MKT[match(CB_Short_NF$Unique_Keys,Yesterday$Key)],2)
CB_Short_NF$Yesterday_MKT_Accel <- round((Yesterday$CK_MKT[match(CB_Short_NF$Unique_Keys,Yesterday$Key)] - Week_Ago$CK_MKT[match(CB_Short_NF$Unique_Keys,Week_Ago$Key)])/Week_Ago$CK_MKT[match(CB_Short_NF$Unique_Keys,Week_Ago$Key)],2)
CB_Short_NF$Week_Ago_MKT_Accel <- round((Week_Ago$CK_MKT[match(CB_Short_NF$Unique_Keys,Week_Ago$Key)] - Month_Ago$CK_MKT[match(CB_Short_NF$Unique_Keys,Month_Ago$Key)])/Month_Ago$CK_MKT[match(CB_Short_NF$Unique_Keys,Month_Ago$Key)],2)

CB_CK_Final <- CB_Short_NF[which(CB_Short_NF$Growth != "Mixed" & CB_Short_NF$Highest == "Today"),]
CB_CK_Final$Growth <- ifelse((CB_CK_Final$CK_MKT > CB_CK_Final$Yest_CK_MKT & CB_CK_Final$Yest_CK_MKT > CB_CK_Final$Week_CK_MKT & CB_CK_Final$Week_CK_MKT > CB_CK_Final$Month_CK_MKT), "Yes", ifelse(((CB_CK_Final$CK_MKT < CB_CK_Final$Yest_CK_MKT & CB_CK_Final$Yest_CK_MKT < CB_CK_Final$Week_CK_MKT & CB_CK_Final$Week_CK_MKT < CB_CK_Final$Month_CK_MKT)), "No", "Mixed"))

Stringent_CB_CK_Final <- CB_CK_Final[which(CB_CK_Final$Yesterday_BL_Accel > 0 & CB_CK_Final$Week_Ago_BL_Accel > 0 & CB_CK_Final$Growth == "Yes"),]
Goldilocks_CB_CK_Final <- CB_CK_Final[which(CB_CK_Final$Yesterday_BL_Accel > 0 & CB_CK_Final$Growth == "Yes"),]
Relaxed_CB_CK_Final <- CB_CK_Final[which(CB_CK_Final$Yesterday_BL_Accel > 0),]

setwd("/home/cujo253/Reports/A_Game/Strict")
csvFileName <- paste(currentDate,"_Strict",".csv",sep="")
write.csv(Stringent_CB_CK_Final, file=csvFileName, row.names = FALSE)
setwd("/home/cujo253/Reports/A_Game/Goldilocks")
csvFileName <- paste(currentDate,"_Goldilocks",".csv",sep="")
write.csv(Goldilocks_CB_CK_Final, file=csvFileName, row.names = FALSE)
setwd("/home/cujo253/Reports/A_Game/Relaxed")
csvFileName <- paste(currentDate,"_Relaxed",".csv",sep="")
write.csv(Relaxed_CB_CK_Final, file=csvFileName, row.names = FALSE)

#Reduce & Add TCG to Top Movers####
Stringent_TCG <- Stringent_CB_CK_Final[,c(1,2,3,4,5,6,12,13,19,22)]
Goldilocks_TCG <- Goldilocks_CB_CK_Final[,c(1,2,3,4,5,6,12,13,19,22)]
Relaxed_TCG <- Relaxed_CB_CK_Final[,c(1,2,3,4,5,6,12,13,19,22)]
YEST <- Sys.Date()-2
WK <- Sys.Date()-7
MTH <- Sys.Date()-30
YEST_BL_Import <- paste("/home/cujo253/Reports/TCG Vendor/",YEST,"_TCG.csv", sep = "")
WK_BL_Import <- paste("/home/cujo253/Reports/TCG Vendor/",WK,"_TCG.csv", sep = "")
MTH_BL_Import <- paste("/home/cujo253/Reports/TCG Vendor/",MTH,"_TCG.csv", sep = "")
Yesterday <- read_csv(YEST_BL_Import, col_types = cols(MKT_EST = col_number(), Rank = col_number(), `Vendor Listings` = col_number()))
Week_Ago <- read_csv(WK_BL_Import, col_types = cols(MKT_EST = col_number(), Rank = col_number(), `Vendor Listings` = col_number()))
Month_Ago <-read_csv(MTH_BL_Import, col_types = cols(MKT_EST = col_number(), Rank = col_number(), `Vendor Listings` = col_number()))

Special_Rep$Sellers <- as.numeric(Special_Rep$Sellers)
TCG_Vendor$Primary_Key <- as.character(TCG_Vendor$Primary_Key)
TCG_Vendor$Primary_Key <- trimws(TCG_Vendor$Primary_Key)
TCG_Vendor$Primary_Key <- as.factor(TCG_Vendor$Primary_Key)

Goldilocks_TCG$Vendors <- Special_Rep$Sellers[match(Goldilocks_TCG$Unique_Keys,Special_Rep$Key)]
Goldilocks_TCG$Month_Vendors <- Month_Ago$`Vendor Listings`[match(Goldilocks_TCG$Unique_Keys, Month_Ago$Primary_Key)]
Goldilocks_TCG$TCG_Demand <- Special_Rep$TCG_Rank[match(Goldilocks_TCG$Unique_Keys,Special_Rep$Key)]
Goldilocks_TCG$Month_TCG_Demand <-Month_Ago$Rank[match(Goldilocks_TCG$Unique_Keys, Month_Ago$Primary_Key)]
Goldilocks_TCG$CK_Demand <- Special_Rep$CK_ADJ_Rank[match(Goldilocks_TCG$Unique_Keys,Special_Rep$Key)]
MTH_BL_Import <- paste("/home/cujo253/Reports/High Confidence Reps/",MTH,"_Premium.csv", sep = "")
Month_Ago <-read_csv(MTH_BL_Import, col_types = cols(`F/NF` = col_character(), Sellers = col_character(), TCG_Rank = col_character(), BL_QTY = col_character()))
#View(Month_Ago)
Goldilocks_TCG$Month_CK_Demand <- Month_Ago$CK_ADJ_Rank[match(Goldilocks_TCG$Unique_Keys, Month_Ago$Key)]
MTH_BL_Import <- paste("/home/cujo253/Reports/TCG Vendor/",MTH,"_TCG.csv", sep = "")
Month_Ago <-read_csv(MTH_BL_Import, col_types = cols(MKT_EST = col_number(), Rank = col_number(), `Vendor Listings` = col_number()))
Goldilocks_TCG$TCG_MKT <- Final_Export$MKT[match(Goldilocks_TCG$Unique_Keys, Final_Export$Key)]
Goldilocks_TCG$Month_TCG_Market <- Month_Ago$MKT_EST[match(Goldilocks_TCG$Unique_Keys, Month_Ago$Primary_Key)]
Goldilocks_Final <- Goldilocks_TCG[,c(1,2,3,4,5,6,8,7,9)]
Goldilocks_Final$CK_MKT_MoM <- Goldilocks_TCG$CK_MKT - Goldilocks_TCG$Month_CK_MKT
Goldilocks_Final$TCG_MKT <- Goldilocks_TCG$TCG_MKT
Goldilocks_Final$TCG_MoM <- Goldilocks_TCG$TCG_MKT - Goldilocks_TCG$Month_TCG_Market
Goldilocks_Final$CK_DMD <- Goldilocks_TCG$CK_Demand
Goldilocks_Final$CK_DMD_MoM <- (Goldilocks_TCG$CK_Demand - Goldilocks_TCG$Month_CK_Demand)*-1
Goldilocks_Final$TCG_DMD <- Goldilocks_TCG$TCG_Demand
Goldilocks_TCG$TCG_Demand <- as.numeric(Goldilocks_TCG$TCG_Demand)
Goldilocks_TCG$Month_TCG_Market <- as.numeric(Goldilocks_TCG$Month_TCG_Demand)
Goldilocks_Final$TCG_DMD_MoM <- (Goldilocks_TCG$TCG_Demand - Goldilocks_TCG$Month_TCG_Demand)*-1
Goldilocks_Final$Vendors <- Goldilocks_TCG$Vendors
Goldilocks_Final$Vendor_MoM <- Goldilocks_TCG$Vendors - Goldilocks_TCG$Month_Vendors


#Google Components####
library(devtools)
#devtools::install_github("tidyverse/googlesheets4",force = TRUE)
#devtools::install_github("tidyverse/googledrive",force = TRUE)
library(googlesheets4)
library(googledrive)
library(gargle)
library(httr)
options(httr_oob_default=TRUE) 
options(gargle_oauth_email = "pachun95@gmail.com")

drive_auth(email = "pachun95@gmail.com",use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com",use_oob=TRUE)

# my_dfs <- list(Final_Export_1, Funny_Money_Analysis )
# sheets_deauth()
# sheets_auth()
# sheets_create(
#   paste(currentDate,"_Market_Review",sep=""),
#   sheets = my_dfs
# )
ss <- drive_get("Market_Review")
#sheets_deauth()
#gs4_auth(email = "pachun95@gmail.com")
sheet_write(
  Final_Export_1,
  ss = ss,
  sheet = "TCG_Market_Angle"
)

sheet_write(
  Funny_Money_Analysis,
  ss = ss,
  sheet = "Funny_Money_Conversion"
)


CB_Short <- CB_Short[which(CB_Short$Name != ""),]
# Consistent_Vendors<- Consistent_Vendors[which(as.numeric(as.character(Consistent_Vendors$Yesterday_Sellers_Chg)) < .15),]
# Consistent_Vendors<- Consistent_Vendors[which(as.numeric(as.character(Consistent_Vendors$Yesterday_Sellers_Chg)) > (-.15)),]
CB_Short <- CB_Short[order(-CB_Short$Yesterday_BL_Chg),]
Consistent_Sellers <- Consistent_Sellers[order(-Consistent_Sellers$Yesterday_TCG_Chg),]
Consistent_Vendors <- Consistent_Vendors[order(-Consistent_Vendors$Yesterday_Sellers_Chg),]

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


# Refined <- list(Relaxed_CB_CK_Final,Goldilocks_CB_CK_Final, Stringent_CB_CK_Final)
# sheets_create(
#   paste(currentDate,"_Wolfs_Warrens",sep=""),
#   sheets = Refined
# )

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
#sheets_deauth()
sheet_write(
  Final_Export_1,
  ss = ss,
  sheet = "BL"
)

#Metric Aggregation####
currentDate <- Sys.Date()
YesterdayDate <- Sys.Date()-1
Number = 1
Yesterdays_Buylist_Tracker <- paste("/home/cujo253/Metrics/TBD Updated Roster/",Number,"_BuyList_History.csv", sep ="")
Yesterdays_Vendor_Tracker <- paste("/home/cujo253/Metrics/TBD Updated Roster/",Number,"_Vendor_History.csv", sep ="")
Yesterdays_TCG_Ranks <- paste("/home/cujo253/Metrics/TBD Updated Roster/",Number,"_TCG_History.csv", sep ="")
Yesterdays_CK_Ranks <- paste("/home/cujo253/Metrics/TBD Updated Roster/",Number,"_CK_History.csv", sep ="")

#Buylist_Tracker <- read_csv(Yesterdays_Buylist_Tracker, col_types = cols(.default = "c"))

setwd("/home/cujo253/Reports/High Confidence Reps")

temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp)-1
currentDate <- Sys.Date()

Updated_Tracking_Keys <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(`hasFoil` = col_character()))
Updated_Tracking_Keys <- Updated_Tracking_Keys[c(8:12)]
colnames(Updated_Tracking_Keys) <- c("Key","name","Set","Rarity","Foil")
Updated_Tracking_Keys$Key <- trimws(Updated_Tracking_Keys$Key)
#View(Updated_Tracking_Keys)
New_Roster <- Updated_Tracking_Keys

for (i in 1:Number_Of_Files){
  tmp  <- read_csv(temp[i], col_types = cols(.default =  "c"))
  tmp$Sellers <- gsub("[^0-9.-]", NA,as.character(tmp$Sellers))
  Updated_Tracking_Keys$New <- tmp$BL[match(Updated_Tracking_Keys$Key,tmp$Key)]
  Updated_Tracking_Keys$New <- as.numeric(as.character(Updated_Tracking_Keys$New))
  New_Roster <- cbind(New_Roster,Updated_Tracking_Keys$New) 
}

C20_Updated_Roster <- New_Roster

colnames(New_Roster) <- c("Key","Name","Set","Rarity","Foil",format(seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day'),format = "%Y-%m-%d") )
New_Roster$Foil <- as.character(New_Roster$Foil)

New_Roster$Foil[is.na(New_Roster$Foil)==T] <- ""
Buylist_Tracker <- New_Roster


#Vendor_Tracker <- read_csv(Yesterdays_Vendor_Tracker,col_types = cols(.default = "c"))
#Vendor_Tracker[6:ncol(Vendor_Tracker)] <- lapply(Vendor_Tracker[6:ncol(Vendor_Tracker)], factor)

setwd("/home/cujo253/Reports/High Confidence Reps")

temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp)-1
currentDate <- Sys.Date()

#View(Updated_Tracking_Keys)
New_Roster <- Updated_Tracking_Keys

for (i in 1:Number_Of_Files){
  tmp  <- read_csv(temp[i], col_types = cols(.default =  "c"))
  tmp$Sellers <- gsub("[^0-9.-]", NA,as.character(tmp$Sellers))
  Updated_Tracking_Keys$New <- tmp$Sellers[match(Updated_Tracking_Keys$Key,tmp$Key)]
  Updated_Tracking_Keys$New <- as.numeric(as.character(Updated_Tracking_Keys$New))
  New_Roster <- cbind(New_Roster,Updated_Tracking_Keys$New) 
}

C20_Updated_Roster <- New_Roster

colnames(New_Roster) <- c("Key","Name","Set","Rarity","Foil",format(seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day'),format = "%Y-%m-%d") )
New_Roster$Foil <- as.character(New_Roster$Foil)

New_Roster$Foil[is.na(New_Roster$Foil)==T] <- ""
Vendor_Tracker <- New_Roster


#TCG_Ranks <- read_csv(Yesterdays_TCG_Ranks,col_types = cols(.default = "c"))
setwd("/home/cujo253/Reports/High Confidence Reps")

temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp)-1
currentDate <- Sys.Date()

#View(Updated_Tracking_Keys)
New_Roster <- Updated_Tracking_Keys

for (i in 1:Number_Of_Files){
  tmp  <- read_csv(temp[i], col_types = cols(.default =  "c"))
  tmp$Sellers <- gsub("[^0-9.-]", NA,as.character(tmp$Sellers))
  Updated_Tracking_Keys$New <- tmp$TCG_Rank[match(Updated_Tracking_Keys$Key,tmp$Key)]
  Updated_Tracking_Keys$New <- as.numeric(as.character(Updated_Tracking_Keys$New))
  New_Roster <- cbind(New_Roster,Updated_Tracking_Keys$New) 
}

C20_Updated_Roster <- New_Roster

colnames(New_Roster) <- c("Key","Name","Set","Rarity","Foil",format(seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day'),format = "%Y-%m-%d") )
New_Roster$Foil <- as.character(New_Roster$Foil)

New_Roster$Foil[is.na(New_Roster$Foil)==T] <- ""
TCG_Ranks <- New_Roster

#CK_Ranks <- read_csv(Yesterdays_CK_Ranks,col_types = cols(.default = "c"))
setwd("/home/cujo253/Reports/High Confidence Reps")

temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp)-1
currentDate <- Sys.Date()

#View(Updated_Tracking_Keys)
New_Roster <- Updated_Tracking_Keys

for (i in 1:Number_Of_Files){
  tmp  <- read_csv(temp[i], col_types = cols(.default =  "c"))
  tmp$Sellers <- gsub("[^0-9.-]", NA,as.character(tmp$Sellers))
  Updated_Tracking_Keys$New <- tmp$CK_ADJ_Rank[match(Updated_Tracking_Keys$Key,tmp$Key)]
  Updated_Tracking_Keys$New <- as.numeric(as.character(Updated_Tracking_Keys$New))
  New_Roster <- cbind(New_Roster,Updated_Tracking_Keys$New) 
}

C20_Updated_Roster <- New_Roster

colnames(New_Roster) <- c("Key","Name","Set","Rarity","Foil",format(seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day'),format = "%Y-%m-%d") )
New_Roster$Foil <- as.character(New_Roster$Foil)

New_Roster$Foil[is.na(New_Roster$Foil)==T] <- ""
CK_Ranks <- New_Roster

Number = 3
Number = Number - 2
setwd("/home/cujo253/Metrics/TBD Updated Roster")
csvFileName <- paste(Number,"_BuyList_History",".csv",sep="")
write.csv(Buylist_Tracker, file=csvFileName, row.names = FALSE) 

csvFileName <- paste(Number,"_Vendor_History",".csv",sep="")
write.csv(Vendor_Tracker, file=csvFileName, row.names = FALSE) 

csvFileName <- paste(Number,"_TCG_History",".csv",sep="")
write.csv(TCG_Ranks, file=csvFileName, row.names = FALSE) 

csvFileName <- paste(Number,"_CK_History",".csv",sep="")
write.csv(CK_Ranks, file=csvFileName, row.names = FALSE)


TodaysPremium <- paste("/home/cujo253/Reports/High Confidence Reps/",currentDate,"_Premium.csv", sep="")

Data <- as.data.frame(read_csv(TodaysPremium,col_types = cols(.default = "c")))

# bq_auth(email = "wolfoftinstreet@gmail.com", use_oob = TRUE)
# Title_Date <- gsub("\\-","\\_",currentDate)
# tmp <- Data[1:12]
# vcolnames(tmp)[5] <- "Foil_Status"
# mybq <- bq_table(project = "gaeas-cradle", dataset = "premiums", table = paste(Title_Date,"_premium",sep=""))
# bq_table_upload(x=mybq, values = tmp, fields=as_bq_fields(tmp),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")



Buylist_Tracker$V1 <- Data$BL[match(Buylist_Tracker$Key,Data$Key)]
Vendor_Tracker$V1 <- Data$Sellers[match(Buylist_Tracker$Key,Data$Key)]
TCG_Ranks$V1 <- Data$TCG_Rank[match(Buylist_Tracker$Key,Data$Key)]
CK_Ranks$V1 <- Data$CK_ADJ_Rank[match(Buylist_Tracker$Key,Data$Key)]

formatted_date <- format(currentDate, format = "%Y-%m-%d")

names(Buylist_Tracker)[ncol(Buylist_Tracker)] <- formatted_date
names(Vendor_Tracker)[ncol(Vendor_Tracker)] <- formatted_date
names(TCG_Ranks)[ncol(TCG_Ranks)] <- formatted_date
names(CK_Ranks)[ncol(CK_Ranks)] <- formatted_date


Buylist_Tracker[is.na(Buylist_Tracker)] <- ""
Vendor_Tracker[is.na(Vendor_Tracker)] <- ""
TCG_Ranks[is.na(TCG_Ranks)] <- ""
CK_Ranks[is.na(CK_Ranks)] <- ""


Buylist_Tracker$Foil <- ifelse(Buylist_Tracker$Foil == "FOIL", Buylist_Tracker$Foil, "")
Vendor_Tracker$Foil <- ifelse(Vendor_Tracker$Foil == "FOIL", Vendor_Tracker$Foil, "")
TCG_Ranks$Foil <- ifelse(TCG_Ranks$Foil == "FOIL", TCG_Ranks$Foil, "")
CK_Ranks$Foil <- ifelse(CK_Ranks$Foil == "FOIL", CK_Ranks$Foil, "")

Number = Number + 1
setwd("/home/cujo253/Metrics/TBD Updated Roster")
csvFileName <- paste(Number,"_BuyList_History",".csv",sep="")
write.csv(Buylist_Tracker, file=csvFileName, row.names = FALSE) 

csvFileName <- paste(Number,"_Vendor_History",".csv",sep="")
write.csv(Vendor_Tracker, file=csvFileName, row.names = FALSE) 

csvFileName <- paste(Number,"_TCG_History",".csv",sep="")
write.csv(TCG_Ranks, file=csvFileName, row.names = FALSE) 

csvFileName <- paste(Number,"_CK_History",".csv",sep="")
write.csv(CK_Ranks, file=csvFileName, row.names = FALSE)


MBT <- Buylist_Tracker[,-c(1:6)]
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
ncol(Binary_Form)
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

BL_Final <- data.frame(Buylist_Tracker[,1:5],bl_metrics$SPS_21.SPS_21,bl_metrics$SPS_15.SPS_15,bl_metrics$SPS_7.SPS_7,why)
colnames(BL_Final) <- c("Key","Name","Set","Rarity","N/NF","21_Day_Rank","15_Day_Rank","7_Day_Rank","Rank_Sums")
BL_Final <- BL_Final[order(-BL_Final$Rank_Sums),]
BL_Final_M <- BL_Final[which(BL_Final$Rarity == "M"),]
BL_Final_R <- BL_Final[which(BL_Final$Rarity == "R"),]
BL_Final_U <- BL_Final[which(BL_Final$Rarity == "U"),]
BL_Final_C <- BL_Final[which(BL_Final$Rarity == "C"),]


MBT <-Vendor_Tracker[,-c(1:6)]
ncol(MBT)
MBT <- sapply(MBT,as.numeric)
New <- Buylist_Tracker$Key
for (i in 1:ncol(MBT)-1){
  New_Element <- MBT[,(i+1)] - MBT[,i]
  New_Element <- as.data.frame(New_Element)
  New <- cbind(New, New_Element)
}
Sys.sleep(sample(1:3, 1))
New <- as.data.frame(New)
New[is.na(New)] <- 0
ncol(New)
Binary_Form <- ifelse(New[,c(2:ncol(New))]>0,-1,ifelse(New[,c(2:ncol(New))]<0,1,0))
Binary_Form <- as.data.frame(Binary_Form)
Binary_Form <- sapply(Binary_Form, as.numeric)
Binary_Form <- as.data.frame(Binary_Form)
Binary_Form$Key <- Buylist_Tracker$Key
#csvFileName <- paste(currentDate,"Binary_Form",".csv",sep="")
#write.csv(Binary_Form, file=csvFileName, row.names = FALSE)
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

ven_metrics <- data.frame(SPS_21$SPS_21,SPS_15$SPS_15,SPS_7$SPS_7)
ven_metrics$Key <- Vendor_Tracker$Key
ven_metrics <- ven_metrics[moveme(names(ven_metrics), "Key first")]
tven_metrics <- t(ven_metrics)
tven_metrics <- as.data.frame(tven_metrics)
tven_metrics <- as.data.frame(tven_metrics)
tven_numbers <- (tven_metrics[-1,])
tven_numbers <- lapply(tven_numbers,as.character)
tven_numbers <- lapply(tven_numbers,as.numeric)
tven_numbers <- data.frame(tven_numbers)

why <- colSums(tven_numbers)

VEN_Final <- data.frame(Vendor_Tracker[,1:5],ven_metrics$SPS_21.SPS_21,ven_metrics$SPS_15.SPS_15,ven_metrics$SPS_7.SPS_7,why)
colnames(VEN_Final) <- c("Key","Name","Set","Rarity","N/NF","21_Day_Rank","15_Day_Rank","7_Day_Rank","Rank_Sums")
VEN_Final <- VEN_Final[order(-VEN_Final$Rank_Sums),]
VEN_Final_M <- VEN_Final[which(VEN_Final$Rarity == "M"),]
VEN_Final_R <- VEN_Final[which(VEN_Final$Rarity == "R"),]
VEN_Final_U <- VEN_Final[which(VEN_Final$Rarity == "U"),]
VEN_Final_C <- VEN_Final[which(VEN_Final$Rarity == "C"),]

MBT <- TCG_Ranks[,-c(1:6)]
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
Binary_Form <- ifelse(New[,c(2:ncol(New))]>0,-1,ifelse(New[,c(2:ncol(New))]<0,1,0))
Binary_Form <- as.data.frame(Binary_Form)
Binary_Form <- sapply(Binary_Form, as.numeric)
ncol(Binary_Form)
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

tcg_metrics <- data.frame(SPS_21$SPS_21,SPS_15$SPS_15,SPS_7$SPS_7)
tcg_metrics$Key <- TCG_Ranks$Key
tcg_metrics <- bl_metrics[moveme(names(bl_metrics), "Key first")]
ttcg_metrics <- t(bl_metrics)
ttcg_metrics <- as.data.frame(ttcg_metrics)
ttcg_metrics <- as.data.frame(ttcg_metrics)
ttcg_numbers <- (ttcg_metrics[-1,])
ttcg_numbers <- lapply(ttcg_numbers,as.character)
ttcg_numbers <- lapply(ttcg_numbers,as.numeric)
ttcg_numbers <- data.frame(ttcg_numbers)
why <- colSums(ttcg_numbers)

TCG_Final <- data.frame(TCG_Ranks[,1:5],tcg_metrics$SPS_21.SPS_21,tcg_metrics$SPS_15.SPS_15,tcg_metrics$SPS_7.SPS_7,why)
colnames(TCG_Final) <- c("Key","Name","Set","Rarity","N/NF","21_Day_Rank","15_Day_Rank","7_Day_Rank","Rank_Sums")
TCG_Final <- TCG_Final[order(-TCG_Final$Rank_Sums),]
TCG_Final_M <- TCG_Final[which(TCG_Final$Rarity == "M"),]
TCG_Final_R <- TCG_Final[which(TCG_Final$Rarity == "R"),]
TCG_Final_U <- TCG_Final[which(TCG_Final$Rarity == "U"),]
TCG_Final_C <- TCG_Final[which(TCG_Final$Rarity == "C"),]

MBT <-  CK_Ranks[,-c(1:6)]
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
Binary_Form <- ifelse(New[,c(2:ncol(New))]>0,-1,ifelse(New[,c(2:ncol(New))]<0,1,0))
#Binary_Form <- ifelse(New[,c(2:51)]>0,1,ifelse(New[,c(2:51)]<0,-1,0))
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

ck_metrics <- data.frame(SPS_21$SPS_21,SPS_15$SPS_15,SPS_7$SPS_7)
ck_metrics$Key <- CK_Ranks$Key
ck_metrics <- ck_metrics[moveme(names(ck_metrics), "Key first")]
tck_metrics <- t(ck_metrics)
tck_metrics <- as.data.frame(tck_metrics)
tck_metrics <- as.data.frame(tck_metrics)
tck_numbers <- (tck_metrics[-1,])
tck_numbers <- lapply(tck_numbers,as.character)
tck_numbers <- lapply(tck_numbers,as.numeric)
tck_numbers <- data.frame(tck_numbers)
why <- colSums(tck_numbers)

CK_Final <- data.frame(CK_Ranks[,1:5],ck_metrics$SPS_21.SPS_21,ck_metrics$SPS_15.SPS_15,ck_metrics$SPS_7.SPS_7,why)
colnames(CK_Final) <- c("Key","Name","Set","Rarity","N/NF","21_Day_Rank","15_Day_Rank","7_Day_Rank","Rank_Sums")
CK_Final <- CK_Final[order(-CK_Final$Rank_Sums),]
CK_Final_M <- CK_Final[which(CK_Final$Rarity == "M"),]
CK_Final_R <- CK_Final[which(CK_Final$Rarity == "R"),]
CK_Final_U <- CK_Final[which(CK_Final$Rarity == "U"),]
CK_Final_C <- CK_Final[which(CK_Final$Rarity == "C"),]


BL_Final$Rank_Groups <- as.numeric(as.factor(BL_Final$Rank_Sums))
VEN_Final$Rank_Groups <- as.numeric(as.factor(VEN_Final$Rank_Sums))
TCG_Final$Rank_Groups <- as.numeric(as.factor(TCG_Final$Rank_Sums))
CK_Final$Rank_Groups <- as.numeric(as.factor(CK_Final$Rank_Sums))

BL_Upper_Esch <- BL_Final[which(BL_Final$Rank_Groups >= (max(BL_Final$Rank_Groups)-9)),]
VEN_Upper_Esch <- VEN_Final[which(VEN_Final$Rank_Groups >= (max(VEN_Final$Rank_Groups)-9)),]
TCG_Upper_Esch <- TCG_Final[which(TCG_Final$Rank_Groups >= (max(TCG_Final$Rank_Groups)-9)),]
CK_Upper_Esch <- CK_Final[which(CK_Final$Rank_Groups >= (max(CK_Final$Rank_Groups)-9)),]

Combined_Upper_Esch <- rbind(BL_Upper_Esch[,1:5], VEN_Upper_Esch[,1:5], TCG_Upper_Esch[,1:5], CK_Upper_Esch[,1:5])
Combined_Upper_Esch[,5][is.na(Combined_Upper_Esch[,5])] <- ""
Unique_Combined_Upper_Esch <- unique(Combined_Upper_Esch)
#View(Unique_Combined_Upper_Esch)
nrow(Combined_Upper_Esch)
nrow(Unique_Combined_Upper_Esch)

CUE <- NULL
BLUE <- NULL
VENUE <- NULL
TCGUE <- NULL
CKUE <- NULL
library(dplyr)
CUE <- Combined_Upper_Esch %>% group_by(Key) %>% add_tally()
BLUE <- BL_Upper_Esch %>% group_by(Key) %>% add_tally()
VENUE <- VEN_Upper_Esch %>% group_by(Key) %>% add_tally()
TCGUE <- TCG_Upper_Esch %>% group_by(Key) %>% add_tally()
CKUE <- CK_Upper_Esch %>% group_by(Key) %>% add_tally()

Unique_Combined_Upper_Esch$Total_KPI_CT <- CUE$n[match(Unique_Combined_Upper_Esch$Key,CUE$Key)]
Unique_Combined_Upper_Esch$BL_KPI <- BLUE$n[match(Unique_Combined_Upper_Esch$Key,BLUE$Key)]
Unique_Combined_Upper_Esch$VEN_KPI <- VENUE$n[match(Unique_Combined_Upper_Esch$Key,VENUE$Key)]
Unique_Combined_Upper_Esch$TCG_KPI <- TCGUE$n[match(Unique_Combined_Upper_Esch$Key,TCGUE$Key)]
Unique_Combined_Upper_Esch$CK_KPI <- CKUE$n[match(Unique_Combined_Upper_Esch$Key,CKUE$Key)]

Unique_Combined_Upper_Esch$BL_Bracket <- BL_Upper_Esch$Rank_Groups[match(Unique_Combined_Upper_Esch$Key,BL_Upper_Esch$Key)]
Unique_Combined_Upper_Esch$VEN_Bracket <- VEN_Upper_Esch$Rank_Groups[match(Unique_Combined_Upper_Esch$Key,VEN_Upper_Esch$Key)]
Unique_Combined_Upper_Esch$TCG_Bracket <- TCG_Upper_Esch$Rank_Groups[match(Unique_Combined_Upper_Esch$Key,TCG_Upper_Esch$Key)]
Unique_Combined_Upper_Esch$CK_Bracket <- CK_Upper_Esch$Rank_Groups[match(Unique_Combined_Upper_Esch$Key,CK_Upper_Esch$Key)]

Unique_Combined_Upper_Esch$BL_Bracket <- ifelse(Unique_Combined_Upper_Esch$BL_Bracket == max(BL_Final$Rank_Groups),1,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-1),2,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-3),3,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-4),4,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-5),5,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-6),6,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-7),7,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-8),8,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-9),9,ifelse(Unique_Combined_Upper_Esch$BL_Bracket == (max(BL_Final$Rank_Groups)-10),10,""))))))))))
Unique_Combined_Upper_Esch$VEN_Bracket <- ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == max(VEN_Final$Rank_Groups),1,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-1),2,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-3),3,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-4),4,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-5),5,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-6),6,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-7),7,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-8),8,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-9),9,ifelse(Unique_Combined_Upper_Esch$VEN_Bracket == (max(VEN_Final$Rank_Groups)-10),10,""))))))))))
Unique_Combined_Upper_Esch$TCG_Bracket <- ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == max(TCG_Final$Rank_Groups),1,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-1),2,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-3),3,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-4),4,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-5),5,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-6),6,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-7),7,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-8),8,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-9),9,ifelse(Unique_Combined_Upper_Esch$TCG_Bracket == (max(TCG_Final$Rank_Groups)-10),10,""))))))))))
Unique_Combined_Upper_Esch$CK_Bracket <- ifelse(Unique_Combined_Upper_Esch$CK_Bracket == max(CK_Final$Rank_Groups),1,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-1),2,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-3),3,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-4),4,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-5),5,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-6),6,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-7),7,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-8),8,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-9),9,ifelse(Unique_Combined_Upper_Esch$CK_Bracket == (max(CK_Final$Rank_Groups)-10),10,""))))))))))

Buylist_Tiers <- max(BL_Final$Rank_Groups)
Vendor_Tiers <- max(VEN_Final$Rank_Groups)
TCG_Demand_Tiers <- max(TCG_Final$Rank_Groups)
CK_Demand_Tiers <- max(CK_Final$Rank_Groups)


Unique_Combined_Upper_Esch[,c(11:14)][is.na(Unique_Combined_Upper_Esch[c(11:14)])] <- 10
ncol(Unique_Combined_Upper_Esch)
Unique_Combined_Upper_Esch[is.na(Unique_Combined_Upper_Esch)] <- ""
library(magrittr)
Unique_Combined_Upper_Esch$WMS <-   Unique_Combined_Upper_Esch[,c(11:14)] %>% 
  rowwise() %>% # compute for each row
  do(data.frame(
    WMS=weighted.mean(
      x=c(as.numeric(.$BL_Bracket),as.numeric(.$VEN_Bracket),as.numeric(.$TCG_Bracket),as.numeric(.$CK_Bracket)),
      w=c(.35,.57,.05,.03)
    )
  )
  ) %>% 
  ungroup() %>% # undo row groups
  use_series("WMS")

Unique_Combined_Upper_Esch <- Unique_Combined_Upper_Esch[order(Unique_Combined_Upper_Esch$WMS),]
Unique_Combined_Upper_Esch$Ranking <- seq.int(nrow(Unique_Combined_Upper_Esch))
OVR_KPI_DF <- data.frame(Unique_Combined_Upper_Esch[,1:5],Unique_Combined_Upper_Esch[,16])
colnames(OVR_KPI_DF) <- c("Key","Name","Set","Rarity","F/NF","Ranking")
OVR_KPI_DF$Retail <- Data$MKT[match(OVR_KPI_DF$Key,Data$Key)]
OVR_KPI_DF$Buylist <- Data$BL[match(OVR_KPI_DF$Key,Data$Key)]
OVR_KPI_DF$Vendors <- Data$Sellers[match(OVR_KPI_DF$Key,Data$Key)]
OVR_KPI_DF[is.na(OVR_KPI_DF)] <- ""
M_KPI <- OVR_KPI_DF[which(OVR_KPI_DF$Rarity == "M"),]
R_KPI <- OVR_KPI_DF[which(OVR_KPI_DF$Rarity == "R"),]
U_KPI <- OVR_KPI_DF[which(OVR_KPI_DF$Rarity == "U"),]
C_KPI <- OVR_KPI_DF[which(OVR_KPI_DF$Rarity == "C"),]
M_KPI$Ranking <- seq.int(nrow(M_KPI))
R_KPI$Ranking <- seq.int(nrow(R_KPI))
U_KPI$Ranking <- seq.int(nrow(U_KPI))
C_KPI$Ranking <- seq.int(nrow(C_KPI))
nrow(M_KPI)
nrow(R_KPI)
nrow(U_KPI)
nrow(C_KPI)

#View(OVR_KPI_DF)
setwd("/home/cujo253/Reports/KPI/Master")
csvFileName <- paste(currentDate,"_Master_KPI",".csv",sep="")
write.csv(OVR_KPI_DF, file=csvFileName, row.names = FALSE)
setwd("/home/cujo253/Reports/KPI/Mythic")
csvFileName <- paste(currentDate,"_Mythic_KPI",".csv",sep="")
write.csv(M_KPI, file=csvFileName, row.names = FALSE)
setwd("/home/cujo253/Reports/KPI/Rare")
csvFileName <- paste(currentDate,"_Rare_KPI",".csv",sep="")
write.csv(M_KPI, file=csvFileName, row.names = FALSE)
setwd("/home/cujo253/Reports/KPI/Uncommon")
csvFileName <- paste(currentDate,"_Uncommon_KPI",".csv",sep="")
write.csv(M_KPI, file=csvFileName, row.names = FALSE)
setwd("/home/cujo253/Reports/KPI/Common")
csvFileName <- paste(currentDate,"_Common_KPI",".csv",sep="")
write.csv(C_KPI, file=csvFileName, row.names = FALSE)
setwd("/home/cujo253/")


library(devtools)
#install.packages("googlesheets4")
library(googlesheets4)
library(googledrive)
library(gargle)
library(httr)
options(httr_oob_default=TRUE) 
options(gargle_oauth_email = "pachun95@gmail.com")
# gs4_auth(token = drive_token())

drive_auth(email = "pachun95@gmail.com", use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com", use_oob=TRUE)


# MKPI <- list(OVR_KPI_DF,M_KPI,R_KPI,U_KPI,C_KPI)
# gs4_auth()
# sheets_create(
#   paste(currentDate,"_Master_KPI_Review",sep=""),
#   sheets = MKPI
# )
ss <- drive_get("Master_KPI_Review")
#sheets_deauth()
gs4_auth(email = "pachun95@gmail.com")
sheet_write(
  OVR_KPI_DF,
  ss = ss,
  sheet = "Master"
)
sheet_write(
  M_KPI,
  ss = ss,
  sheet = "Mythics"
)
sheet_write(
  R_KPI,
  ss = ss,
  sheet = "Rares"
)
sheet_write(
  U_KPI,
  ss = ss,
  sheet = "Uncommons"
)
sheet_write(
  C_KPI,
  ss = ss,
  sheet = "Commons"
)


#CK vs TCG Dollar Differences####
Updated_Tracking_Keys <-  read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(`hasFoil` = col_character()))
Updated_Tracking_Keys <- Updated_Tracking_Keys[c(8:12)]
colnames(Updated_Tracking_Keys) <- c("Key","name","Set","Rarity","F/NF")


Mystery_Booster_Reprint <- Updated_Tracking_Keys[which(Updated_Tracking_Keys$Set == "Mystery Booster"),]

setwd("/home/cujo253/Funny Money/")
currentDate <- Sys.Date()
temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp)
Month_Ago <- Number_Of_Files - 60
CK_Market__Tracker <- Updated_Tracking_Keys
New_Info <- NULL
for (i in Month_Ago:Number_Of_Files){
  Desired_Date <- currentDate - (Number_Of_Files - i)
  tmp  <- read_csv(temp[i], col_types = cols(.default = "c"))
  #tmp$Key <- as.factor(tmp$Key)
  New_Info <- as.data.frame(tmp$CK_MKT[match(CK_Market__Tracker$Key,tmp$Key)])
  New_Info <- as.data.frame(New_Info)
  colnames(New_Info) <- c(as.Date(Desired_Date))
  CK_Market__Tracker <- cbind(CK_Market__Tracker, New_Info[1])
}

#CK_Market__Tracker[6:ncol(CK_Market__Tracker)] <- sapply(CK_Market__Tracker[6:ncol(CK_Market__Tracker)], as.character)
CK_Market__Tracker[6:ncol(CK_Market__Tracker)] <- sapply(CK_Market__Tracker[6:ncol(CK_Market__Tracker)], as.numeric)
#CK_Market__Tracker <- na.omit(CK_Market__Tracker)
#CK_Market__Tracker <- as.data.frame(CK_Market__Tracker)
#View(CK_Market__Tracker)
temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp)
TCG_Market_Tracker <- Updated_Tracking_Keys
New_Info <- NULL
for (i in Month_Ago:Number_Of_Files){
  Desired_Date <- currentDate - (Number_Of_Files - i)
  tmp  <- read_csv(temp[i], col_types = cols(.default = "c"))
  #tmp$Key <- as.factor(tmp$Key)
  New_Info <- as.data.frame(as.character(tmp$TCG_MKT)[match(TCG_Market_Tracker$Key,tmp$Key)])
  New_Info <- as.data.frame(New_Info)
  summary(New_Info)
  colnames(New_Info) <- c(as.Date(Desired_Date))
  TCG_Market_Tracker <- cbind(TCG_Market_Tracker, New_Info[1])
}
#View(TCG_Market_Tracker)
TCG_Market_Tracker[6:ncol(TCG_Market_Tracker)] <- sapply(TCG_Market_Tracker[6:ncol(TCG_Market_Tracker)], as.character)
TCG_Market_Tracker[6:ncol(TCG_Market_Tracker)] <- sapply(TCG_Market_Tracker[6:ncol(TCG_Market_Tracker)], as.numeric)
range = ncol(TCG_Market_Tracker)
CK_Market__Tracker[is.na(CK_Market__Tracker)]<- 0
TCG_Market_Tracker[is.na(TCG_Market_Tracker)]<- 0


CK_Retail_Comparison <- TCG_Market_Tracker[1:5]
for (i in 6:range){
  CK_Retail_Comparison[i] <- round(CK_Market__Tracker[i] -  TCG_Market_Tracker[i],2)
}

MBT <- CK_Retail_Comparison[,-c(1:5)]
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
sumup <- rowSums(Three_Weeks_Sum)
Up_Down <- as.data.frame(sumup)
summary(Up_Down)

Fifteen_Days_Sum <- New[,c((ncol(New)-21):ncol(New))]
sumup <- rowSums(Fifteen_Days_Sum)
Up_Down_Three_Weeks <- as.data.frame(sumup)

Seven_Day_Sum <- New[,c((ncol(New)-7):ncol(New))]
sumup <- rowSums(Seven_Day_Sum)
Up_Down_Seven_Days <- as.data.frame(sumup)

CK_Retail_Comparison$All_Time <- Up_Down$sumup
CK_Retail_Comparison$Three_Weeks <- Up_Down_Three_Weeks$sumup
CK_Retail_Comparison$One_Week <- Up_Down_Seven_Days$sumup

CK_Retail_Comparison_AT <- CK_Retail_Comparison[order(CK_Retail_Comparison$All_Time),]
CK_Retail_Comparison_AT <- CK_Retail_Comparison_AT[c(1:200),]

CK_Retail_Comparison_Three_Weeks <- CK_Retail_Comparison[order(CK_Retail_Comparison$Three_Weeks),]
CK_Retail_Comparison_Three_Weeks <- CK_Retail_Comparison_Three_Weeks[c(1:200),]

CK_Retail_Comparison_One_Weeks <- CK_Retail_Comparison[order(CK_Retail_Comparison$One_Week),]
CK_Retail_Comparison_One_Weeks <- CK_Retail_Comparison_One_Weeks[c(1:200),]



AT_CK <- data.frame(CK_Retail_Comparison_AT[1:5],CK_Retail_Comparison_AT[ncol(CK_Retail_Comparison_AT)-2])
Three_Week_CK <- data.frame(CK_Retail_Comparison_Three_Weeks[1:5],CK_Retail_Comparison_Three_Weeks[ncol(CK_Retail_Comparison_Three_Weeks)-1])
One_Week_CK <- data.frame(CK_Retail_Comparison_One_Weeks[1:5],CK_Retail_Comparison_One_Weeks[ncol(CK_Retail_Comparison_One_Weeks)])

AT_CK$Rank <- seq(nrow(AT_CK))
Three_Week_CK$Rank <-seq(nrow(Three_Week_CK))
One_Week_CK$Rank <- seq(nrow(One_Week_CK))

AT_CK$CK_Retail <- CK_Market__Tracker$`2020-03-22`[match(AT_CK$Key,CK_Market__Tracker$Key)]
AT_CK$TCG_Retail <- TCG_Market_Tracker$`2020-03-22`[match(AT_CK$Key, TCG_Market_Tracker$Key)]

Three_Week_CK$CK_Retail<- CK_Market__Tracker$`2020-03-22`[match(Three_Week_CK$Key,CK_Market__Tracker$Key)]
Three_Week_CK$TCG_Retail<- TCG_Market_Tracker$`2020-03-22`[match(Three_Week_CK$Key, TCG_Market_Tracker$Key)]

One_Week_CK$CK_Retail<- CK_Market__Tracker$`2020-03-22`[match(One_Week_CK$Key,CK_Market__Tracker$Key)]
One_Week_CK$TCG_Retail<- TCG_Market_Tracker$`2020-03-22`[match(One_Week_CK$Key, TCG_Market_Tracker$Key)]

Mystery_Booster_Reprint <- Updated_Tracking_Keys[which(Updated_Tracking_Keys$Set == "Mystery Booster"),]
AT_CK$`MB1` <- Mystery_Booster_Reprint$Set[match(AT_CK$name,Mystery_Booster_Reprint$name)]
Three_Week_CK$`MB1`<- Mystery_Booster_Reprint$Set[match(Three_Week_CK$name,Mystery_Booster_Reprint$name)]
One_Week_CK$`MB1`<- Mystery_Booster_Reprint$Set[match(One_Week_CK$name,Mystery_Booster_Reprint$name)]
AT_CK$`MB1`[is.na(AT_CK$`MB1`) == T] <- ""
Three_Week_CK$`MB1`[is.na(Three_Week_CK$`MB1`) == T] <- ""
One_Week_CK$`MB1`[is.na(One_Week_CK$`MB1`)==T] <- ""


options(httr_oob_default=TRUE) 
options(gargle_oauth_email = "pachun95@gmail.com")
gs4_auth(email = "pachun95@gmail.com")
drive_auth(email = "pachun95@gmail.com")
ss <- drive_get("CK_VS_TCG_Review")


sheet_write(
  AT_CK,
  ss = ss,
  sheet = "3_Months"
)
sheet_write(
  Three_Week_CK,
  ss = ss,
  sheet = "3_Weeks"
)
sheet_write(
  One_Week_CK,
  ss = ss,
  sheet = "Prior_Week"
)



#BQ Upload####
library(bigrquery)
Updated_Tracking_Keys <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv",col_types = cols(hasFoil = col_character()))
Updated_Tracking_Keys <- Updated_Tracking_Keys[c(3,5,6,8,9,10,11,12)]
colnames(Updated_Tracking_Keys) <- c("scryfall","param","abbr","Key","name","Set","Rarity","Foil")
ck_conversion <- read_csv("~/Essential_Referential_CSVS/mtgjson_ck_sets.csv")
currentDate <- Sys.Date()
todays_date <- gsub("\\-","\\_",currentDate)

bq_auth(email = "wolfoftinstreet@gmail.com", use_oob = TRUE)
con <- dbConnect(
  bigrquery::bigquery(),
  project = "gaeas-cradle",
  dataset = "premiums",
  billing = "gaeas-cradle"
)
#Q <- read_csv("/home/cujo253/Reports/High Confidence Reps/2020-07-20_Premium.csv",col_types = cols(.default = "c"))
#Final_Export_2 <- Q[c(1:12)]
Final_Export_2 <- Final_Export_1[c(1:12)]
colnames(Final_Export_2)[5] <- c("Foil_Status")
colnames(Final_Export_2)[12] <- c("CK_ADJ_Rank")
Final_Export_2$Date <- currentDate
Final_Export_2$Foil_Status <- ifelse(is.na(Final_Export_2$Foil_Status)==T,"",Final_Export_2$Foil_Status)
Final_Export_2$Set <- gsub("Ikoria: Lair of Behemoths Variants","Ikoria: Lair of Behemoths",gsub("Theros Beyond Death Variants","Theros Beyond Death",gsub("Vanguard","Vanguard Series",gsub("Deckmaster","Deckmasters",gsub("Promo Pack","M20 Promo Packs",gsub("Throne of Eldraine Variants","Throne of Eldraine",gsub("War of the Spark JPN Planeswalkers","War of the Spark",gsub("Collectors Ed.*","Intl. Collectorsâ€™ Edition",gsub("Duel Decks: Merfolk Vs. Goblins","Duel Decks: Merfolk vs. Goblins",gsub("Ravnica Allegiance: Guild Kits","RNA Guild Kit",gsub("Beatdown","Beatdown Box Set",gsub("Battle Royale","Battle Royale Box Set",gsub("Timeshifted","Time Spiral Timeshifted",gsub("Beta","Limited Edition Beta",gsub("Alpha","Limited Edition Alpha",gsub("3rd Edition","Revised Edition",gsub("Archenemy - Nicol Bolas","Archenemy",gsub("Modern Event Deck","Modern Event Deck 2014", Final_Export_2$Set))))))))))))))))))
Final_Export_2$Set <- ck_conversion$Standardized[match(Final_Export_2$Set,ck_conversion$CK)]
Final_Export_2$Key <- trimws(paste(Final_Export_2$Card,Final_Export_2$Set,Final_Export_2$Rarity," ",Final_Export_2$Foil_Status,sep=""))
Final_Export_2$BL_QTY <- as.numeric(as.character(Final_Export_2$BL_QTY))
Final_Export_2$BL <- as.numeric(as.character(Final_Export_2$BL))
Final_Export_2$MKT <- as.numeric(as.character(Final_Export_2$MKT))
Final_Export_2$Arb <- as.numeric(as.character(Final_Export_2$Arb))
Final_Export_2$TCG_Rank <- as.numeric(as.character(Final_Export_2$TCG_Rank))
Final_Export_2$CK_ADJ_Rank <- as.numeric(as.character(Final_Export_2$CK_ADJ_Rank))
Final_Export_2$Sellers <- as.numeric(as.character(Final_Export_2$Sellers))
Final_Export_2$param <- as.character(Updated_Tracking_Keys$param)[match(Final_Export_2$Key,Updated_Tracking_Keys$Key)]
Final_Export_2$scryfall <- Updated_Tracking_Keys$scryfall[match(Final_Export_2$Key,Updated_Tracking_Keys$Key)]


mybq <- bq_table(project = "gaeas-cradle", dataset = "premiums", table = paste(todays_date,"_TCG_CK_Data",sep=""))
bq_table_upload(x=mybq, values = Final_Export_2, fields=as_bq_fields(Final_Export_2),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")
print("BQ Premium Upload Successful!")
#Funny Money
Funny_Money_Analysis_1 <- Funny_Money_Analysis[c(1:14)]
colnames(Funny_Money_Analysis_1)[5] <- c("Foil_Status")
colnames(Funny_Money_Analysis_1)[12] <- c("CK_Backing")
colnames(Funny_Money_Analysis_1)[13] <- c("TCG_Backing")
Funny_Money_Analysis_1$Date <- currentDate
Funny_Money_Analysis_1$Foil_Status <- ifelse(is.na(Funny_Money_Analysis_1$Foil_Status)==T,"",Funny_Money_Analysis_1$Foil_Status)
Funny_Money_Analysis_1$Set <- gsub("Ikoria: Lair of Behemoths Variants","Ikoria: Lair of Behemoths",gsub("Theros Beyond Death Variants","Theros Beyond Death",gsub("Vanguard","Vanguard Series",gsub("Deckmaster","Deckmasters",gsub("Promo Pack","M20 Promo Packs",gsub("Throne of Eldraine Variants","Throne of Eldraine",gsub("War of the Spark JPN Planeswalkers","War of the Spark",gsub("Collectors Ed.*","Intl. Collectorsâ€™ Edition",gsub("Duel Decks: Merfolk Vs. Goblins","Duel Decks: Merfolk vs. Goblins",gsub("Ravnica Allegiance: Guild Kits","RNA Guild Kit",gsub("Beatdown","Beatdown Box Set",gsub("Battle Royale","Battle Royale Box Set",gsub("Timeshifted","Time Spiral Timeshifted",gsub("Beta","Limited Edition Beta",gsub("Alpha","Limited Edition Alpha",gsub("3rd Edition","Revised Edition",gsub("Archenemy - Nicol Bolas","Archenemy",gsub("Modern Event Deck","Modern Event Deck 2014", Funny_Money_Analysis_1$Set))))))))))))))))))
Funny_Money_Analysis_1$Set <- ck_conversion$Standardized[match(Funny_Money_Analysis_1$Set,ck_conversion$CK)]
Funny_Money_Analysis_1$Key <- trimws(paste(Funny_Money_Analysis_1$Card,Funny_Money_Analysis_1$Set,Funny_Money_Analysis_1$Rarity," ",Funny_Money_Analysis_1$Foil_Status,sep=""))
Funny_Money_Analysis_1$BL_QTY <- as.numeric(as.character(Funny_Money_Analysis_1$BL_QTY))
Funny_Money_Analysis_1$BL <- as.numeric(as.character(Funny_Money_Analysis_1$BL))
Funny_Money_Analysis_1$TCG_MKT <- as.numeric(as.character(Funny_Money_Analysis_1$TCG_MKT))
Funny_Money_Analysis_1$CK_MKT <- as.numeric(as.character(Funny_Money_Analysis_1$CK_MKT))
Funny_Money_Analysis_1$MKT_Diff <- as.numeric(as.character(Funny_Money_Analysis_1$MKT_Diff))
Funny_Money_Analysis_1$CK_Backing <- as.numeric(as.character(Funny_Money_Analysis_1$CK_Backing))
Funny_Money_Analysis_1$TCG_Backing <- as.numeric(as.character(Funny_Money_Analysis_1$TCG_Backing))
Funny_Money_Analysis_1$Sellers <- as.numeric(as.character(Funny_Money_Analysis_1$Sellers))
Funny_Money_Analysis_1$param <- as.character(Updated_Tracking_Keys$param)[match(Funny_Money_Analysis_1$Key,Updated_Tracking_Keys$Key)]
Funny_Money_Analysis_1$scryfall <- Updated_Tracking_Keys$scryfall[match(Funny_Money_Analysis_1$Key,Updated_Tracking_Keys$Key)]
# colnames(Funny_Money_Analysis_1)
# colnames(combined_file)
mybq <- bq_table(project = "gaeas-cradle", dataset = "ck_funny_money", table = paste(todays_date,"_CK_Credit",sep=""))
bq_table_upload(x=mybq, values = Funny_Money_Analysis_1, fields=as_bq_fields(Funny_Money_Analysis_1),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")
print("BQ CK_Funny_Money Upload Successful!")
