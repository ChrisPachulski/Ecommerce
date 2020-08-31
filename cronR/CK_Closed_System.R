#packages & functions####
pacman::p_load(tidyverse,rvest,jsonlite,devtools,googlesheets4,googledrive,googlesheets,readr,dplyr,gargle,httr,bigrquery)
invisible(clean_names <- function(.data, unique = FALSE) {
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
})
invisible(right <- function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
}) #Recreating the right function from Excel 
invisible(left <- function(text, num_char) {
  substr(text, 1, num_char)
})  #Recreating the left function from Excel 
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
Updated_Tracking_Keys <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(hasFoil = col_character())) %>%
  #rename(c("scryfall_id" = "scryfall","tcg_ID"="param","card" = "name", "set" = "Set", "rarity" = "Rarity","hasFoil" = "Foil")) %>%
  rename(c("scryfall" = "scryfall_id","param"="tcg_ID","name" = "card", "Set" = "set", "Rarity" = "rarity","Foil" = "hasFoil")) %>%
  mutate(Semi = paste(name, Set,sep=""))
Sets <- read.csv("/home/cujo253/Essential_Referential_CSVS/Sets.csv",stringsAsFactors = TRUE)
#Exclusion Classification####
Exclusion <- data.frame(Sets$Set_Excl,Sets$Excl_Excl, stringsAsFactors = TRUE)
colnames(Exclusion) <- c("Set_Excl", "Excl_Excl")
BL_Ratio_Ranking <- c("1",	"2",	"3",	"4",	"5",	"6",	"7",	"8",	"9",	"10",	"11",	"12",	"13",	"14",	"15",	"16",	"17",	"18",	"19",	"20",	"21",	"22",	"23",	"24",	"25",	"26",	"27",	"28",	"29",	"30",	"31",	"32",	"33",	"34",	"35",	"36",	"37",	"38",	"39",	"40",	"41",	"42",	"43",	"44",	"45",	"46",	"47",	"48",	"49",	"50",	"51",	"52",	"53",	"54",	"55",	"56",	"57",	"58",	"59",	"60",	"61",	"62",	"63",	"64",	"65",	"66",	"67")
BL_Ratio_Value <- c("0.67",	"0.66",	"0.65",	"0.64",	"0.63",	"0.62",	"0.61",	"0.6",	"0.59",	"0.58",	"0.57",	"0.56",	"0.55",	"0.54",	"0.53",	"0.52",	"0.51",	"0.5",	"0.49",	"0.48",	"0.47",	"0.46",	"0.45",	"0.44",	"0.43",	"0.42",	"0.41",	"0.4",	"0.39",	"0.38",	"0.37",	"0.36",	"0.35",	"0.34",	"0.33",	"0.32",	"0.31",	"0.3",	"0.29",	"0.28",	"0.27",	"0.26",	"0.25",	"0.24",	"0.23",	"0.22",	"0.21",	"0.2",	"0.19",	"0.18",	"0.17",	"0.16",	"0.15",	"0.14",	"0.13",	"0.12",	"0.11",	"0.1",	"0.09",	"0.08",	"0.07",	"0.06",	"0.05",	"0.04",	"0.03",	"0.02",	"0.01")
BL_Ratio <- tibble(BL_Ratio_Ranking,BL_Ratio_Value)
QTY_Ratio_Ranking <- c("1",	"2",	"3",	"4",	"5",	"6",	"7",	"8",	"9",	"10",	"11",	"12",	"13",	"14",	"15",	"16",	"17",	"18",	"19",	"20",	"21",	"22",	"23",	"24",	"25",	"26",	"27",	"28",	"29",	"30",	"31",	"32",	"33",	"34",	"35",	"36",	"37",	"38",	"39",	"40",	"41",	"42",	"43",	"44",	"45",	"46",	"47",	"48",	"49",	"50",	"51",	"52",	"53",	"54",	"55",	"56",	"57",	"58",	"59",	"60",	"61",	"62",	"63",	"64",	"65",	"66",	"67",	"68",	"69",	"70",	"71",	"72",	"73",	"74",	"75",	"76",	"77",	"78",	"79",	"80",	"81",	"82",	"83",	"84",	"85",	"86",	"87",	"88",	"89",	"90",	"91",	"92",	"93",	"94",	"95",	"96",	"97",	"98",	"99")
QTY_Ratio_Value <- c(".99",	".98",	".97",	".96",	".95",	".94",	".93",	".92",	".91",	".90",	".89",	".88",	".87",	".86",	".85",	".84",	".83",	".82",	".81",	".80",	".79",	".78",	".77",	".76",	".75",	".74",	".73",	".72",	".71",	".70",	".69",	".68",	".67",	".66",	".65",	".64",	".63",	".62",	".61",	".60",	".59",	".58",	".57",	".56",	".55",	".54",	".53",	".52",	".51",	".50",	".49",	".48",	".47",	".46",	".45",	".44",	".43",	".42",	".41",	".40",	".39",	".38",	".37",	".36",	".35",	".34",	".33",	".32",	".31",	".30",	".29",	".28",	".27",	".26",	".25",	".24",	".23",	".22",	".21",	".20",	".19",	".18",	".17",	".16",	".15",	".14",	".13",	".12",	".11",	".10",	".9",	".8",	".7",	".6",	".5",	".4",	".3",	".2",	".1")
QTY_Ratio <-tibble(QTY_Ratio_Ranking, QTY_Ratio_Value)
BL_Ratio$BL_Ratio_Ranking <- as.numeric(as.character(BL_Ratio$BL_Ratio_Ranking))
BL_Ratio$BL_Ratio_Value <- as.numeric(as.character(BL_Ratio$BL_Ratio_Value))
QTY_Ratio$QTY_Ratio_Ranking <- as.numeric(as.character(QTY_Ratio$QTY_Ratio_Ranking))
QTY_Ratio$QTY_Ratio_Value <- as.numeric(as.character(QTY_Ratio$QTY_Ratio_Value))
All_Cards <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv",col_types = cols(`F/NF` = col_character()))
Printings <- All_Cards %>% group_by(card) %>% add_tally()

currentDate <- Sys.Date()
#API Abuse####
CK_Buylist <- fromJSON("https://api.cardkingdom.com/api/pricelist")
CK_Buylist <- as.data.frame(CK_Buylist)
CK_Buylist <- CK_Buylist[which(CK_Buylist$data.variation == ""),]
#CK_Buylist <- CK_Buylist[which(CK_Buylist$data.edition == "(Other)"),]
No_Foils <- CK_Buylist[which(CK_Buylist$data.is_foil == "false"),]
Foils <- CK_Buylist[which(CK_Buylist$data.is_foil == "true"),]
Set_Breakdown <- as.data.frame(summary(as.factor(CK_Buylist$data.edition),maxsum = 5000))
NF_Set_Breakdown <- as.data.frame(summary(as.factor(No_Foils$data.edition),maxsum = 5000))
F_Set_Breakdown <- as.data.frame(summary(as.factor(Foils$data.edition),maxsum = 5000))
colnames(Set_Breakdown) <- "Set"
colnames(NF_Set_Breakdown) <- "Set"
colnames(F_Set_Breakdown) <- "Set"
Total_Offers <- sum(Set_Breakdown$Set)
NF_Total_Offers <- sum(NF_Set_Breakdown$Set)
F_Total_Offers <- sum(F_Set_Breakdown$Set)
Set_Breakdown$Set_Composition <- round(Set_Breakdown$Set/Total_Offers,4)*100
NF_Set_Breakdown$Set_Composition <- round(NF_Set_Breakdown$Set/NF_Total_Offers,4)*100
F_Set_Breakdown$Set_Composition <- round(F_Set_Breakdown$Set/F_Total_Offers,4)*100

Slim_CK_Buylist <- CK_Buylist[c(1,6,8,9,10,11,12,13)]
Slim_CK_Buylist$Exclusion <- Exclusion$Excl_Excl[match(Slim_CK_Buylist$data.edition,Exclusion$Set_Excl)]
Slim_CK_Buylist$Exclusion[is.na(Slim_CK_Buylist$Exclusion)==TRUE] <- "Unclear"

Slim_CK_Buylist <- Slim_CK_Buylist[which(Slim_CK_Buylist$Exclusion != "Exclude"),]
Slim_CK_Buylist <- Slim_CK_Buylist[,-9]
Slim_CK_Buylist$data.is_foil <- ifelse(Slim_CK_Buylist$data.is_foil == "false", ""," FOIL")

Slim_CK_Buylist$data.qty_retail <- ifelse(Slim_CK_Buylist$data.qty_retail == 0, 1,Slim_CK_Buylist$data.qty_retail)
Slim_CK_Buylist <- Slim_CK_Buylist[which(Slim_CK_Buylist$data.qty_buying != 0),]
Slim_CK_Buylist$QTY_Diff <- round((Slim_CK_Buylist$data.qty_buying-Slim_CK_Buylist$data.qty_retail)/Slim_CK_Buylist$data.qty_buying,2)
Slim_CK_Buylist$Price_Diff <- round((as.numeric(as.character(Slim_CK_Buylist$data.price_buy))/as.numeric(as.character(Slim_CK_Buylist$data.price_retail))),2)

Dollar_Slim_CK_Buylist <- Slim_CK_Buylist[which(Slim_CK_Buylist$data.price_buy > .25),]
Dollar_Slim_CK_Buylist$data.qty_retail <- abs(Dollar_Slim_CK_Buylist$data.qty_retail)
Dollar_Slim_CK_Buylist <- Dollar_Slim_CK_Buylist[order(-Dollar_Slim_CK_Buylist$QTY_Diff),]
Dollar_Slim_CK_Buylist$Tiers <- ifelse(as.numeric(as.character(Dollar_Slim_CK_Buylist$QTY_Diff)) >= .90 & as.numeric(as.character(Dollar_Slim_CK_Buylist$Price_Diff)) >= .58 & as.numeric(as.character(Dollar_Slim_CK_Buylist$data.qty_buying)) > 100, 0 , "Awaiting Tier")
Dollar_Slim_CK_Buylist$Tiers <- ifelse(as.numeric(as.character(Dollar_Slim_CK_Buylist$QTY_Diff)) >= .80 & as.numeric(as.character(Dollar_Slim_CK_Buylist$Price_Diff)) >= .53 & Dollar_Slim_CK_Buylist$Tiers != 0, 1, Dollar_Slim_CK_Buylist$Tiers)
Dollar_Slim_CK_Buylist$Tiers <- ifelse(as.numeric(as.character(Dollar_Slim_CK_Buylist$QTY_Diff)) >= .70 & as.numeric(as.character(Dollar_Slim_CK_Buylist$Price_Diff)) >= .50 & as.factor(Dollar_Slim_CK_Buylist$Tiers) == "Awaiting Tier", 2, Dollar_Slim_CK_Buylist$Tiers)
Dollar_Slim_CK_Buylist$Tiers <- ifelse(as.numeric(as.character(Dollar_Slim_CK_Buylist$QTY_Diff)) >= .60 & as.numeric(as.character(Dollar_Slim_CK_Buylist$Price_Diff)) >= .47 & as.factor(Dollar_Slim_CK_Buylist$Tiers) == "Awaiting Tier", 3, Dollar_Slim_CK_Buylist$Tiers)
Dollar_Slim_CK_Buylist$Tiers <- ifelse(as.factor(Dollar_Slim_CK_Buylist$Tiers) == "Awaiting Tier",4, Dollar_Slim_CK_Buylist$Tiers)

Dollar_Slim_CK_Buylist$Ratio_Rank <- BL_Ratio$BL_Ratio_Ranking[match(Dollar_Slim_CK_Buylist$Price_Diff,BL_Ratio$BL_Ratio_Value)]
Dollar_Slim_CK_Buylist$QTY_Rank <- QTY_Ratio$QTY_Ratio_Ranking[match(Dollar_Slim_CK_Buylist$QTY_Diff, QTY_Ratio$QTY_Ratio_Value)]
Dollar_Slim_CK_Buylist$QTY_Rank[is.na(Dollar_Slim_CK_Buylist$QTY_Rank) == TRUE] <- 5
Dollar_Slim_CK_Buylist <- Dollar_Slim_CK_Buylist[which(as.factor(Dollar_Slim_CK_Buylist$data.is_foil) != "FOIL"),]
Dollar_Slim_CK_Buylist$semi_key <- paste(Dollar_Slim_CK_Buylist$data.name,Dollar_Slim_CK_Buylist$data.edition,sep="")

CK_Sold <-NULL #Assign NULL value
CK_Prices <- NULL
total = 167 #For loading bar, match up this number for the pages to be run through
pb <- txtProgressBar(min=0, max = total, style = 3) # Format the size of the loading bar we want to see in the console
Start_Time <- Sys.time()
for(i in 1:167){
  url <- paste0("https://www.cardkingdom.com/catalog/view?filter%5Bipp%5D=60&filter%5Bsort%5D=most_popular&filter%5Bsearch%5D=mtg_advanced&filter%5Bcategory_id%5D=0&filter%5Btype_mode%5D=any&filter%5Bmanaprod_select%5D=any&page=",i)
  html <- read_html(url)
  CK_name <- html %>% html_nodes(".productDetailTitle") %>% html_text()
  CK_name <- as.data.frame(CK_name)
  CK_set <- html %>% html_nodes(".productDetailSet") %>% html_text()
  CK_set_a <- data.frame(do.call('rbind', strsplit(as.character(CK_set),'(',fixed=TRUE)))
  CK_set_b <- data.frame(do.call('rbind', strsplit(as.character(CK_set_a$X1),'\n',fixed=TRUE)))
  CK_set <- trimws(CK_set_b$X2)
  CK_set <- as.data.frame(CK_set)
  prices <- html %>% html_nodes(".stylePrice") %>% html_text()
  prices <- data.frame(do.call('rbind',strsplit(as.character(prices),'$',fixed=TRUE)))
  prices <- data.frame(do.call('rbind',strsplit(as.character(prices$X2),'\n',fixed=TRUE)))
  prices <- prices[,1]
  prices <- as.data.frame(prices)
  prices = prices[seq(1, nrow(prices), 4), ]
  prices <- as.data.frame(prices)
  CK_Sold <- data.frame(
    X2 <- CK_name,
    X3 <- CK_set,
    x4 <- prices)
  CK_Prices <- rbind(CK_Prices, CK_Sold)
  
  setTxtProgressBar(pb,i)
}
CK_Sales_Data <- CK_Prices
#If Weighted and Adj are "Inf" <- change Line 1596 from c(1) to c(2)####
CK_Sales_Data$CK_Rank <- seq(nrow(CK_Sales_Data))
CK_Sales_Data$prices <- as.numeric(as.character(CK_Sales_Data$prices))
Anchor_CK_price <- CK_Sales_Data[1,3]
#View(Ranking[,(9)])
CK_Sales_Data$CK_Rank <- round(((CK_Sales_Data$prices/Anchor_CK_price)*CK_Sales_Data$CK_Rank),5)
CK_Sales_Data <- CK_Sales_Data[order(-CK_Sales_Data$CK_Rank),]
Worst_CK_Rank <- (CK_Sales_Data[c(1),(4)])+1
CK_Sales_Data$CK_Rank <- ifelse(CK_Sales_Data$CK_Rank == 0,Worst_CK_Rank,CK_Sales_Data$CK_Rank)
CK_Sales_Data$CK_Rank[is.na(CK_Sales_Data$CK_Rank)] <- Worst_CK_Rank
CK_Sales_Data <- CK_Sales_Data[order(CK_Sales_Data$CK_Rank),]
Absolute_CK_CK_Sales_Data <- CK_Sales_Data$CK_Rank
Absolute_CK_CK_Sales_Data <- seq.int(nrow(CK_Sales_Data))
CK_Sales_Data$CK_Rank <- Absolute_CK_CK_Sales_Data
summary(CK_Sales_Data)
End_Time <- Sys.time()
End_Time - Start_Time
#Import_Name <- paste("/home/cujo253/Funny Money/",currentDate,"_CK_Credit_Data.csv",sep="")
#CK_Full_Data <- read.csv(Import_Name)
CK_Sales_Data$semi_Key <- paste(CK_Sales_Data$CK_name,CK_Sales_Data$CK_set,sep="")
Dollar_Slim_CK_Buylist$Sell_Rank <- CK_Sales_Data$CK_Rank[match(Dollar_Slim_CK_Buylist$semi_key,CK_Sales_Data$semi_Key)]

Dollar_Slim_CK_Buylist <- Dollar_Slim_CK_Buylist[,-14]
Dollar_Slim_CK_Buylist$Tiers <- as.numeric(as.character(Dollar_Slim_CK_Buylist$Tiers))
Dollar_Slim_CK_Buylist$Ratio_Rank <- as.numeric(as.character(Dollar_Slim_CK_Buylist$Ratio_Rank))
Dollar_Slim_CK_Buylist$QTY_Rank <- as.numeric(as.character(Dollar_Slim_CK_Buylist$QTY_Rank))
Dollar_Slim_CK_Buylist$Sell_Rank <- round(as.numeric(as.character(Dollar_Slim_CK_Buylist$Sell_Rank)),2)
Dollar_Slim_CK_Buylist$Sell_Rank[is.na(Dollar_Slim_CK_Buylist$Sell_Rank)] <- Worst_CK_Rank +1
New <- NULL
for (i in 1:nrow(Dollar_Slim_CK_Buylist)){
  A = 11
  B = 12
  C = 13
  D = 14
  New_Element <- round((Dollar_Slim_CK_Buylist[i,A] + Dollar_Slim_CK_Buylist[i,B] + Dollar_Slim_CK_Buylist[i,C]+ Dollar_Slim_CK_Buylist[i,D])/4,1)
  New_Element <- as.data.frame(New_Element)
  New <- rbind(New, New_Element)
}
Dollar_Slim_CK_Buylist$Velocity <- New$New_Element
Dollar_Slim_CK_Buylist <- Dollar_Slim_CK_Buylist[order(Dollar_Slim_CK_Buylist$Velocity),]
Dollar_Slim_CK_Buylist$Rank <- seq(nrow(Dollar_Slim_CK_Buylist))
Dollar_Slim_CK_Buylist <- Dollar_Slim_CK_Buylist[moveme(names(Dollar_Slim_CK_Buylist),"Rank first")]
currentTime<- as.POSIXct(Dollar_Slim_CK_Buylist$meta.created_at[1])
currentTime <- format(currentTime, tz="Etc/GMT-3",usetz=TRUE)
currentTime <- right(currentTime,12)
currentTime <- left(currentTime,8)
Dollar_Slim_CK_Buylist$meta.created_at <-currentTime
Dollar_Slim_CK_Buylist$Printings <- Printings$n[match(Dollar_Slim_CK_Buylist$data.name,Printings$card)]
Dollar_Slim_CK_Buylist$Set_Bucket <- Exclusion$Excl_Excl[match(Dollar_Slim_CK_Buylist$data.edition,Exclusion$Set_Excl)]
Dollar_Slim_CK_Buylist$Printing_Status <- ifelse(Dollar_Slim_CK_Buylist$Set_Bucket == "Standard", "In Print", "No Longer In Print")
Dollar_Slim_CK_Buylist$Velocity_Adjusted <- Dollar_Slim_CK_Buylist$Velocity * Dollar_Slim_CK_Buylist$Printings
Dollar_Slim_CK_Buylist$Velocity_Adjusted <- ifelse(Dollar_Slim_CK_Buylist$Printing_Status == "No Longer In Print", round(Dollar_Slim_CK_Buylist$Velocity_Adjusted * .75,2), Dollar_Slim_CK_Buylist$Velocity_Adjusted)
Dollar_Slim_CK_Buylist$data.price_buy <- as.numeric(Dollar_Slim_CK_Buylist$data.price_buy)
Dollar_Slim_CK_Buylist$data.price_retail <- as.numeric(Dollar_Slim_CK_Buylist$data.price_retail)


library(devtools)
#devtools::install_github("tidyverse/googlesheets4")
library(googlesheets4)
library(googledrive)
#library(googlesheets)
library(gargle)
library(httr)
options(httr_oob_default=TRUE) 
options(gargle_oauth_email = "pachun95@gmail.com")

#drive_auth(email = "pachun95@gmail.com")
#drive_auth(use_oob=TRUE)
Public_List <- Dollar_Slim_CK_Buylist[,-c(10:16)]
Public_List <- Public_List[,-ncol(Public_List)]
ck_df <- list(Dollar_Slim_CK_Buylist)
ss <- drive_get("Buylist_Review")
#sheets_deauth()
drive_auth(email = "pachun95@gmail.com", use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com", use_oob=TRUE)
sheet_write(
  Public_List,
  ss = ss,
  sheet = "Current_BuyList"
)

Dollar_Slim_CK_Buylist <- Dollar_Slim_CK_Buylist[which(Dollar_Slim_CK_Buylist$data.is_foil != " FOIL"),]
Dollar_Slim_CK_Buylist <- Dollar_Slim_CK_Buylist[which(Dollar_Slim_CK_Buylist$Price_Diff >= .50),]
Dollar_Slim_CK_Buylist$Margin <- round(Dollar_Slim_CK_Buylist$data.price_retail - Dollar_Slim_CK_Buylist$data.price_buy,2)
Dollar_Slim_CK_Buylist$Margin_Perc <- round(Dollar_Slim_CK_Buylist$Margin/Dollar_Slim_CK_Buylist$data.price_buy,2)
Wolfs_Buylist <- Dollar_Slim_CK_Buylist[which(Dollar_Slim_CK_Buylist$Printing_Status == "No Longer In Print"),]
Wolfs_Buylist <- Wolfs_Buylist[which(Wolfs_Buylist$data.is_foil == ""),]
Wolfs_Buylist <- Wolfs_Buylist[which(Wolfs_Buylist$data.price_buy <= 20),]
Wolfs_Buylist <- Wolfs_Buylist[which(Wolfs_Buylist$data.price_buy >= 1),]
Wolfs_Buylist <- Wolfs_Buylist[which(Wolfs_Buylist$Printings <= 5),]
Wolfs_Buylist <- Wolfs_Buylist[which(Wolfs_Buylist$Margin_Perc <= .85),]
Wolfs_Buylist <- Wolfs_Buylist[order(Wolfs_Buylist$Velocity_Adjusted),]
Wolfs_Targets <- data.frame(Wolfs_Buylist$data.name,Wolfs_Buylist$data.edition,Wolfs_Buylist$data.price_retail,Wolfs_Buylist$data.price_buy,Wolfs_Buylist$Printings)
Wolfs_Targets$Semi_Key <- paste(Wolfs_Targets$Wolfs_Buylist.data.name,Wolfs_Targets$Wolfs_Buylist.data.edition,sep="")

drive_auth(email = "pachun95@gmail.com", use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com", use_oob=TRUE)
#View(Wolfs_Targets)

ss <- drive_get("Wolfs_Buylist_Review")
sheet_write(
  Dollar_Slim_CK_Buylist,
  ss = ss,
  sheet = "Current_BuyList"
)

setwd("/home/cujo253/Metrics/Daily_Velocity_Trackers")
csvFileName <- paste(currentDate,"_Velocity",".csv",sep="")
write.csv(Dollar_Slim_CK_Buylist, file=csvFileName, row.names = FALSE)

Sets <- read.csv("/home/cujo253/Essential_Referential_CSVS/Sets.csv",stringsAsFactors = TRUE)

ck_conversion <- read_csv("~/Essential_Referential_CSVS/mtgjson_ck_sets.csv")

Updated_Tracking_Keys <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(hasFoil = col_character())) %>%
  #rename(c("scryfall_id" = "scryfall","tcg_ID"="param","card" = "name", "set" = "Set", "rarity" = "Rarity","hasFoil" = "Foil")) %>%
  rename(c("scryfall" = "scryfall_id","param"="tcg_ID","name" = "card", "Set" = "set", "Rarity" = "rarity","Foil" = "hasFoil")) %>%
  mutate(Semi = paste(name, Set,sep=""))


Dollar_Slim_CK_Buylist <- clean_names(Dollar_Slim_CK_Buylist)

Dollar_Slim_CK_Buylist <- Dollar_Slim_CK_Buylist%>% mutate(Semi = paste(data_name,data_edition,sep=""),
                                                           Rarity = Updated_Tracking_Keys$Rarity[match(Semi,Updated_Tracking_Keys$Semi)],
                                                           Unique_Keys = trimws(paste(data_name,data_edition,Rarity,data_is_foil,sep=""))) %>%
  mutate(Param = Updated_Tracking_Keys$param[match(Unique_Keys,Updated_Tracking_Keys$Key)]) %>% select(!c("Semi")) %>%
  mutate(printings = as.numeric(printings),
         velocity_adjusted = as.numeric(velocity_adjusted))

con <- gaeas_cradle("wolfoftinstreet@gmail.com")
mybq <- bq_table(project = "gaeas-cradle", dataset = "ck_velocity", table = paste(gsub("-","_",Sys.Date()),"_CK_VELOCITY",sep=""))
bq_table_upload(x=mybq, values = Dollar_Slim_CK_Buylist, fields=as_bq_fields(Dollar_Slim_CK_Buylist),nskip = 1, source_format = "CSV",create_disposition = "CREATE_IF_NEEDED", write_disposition = "WRITE_APPEND")


