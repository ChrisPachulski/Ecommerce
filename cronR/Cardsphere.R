#Cardsphere Mapped out####
library(XML)
library(RCurl)
library(rvest) 
library(tidyverse)
library(plyr)
library(jsonlite)
library(readr)
library(lubridate)

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


currentDate <- Sys.Date()
Sets <- read.csv("/home/cujo253/Essential_Referential_CSVS/Sets.csv",stringsAsFactors = TRUE)
Exclusion <- data.frame(Sets$Set_Excl,Sets$Excl_Excl)
colnames(Exclusion) <- c("Set_Excl","Excl_Excl")
Dollar_Fix <- function(t){
  ifelse(nchar(t) == 5, right(t,4),ifelse(nchar(t) == 6,right(t,5),ifelse(nchar(t) == 7,right(t,6),ifelse(nchar(t) == 8,right(t,7),0))))
}

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
Slim_CK_Buylist$data.is_foil <- ifelse(Slim_CK_Buylist$data.is_foil == "false", "","FOIL")

Slim_CK_Buylist$data.qty_retail <- ifelse(Slim_CK_Buylist$data.qty_retail == 0, 1,Slim_CK_Buylist$data.qty_retail)
Slim_CK_Buylist <- Slim_CK_Buylist[which(Slim_CK_Buylist$data.qty_buying != 0),]
Slim_CK_Buylist$QTY_Diff <- round((Slim_CK_Buylist$data.qty_buying-Slim_CK_Buylist$data.qty_retail)/Slim_CK_Buylist$data.qty_buying,2)
Slim_CK_Buylist$Price_Diff <- round((as.numeric(as.character(Slim_CK_Buylist$data.price_buy))/as.numeric(as.character(Slim_CK_Buylist$data.price_retail))),2)

Dollar_Slim_CK_Buylist <- Slim_CK_Buylist[which(Slim_CK_Buylist$data.price_buy > 1.50),]
Dollar_Slim_CK_Buylist <- Dollar_Slim_CK_Buylist[order(-Dollar_Slim_CK_Buylist$QTY_Diff),]
Dollar_Slim_CK_Buylist$Tiers <- ifelse(as.numeric(as.character(Dollar_Slim_CK_Buylist$QTY_Diff)) >= .90 & as.numeric(as.character(Dollar_Slim_CK_Buylist$Price_Diff)) >= .64 & as.numeric(as.character(Dollar_Slim_CK_Buylist$data.qty_buying)) > 100, 0 , "Awaiting Tier")

Dollar_Slim_CK_Buylist$Tiers <- ifelse(as.numeric(as.character(Dollar_Slim_CK_Buylist$QTY_Diff)) >= .90 & as.numeric(as.character(Dollar_Slim_CK_Buylist$Price_Diff)) >= .64 & Dollar_Slim_CK_Buylist$Tiers != 0, 1, Dollar_Slim_CK_Buylist$Tiers)
Dollar_Slim_CK_Buylist$Tiers <- ifelse(as.numeric(as.character(Dollar_Slim_CK_Buylist$QTY_Diff)) >= .80 & as.numeric(as.character(Dollar_Slim_CK_Buylist$Price_Diff)) >= .60 & as.factor(Dollar_Slim_CK_Buylist$Tiers) == "Awaiting Tier", 2, Dollar_Slim_CK_Buylist$Tiers)
Curated_CK_Buylist <- Dollar_Slim_CK_Buylist[which(as.factor(Dollar_Slim_CK_Buylist$Tiers) != "Awaiting Tier"),]
Curated_CK_Buylist$Tiers <- as.numeric(as.character(Curated_CK_Buylist$Tiers))
Curated_CK_Buylist <- Curated_CK_Buylist[order(Curated_CK_Buylist$Tiers),]

#Cardsphere####
library(XML)
library(RCurl)
TCG_URL <- "https://www.cardsphere.com/sets" 
TCG_URL_1 <- getURL(TCG_URL)
html <- read_html(TCG_URL)
html_nodes(html,".body")

Page_Contents <- htmlTreeParse(TCG_URL_1, useInternalNode=TRUE)

Xpath_contents <- xpathSApply(Page_Contents,"//li", xmlValue)
Xpath_contents <- as.data.frame(Xpath_contents)
CardSphere_Secrets <-data.frame(do.call('rbind', strsplit(as.character(Xpath_contents$Xpath_contents),'  ',fixed=TRUE))) #Delimiting off "\n" or newline escape sequences
CardSphere_Secrets <- CardSphere_Secrets$X29
CardSphere_Secrets <- data.frame(do.call('rbind', strsplit(as.character(Xpath_contents$Xpath_contents),'\n',fixed=TRUE))) #Delimiting off "\n" or newline escape sequences
CardSphere_Secrets <- CardSphere_Secrets$X3
CardSphere_Secrets <- as.data.frame(CardSphere_Secrets)
CardSphere_Secrets <- CardSphere_Secrets[-c(1:3),]
CardSphere_Secrets <- as.data.frame(CardSphere_Secrets)
CardSphere_Secrets <- CardSphere_Secrets[-c((nrow(CardSphere_Secrets)-13):nrow(CardSphere_Secrets)),]
CardSphere_Secrets <- unlist(CardSphere_Secrets)
CardSphere_Printed_Sets <- as.data.frame(CardSphere_Secrets)

Xpath_contents <- xpathSApply(Page_Contents,"//a", xmlGetAttr, 'href')
Xpath_contents <- gsub("/sets/","",Xpath_contents)
Xpath_contents <- as.data.frame(Xpath_contents)
Xpath_contents <- Xpath_contents[-c(1:4),]
Xpath_contents <- as.data.frame(Xpath_contents)
Total_Rows <- nrow(Xpath_contents)
TBR_Rows <- nrow(Xpath_contents)-15
CardSphere_Set_Numbers <- Xpath_contents[-c(TBR_Rows:Total_Rows),]
Cardsphere_Outer_Shell <- data.frame(CardSphere_Printed_Sets,CardSphere_Set_Numbers)


All_CardSphere <- NULL
total = nrow(Cardsphere_Outer_Shell) 
pb <- txtProgressBar(min=0, max = total, style = 3)
Q <- 1
for (set in Cardsphere_Outer_Shell$CardSphere_Set_Numbers) {
  Set_Url <- paste("https://www.cardsphere.com/sets/",set,sep="")
  Set_Number <- set
  html <- read_html(Set_Url)
  cs_no <- html %>% html_nodes(".cs-row") %>% html_text()
  cs_attempt <-gsub(" '/(\r\n)+|\r+|\n\\s+|\t+/i'","",cs_no)
  cs_attempt <- as.data.frame(cs_attempt)
  cs_attempt <- cs_attempt[-c(1,nrow(cs_attempt)),]
  cs_attempt <- data.frame(do.call('rbind', strsplit(as.character(cs_attempt),'$',fixed=TRUE))) #Delimiting off "\n" or newline escape sequences
  cs_attempt <- as.data.frame(cs_attempt)
  cs_attempt$X3 <- ifelse(ncol(cs_attempt)==2, cs_attempt$X2, cs_attempt$X3)
  cs_attempt$X1 <- as.character(cs_attempt$X1)
  cs_attempt$X3 <- as.character(cs_attempt$X3)
  cs_attempt$X3 <- ifelse(cs_attempt$X3 == cs_attempt$X1, NA, cs_attempt$X3)
  cs_attempt$edition <- Cardsphere_Outer_Shell$CardSphere_Secrets[match(Set_Number,Cardsphere_Outer_Shell$CardSphere_Set_Numbers)]
  cs_attempt$'F/NF' <- ""
  colnames(cs_attempt) <- c("name","nf_price","f_price","edition",'F/NF')
  cs_nonfoil_prices <- data.frame(cs_attempt$name,cs_attempt$edition,cs_attempt$`F/NF`,cs_attempt$nf_price)
  cs_foil_prices <- data.frame(cs_attempt$name,cs_attempt$edition,cs_attempt$`F/NF`,cs_attempt$f_price)
  cs_foil_prices$cs_attempt..F.NF. <- "FOIL"
  
  cs_nonfoil_prices$cs_attempt.name <- trimws(cs_nonfoil_prices$cs_attempt.name)
  cs_nonfoil_prices$cs_attempt.edition <- trimws(cs_nonfoil_prices$cs_attempt.edition)
  
  cs_foil_prices$cs_attempt.name <- trimws(cs_foil_prices$cs_attempt.name)
  cs_foil_prices$cs_attempt.edition <- trimws(cs_foil_prices$cs_attempt.edition)
  
  cs_nonfoil_prices$Key <- paste(cs_nonfoil_prices$cs_attempt.name,cs_nonfoil_prices$cs_attempt.edition,cs_nonfoil_prices$cs_attempt..F.NF.,sep="")
  cs_foil_prices$Key <- paste(cs_foil_prices$cs_attempt.name,cs_foil_prices$cs_attempt.edition,cs_foil_prices$cs_attempt..F.NF.,sep="")
  
  cs_nonfoil_prices <- cs_nonfoil_prices[,c(5,1,2,3,4)]
  cs_foil_prices <- cs_foil_prices[,c(5,1,2,3,4)]
  #cs_nonfoil_prices <-unname(cs_nonfoil_prices)
  #cs_foil_prices <-unname(cs_foil_prices)
  cs_prices <- rbind.fill(cs_nonfoil_prices,cs_foil_prices)
  cs_prices$cs_attempt.f_price <- as.numeric(as.character(cs_prices$cs_attempt.f_price))
  cs_prices$cs_attempt.nf_price <- as.numeric(as.character(cs_prices$cs_attempt.nf_price))
  cs_prices$cs_attempt.nf_price <- ifelse(is.na(cs_prices$cs_attempt.nf_price)==TRUE, cs_prices$cs_attempt.f_price, cs_prices$cs_attempt.nf_price)
  cs_prices <- cs_prices[,-ncol(cs_prices)]
  cs_prices <- unname(cs_prices)
  colnames(cs_prices) <- c("Key","name","edition","isfoil","retail")
  All_CardSphere <- rbind(All_CardSphere, cs_prices)
  Sys.sleep(sample(.29:1.63, 1))
  setTxtProgressBar(pb,Q)
  Q <- Q+1
}

CardSphere_Final_Output <- All_CardSphere
CardSphere_Final_Output$retail_.80 <- round(CardSphere_Final_Output$retail*.80,1)
CK_Equivalent <- Slim_CK_Buylist
CK_Equivalent$meta.created_at <- paste(CK_Equivalent$data.name,CK_Equivalent$data.edition,CK_Equivalent$data.is_foil,sep="")
CardSphere_Final_Output$CK_BL_Offer <- CK_Equivalent$data.price_buy[match(CardSphere_Final_Output$Key,CK_Equivalent$meta.created_at)]
CardSphere_Final_Output <- na.omit(CardSphere_Final_Output)
CardSphere_Final_Output$Opportunities <- as.numeric(as.character(CardSphere_Final_Output$CK_BL_Offer)) - as.numeric(as.character(CardSphere_Final_Output$retail_.80))
CardSphere_Final_Output <- CardSphere_Final_Output[order(-CardSphere_Final_Output$Opportunities),]
CardsphereNF <- CardSphere_Final_Output[which(CardSphere_Final_Output$isfoil == ""),]


Reference<- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(`F/NF` = col_character()))
Reference <- Reference[c(8:12)]
colnames(Reference) <- c("Key","name","Set","Rarity","Foil")
Reference$Semi_Key <- substr(Reference$Key,1,nchar(Reference$Key)-1)
CardsphereNF$Rarity <- Reference$Rarity[match(CardsphereNF$Key,Reference$Semi_Key)]
Cardsphere_Mythics <- CardsphereNF[which(CardsphereNF$Rarity == "M"),]
Cardsphere_Rares <- CardsphereNF[which(CardsphereNF$Rarity == "R"),]
Cardsphere_Uncommons <- CardsphereNF[which(CardsphereNF$Rarity == "U"),]
Cardsphere_Commons <- CardsphereNF[which(CardsphereNF$Rarity == "C"),]
Cardsphere_Unknowns <- CardsphereNF[which(is.na(CardsphereNF$Rarity) == TRUE),]


CardsphereNF <- CardsphereNF[,-9]
Cardsphere_Mythics <- Cardsphere_Mythics[,-9]
Cardsphere_Rares <- Cardsphere_Rares[,-9]
Cardsphere_Uncommons <- Cardsphere_Uncommons[,-9]
Cardsphere_Commons <- Cardsphere_Commons[,-9]
Cardsphere_Unknowns <- Cardsphere_Unknowns[,-9]

currentDate <- Sys.Date()
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

# cs <- list(CardsphereNF,Cardsphere_Mythics,Cardsphere_Rares,Cardsphere_Uncommons,Cardsphere_Commons,Cardsphere_Unknowns)
# #sheets_deauth()
# gs4_auth()
# sheets_create(
#   paste(currentDate,"_Cardsphere_Review",sep=""),
#   sheets = cs
# )
drive_auth(email = "pachun95@gmail.com", use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com", use_oob=TRUE)
ss <- drive_get("Cardsphere_Review")
#sheets_deauth()
gs4_auth(email = "pachun95@gmail.com")
sheet_write(
  CardsphereNF,
  ss = ss,
  sheet = "All_CS_NF"
)
sheet_write(
  Cardsphere_Mythics,
  ss = ss,
  sheet = "Mythics"
)
sheet_write(
  Cardsphere_Rares,
  ss = ss,
  sheet = "Rares"
)
sheet_write(
  Cardsphere_Uncommons,
  ss = ss,
  sheet = "Uncommons"
)
sheet_write(
  Cardsphere_Commons,
  ss = ss,
  sheet = "Commons"
)
sheet_write(
  Cardsphere_Unknowns,
  ss = ss,
  sheet = "Unknowns"
)
drive_auth(email = "pachun95@gmail.com", use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com", use_oob=TRUE)

ss <- drive_get("Wolfs_Buylist_Review")
Wolfs_Buylist <- range_read(ss)
Wolfs_Buylist <- as.data.frame(Wolfs_Buylist)
Buylist <- Wolfs_Buylist
Buylist$Semi_Key <- paste(Buylist$data.name,Buylist$data.edition, sep="")
Wolfs_Buylist$data.is_foil[is.na(Wolfs_Buylist$data.is_foil) == T] <- ""
Wolfs_Buylist <- Wolfs_Buylist[which(Wolfs_Buylist$data.is_foil == ""),]
Wolfs_Buylist <- Wolfs_Buylist[which(Wolfs_Buylist$data.price_buy <= 20),]
Wolfs_Buylist <- Wolfs_Buylist[which(Wolfs_Buylist$data.price_buy >= 1),]
Wolfs_Buylist <- Wolfs_Buylist[which(Wolfs_Buylist$Printings <= 5),]
Wolfs_Buylist <- Wolfs_Buylist[order(Wolfs_Buylist$Velocity_Adjusted),]
Wolfs_Targets <- data.frame(Wolfs_Buylist$data.name,Wolfs_Buylist$data.edition,Wolfs_Buylist$data.price_retail,Wolfs_Buylist$data.price_buy,Wolfs_Buylist$Printings)
Wolfs_Targets$Semi_Key <- paste(Wolfs_Targets$Wolfs_Buylist.data.name,Wolfs_Targets$Wolfs_Buylist.data.edition,sep="")
#View(Wolfs_Targets)

SS <- drive_get("Cardsphere_Review")
Cardsphere_Retrieval <- range_read(SS, sheet = "All_CS_NF")
Cardsphere_Retrieval <- as.data.frame(Cardsphere_Retrieval)
CS <- Cardsphere_Retrieval

Cardsphere_Retrieval$isfoil[is.na(Cardsphere_Retrieval$isfoil) == T] <- ""
Cardsphere_Retrieval <- Cardsphere_Retrieval[which(Cardsphere_Retrieval$Opportunities >= 1.00),]
Cardsphere_Retrieval <- Cardsphere_Retrieval[which(Cardsphere_Retrieval$CK_BL_Offer >= .50),]

Target_List <- append(Cardsphere_Retrieval$Key,Wolfs_Targets$Semi_Key)
Target_List <- as.data.frame(Target_List)
Target_List <- unique(Target_List$Target_List)
Target_List <- as.data.frame(Target_List)

Target_List$Name <- Buylist$data.name[match(Target_List$Target_List,Buylist$Semi_Key)]
Target_List$Edition <- Buylist$data.edition[match(Target_List$Target_List,Buylist$Semi_Key)]
All_Cards <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv",col_types = cols(hasFoil = col_character()))
All_Cards$Semi <- paste(All_Cards$card,All_Cards$set,sep="")
Target_List$Rarity <-All_Cards$rarity[match(Target_List$Target_List,All_Cards$Semi)]
Target_List$CK_Buylist <- CS$CK_BL_Offer[match(Target_List$Target_List,CS$Key)]
Target_List$CS_Retail <- CS$retail[match(Target_List$Target_List,CS$Key)]
Target_List$CS_Offer <- round((Target_List$CS_Retail * .72),1)
Target_List$My_Offer <- Target_List$CS_Offer - as.numeric(as.character(Target_List$CK_Buylist))
Target_List <- na.omit(Target_List)
Target_List <- Target_List[order(Target_List$My_Offer),]

Target_List <- Target_List[which(Target_List$My_Offer <= 0),]
Target_List <- Target_List[which(Target_List$My_Offer <= -2.00),]
colnames(Target_List) <- c("Key","name","edition","Rarity","CK_Buylist","CS_Retail","CS_Offer","My_Offer")
Target_List <- unique(as.data.frame(rbind(Target_List[1:3],Cardsphere_Retrieval[1:3])))
ss <- drive_get("Derivative Stuff")
trending <- as.data.frame(range_read(ss,"Actual_Copies"))
Desired_Column <- ncol(trending)
trending[Desired_Column] <- as.numeric(unlist(trending[Desired_Column]))
trending <- trending[which(trending[Desired_Column] <= 125),]
trending <- trending[2:3]
trending$Key <- paste(trending$Name,trending$Set,sep="")
trending <- trending[c(3,1,2)]
colnames(trending) <- c("Key","name","edition")
Target_List <- rbind(Target_List,trending)
colnames(Target_List) <- c("Key","Name","Edition")
Target_List <- unique(Target_List)

CS_Import_List <- NULL
CS_Import_List$Name <- Target_List$Name
CS_Import_List <- as.data.frame(CS_Import_List)
CS_Import_List$Edition <- Target_List$Edition
CS_Import_List$Condition <- "Near Mint"
CS_Import_List$Language <- "English"
CS_Import_List$Finish <- ""
CS_Import_List$Tags <- ""
CS_Import_List$Quantity <- 12
CS_Import_List$Tradelist_Count <- 0

CS_Import_List <- CS_Import_List[moveme(names(CS_Import_List), "Tradelist_Count first")]
CS_Import_List <- CS_Import_List[moveme(names(CS_Import_List), "Quantity first")]
colnames(CS_Import_List)[2] <- c("Tradelist Count")

drive_auth(email = "pachun95@gmail.com", use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com", use_oob=TRUE)

ss <- drive_get("Cardsphere_Import_List")
#sheets_deauth()

sheet_write(
  CS_Import_List,
  ss = ss,
  sheet = "CS_Import_List"
)
CS_Import_List <- CS_Import_List[!grepl("Urza's",CS_Import_List),]
library(RSelenium)
remDr = remoteDriver(remoteServerAddr = "159.65.219.70", port = 4445L, browser = "chrome")
remDr$open()

remDr$navigate("https://www.cardsphere.com/login")
Sys.sleep(5)

username <- remDr$findElement(using = "id", value = "email")
username$clearElement()
username$sendKeysToElement(list("cjpach@mac.com"))

passwd <- remDr$findElement(using = "id", value = "password")
passwd$clearElement()
passwd$sendKeysToElement(list("Tasigur95$"))

Post_Credential_Login <- remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/div/div/div/form/button")
Post_Credential_Login$submitElement()
Sys.sleep(2)
remDr$navigate("https://www.cardsphere.com/wants")
Actions <- remDr$findElement(using = "xpath", value = '//*[@id="wants"]/ul/li[3]/a')
Actions$clickElement()

Delete_Wants <- remDr$findElement(using = "class", value = 'btn-danger')
Delete_Wants$clickElement()
Delete_Wants_Check <- remDr$findElement(using = "xpath", value = '//*[@id="myModal"]/div/div/div[2]/form/div/label/input')
Delete_Wants_Check$clickElement()
Delete_Final <-remDr$findElement(using = "xpath", value = '//*[@id="myModal"]/div/div/div[3]/button[2]')
Delete_Final$clickElement()
New_Wants <-remDr$findElement(using = "xpath", value = '//*[@id="wants"]/ul/li[1]/a')
New_Wants$clickElement()


for(i in 1:26){
  Offer_Perc <- remDr$findElement("xpath", '//*[@id="wants"]/div[1]/form/div[6]/div/div[1]')
  Offer_Perc$clickElement()
  Sys.sleep(.25)
}

i = 4
for(i in 1:nrow(CS_Import_List)){
  Card_Search <-remDr$findElement(using = "xpath", value = '//*[@id="wants"]/div[1]/form/div[1]/div/input')
  Card_Search$clickElement()
  Card_Search$sendKeysToElement(list(CS_Import_List$Name[i]))
  Sys.sleep(3)
  Card_Search <- remDr$findElement("xpath", '//*[@id="wants"]/div[1]/form/div[1]/div/div[2]/ul/li[1]')
  Sys.sleep(1)
  Card_Search$clickElement()
  Card_Search$clickElement()

  Set_Search <-remDr$findElement(using = "xpath", value = '//*[@id="wants"]/div[1]/form/div[2]/div/a/span[1]')
  Set_Search$clickElement()
  Sys.sleep(1)
  remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/a/span[1]')$clickElement()
  Set_Selection <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/div[1]/div[2]/input')
  Set_Selection$clickElement()
  Set_Selection$sendKeysToElement(list(CS_Import_List$Edition[i]))
  Sys.sleep(1)
  Set_Select <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/div[1]/ul/li/a')
  Set_Select$clickElement()
  Set_Select$click(buttonId = 2)
  if(unlist(Set_Search$getElementText()) == "Select sets"){
    Sys.sleep(1)
    Set_Search <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div')
    Set_Search$clickElement()
    
    # Set_Search <-remDr$findElement(using = "xpath", value = '//*[@id="wants"]/div[1]/form/div[2]/div/a/span[1]')
    # Set_Search$clickElement()
    Sys.sleep(1)
    Set_Selection <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/div[1]/div[2]/input')
    Set_Selection$sendKeysToElement(list(CS_Import_List$Edition[i]))
    Sys.sleep(1)
    Set_Select <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[2]/div/div[1]/ul/li/a')
    Set_Select$clickElement()
    Set_Select$click(buttonId = 2)
  }
  
  
  
  Sys.sleep(2)
  remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[4]/div/a')$clickElement()
  Sys.sleep(.25)
  Finish_Selection <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[4]/div/div[1]/ul/li[1]/a')
  Sys.sleep(.25)
  Finish_Selection$clickElement()
  Finish_Selection$click(buttonId = 2)
  
  remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[4]/div/a')$clickElement()
  Sys.sleep(.25)
  Finish_Selection <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[4]/div/div[1]/ul/li[1]/a')
  Sys.sleep(.25)
  Finish_Selection$clickElement()
  Finish_Selection$click(buttonId = 2)
  
  Sys.sleep(1)
  for(i in 1:11){
    Quantity_Selection <- remDr$findElement("xpath", '//*[@id="wants"]/div[1]/form/div[9]/div/div[2]')
    Sys.sleep(.20)
    Quantity_Selection$clickElement()
    Sys.sleep(.20)
  }
  Sys.sleep(1)
  Submition <- remDr$findElement("xpath",'//*[@id="wants"]/div[1]/form/div[12]/button')
  Sys.sleep(2)
  Submition$clickElement()
  Sys.sleep(.25)
}


