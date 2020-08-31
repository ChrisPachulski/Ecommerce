#install.packages(c("rvest","tidyverse","RSelenium","googledrive"))
d#evtools::install_github("tidyverse/googlesheets4", INSTALL_opts= '--no-lock',force = TRUE)
library(tidyverse)
library(rvest)
library(RSelenium)
right = function(text, num_char) {
  substr(text, nchar(text) - (num_char-1), nchar(text))
} #Recreating the right function from Excel 
left = function(text, num_char) {
  substr(text, 1, num_char)
} #Recreating the left function from Excel 
funk <- function(t){
  ifelse(nchar(t) <= 10, right(t,1),ifelse(nchar(t)<=190, right(t,2),ifelse(nchar(t)>=191, right((t),3),0)))
}

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

#Japanese Palantir####
remDr = remoteDriver(remoteServerAddr = "167.99.63.23", port = 4445L, browser = "chrome")
remDr$open()
remDr$maxWindowSize()
remDr$navigate("https://tokyomtg.com/")
English <- remDr$findElement(using = "class", value = "fa-refresh")
English$clickElement()
Sys.sleep(2)
Currency <- remDr$findElement(using = "xpath", '//*[@id="currency-select"]/option[2]')
Currency$clickElement()
Sys.sleep(3)
remDr$navigate("https://tokyomtg.com/cardpage.html?p=s&s=8")
Select_Language <- remDr$findElement("xpath",'//*[@id="store"]/div/form/div[4]/div/div[1]/div')$clickElement()
Remove_JPN <- remDr$findElement("xpath",'//*[@id="language-checkboxes"]/div[2]/label/span')$clickElement()
Sys.sleep(1)
Select_Language <- remDr$findElement("xpath",'//*[@id="store"]/div/form/div[4]/div/div[1]/div')$clickElement()
Sys.sleep(1)
Select_Foils <- remDr$findElement("xpath",'//*[@id="store"]/div/form/div[3]/div/div[1]/div')$clickElement()
Remove_Foils <- remDr$findElement("xpath", '//*[@id="premium-checkboxes"]/div[1]/label/span')$clickElement()
Sys.sleep(1)
Remove_Commons <- remDr$findElement("xpath",'//*[@id="rarity-checkboxes"]/div[4]/label/span')$clickElement()
Sys.sleep(1)
Select_Foils <- remDr$findElement("xpath",'//*[@id="store"]/div/form/div[3]/div/div[1]/div')$clickElement()

Refresh <- remDr$findElement("class", "filter-button")$clickElement()
Table_of_Contents <- data.frame("Pages" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50), 
                                "Page_Address" = c(20,40,60,80,100,120,140,160,180,200,220,240,260,280,300,320,340,360,380,400,420,440,460,480,500,520,540,560,580,600,620,640,660,680,700,720,740,760,780,800,820,840,860,880,900,920,940,960,980,1000))

Tokyos_Contents <- NULL

#Data Acquisition####
for (i in 1:291){
Desired_URL <- paste("https://tokyomtg.com/cardpage.html?p=s&s=",i,sep="")
remDr$navigate(Desired_URL)
Skip = "No"
Skip = tryCatch(expr = {remDr$findElement("xpath",'/html/body/div[2]/div/section/table/tbody/tr/td[2]')$getElementText()}, error = function(e){Skip = "Yes"})
No_Options = tryCatch(expr = {unlist(remDr$findElement("xpath","/html/body/div[1]/div/section/h3")$getElementText())}, error = function(e){No_Options = "Passes"})
if(No_Options == "All of the cards are hidden by your filter settings."){Skip = "Yes"}else{Skip = Skip}  
  if(Skip == "Yes"){} else {
    Pages_In_Set <- tryCatch(expr = {as.numeric(right(trimws(unlist(remDr$findElement("class","store-pagination")$getElementText())),1))}, error = function(e){Pages_In_Set = NA})
    Pages_In_Set <- tryCatch(expr = {as.numeric(right(trimws(unlist(remDr$findElement("class","store-pagination")$getElementText())),2))}, error = function(e){Pages_In_Set = Pages_In_Set})
    if(is.na(Pages_In_Set) == T){
    Pages_To_Scrape <- 1
    Unique_Contents <- Table_of_Contents[which(Table_of_Contents$Pages <= Pages_To_Scrape),]
    }else{
      Pages_To_Scrape <- Pages_In_Set
      Unique_Contents <- Table_of_Contents[which(Table_of_Contents$Pages <= Pages_To_Scrape),]
    }
  if(length(Unique_Contents$Pages) == 0 ){ Unique_Contents = data.frame(1,20) } else {Unique_Contents$Pages == Unique_Contents$Pages}
  colnames(Unique_Contents) <- c("Pages","Page_Address")
      for(j in 1:(max(Unique_Contents$Pages))){
      needed_specifier <- paste("https://tokyomtg.com/cardpage.html?p=s&s=",i,"&b=",(Unique_Contents$Page_Address[j-1]),sep="")
      if((is.na(as.numeric(right(needed_specifier,1))) != T) & right(needed_specifier,1) != "A"){
      remDr$navigate(needed_specifier)
      Sys.sleep(.25)
      Kyoko_Fukada <- remDr$getPageSource()[[1]]%>% read_html()
        
      Kyoko_Fukada_First_Secret <-  Kyoko_Fukada %>% html_nodes("h3") %>% html_text()
      Kyoko_Fukada_First_Secret <- as.data.frame(Kyoko_Fukada_First_Secret)
      Kyoko_Fukada_First_Secret$Kyoko_Fukada_First_Secret <- as.character(Kyoko_Fukada_First_Secret$Kyoko_Fukada_First_Secret)
      if (nrow(as.data.frame(Kyoko_Fukada_First_Secret[-grep("Out of stock", Kyoko_Fukada_First_Secret$Kyoko_Fukada_First_Secret),])) == 0){Kyoko_Fukada_Names <- Kyoko_Fukada_First_Secret$Kyoko_Fukada_First_Secret}else{Kyoko_Fukada_Names <- Kyoko_Fukada_First_Secret[-grep("Out of stock", Kyoko_Fukada_First_Secret$Kyoko_Fukada_First_Secret),]}
      Kyoko_Fukada_Cards <- as.data.frame(gsub(" \\(English\\)","", Kyoko_Fukada_Names))
      Kyoko_Fukada_Names <- Kyoko_Fukada_Cards[c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39),]
      Kyoko_Fukada_Names <- na.omit(Kyoko_Fukada_Names)
        
      Kyoko_Fukada_Second_Secret <- Kyoko_Fukada_Cards[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40),]
      Kyoko_Fukada_Second_Secret <- Kyoko_Fukada_Second_Secret[!grepl("SUBTOTAL",Kyoko_Fukada_Second_Secret)]
      Kyoko_Fukada_Second_Secret <- data.frame(do.call('rbind', strsplit(as.character(Kyoko_Fukada_Second_Secret),' - ',fixed=TRUE)))
      colnames(Kyoko_Fukada_Second_Secret) <- c("Edition","Rarity")
      Kyoko_Fukada_Second_Secret <- na.omit(Kyoko_Fukada_Second_Secret)
        
      Kyoko_Fukada_Third_Secret <- Kyoko_Fukada %>% html_nodes(".tab-content") %>% html_text()
      Kyoko_Fukada_Third_Secret <- gsub("\\n","",Kyoko_Fukada_Third_Secret)
      Kyoko_Fukada_Third_Secret <- gsub("\\$","",Kyoko_Fukada_Third_Secret)
      Kyoko_Fukada_Third_Secret <- gsub("add to cart","",Kyoko_Fukada_Third_Secret)
      Kyoko_Fukada_Third_Secret <- gsub("Out of stock","",Kyoko_Fukada_Third_Secret)
      Kyoko_Fukada_Third_Secret <- gsub("QTY\\:\\d+.*","",Kyoko_Fukada_Third_Secret)
      Kyoko_Fukada_Third_Secret <- gsub("\\)","",Kyoko_Fukada_Third_Secret)
      Kyoko_Fukada_Third_Secret <- data.frame(do.call('rbind',strsplit(as.character(Kyoko_Fukada_Third_Secret), ' (Stock: ', fixed=T)))
      colnames(Kyoko_Fukada_Third_Secret) <- c("Retail","Stock")
      
      Kyoko_Fukada_Unifies <- data.frame(Kyoko_Fukada_Names,Kyoko_Fukada_Second_Secret$Edition, Kyoko_Fukada_Second_Secret$Rarity,Kyoko_Fukada_Third_Secret$Retail, Kyoko_Fukada_Third_Secret$Stock)
      colnames(Kyoko_Fukada_Unifies) <- c("Card","Edition","Rarity","Retail","Stock")
      Tokyos_Contents <- rbind(Tokyos_Contents, Kyoko_Fukada_Unifies)
      Tokyos_Contents <- unique(Tokyos_Contents)
      }
    }
  } 
}
for (i in 293){
  Desired_URL <- paste("https://tokyomtg.com/cardpage.html?p=s&s=",i,sep="")
  remDr$navigate(Desired_URL)
  Skip = "No"
  Skip = tryCatch(expr = {remDr$findElement("xpath",'/html/body/div[2]/div/section/table/tbody/tr/td[2]')$getElementText()}, error = function(e){Skip = "Yes"})
  No_Options = tryCatch(expr = {unlist(remDr$findElement("xpath","/html/body/div[1]/div/section/h3")$getElementText())}, error = function(e){No_Options = "Passes"})
  if(No_Options == "All of the cards are hidden by your filter settings."){Skip = "Yes"}else{Skip = Skip}  
  if(Skip == "Yes"){} else {
    Pages_In_Set <- tryCatch(expr = {as.numeric(right(trimws(unlist(remDr$findElement("class","store-pagination")$getElementText())),1))}, error = function(e){Pages_In_Set = NA})
    Pages_In_Set <- tryCatch(expr = {as.numeric(right(trimws(unlist(remDr$findElement("class","store-pagination")$getElementText())),2))}, error = function(e){Pages_In_Set = Pages_In_Set})
    if(is.na(Pages_In_Set) == T){
      Pages_To_Scrape <- 1
      Unique_Contents <- Table_of_Contents[which(Table_of_Contents$Pages <= Pages_To_Scrape),]
    }else{
      Pages_To_Scrape <- Pages_In_Set
      Unique_Contents <- Table_of_Contents[which(Table_of_Contents$Pages <= Pages_To_Scrape),]
    }
    if(length(Unique_Contents$Pages) == 0 ){ Unique_Contents = data.frame(1,20) } else {Unique_Contents$Pages == Unique_Contents$Pages}
    colnames(Unique_Contents) <- c("Pages","Page_Address")
    for(j in 1:(max(Unique_Contents$Pages))){
      needed_specifier <- paste("https://tokyomtg.com/cardpage.html?p=s&s=",i,"&b=",(Unique_Contents$Page_Address[j-1]),sep="")
      if((is.na(as.numeric(right(needed_specifier,1))) != T) & right(needed_specifier,1) != "A"){
        remDr$navigate(needed_specifier)
        Sys.sleep(.25)
        Kyoko_Fukada <- remDr$getPageSource()[[1]]%>% read_html()
        
        Kyoko_Fukada_First_Secret <-  Kyoko_Fukada %>% html_nodes("h3") %>% html_text()
        Kyoko_Fukada_First_Secret <- as.data.frame(Kyoko_Fukada_First_Secret)
        Kyoko_Fukada_First_Secret$Kyoko_Fukada_First_Secret <- as.character(Kyoko_Fukada_First_Secret$Kyoko_Fukada_First_Secret)
        if (nrow(as.data.frame(Kyoko_Fukada_First_Secret[-grep("Out of stock", Kyoko_Fukada_First_Secret$Kyoko_Fukada_First_Secret),])) == 0){Kyoko_Fukada_Names <- Kyoko_Fukada_First_Secret$Kyoko_Fukada_First_Secret}else{Kyoko_Fukada_Names <- Kyoko_Fukada_First_Secret[-grep("Out of stock", Kyoko_Fukada_First_Secret$Kyoko_Fukada_First_Secret),]}
        Kyoko_Fukada_Cards <- as.data.frame(gsub(" \\(English\\)","", Kyoko_Fukada_Names))
        Kyoko_Fukada_Names <- Kyoko_Fukada_Cards[c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39),]
        Kyoko_Fukada_Names <- na.omit(Kyoko_Fukada_Names)
        
        Kyoko_Fukada_Second_Secret <- Kyoko_Fukada_Cards[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40),]
        Kyoko_Fukada_Second_Secret <- Kyoko_Fukada_Second_Secret[!grepl("SUBTOTAL",Kyoko_Fukada_Second_Secret)]
        Kyoko_Fukada_Second_Secret <- data.frame(do.call('rbind', strsplit(as.character(Kyoko_Fukada_Second_Secret),' - ',fixed=TRUE)))
        colnames(Kyoko_Fukada_Second_Secret) <- c("Edition","Rarity")
        Kyoko_Fukada_Second_Secret <- na.omit(Kyoko_Fukada_Second_Secret)
        
        Kyoko_Fukada_Third_Secret <- Kyoko_Fukada %>% html_nodes(".tab-content") %>% html_text()
        Kyoko_Fukada_Third_Secret <- gsub("\\n","",Kyoko_Fukada_Third_Secret)
        Kyoko_Fukada_Third_Secret <- gsub("\\$","",Kyoko_Fukada_Third_Secret)
        Kyoko_Fukada_Third_Secret <- gsub("add to cart","",Kyoko_Fukada_Third_Secret)
        Kyoko_Fukada_Third_Secret <- gsub("Out of stock","",Kyoko_Fukada_Third_Secret)
        Kyoko_Fukada_Third_Secret <- gsub("QTY\\:\\d+.*","",Kyoko_Fukada_Third_Secret)
        Kyoko_Fukada_Third_Secret <- gsub("\\)","",Kyoko_Fukada_Third_Secret)
        Kyoko_Fukada_Third_Secret <- data.frame(do.call('rbind',strsplit(as.character(Kyoko_Fukada_Third_Secret), ' (Stock: ', fixed=T)))
        colnames(Kyoko_Fukada_Third_Secret) <- c("Retail","Stock")
        
        Kyoko_Fukada_Unifies <- data.frame(Kyoko_Fukada_Names,Kyoko_Fukada_Second_Secret$Edition, Kyoko_Fukada_Second_Secret$Rarity,Kyoko_Fukada_Third_Secret$Retail, Kyoko_Fukada_Third_Secret$Stock)
        colnames(Kyoko_Fukada_Unifies) <- c("Card","Edition","Rarity","Retail","Stock")
        Tokyos_Contents <- rbind(Tokyos_Contents, Kyoko_Fukada_Unifies)
        Tokyos_Contents <- unique(Tokyos_Contents)
      }
    }
  } 
}
for (i in 304:311){
  Desired_URL <- paste("https://tokyomtg.com/cardpage.html?p=s&s=",i,sep="")
  remDr$navigate(Desired_URL)
  Skip = "No"
  Skip = tryCatch(expr = {remDr$findElement("xpath",'/html/body/div[2]/div/section/table/tbody/tr/td[2]')$getElementText()}, error = function(e){Skip = "Yes"})
  No_Options = tryCatch(expr = {unlist(remDr$findElement("xpath","/html/body/div[1]/div/section/h3")$getElementText())}, error = function(e){No_Options = "Passes"})
  if(No_Options == "All of the cards are hidden by your filter settings."){Skip = "Yes"}else{Skip = Skip}  
  if(Skip == "Yes"){} else {
    Pages_In_Set <- tryCatch(expr = {as.numeric(right(trimws(unlist(remDr$findElement("class","store-pagination")$getElementText())),1))}, error = function(e){Pages_In_Set = NA})
    Pages_In_Set <- tryCatch(expr = {as.numeric(right(trimws(unlist(remDr$findElement("class","store-pagination")$getElementText())),2))}, error = function(e){Pages_In_Set = Pages_In_Set})
    if(is.na(Pages_In_Set) == T){
      Pages_To_Scrape <- 1
      Unique_Contents <- Table_of_Contents[which(Table_of_Contents$Pages <= Pages_To_Scrape),]
    }else{
      Pages_To_Scrape <- Pages_In_Set
      Unique_Contents <- Table_of_Contents[which(Table_of_Contents$Pages <= Pages_To_Scrape),]
    }
    if(length(Unique_Contents$Pages) == 0 ){ Unique_Contents = data.frame(1,20) } else {Unique_Contents$Pages == Unique_Contents$Pages}
    colnames(Unique_Contents) <- c("Pages","Page_Address")
    for(j in 1:(max(Unique_Contents$Pages))){
      needed_specifier <- paste("https://tokyomtg.com/cardpage.html?p=s&s=",i,"&b=",(Unique_Contents$Page_Address[j-1]),sep="")
      if((is.na(as.numeric(right(needed_specifier,1))) != T) & right(needed_specifier,1) != "A"){
        remDr$navigate(needed_specifier)
        Sys.sleep(.25)
        Kyoko_Fukada <- remDr$getPageSource()[[1]]%>% read_html()
        
        Kyoko_Fukada_First_Secret <-  Kyoko_Fukada %>% html_nodes("h3") %>% html_text()
        Kyoko_Fukada_First_Secret <- as.data.frame(Kyoko_Fukada_First_Secret)
        Kyoko_Fukada_First_Secret$Kyoko_Fukada_First_Secret <- as.character(Kyoko_Fukada_First_Secret$Kyoko_Fukada_First_Secret)
        if (nrow(as.data.frame(Kyoko_Fukada_First_Secret[-grep("Out of stock", Kyoko_Fukada_First_Secret$Kyoko_Fukada_First_Secret),])) == 0){Kyoko_Fukada_Names <- Kyoko_Fukada_First_Secret$Kyoko_Fukada_First_Secret}else{Kyoko_Fukada_Names <- Kyoko_Fukada_First_Secret[-grep("Out of stock", Kyoko_Fukada_First_Secret$Kyoko_Fukada_First_Secret),]}
        Kyoko_Fukada_Cards <- as.data.frame(gsub(" \\(English\\)","", Kyoko_Fukada_Names))
        Kyoko_Fukada_Names <- Kyoko_Fukada_Cards[c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39),]
        Kyoko_Fukada_Names <- na.omit(Kyoko_Fukada_Names)
        
        Kyoko_Fukada_Second_Secret <- Kyoko_Fukada_Cards[c(2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40),]
        Kyoko_Fukada_Second_Secret <- Kyoko_Fukada_Second_Secret[!grepl("SUBTOTAL",Kyoko_Fukada_Second_Secret)]
        Kyoko_Fukada_Second_Secret <- data.frame(do.call('rbind', strsplit(as.character(Kyoko_Fukada_Second_Secret),' - ',fixed=TRUE)))
        colnames(Kyoko_Fukada_Second_Secret) <- c("Edition","Rarity")
        Kyoko_Fukada_Second_Secret <- na.omit(Kyoko_Fukada_Second_Secret)
        
        Kyoko_Fukada_Third_Secret <- Kyoko_Fukada %>% html_nodes(".tab-content") %>% html_text()
        Kyoko_Fukada_Third_Secret <- gsub("\\n","",Kyoko_Fukada_Third_Secret)
        Kyoko_Fukada_Third_Secret <- gsub("\\$","",Kyoko_Fukada_Third_Secret)
        Kyoko_Fukada_Third_Secret <- gsub("add to cart","",Kyoko_Fukada_Third_Secret)
        Kyoko_Fukada_Third_Secret <- gsub("Out of stock","",Kyoko_Fukada_Third_Secret)
        Kyoko_Fukada_Third_Secret <- gsub("QTY\\:\\d+.*","",Kyoko_Fukada_Third_Secret)
        Kyoko_Fukada_Third_Secret <- gsub("\\)","",Kyoko_Fukada_Third_Secret)
        Kyoko_Fukada_Third_Secret <- data.frame(do.call('rbind',strsplit(as.character(Kyoko_Fukada_Third_Secret), ' (Stock: ', fixed=T)))
        colnames(Kyoko_Fukada_Third_Secret) <- c("Retail","Stock")
        
        Kyoko_Fukada_Unifies <- data.frame(Kyoko_Fukada_Names,Kyoko_Fukada_Second_Secret$Edition, Kyoko_Fukada_Second_Secret$Rarity,Kyoko_Fukada_Third_Secret$Retail, Kyoko_Fukada_Third_Secret$Stock)
        colnames(Kyoko_Fukada_Unifies) <- c("Card","Edition","Rarity","Retail","Stock")
        Tokyos_Contents <- rbind(Tokyos_Contents, Kyoko_Fukada_Unifies)
        Tokyos_Contents <- unique(Tokyos_Contents)
      }
    }
  } 
}
Tokyos_Safety <- Tokyos_Contents
#Tokyos_Contents <- Tokyos_Safety
#View(Tokyos_Contents)
Tokyos_Contents$Rarity <- ifelse(Tokyos_Contents$Rarity == "Mythic","M", ifelse(Tokyos_Contents$Rarity == "Rare", "R", ifelse(Tokyos_Contents$Rarity == "Uncommon", "U", ifelse(Tokyos_Contents$Rarity == "Common", "C", Tokyos_Contents$Rarity))))
Tokyos_Contents$Key <- paste(Tokyos_Contents$Card,Tokyos_Contents$Edition,Tokyos_Contents$Rarity, sep ="")
Tokyos_Contents <- Tokyos_Contents[moveme(names(Tokyos_Contents), "Key first")]

setwd("/home/cujo253/Reports/Tokyo")
currentDate <- Sys.Date()
csvFileName <- paste(currentDate,"_Tokyo",".csv",sep="")
write.csv(Tokyos_Contents, file=csvFileName, row.names = FALSE)

#USA Comparison####
TodaysPremium <- paste("/home/cujo253/Reports/High Confidence Reps/",currentDate,"_Premium.csv", sep="")

USA_Data <- read_csv(TodaysPremium,col_types = cols(.default = "c"))

Tokyos_Contents$USA_Retail <- as.numeric(USA_Data$MKT)[match(Tokyos_Contents$Key,USA_Data$Key)]
Tokyos_Contents <- na.omit(Tokyos_Contents)
Tokyos_Contents$Arbit <- round(as.numeric(as.character(Tokyos_Contents$Retail)) - Tokyos_Contents$USA_Retail ,2)
Tokyos_Contents_Mkt <- Tokyos_Contents[order(Tokyos_Contents$Arbit),]


Tokyos_Contents$USA_BL <- as.numeric(USA_Data$BL)[match(Tokyos_Contents$Key,USA_Data$Key)]
Tokyos_Contents <- na.omit(Tokyos_Contents)
Tokyos_Contents$Arbit_BL <- round(as.numeric(as.character(Tokyos_Contents$Retail)) - Tokyos_Contents$USA_BL ,2)
Tokyos_Contents_BL <- Tokyos_Contents[order(Tokyos_Contents$Arbit_BL),]
Tokyos_Contents_BL$Stock <- as.numeric(as.character(left(Tokyos_Contents_BL$Stock,1)))
#View(Tokyos_Contents_BL)

library(devtools)
#devtools::install_github("tidyverse/googlesheets4")
library(googlesheets4)
library(googledrive)
drive_auth(email = "pachun95@gmail.com", use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com", use_oob=TRUE)

ss <- drive_get("Japans MTG Market")
#sheets_deauth()

# sheet_write(
#   Tokyos_Contents_Mkt,
#   ss = ss,
#   sheet = "Market_Comparison"
# )
sheet_write(
  Tokyos_Contents_BL,
  ss = ss,
  sheet = "Market_vs_Buylist_Comparison"
)

