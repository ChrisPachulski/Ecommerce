pacman::p_load(devtools,googlesheets4,googledrive,httr,jsonlite,RSelenium,tidyverse,anytime,lubridate)
right = function(text, num_char) {
    substr(text, nchar(text) - (num_char-1), nchar(text))
}
left = function(text, num_char) {
    substr(text, 1, num_char)
}
ovr_sold_ledger <- function(doc,sheet){
    ss <- drive_get(doc)
    Sold_Ledger <- range_read(ss, sheet)
    Sold_Ledger <- Sold_Ledger[which(is.na(Sold_Ledger$`Unique Card Tag`) == F ),]
    Sold_Ledger$Qty<- unlist(Sold_Ledger$Qty)
    Sold_Ledger$`# Sold` <- unlist(Sold_Ledger$`# Sold`)
    Sold_Ledger <- Sold_Ledger[c(1:36)]
    return(Sold_Ledger)
}
ovr_inventory_ledger <- function(doc,sheet){
    drive_auth(email = "xyz@gmail.com",use_oob=TRUE)
    gs4_auth(email = "xyz@gmail.com",use_oob=TRUE)
    ss <- drive_get(doc)
    Raw_Inventory <- range_read(ss, sheet,range = "F2:AD") %>% filter(is.na(`...1`) != T)
    Raw_Inventory$`Buy Qty` <- round(as.numeric(as.character(unlist(Raw_Inventory$`Buy Qty`))),2)
    Raw_Inventory$DOP <- format(Raw_Inventory$DOP,"%Y-%m-%d")
    return(Raw_Inventory)
}
chrome <-function(ip){
    remDr = remoteDriver(remoteServerAddr = ip, port = 4445, browser = "chrome")
    remDr$open()
    remDr$maxWindowSize()
    remDr
}
tcg_login_sales <-function(){
    remDr$navigate("https://tcgplayer.com")
    Sys.sleep(sample(5:9, 1))
    remDr$findElement("xpath",'//*[@id="app"]/div/header/div/div[3]/div[1]/div[2]/div[1]')$clickElement()
    Sys.sleep(sample(5:9, 1))
    remDr$findElement("css",'.account-actions-menu__title a')$clickElement()
    username <- remDr$findElement("id","Email")
    Sys.sleep(sample(1:3, 1))
    username$clickElement()
    Sys.sleep(sample(1:3, 1))
    username$sendKeysToElement(list("username"))
    Password <- remDr$findElement("id","Password")
    Sys.sleep(sample(1:3, 1))
    Password$clickElement()
    Sys.sleep(sample(1:3, 1))
    Password$sendKeysToElement(list("Password"))
    
    Sys.sleep(3)
    remDr$findElement("id",'loginButton')$clickElement()
    
    remDr$navigate("https://store.tcgplayer.com/admin/orders/orderlist")
    
    Sys.sleep(3)
    Latest_Dates <- format(Sold_Ledger$DOS + 1,"%m/%d/%Y")
    Latest_Date <- Latest_Dates[length(Latest_Dates)]
    Date_Input <- remDr$findElement("xpath", '//*[@id="rightSide"]/div/div[4]/div/div[2]/div[1]/div[1]/div[2]/div[1]/div[3]/div/div/div[1]/div[1]/div/div[1]/div/input')
    Date_Input$clickElement()
    Date_Input$sendKeysToElement(list(Latest_Date))
    Sys.sleep(.5)
    currentDate <- format((Sys.Date()),"%m/%d/%Y")
    Date_Endpoint <- remDr$findElement("xpath",'//*[@id="rightSide"]/div/div[4]/div/div[2]/div[1]/div[1]/div[2]/div[1]/div[3]/div/div/div[1]/div[2]/div/div[1]/div/input')
    Date_Endpoint$clickElement()
    Date_Endpoint$sendKeysToElement(list(currentDate))
    Sys.sleep(2)
    remDr$findElement("xpath",'//*[@id="rightSide"]/div/div[4]/div/div[2]/div[1]/div[1]/div[2]/div[4]/div[2]/div/div[2]/button')$clickElement()
    Sys.sleep(.5)
    remDr$findElement("xpath",'//*[@id="table-page-counts"]/span[2]/select/option[5]')$clickElement()
    Sys.sleep(2)
    remDr$findElement("xpath",'//*[@id="rightSide"]/div/div[4]/div/span/div/div[3]/div/div[2]/table/thead/tr/th[4]')$clickElement()
    Sys.sleep(2)
    #remDr$findElement("xpath",'//*[@id="rightSide"]/div/div[4]/div/span/div/div[3]/div/div[2]/table/thead/tr/th[4]')$clickElement()
    
    #remDr$findElement("xpath",'//*[@id="rightSide"]/div/div[4]/div/span/div/div[3]/div/div[2]/table/thead/tr/th[4]')$clickElement()
    Orders <- NULL
    Orders <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('a') %>% html_attr("href")
    Orders <- data.frame(Orders[grepl("manageorder",Orders)])
    colnames(Orders) <- "Order_ID"
    Orders <- Orders %>% separate(Order_ID,c("A","B","C","D","Trans_ID"),"/")
    Orders <- as.data.frame(Orders[5])
    Order_Links <- Orders
    Order_Links$Links <- paste("https://store.tcgplayer.com/admin/orders/manageorder/",Order_Links$Trans_ID,sep="")
    
    Order_Links <- unique(Order_Links)
    Recent_Sales <- NULL
    for(i in 1:nrow(Order_Links)){
        remDr$navigate(Order_Links$Links[i])
        if(length(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text()) == 0){
            Test_Number = length(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text())
        }else(
            Test_Number = length(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text())
        )
        if (Test_Number == 1){
            Card_Info <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('a') %>% html_attr("href")
            Card_Info <- data.frame(Card_Info[grepl("productSearch",Card_Info)])
            colnames(Card_Info) <- "link"
            Card_Link <- gsub("../../..","https://store.tcgplayer.com",Card_Info$link)
            
            
            Param <- as.data.frame(Card_Link) %>% separate(Card_Link[1],c("1","2"),"/productSearch/")
            Param <- as.data.frame(Param$`2`)
            colnames(Param) <- "param"
            Param$Trans_ID <- Order_Links$Trans_ID[i]
            Param$Key <- Updated_Tracking_Keys$Key[match(Param$param,Updated_Tracking_Keys$param)]
            Param$name <- Updated_Tracking_Keys$name[match(Param$param,Updated_Tracking_Keys$param)]
            Param$Set <- Updated_Tracking_Keys$Set[match(Param$param,Updated_Tracking_Keys$param)]
            Param$abbr <- Updated_Tracking_Keys$abbr[match(Param$param,Updated_Tracking_Keys$param)]
            Param$Rarity <- Updated_Tracking_Keys$Rarity[match(Param$param,Updated_Tracking_Keys$param)]
            Param$Foil <- Updated_Tracking_Keys$Foil[match(Param$param,Updated_Tracking_Keys$param)]
            Param$Foil[is.na(Param$Foil)]<-""
            Character_Check <- character(0)
            Param$Ind_Qty <-  if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text(),Character_Check) == T){
                remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text()
            }else{
                remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text()
            }
            
            Param$Order_Qty <- Param$Ind_Qty
            
            Param$Order_Amt <- if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[3]') %>% html_text(),Character_Check)){gsub("Order Amount\\: \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[3]') %>% html_text())}else{gsub("Order Amount\\: \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[3]') %>% html_text())}
            
            Param$Base_Fee <- if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[4]') %>% html_text(),Character_Check)){gsub("\\)","",gsub("Fee Amount\\: \\(\\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[4]') %>% html_text()))}else{gsub("\\)","",gsub("Fee Amount\\: \\(\\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[4]') %>% html_text()))}
            
            Test_For_Direct <- if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text(),Character_Check)==T){remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text()}else{remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text()}
            
            if(any(grepl("Direct",Test_For_Direct))==T){
                Param$Direct_Fee <- if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text(),Character_Check)==T){as.numeric(gsub("\\)","",gsub("Direct Program Fee\\: \\(*\\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text())))}else{as.numeric(gsub("\\)","",gsub("Direct Program Fee\\: \\(*\\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text())))}
            } else{
                Param$Direct_Fee <- 0.00025
            }
            
            suppressWarnings(if(Param$Direct_Fee == 0.00025){
                Param$Net_Rev <- if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text(),Character_Check)){as.numeric(gsub("Net Amount\\: \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text()))}else{as.numeric(gsub("Net Amount\\: \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text()))}
            } else {
                Param$Net_Rev <- if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[6]') %>% html_text(),Character_Check)){as.numeric(gsub("Net Amount\\: \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[6]') %>% html_text()))}else{as.numeric(gsub("Net Amount\\: \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[6]') %>% html_text()))}
            })
            Param$Ind_Amt <- Param$Order_Amt
            
            Param$DOS <- if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[1]/div/table/tbody/tr[5]/td[2]') %>% html_text(),Character_Check)){remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[1]/div/table/tbody/tr[5]/td[2]') %>% html_text()}else{remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[1]/div/table/tbody/tr[5]/td[2]') %>% html_text()}
            
            Param$Ind_Rev <- Param$Net_Rev
            
            Recent_Sales <- rbind(Recent_Sales,Param)
            Sys.sleep(3)
        }else{
            #length(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text())
            Card_Info <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('a') %>% html_attr("href")
            Card_Info <- data.frame(Card_Info = Card_Info[grepl("productSearch",Card_Info)])
            Card_Info <- data.frame(do.call('rbind', strsplit(as.character(Card_Info$Card_Info),'/productSearch/',fixed=TRUE)))
            Param <-data.frame(param = Card_Info$X2)
            Param$Trans_ID <- Order_Links$Trans_ID[i]
            Param$Key <- Updated_Tracking_Keys$Key[match(Param$param,Updated_Tracking_Keys$param)]
            Param$name <- Updated_Tracking_Keys$name[match(Param$param,Updated_Tracking_Keys$param)]
            Param$Set <- Updated_Tracking_Keys$Set[match(Param$param,Updated_Tracking_Keys$param)]
            Param$abbr <- Updated_Tracking_Keys$abbr[match(Param$param,Updated_Tracking_Keys$param)]
            Param$Rarity <- Updated_Tracking_Keys$Rarity[match(Param$param,Updated_Tracking_Keys$param)]
            Param$Foil <- Updated_Tracking_Keys$Foil[match(Param$param,Updated_Tracking_Keys$param)]
            Param$Foil[is.na(Param$Foil)]<-""
            Character_Check <- character(0)
            Param$Ind_Qty <-  if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text(),Character_Check) == T){
                remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text()
            }else{
                remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[6]/div[2]/div/table/tbody/tr/td[3]/div') %>% html_text()
            }
            Param$Order_Qty <- sum(as.numeric(Param$Ind_Qty))
            
            Param$Order_Amt <- as.numeric(gsub("Order Amount\\: \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[3]') %>% html_text()))
            
            Param$Order_Amt <-  if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[3]') %>% html_text(),Character_Check) == T){
                as.numeric(gsub("\\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[3]/td[2]/b') %>% html_text()))
            }else{
                as.numeric(gsub("Order Amount\\: \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[3]') %>% html_text()))
            }
            
            Param$Base_Fee <- if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[4]') %>% html_text(),Character_Check) == T){
                as.numeric(gsub("\\)","",gsub("Fee Amount\\: \\(\\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[4]') %>% html_text())))
            }else{
                as.numeric(gsub("\\)","",gsub("Fee Amount\\: \\(\\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[4]') %>% html_text())))
            }
            
            
            
            Test_For_Direct <- if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text(),Character_Check)==T){remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text()}else{remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text()}
            
            if(any(grepl("Direct",Test_For_Direct))==T){
                Param$Direct_Fee <- if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text(),Character_Check)==T){as.numeric(gsub("\\)","",gsub("Direct Program Fee\\: \\(*\\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text())))}else{as.numeric(gsub("\\)","",gsub("Direct Program Fee\\: \\(*\\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text())))}
            } else{
                Param$Direct_Fee <- 0.00025
            }
            
            suppressWarnings(if(Param$Direct_Fee == 0.00025){
                Param$Net_Rev <- if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text(),Character_Check)){as.numeric(gsub("Net Amount\\: \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[6]') %>% html_text()))}else{as.numeric(gsub("Net Amount\\: \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[5]') %>% html_text()))}
            } else {
                Param$Net_Rev <- if(identical(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[6]') %>% html_text(),Character_Check)){as.numeric(gsub("Net Amount\\: \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[5]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[6]') %>% html_text()))}else{as.numeric(gsub("Net Amount\\: \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[2]/table/tbody/tr[6]') %>% html_text()))}
            })
            
            Param$Ind_Amt <- as.numeric(gsub(" .*","",gsub(".*  \\$","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('.gradeA') %>% html_text())))* as.numeric(Param$Ind_Qty)
            Param$DOS <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="rightSide"]/div/div[4]/div[2]/div[1]/div[3]/div[1]/div/table/tbody/tr[5]/td[2]') %>% html_text()
            Param$Base_Fee <- round(Param$Ind_Amt / as.numeric(Param$Order_Amt) * Param$Base_Fee,2)
            Param$Direct_Fee <- round(Param$Ind_Amt / as.numeric(Param$Order_Amt) * Param$Direct_Fee,2)
            Param$Ind_Rev <- round(Param$Ind_Amt / as.numeric(Param$Order_Amt) * Param$Net_Rev,2)
            Recent_Sales <- rbind(Recent_Sales,Param)
        }
    }
    Safety_Sales <- Recent_Sales %>% distinct()
    #Recent_Sales <- Safety_Sales
    NA_Saviours <- Recent_Sales[which(is.na(Recent_Sales$name)==T ),]
    Replacements <- NA_Saviours
    Saviours <- NULL
    for (i in 1:nrow(NA_Saviours)){
        Saviour_Links <- paste("https://store.tcgplayer.com/productCatalog/product/productSearch/",NA_Saviours$param[i],sep="")
        Sys.sleep(3)
        remDr$navigate(Saviour_Links)
        Replacements$name[i] <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '/html/body/div[4]/section[1]/div/section/div[3]/div[1]/h1') %>% html_text()
        Sys.sleep(3)
        Replacements$Set[i] <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '/html/body/div[4]/section[1]/div/section/div[3]/div[1]/div/a') %>% html_text()
        Sys.sleep(3)
        Replacements$abbr[i] <- Updated_Tracking_Keys$abbr[match(Replacements$Set,Updated_Tracking_Keys$Set)]
        Replacements$Rarity[i] <- gsub("\\,.*","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '/html/body/div[4]/section[1]/div/section/div[3]/table/tbody/tr/td/dl/dd[1]') %>% html_text())
        Sys.sleep(3)
        Replacements$Key[i] <- paste(Replacements$name[i],Replacements$Set[i],Replacements$Rarity[i],sep="")
        
    }
    Recent_Sales <- rbind(Recent_Sales,Replacements)
    
    
    Recent_Sales <- na.omit(Recent_Sales)
    
    
    Recent_Sales$Direct_Fee <- round(Recent_Sales$Direct_Fee,2)
    #devtools::install_github("eddelbuettel/anytime",force = TRUE)
    Recent_Sales$DOS <- anydate(Recent_Sales$DOS)
    Recent_Sales$abbr<- ifelse(Recent_Sales$Foil=="FOIL",paste(Recent_Sales$abbr,"_F",sep=""), Recent_Sales$abbr)
    Recent_Sales$Key <- gsub(" Promos","",Recent_Sales$Key)
    Recent_Sales$DOP <- anytime(r_inv$DOP)[match(trimws(Recent_Sales$Key),trimws(r_inv$Key))]
    Sets_for_NA_DOP <- as.data.frame(read_html("https://mtg.gamepedia.com/Set") %>% html_nodes(xpath ='//*[@id="mw-content-text"]/div/table[4]') %>% html_table() )
    Sets_for_NA_DOP <- Sets_for_NA_DOP[c(1:2)]
    Sets_for_NA_DOP$Released <- as.Date(anydate(paste(Sets_for_NA_DOP$Released,"-01",sep="")))
    Recent_Sales$DOP <- ifelse(is.na(Recent_Sales$DOP)==T, as.character(Sets_for_NA_DOP$Released)[match(Recent_Sales$Set,Sets_for_NA_DOP$Set)],as.character(Recent_Sales$DOP))
    Recent_Sales$DOP <- format(anydate(Recent_Sales$DOP),"%Y-%m-%d")
    Recent_Sales$DOP[is.na(Recent_Sales$DOP)] <- format(anydate("1900-01-01"),"%Y-%m-%d")
    Recent_Sales$COGS <- r_inv$`Actual COG`[match(trimws(Recent_Sales$Key),trimws(r_inv$Key))]
    Recent_Sales$COGS[is.na(Recent_Sales$COGS)] <- 0
    Recent_Sales$Platform_Buy <- r_inv$`Purchased Via`[match(trimws(Recent_Sales$Key),trimws(r_inv$Key))]
    Recent_Sales$Platform_Buy[is.na(Recent_Sales$Platform_Buy)] <- "Bulk/Sealed"
    Recent_Sales$Scryfall <- Updated_Tracking_Keys$scryfall[match(Recent_Sales$param,Updated_Tracking_Keys$param)]
    
    return(Recent_Sales)
}
scryfall_api <- function(df){
    combined_attributes<- NULL
    for(i in 1:nrow(df)){
        scryfall_link <- paste("https://api.scryfall.com/cards/",df$Scryfall[i],sep="")
        scryfall <- GET(scryfall_link)
        card <- (content(scryfall,"parsed")$name)
        if(is.null(card) == T){card = NA}
        commander <- gsub("legal","E",(content(scryfall,"parsed")$legalities$commander))
        if(identical(commander,Character_Check)==T){commander <- "?"}
        if(is.null(commander) == T){commander = NA}
        type_line <- gsub(" .*","",gsub("//.*","",gsub("Legendary ","",gsub(" — .*","",(content(scryfall,"parsed")$type_line)))))
        if(is.null(type_line) == T){type_line = NA}
        if(identical(type_line,Character_Check)==T){type_line <- "?"}
        rarity <- (content(scryfall,"parsed")$rarity)
        if(is.null(rarity) == T){rarity = "NA"}
        color_1 <- tryCatch(expr = {content(scryfall,"parsed")$colors[[1]]}, error = function(e){color_1 = "NA"})
        color_2 <- tryCatch(expr = {content(scryfall,"parsed")$colors[[2]]}, error = function(e){color_2 = "NA"})
        color_3 <- tryCatch(expr = {content(scryfall,"parsed")$colors[[3]]}, error = function(e){color_3 = "NA"})
        color_4 <- tryCatch(expr = {content(scryfall,"parsed")$colors[[4]]}, error = function(e){color_4 = "NA"})
        color_5 <- tryCatch(expr = {content(scryfall,"parsed")$colors[[5]]}, error = function(e){color_5 = "NA"})
        color_6 <- tryCatch(expr = {content(scryfall,"parsed")$colors[[6]]}, error = function(e){color_6 = "NA"})
        if(is.null(color_1) == T){color_1 = "NA"}
        if(is.null(color_2) == T){color_2 = "NA"}
        if(is.null(color_3) == T){color_3 = "NA"}
        if(is.null(color_4) == T){color_4 = "NA"}
        if(is.null(color_5) == T){color_5 = "NA"}
        if(is.null(color_6) == T){color_6 = "NA"}
        if(color_2 != "NA"){final_color <- "Multi"}else if(color_1 == "G"){final_color <- "Green"}else if(color_1 == "R"){final_color <- "Red"}else if(color_1 == "W"){final_color <- "White"}else if(color_1 == "B"){final_color <- "Black"}else if(color_1 == "U"){final_color <- "Blue"}else{final_color <- "Brown"}
        if(rarity == "mythic"){rarity <- "M"}else if(rarity == "rare"){rarity <- "R"}else if(rarity == "uncommon"){rarity <- "U"}else if(rarity == "common"){rarity <- "C"}else{rarity <- "S"}
        attributes <- cbind(final_color,type_line,commander,rarity)
        combined_attributes <- rbind(combined_attributes,attributes)
        Sys.sleep(.12)
    }
    return(combined_attributes)
}
tcg_sales_scryfall_marriage <- function(Recent_Sales,combined_attributes){
    Recent_Sales <- cbind(Recent_Sales[-ncol(Recent_Sales)], combined_attributes)
    
    Recent_Sales_Export <- data.frame(Recent_Sales$name,
                                      as.numeric(as.character(Recent_Sales$Ind_Qty)),
                                      Recent_Sales$Foil,
                                      Recent_Sales$abbr,
                                      format(as.Date(as.character(Recent_Sales$DOP)),"%m/%d/%Y"),
                                      Recent_Sales$Platform_Buy,
                                      Recent_Sales$COGS,
                                      "",
                                      "",
                                      as.numeric(Recent_Sales$Ind_Qty),
                                      "",
                                      as.numeric(as.character(Recent_Sales$Ind_Amt)),
                                      "",
                                      "",
                                      Recent_Sales$Ind_Rev,
                                      "",
                                      "",
                                      format(as.Date(as.character(Recent_Sales$DOS)),"%m/%d/%Y"),
                                      Recent_Sales$final_color,
                                      Recent_Sales$type_line,
                                      Recent_Sales$commander,
                                      Recent_Sales$rarity
    )
    Recent_Sales_Export$X...2 <- ifelse(Recent_Sales$Direct_Fee == 0, "TCG", "TCG - Direct")
    Recent_Sales_Export$X...2 <- ifelse(Recent_Sales_Export$X...2 == "TCG" & Recent_Sales_Export$Recent_Sales.Ind_Amt <= 2.99, "TCG - Direct", Recent_Sales_Export$X...2 )
    Recent_Sales_Export$X...3 <- as.numeric(Recent_Sales$Base_Fee) + as.numeric(Recent_Sales$Direct_Fee)
    return(Recent_Sales)
}
ck_login_sales <- function(){
    remDr$navigate('https://cardkingdom.com/customer_login')
    username <- remDr$findElement('xpath','//*[@id="appWrapper"]/div[2]/div[2]/div[1]/form/div[1]/input')
    username$clickElement()
    username$sendKeysToElement(list("username"))
    Sys.sleep(.5)
    password <- remDr$findElement('xpath','//*[@id="appWrapper"]/div[2]/div[2]/div[1]/form/div[2]/input')
    password$clickElement()
    password$sendKeysToElement(list("password"))
    Sys.sleep(.5)
    remDr$findElement('xpath','//*[@id="appWrapper"]/div[2]/div[2]/div[1]/form/div[3]/button')$clickElement()
    Sys.sleep(.5)
    
    remDr$navigate('https://cardkingdom.com/myaccount/selling_history')
    
    last_buylist_date <- Sold_Ledger %>% select(`Platform Sold`,DOS) %>% mutate(CK_Check = (grepl(".*CK.*",`Platform Sold`))) %>% filter(CK_Check == T) %>% select(DOS) %>%arrange(desc(DOS)) %>% slice(1)
    
    status <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('table') %>% html_table(fill=T) %>% as.data.frame()
    colnames(status) <- status[1,]
    
    ck_sell_orders <- status[-1,] %>% filter(Status != "CANCELED") %>% 
        mutate(Status = gsub("[A-Za-z]+\n\\s*\\-*\\s*[A-Za-z]+\\s","",Status),
               `Order Date` = mdy(gsub("\\s\\d{2}\\:.*$","",`Order Date`))) %>%
        mutate(Status = suppressWarnings(mdy(Status))) %>%
        mutate(Status = as.Date(ifelse(is.na(Status), `Order Date` + 14, Status),origin = "1970-01-01" ),
               `Payment Method` = ifelse( grepl("STORECREDIT.*",`Payment Method`) == T,"CK-B","CK")) %>% 
        select(-`Mailing Address`) %>% filter(`Order Date` >= last_buylist_date$DOS)
    
    i = 1
    ck_sell_order <- NULL
    for(i in 1:length(ck_sell_orders$`Order ID`)){
        remDr$navigate(paste("https://cardkingdom.com/myaccount/po_invoice/",ck_sell_orders$`Order ID`[i],sep=""))
        order_contents <- data.frame(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('table') %>% html_table(fill = T)) %>% slice(-c(1:2)) %>%
            mutate(edition = ifelse((X1 == X2) & (X1 == X3),X1,NA)) %>% fill(edition) %>%
            filter(is.na(X2) != T) %>% filter(grepl("@",X2)==T) %>%
            separate(X2,c("X4","ind_value"),sep="\\s\\@\\s\\$") %>%
            separate(X4,c("qty","condition"),sep="\\s") %>%
            mutate(total_value = gsub("^\\$","",X3),
                   card_name = gsub("\\s*\\(.*\\)\\s*","",X1),
                   sell_id = ck_sell_orders$`Order ID`[i],
                   dos = ck_sell_orders$Status[i],
                   `Platform Sold` = ck_sell_orders$`Payment Method`[i],
                   hasFoil = ifelse(grepl("Foil",edition)==T,"FOIL",""),
                   edition = gsub(" Foil","",edition),
                   semi = paste(card_name,edition,sep="")) %>%
            select(semi,card_name,edition,hasFoil,qty,ind_value,total_value,sell_id,dos,`Platform Sold`)
        
        
        
        ck_sell_order <- rbind(ck_sell_order,order_contents)
        
    }
    Updated_Tracking_Keys$semi = paste(Updated_Tracking_Keys$name,Updated_Tracking_Keys$Set,sep="")
    ck_sell_order$Scryfall <- Updated_Tracking_Keys$scryfall[match(trimws(ck_sell_order$semi),Updated_Tracking_Keys$semi)]
    return(ck_sell_order)
}
tcg_purchases <- function(n = 2){
    remDr$navigate("https://store.tcgplayer.com/myaccount/orderhistory")
    Sys.sleep(1)
    username <- remDr$findElement("id","Email")
    Sys.sleep(sample(1:3, 1))
    username$clickElement()
    Sys.sleep(sample(1:3, 1))
    username$sendKeysToElement(list("username"))
    Sys.sleep(.5)
    Password <- remDr$findElement("id","Password")
    Sys.sleep(sample(1:3, 1))
    Password$clickElement()
    Sys.sleep(sample(1:3, 1))
    Password$sendKeysToElement(list("password"))
    Sys.sleep(.5)
    
    
    
    remDr$findElement("id",'loginButton')$clickElement()
    Sys.sleep(3)
    
    remDr$findElement("id","DateRange")$clickElement()
    Sys.sleep(1)
    remDr$findElement("xpath",paste('//*[@id="DateRange"]/option[',n,']',sep=""))$clickElement()
    #option[1] = 30 days
    #option[2] = 90 days
    #options[3] = 120 days
    #options[4] = All Time
    Sys.sleep(10)
    order_count <- as.numeric(gsub("\\s.*","",gsub(".*of\\s","",gsub("\n.*","",trimws(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="maincontentinnerpadding"]/div[1]/div/div/div[2]/div/div[4]/span') %>% html_text())))))
    
    All_Pages <- NULL
    for(i in 1:order_count){
        Order_Content_Raw <- data.frame(content = trimws(gsub("Sold by.*","",gsub("ITEMS","",trimws(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(".orderHistoryItems") %>% html_text())))))
        Order_Content_Raw <- data.frame(do.call('rbind', strsplit(as.character(Order_Content_Raw$content),'\n\n',fixed=TRUE)))
        colnames(Order_Content_Raw) <- c("name","set")
        Order_Content_Raw$abbrev <- Updated_Tracking_Keys$abbr[match(Order_Content_Raw$set,Updated_Tracking_Keys$Set)]
        
        Sys.sleep(1)
        
        Order_Details_Raw <- data.frame(very_raw = trimws(gsub(" - ",":",gsub("DETAILS","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(".orderHistoryDetail") %>% html_text()))))
        Order_Details_Raw <- data.frame(do.call('rbind', strsplit(as.character(Order_Details_Raw$very_raw),'\n',fixed=TRUE)))
        Order_Details_Raw <- suppressWarnings(Order_Details_Raw %>% separate(X1,c("R","rarity"),sep=": ") %>% separate(X2,c("Con","condition","language"),sep=":"))
        Order_Details_Raw$language[is.na(Order_Details_Raw$language)] <- "English"
        Order_Details_Raw$hasFoil <- ifelse(grepl("foil",tolower(Order_Details_Raw$condition)) == F,"","Foil")
        Sys.sleep(1)
        Order_Details <- Order_Details_Raw[c(2,6,4,5)]
        
        
        Order_Content <- data.frame(Order_Content_Raw,Order_Details)
        Order_Content$Key <- paste(Order_Content$name,Order_Content$set,Order_Content$rarity," ",Order_Content$hasFoil,sep="")
        Order_Content <- Order_Content[c(8,1,2,3,4,5,6,7)]
        
        Sys.sleep(1)
        Order_Purchase_Info <- NULL
        
        
        order_amount <- as.numeric(gsub(" order\\(.*","",gsub(".* of ","",trimws(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="maincontentinnerpadding"]/div[1]/div/div/div[2]/div/div[4]/span') %>% html_text()))))
        if(order_amount < 10){order_amount <- order_amount}else{order_amount <- 10}  
        for(j in 1:order_amount){
            Check <- str_count(trimws(gsub("\n\\s\\s+",":",gsub("DETAILS","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = paste('//*[@id="SellerOrderWidgetWrap"]/div[',j,']/div[2]/table') ) %>% html_text()))),"\\$")
            
            Summary <- data.frame( summary = gsub("\\(.*\\)","",gsub("\\$","",gsub(" ","",gsub("\n","",trimws(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = paste('//*[@id="SellerOrderWidgetWrap"]/div[',j,']/div[2]/span[1]/table/tbody/tr',sep="")) %>% html_text()))))))
            Summary_Content_Raw <- t(data.frame(do.call('rbind', strsplit(as.character(Summary$summary),':',fixed=TRUE))))
            colnames(Summary_Content_Raw) <- Summary_Content_Raw[1,]
            Summary_Content_Raw <- Summary_Content_Raw[-1,]
            
            if(Check == 1){Summary_Content_Raw <- as.data.frame(t(Summary_Content_Raw))}else{
                supplamental_pricing <- data.frame(very_raw = trimws(gsub("\\,","",gsub(":","",gsub("\n","",gsub("[A-Za-z]*","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = paste('//*[@id="SellerOrderWidgetWrap"]/div[',j,']/div[2]/table/tbody/tr',sep="")) %>% html_text()))))))
                supplamental_pricing$very_raw <- gsub("^-","",gsub(" ","-",gsub("'","",gsub("\\$","",gsub("   ","",supplamental_pricing$very_raw)))))
                supplamental_pricing <- data.frame(do.call('rbind', strsplit(as.character(supplamental_pricing$very_raw),'-',fixed=TRUE)))
                colnames(supplamental_pricing) <- c("X1","X2")
                supplamental_pricing$X1 <- trimws(supplamental_pricing$X1)
                Summary_Content_Raw <- as.data.frame(t(Summary_Content_Raw))
                for(k in 1:Check){
                    Summary_Content_Raw <- rbind(Summary_Content_Raw,Summary_Content_Raw[1,])
                }
                Summary_Content_Raw <- Summary_Content_Raw[-1,]
                Summary_Content_Raw[2] <- supplamental_pricing$X1
                Summary_Content_Raw[1] <- supplamental_pricing$X2
            }
            
            
            Order_Purchase_Info <- rbind(Order_Purchase_Info,mutate_all(as.data.frame(Summary_Content_Raw),function(x) as.character(x)))
        }
        rownames(Order_Purchase_Info) <- seq(nrow(Order_Purchase_Info))
        Order_Purchase_Info <- mutate_all(as.data.frame(Order_Purchase_Info),function(x) as.numeric(as.character(x)))
        
        Sys.sleep(1)
        
        Compiled_Dates <- NULL
        Compiled_order_Ids <- NULL
        for(j in 1:order_amount){
            Check <- str_count(trimws(gsub("\n\\s\\s+",":",gsub("DETAILS","",remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = paste('//*[@id="SellerOrderWidgetWrap"]/div[',j,']/div[2]/table') ) %>% html_text()))),"\\$")
            Dates_Raw <- trimws(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = paste('//*[@id="SellerOrderWidgetWrap"]/div[',j,']/div[1]/span[1]/span[2]',sep="")) %>% html_text())
            Order_id_Raw <- trimws(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = paste('//*[@id="SellerOrderWidgetWrap"]/div[',j,']/div[1]/span[3]/text()',sep="")) %>% html_text())[2]
            
            if(Check == 1){}else{
                for(k in 1:Check){
                    Dates_Raw <- rbind(Dates_Raw,Dates_Raw[1])
                    Order_id_Raw <- rbind(Order_id_Raw,Order_id_Raw[1])
                }
                Dates_Raw <- as.matrix(Dates_Raw[-1,1])
                Order_id_Raw <- as.matrix(Order_id_Raw[-1,1])
            }
            Compiled_Dates <- rbind(Compiled_Dates,Dates_Raw)
            Compiled_order_Ids <- rbind(Compiled_order_Ids,Order_id_Raw)
        }
        colnames(Compiled_Dates) <- "Purchase_Dates"
        rownames(Compiled_Dates) <- seq(nrow(Compiled_Dates))
        Compiled_Dates <- as.data.frame(Compiled_Dates)
        Compiled_Dates$Purchase_Dates <- format(mdy(Compiled_Dates$Purchase_Dates),"%m/%d/%Y")
        
        Compiled_order_Ids <- as.data.frame(Compiled_order_Ids)
        colnames(Compiled_order_Ids) <- "TCG_Order_Num"
        rownames(Compiled_order_Ids) <- seq(nrow(Compiled_order_Ids))
        
        Sys.sleep(1)
        
        Page_Contents <- data.frame(Order_Content,Order_Purchase_Info,Compiled_Dates,Compiled_order_Ids)
        
        All_Pages <- rbind(All_Pages,Page_Contents)
        remDr$findElement("class","nextPage")$clickElement()
        Sys.sleep(1)
        
    }
    
    TCG_Full_History <- All_Pages
    TCG_Full_History$condition <- gsub(" FOIL","", TCG_Full_History$condition)
    for(i in 1:nrow(TCG_Full_History)){
        if(TCG_Full_History$Shipping[i] != 0.00){TCG_Full_History$Shipping[i] <- round(TCG_Full_History$Shipping[i] / TCG_Full_History$Quantity[i],2)}
    }
    for(i in 1:nrow(TCG_Full_History)){
        if(TCG_Full_History$SalesTax[i] != 0.00){TCG_Full_History$SalesTax[i] <- round(TCG_Full_History$SalesTax[i] / TCG_Full_History$Quantity[i],2)}
    }
    TCG_Full_History$Subtotal <- round(TCG_Full_History$Subtotal + TCG_Full_History$Shipping + TCG_Full_History$SalesTax,2)
    TCG_Full_History$Purchased_Via <- "TCG"
    TCG_Full_History$Scryfall <- Updated_Tracking_Keys$scryfall[match(trimws(TCG_Full_History$Key),Updated_Tracking_Keys$Key)]
    return(TCG_Full_History)
}
ck_purchases <- function(){
    remDr$navigate('https://cardkingdom.com/customer_login')
    username <- remDr$findElement('xpath','//*[@id="appWrapper"]/div[2]/div[2]/div[1]/form/div[1]/input')
    username$clickElement()
    username$sendKeysToElement(list("username"))
    Sys.sleep(.5)
    password <- remDr$findElement('xpath','//*[@id="appWrapper"]/div[2]/div[2]/div[1]/form/div[2]/input')
    password$clickElement()
    password$sendKeysToElement(list("password"))
    Sys.sleep(.5)
    remDr$findElement('xpath','//*[@id="appWrapper"]/div[2]/div[2]/div[1]/form/div[3]/button')$clickElement()
    Sys.sleep(.5)
    
    remDr$navigate('https://cardkingdom.com/myaccount/order_history')
    #raw_invoices <- remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('a') %>% html_attr('href')
    #invoice_numbers <- gsub('/myaccount/invoice/','',raw_invoices[grepl('/myaccount/invoice/',raw_invoices)])
    status <- trimws(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes(xpath = '//*[@id="appWrapper"]/div[2]/div[3]/table/tbody/tr/td') %>% html_text())
    status <- gsub("\n.*","",status[!grepl('COMPLETED|CANCELED',status )])
    
    invoice_numbers <- status[grepl('^\\d+$',status)]
    purchase_dates_ck <- as.Date(mdy(gsub("\\s+\\d+\\:\\d+.*","",status[grepl('^[A-Za-z]{3}\\s',status)])))
    last_purchase_date <- r_inv %>% select(`Purchased Via`,DOP) %>% mutate(CK_Check = (grepl(".*CK.*",`Purchased Via`))) %>% filter(CK_Check == T) %>% select(DOP) %>%arrange(desc(DOP)) %>% slice(1)
    
    
    ck_orders <- data.frame(invoice = invoice_numbers, status = "SHIPPED", dates = purchase_dates_ck) %>% filter(dates >= last_purchase_date$DOP)
    
    ck_order <- NULL
    for(i in 1:length(ck_orders$invoice)){
        remDr$navigate(paste("https://cardkingdom.com/myaccount/invoice/",ck_orders$invoice[i],sep=""))
        order_contents <- data.frame(raw = trimws(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('tr') %>% html_text()))
        order_contents <- order_contents$raw[!grepl('Description Style Qty Price Total',order_contents$raw)]
        order_contents <- data.frame(raw = order_contents[!grepl('SINGLES|PRODUCT|Subtotal|Shipping|Sales Tax|Tax',order_contents)])
        final_amt <- as.numeric(gsub("Total USD \\$","",order_contents[nrow(order_contents),]))
        order_contents <- order_contents[-nrow(order_contents),]
        products <- gsub(":.*","",order_contents)
        products <- gsub(" \\(.*","",products)
        sets <- trimws(gsub("\\d+\\s\\$.*","",gsub(" Foil","",gsub("\\s[A-Z]{2}\\s.*","",gsub(".*:","",order_contents)))))
        
        condition <- suppressWarnings(if(grepl("\\s\\d{4}\\s",order_contents)==F){right(gsub("[a-z]+:*\\s","",gsub("\\s\\d+.*","",order_contents)),2)}else{
            left(gsub(".*\\d{4}\\s","",gsub("Foil ","",order_contents)),2)
        })
        
        language <- "English"
        qty <- str_match(gsub("\\s\\$.*","",order_contents),"\\d+$") 
        ind_amt <- as.numeric(gsub(".*\\$","",order_contents)) %>% as.data.frame() %>% replace(is.na(.),0)
        order_comp <- round(ind_amt/final_amt,2)
        add_fees <- final_amt-sum(ind_amt)
        order_comp <- round(order_comp * add_fees,2)
        ind_amt <- round(ind_amt + order_comp,2)
        DOP <- suppressWarnings(mdy(gsub("\\s\\d+\\:.*","",gsub(".*(S|s)hipped on ","",trimws(remDr$getPageSource() %>% .[[1]] %>% read_html() %>% html_nodes('xpath' = '//*[@id="appWrapper"]/div[2]/div[2]/div/div[4]') %>% html_text())))))
        raw_order <- data.frame(order_id = ck_orders$invoice[i],name = products,sets = sets, condition = condition, language = language,qty = qty, price = ind_amt, DOP = DOP,Purchased_Via = "CardKindom")
        raw_order$hasFoil = as.integer(grepl(" Foil",order_contents))
        raw_order$hasFoil = ifelse(as.numeric(raw_order$hasFoil) == 1, "FOIL","")
        raw_order$sets <- gsub(" Foil","",raw_order$sets)
        raw_order$sets <- gsub(" Drafter Booster Box","",gsub(" Bundle","",ifelse(grepl(" Bundle| Drafter Booster Box",raw_order$sets)==T,paste(raw_order$name,": ",raw_order$sets,sep=""),raw_order$sets)))
        raw_order$language <- ifelse(grepl("JPN",raw_order$name)==T,"Japanese","English")
        raw_order$sets <- gsub(" JPN.*","",raw_order$sets)
        raw_order$abbrev <- Updated_Tracking_Keys$abbr[match(raw_order$sets,Updated_Tracking_Keys$Set)]
        raw_order$rarity <- Updated_Tracking_Keys$Rarity[match(paste(raw_order$name,raw_order$sets,sep=""),paste(Updated_Tracking_Keys$name,Updated_Tracking_Keys$Set,sep=""))]
        raw_order$rarity[is.na(raw_order$rarity)] <- "S"
        raw_order$Key <- trimws(paste(raw_order$name,raw_order$sets,raw_order$rarity," ",raw_order$hasFoil,sep=""))
        order <- raw_order[c(1,13,2,3,11,12,10,4,5,6,7,8,9)] %>% filter(. != 0.00)
        colnames(order)[11] <- "price"
        ck_order <- rbind(ck_order,order)
        ck_order <- ck_order[!grepl("^S$",ck_order$Key),]
    }
    
    ck_order <- ck_order %>% filter(is.na(DOP)==F) %>% select(Key,name,sets,abbrev,rarity,hasFoil,condition,language,qty,price,DOP,order_id,Purchased_Via)
    
    ck_order$Scryfall <- Updated_Tracking_Keys$scryfall[match(trimws(ck_order$Key),Updated_Tracking_Keys$Key)]
    return(ck_order)
}