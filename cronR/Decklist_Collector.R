install.packages("tidyverse")
install.packages("rvest")
install.packages("assertthat")
library(tidyverse)
library(rvest)
library(assertthat)
library(devtools)
#devtools::install_github("tidyverse/googlesheets4")
library(googlesheets4)
library(googledrive)
library(gargle)
library(httr)
options(httr_oob_default=TRUE) 
options(gargle_oauth_email = "pachun95@gmail.com")
currentDate <- Sys.Date()


# for (i in 1:60){
# currentDate <- currentDate + 1
# Sys.sleep(10)
#Standard League####
Premium_WD_Part_1 <- "/home/cujo253/Reports/High Confidence Reps/"
Premium_WD_Part_2 <-  currentDate
Premium_WD_Part_3 <- "_Premium.csv"
URL_Part_1 <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/Standard-League-"
URL_Part_2 <- currentDate
URL_Part_3 <- paste(URL_Part_1,URL_Part_2,sep="")
url = URL_Part_3
tryCatch(
  data <- url %>% 
    read_html() %>% 
    html_nodes(".sorted-by-overview-container") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Deck_Number <- as.data.frame(data)
Deck_number <- nrow(Deck_Number)
Final_Export <- read_csv(paste(Premium_WD_Part_1,Premium_WD_Part_2,Premium_WD_Part_3,sep=""),col_types = cols(.default = "c"))

tryCatch(
  Card_Totals <- url %>% 
    read_html() %>% 
    html_nodes(".card-count") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Totals <- as.data.frame(Card_Totals)

tryCatch(
  Card_Name <- url %>% 
    read_html() %>% 
    html_nodes(".card-name") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Name <- as.data.frame(Card_Name)

Combined_Decklists <- tibble(Card_Totals,
                             Card_Name)
Combined_Decklists <- as.data.frame(Combined_Decklists)
names(Combined_Decklists) <- c("QTY","CARD")

Combined_Decklists <- as.data.frame(Combined_Decklists)
Combined_Decklists$QTY <- unlist(Combined_Decklists$QTY)
Combined_Decklists$QTY <- as.character(Combined_Decklists$QTY)
Combined_Decklists$QTY <- as.numeric(Combined_Decklists$QTY)
Combined_Decklists$CARD <- as.factor(unlist(Combined_Decklists$CARD))
Combined_Decklists <- as.data.frame(Combined_Decklists)

if(dim(Combined_Decklists)[1] != 0 ){
  Reduced_Decklists <- aggregate(. ~ CARD, data=Combined_Decklists, sum)
} else {
  Final_Decklist_Results <- 0
  Reduced_Decklists <- as.data.frame(0)
  print("No Results Were Found")
}
if(dim(Reduced_Decklists)[1] > 1){
  List <- Reduced_Decklists$CARD
  ABC <- NULL
  for(i in List){
    df_sub <- subset(Combined_Decklists, Combined_Decklists$CARD == i)
    df_sub<- as.data.frame(df_sub)
    A  <- round(mean(df_sub$QTY),2)
    ABC <- rbind(ABC,A)
  }
  Reduced_Decklists$Avg_Copies <- ABC
  Rarity_Check <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
  colnames(Rarity_Check) <- c("Key","name","Set","Rarity","Foil")
  Reduced_Decklists$Rarity <- Rarity_Check$Rarity[match(Reduced_Decklists$CARD,Rarity_Check$name)]
  Reduced_Decklists$Rarity <- as.character(Reduced_Decklists$Rarity)
  Reduced_Decklists$Rarity[is.na(Reduced_Decklists$Rarity)] = "Unknown"
  Reduced_Decklists <- as.data.frame(Reduced_Decklists)
  Reduced_Decklists$QTY <- as.numeric(Reduced_Decklists$QTY)
  #View(Reduced_Decklists)
  Reduced_Decklists$QTY_Cards_1 <- round(Reduced_Decklists$QTY / 4,1)
  Reduced_Decklists$QTY_Cards_Aux <- round(Reduced_Decklists$QTY / 3,1)
  Reduced_Decklists$QTY_Cards_1 <- as.character(Reduced_Decklists$QTY_Cards_1)
  Reduced_Decklists$QTY_Cards_Aux <- as.character (Reduced_Decklists$QTY_Cards_Aux)
  Reduced_Decklists$QTY_Cards <- ifelse(grepl("\\.\\d*$",Reduced_Decklists$QTY_Cards_1), Reduced_Decklists$QTY_Cards_Aux, Reduced_Decklists$QTY_Cards_1)
  Reduced_Decklists$QTY_Cards <- as.numeric(Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY_Lands <- ifelse(Reduced_Decklists$Rarity == "L", round(Reduced_Decklists$QTY / 3,0), Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY <- ifelse(Reduced_Decklists$Rarity == "L", Reduced_Decklists$QTY_Lands, Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY_Cards_1 <- Reduced_Decklists$QTY_Cards
  #View(Reduced_Decklists)
  Final_Decklist_Results <- Reduced_Decklists[,1:4]
  Final_Decklist_Results$Avg_Copies <- Final_Decklist_Results$Avg_Copies[,1]
  Final_Decklist_Results$Date <- currentDate
  Final_Decklist_Results$Sample_Size <- Deck_number
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Avg_Copies, -Final_Decklist_Results$QTY),]
  Final_Decklist_Results$Potential_Decks <- round(Final_Decklist_Results$QTY/Final_Decklist_Results$Avg_Copies,0)
  Final_Decklist_Results$QTY <- ifelse(Final_Decklist_Results$Potential_Decks < 1, (Final_Decklist_Results$QTY + 1) ,Final_Decklist_Results$QTY)
  Final_Decklist_Results$Potential_Decks <- ifelse(Final_Decklist_Results$Potential_Decks < 1, Final_Decklist_Results$Potential_Decks + 1, Final_Decklist_Results$Potential_Decks)
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Potential_Decks),]
  Final_Decklist_Results <- Final_Decklist_Results[ which(Final_Decklist_Results$Rarity !='Unknown'),]
  Final_Decklist_Results$Meta_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results$QTY_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results <- Final_Decklist_Results[c(5,6,1,4,8,9,2,3,7)]
  Final_Decklist_Results <- Final_Decklist_Results[order(Final_Decklist_Results$Meta_Rank),]
  Standard_Results <- Final_Decklist_Results
  
  setwd("/home/cujo253/Reports/Decklists/Standard Decklists")
  if(dim(Standard_Results)[1] != 0 ){
    ss <- drive_get("Decklists")
    #sheets_deauth()
    gs4_auth(email="pachun95@gmail.com", use_oob = T)
    sheet_write(
      Standard_Results,
      ss = "/d/1PMVRwPJNPFnpxkksHrgcDaSZFYm5B20SnBsb2gboCEw",
      sheet = "Standard_League"
    )
    csvFileName <- paste(currentDate,"_Standard_Deck_Lists",".csv",sep="")
    write.csv(Standard_Results, file=csvFileName, row.names = FALSE)
  } else {
    print("No Standard Leagues Were logged Today")
  }
} else {
  print("No Standard Leagues Release Today")
}

#Pioneer League####
Premium_WD_Part_1 <- "/home/cujo253/Reports/High Confidence Reps/"
Premium_WD_Part_2 <-  currentDate
Premium_WD_Part_3 <- "_Premium.csv"
URL_Part_1 <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/Pioneer-League-"
URL_Part_2 <- currentDate
URL_Part_3 <- paste(URL_Part_1,URL_Part_2,sep="")
url = URL_Part_3
tryCatch(
 data <- url %>% 
    read_html() %>% 
    html_nodes(".sorted-by-overview-container") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Deck_Number <- as.data.frame(data)
Deck_number <- nrow(Deck_Number)
Final_Export <- read_csv(paste(Premium_WD_Part_1,Premium_WD_Part_2,Premium_WD_Part_3,sep=""),col_types = cols(.default = "c"))

tryCatch(
  Card_Totals <- url %>% 
    read_html() %>% 
    html_nodes(".card-count") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Totals <- as.data.frame(Card_Totals)

tryCatch(
  Card_Name <- url %>% 
    read_html() %>% 
    html_nodes(".card-name") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Name <- as.data.frame(Card_Name)

Combined_Decklists <- tibble(Card_Totals,
                             Card_Name)
Combined_Decklists <- as.data.frame(Combined_Decklists)
names(Combined_Decklists) <- c("QTY","CARD")

Combined_Decklists <- as.data.frame(Combined_Decklists)
Combined_Decklists$QTY <- unlist(Combined_Decklists$QTY)
Combined_Decklists$QTY <- as.character(Combined_Decklists$QTY)
Combined_Decklists$QTY <- as.numeric(Combined_Decklists$QTY)
Combined_Decklists$CARD <- as.factor(unlist(Combined_Decklists$CARD))
Combined_Decklists <- as.data.frame(Combined_Decklists)

if(dim(Combined_Decklists)[1] != 0 ){
  Reduced_Decklists <- aggregate(. ~ CARD, data=Combined_Decklists, sum)
} else {
  Final_Decklist_Results <- 0
  Reduced_Decklists <- as.data.frame(0)
  print("No Results Were Found")
}
if(dim(Reduced_Decklists)[1] > 1){
List <- Reduced_Decklists$CARD
ABC <- NULL
for(i in List){
  df_sub <- subset(Combined_Decklists, Combined_Decklists$CARD == i)
  df_sub<- as.data.frame(df_sub)
  A  <- round(mean(df_sub$QTY),2)
  ABC <- rbind(ABC,A)
}
Reduced_Decklists$Avg_Copies <- ABC
Rarity_Check <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
colnames(Rarity_Check) <- c("Key","name","Set","Rarity","Foil")
Reduced_Decklists$Rarity <- Rarity_Check$Rarity[match(Reduced_Decklists$CARD,Rarity_Check$name)]
Reduced_Decklists$Rarity <- as.character(Reduced_Decklists$Rarity)
Reduced_Decklists$Rarity[is.na(Reduced_Decklists$Rarity)] = "Unknown"
Reduced_Decklists <- as.data.frame(Reduced_Decklists)
Reduced_Decklists$QTY <- as.numeric(Reduced_Decklists$QTY)
#View(Reduced_Decklists)
Reduced_Decklists$QTY_Cards_1 <- round(Reduced_Decklists$QTY / 4,1)
Reduced_Decklists$QTY_Cards_Aux <- round(Reduced_Decklists$QTY / 3,1)
Reduced_Decklists$QTY_Cards_1 <- as.character(Reduced_Decklists$QTY_Cards_1)
Reduced_Decklists$QTY_Cards_Aux <- as.character (Reduced_Decklists$QTY_Cards_Aux)
Reduced_Decklists$QTY_Cards <- ifelse(grepl("\\.\\d*$",Reduced_Decklists$QTY_Cards_1), Reduced_Decklists$QTY_Cards_Aux, Reduced_Decklists$QTY_Cards_1)
Reduced_Decklists$QTY_Cards <- as.numeric(Reduced_Decklists$QTY_Cards)
Reduced_Decklists$QTY_Lands <- ifelse(Reduced_Decklists$Rarity == "L", round(Reduced_Decklists$QTY / 3,0), Reduced_Decklists$QTY_Cards)
Reduced_Decklists$QTY <- ifelse(Reduced_Decklists$Rarity == "L", Reduced_Decklists$QTY_Lands, Reduced_Decklists$QTY_Cards)
Reduced_Decklists$QTY_Cards_1 <- Reduced_Decklists$QTY_Cards
#View(Reduced_Decklists)
Final_Decklist_Results <- Reduced_Decklists[,1:4]
Final_Decklist_Results$Avg_Copies <- Final_Decklist_Results$Avg_Copies[,1]
Final_Decklist_Results$Date <- currentDate
Final_Decklist_Results$Sample_Size <- Deck_number
Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Avg_Copies, -Final_Decklist_Results$QTY),]
Final_Decklist_Results$Potential_Decks <- round(Final_Decklist_Results$QTY/Final_Decklist_Results$Avg_Copies,0)
Final_Decklist_Results$QTY <- ifelse(Final_Decklist_Results$Potential_Decks < 1, (Final_Decklist_Results$QTY + 1) ,Final_Decklist_Results$QTY)
Final_Decklist_Results$Potential_Decks <- ifelse(Final_Decklist_Results$Potential_Decks < 1, Final_Decklist_Results$Potential_Decks + 1, Final_Decklist_Results$Potential_Decks)
Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Potential_Decks),]
Final_Decklist_Results <- Final_Decklist_Results[ which(Final_Decklist_Results$Rarity !='Unknown'),]
Final_Decklist_Results$Meta_Rank <- seq.int(nrow(Final_Decklist_Results))
Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
Final_Decklist_Results$QTY_Rank <- seq.int(nrow(Final_Decklist_Results))
Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
Final_Decklist_Results <- Final_Decklist_Results[c(5,6,1,4,8,9,2,3,7)]
Final_Decklist_Results <- Final_Decklist_Results[order(Final_Decklist_Results$Meta_Rank),]
Pioneer_Results <- Final_Decklist_Results

setwd("/home/cujo253/Reports/Decklists/Pioneer Decklists")
if(dim(Pioneer_Results)[1] != 0 ){
  ss <- drive_get("Decklists")
  #sheets_deauth()
  gs4_auth(email="pachun95@gmail.com", use_oob = T)
  sheet_write(
    Pioneer_Results,
    ss = "/d/1PMVRwPJNPFnpxkksHrgcDaSZFYm5B20SnBsb2gboCEw",
    sheet = "Pioneer_League"
  )
  csvFileName <- paste(currentDate,"_Pioneer_Deck_Lists",".csv",sep="")
  write.csv(Pioneer_Results, file=csvFileName, row.names = FALSE)
} else {
  print("No Pioneer Leagues Were logged Today")
}
} else {
  print("No Pioneer Leagues Release Today")
}

#Modern League####
Premium_WD_Part_1 <- "/home/cujo253/Reports/High Confidence Reps/"
Premium_WD_Part_2 <-  currentDate
Premium_WD_Part_3 <- "_Premium.csv"
URL_Part_1 <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/Modern-League-"
URL_Part_2 <- currentDate
URL_Part_3 <- paste(URL_Part_1,URL_Part_2,sep="")
url = URL_Part_3
tryCatch(
  data <- url %>% 
    read_html() %>% 
    html_nodes(".sorted-by-overview-container") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Deck_Number <- as.data.frame(data)
Deck_number <- nrow(Deck_Number)
Final_Export <- read_csv(paste(Premium_WD_Part_1,Premium_WD_Part_2,Premium_WD_Part_3,sep=""),col_types = cols(.default = "c"))

tryCatch(
  Card_Totals <- url %>% 
    read_html() %>% 
    html_nodes(".card-count") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Totals <- as.data.frame(Card_Totals)

tryCatch(
  Card_Name <- url %>% 
    read_html() %>% 
    html_nodes(".card-name") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Name <- as.data.frame(Card_Name)

Combined_Decklists <- tibble(Card_Totals,
                             Card_Name)
Combined_Decklists <- as.data.frame(Combined_Decklists)
names(Combined_Decklists) <- c("QTY","CARD")

Combined_Decklists <- as.data.frame(Combined_Decklists)
Combined_Decklists$QTY <- unlist(Combined_Decklists$QTY)
Combined_Decklists$QTY <- as.character(Combined_Decklists$QTY)
Combined_Decklists$QTY <- as.numeric(Combined_Decklists$QTY)
Combined_Decklists$CARD <- as.factor(unlist(Combined_Decklists$CARD))
Combined_Decklists <- as.data.frame(Combined_Decklists)

if(dim(Combined_Decklists)[1] != 0 ){
  Reduced_Decklists <- aggregate(. ~ CARD, data=Combined_Decklists, sum)
} else {
  Final_Decklist_Results <- 0
  Reduced_Decklists <- as.data.frame(0)
  print("No Results Were Found")
}
if(dim(Reduced_Decklists)[1] > 1){
  List <- Reduced_Decklists$CARD
  ABC <- NULL
  for(i in List){
    df_sub <- subset(Combined_Decklists, Combined_Decklists$CARD == i)
    df_sub<- as.data.frame(df_sub)
    A  <- round(mean(df_sub$QTY),2)
    ABC <- rbind(ABC,A)
  }
  Reduced_Decklists$Avg_Copies <- ABC
  Rarity_Check <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
  colnames(Rarity_Check) <- c("Key","name","Set","Rarity","Foil")
  Reduced_Decklists$Rarity <- Rarity_Check$Rarity[match(Reduced_Decklists$CARD,Rarity_Check$name)]
  Reduced_Decklists$Rarity <- as.character(Reduced_Decklists$Rarity)
  Reduced_Decklists$Rarity[is.na(Reduced_Decklists$Rarity)] = "Unknown"
  Reduced_Decklists <- as.data.frame(Reduced_Decklists)
  Reduced_Decklists$QTY <- as.numeric(Reduced_Decklists$QTY)
  #View(Reduced_Decklists)
  Reduced_Decklists$QTY_Cards_1 <- round(Reduced_Decklists$QTY / 4,1)
  Reduced_Decklists$QTY_Cards_Aux <- round(Reduced_Decklists$QTY / 3,1)
  Reduced_Decklists$QTY_Cards_1 <- as.character(Reduced_Decklists$QTY_Cards_1)
  Reduced_Decklists$QTY_Cards_Aux <- as.character (Reduced_Decklists$QTY_Cards_Aux)
  Reduced_Decklists$QTY_Cards <- ifelse(grepl("\\.\\d*$",Reduced_Decklists$QTY_Cards_1), Reduced_Decklists$QTY_Cards_Aux, Reduced_Decklists$QTY_Cards_1)
  Reduced_Decklists$QTY_Cards <- as.numeric(Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY_Lands <- ifelse(Reduced_Decklists$Rarity == "L", round(Reduced_Decklists$QTY / 3,0), Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY <- ifelse(Reduced_Decklists$Rarity == "L", Reduced_Decklists$QTY_Lands, Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY_Cards_1 <- Reduced_Decklists$QTY_Cards
  #View(Reduced_Decklists)
  Final_Decklist_Results <- Reduced_Decklists[,1:4]
  Final_Decklist_Results$Avg_Copies <- Final_Decklist_Results$Avg_Copies[,1]
  Final_Decklist_Results$Date <- currentDate
  Final_Decklist_Results$Sample_Size <- Deck_number
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Avg_Copies, -Final_Decklist_Results$QTY),]
  Final_Decklist_Results$Potential_Decks <- round(Final_Decklist_Results$QTY/Final_Decklist_Results$Avg_Copies,0)
  Final_Decklist_Results$QTY <- ifelse(Final_Decklist_Results$Potential_Decks < 1, (Final_Decklist_Results$QTY + 1) ,Final_Decklist_Results$QTY)
  Final_Decklist_Results$Potential_Decks <- ifelse(Final_Decklist_Results$Potential_Decks < 1, Final_Decklist_Results$Potential_Decks + 1, Final_Decklist_Results$Potential_Decks)
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Potential_Decks),]
  Final_Decklist_Results <- Final_Decklist_Results[ which(Final_Decklist_Results$Rarity !='Unknown'),]
  Final_Decklist_Results$Meta_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results$QTY_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results <- Final_Decklist_Results[c(5,6,1,4,8,9,2,3,7)]
  Final_Decklist_Results <- Final_Decklist_Results[order(Final_Decklist_Results$Meta_Rank),]
  Modern_Results <- Final_Decklist_Results
  
  setwd("/home/cujo253/Reports/Decklists/Modern Decklists")
  if(dim(Modern_Results)[1] != 0 ){
    ss <- drive_get("Decklists")
    #sheets_deauth()
    gs4_auth(email="pachun95@gmail.com", use_oob = T)
    sheet_write(
      Modern_Results,
      ss = "/d/1PMVRwPJNPFnpxkksHrgcDaSZFYm5B20SnBsb2gboCEw",
      sheet = "Modern_League"
    )
    csvFileName <- paste(currentDate,"_Modern_Deck_Lists",".csv",sep="")
    write.csv(Modern_Results, file=csvFileName, row.names = FALSE)
  } else {
    print("No Modern Leagues Were logged Today")
  }
} else {
  print("No Modern Leagues Release Today")
}

#Legacy League####
Premium_WD_Part_1 <- "/home/cujo253/Reports/High Confidence Reps/"
Premium_WD_Part_2 <-  currentDate
Premium_WD_Part_3 <- "_Premium.csv"
URL_Part_1 <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/Legacy-League-"
URL_Part_2 <- currentDate
URL_Part_3 <- paste(URL_Part_1,URL_Part_2,sep="")
url = URL_Part_3
tryCatch(
  data <- url %>% 
    read_html() %>% 
    html_nodes(".sorted-by-overview-container") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Deck_Number <- as.data.frame(data)
Deck_number <- nrow(Deck_Number)
Final_Export <- read_csv(paste(Premium_WD_Part_1,Premium_WD_Part_2,Premium_WD_Part_3,sep=""),col_types = cols(.default = "c"))

tryCatch(
  Card_Totals <- url %>% 
    read_html() %>% 
    html_nodes(".card-count") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Totals <- as.data.frame(Card_Totals)

tryCatch(
  Card_Name <- url %>% 
    read_html() %>% 
    html_nodes(".card-name") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Name <- as.data.frame(Card_Name)

Combined_Decklists <- tibble(Card_Totals,
                             Card_Name)
Combined_Decklists <- as.data.frame(Combined_Decklists)
names(Combined_Decklists) <- c("QTY","CARD")

Combined_Decklists <- as.data.frame(Combined_Decklists)
Combined_Decklists$QTY <- unlist(Combined_Decklists$QTY)
Combined_Decklists$QTY <- as.character(Combined_Decklists$QTY)
Combined_Decklists$QTY <- as.numeric(Combined_Decklists$QTY)
Combined_Decklists$CARD <- as.factor(unlist(Combined_Decklists$CARD))
Combined_Decklists <- as.data.frame(Combined_Decklists)

if(dim(Combined_Decklists)[1] != 0 ){
  Reduced_Decklists <- aggregate(. ~ CARD, data=Combined_Decklists, sum)
} else {
  Final_Decklist_Results <- 0
  Reduced_Decklists <- as.data.frame(0)
  print("No Results Were Found")
}
if(dim(Reduced_Decklists)[1] > 1){
  List <- Reduced_Decklists$CARD
  ABC <- NULL
  for(i in List){
    df_sub <- subset(Combined_Decklists, Combined_Decklists$CARD == i)
    df_sub<- as.data.frame(df_sub)
    A  <- round(mean(df_sub$QTY),2)
    ABC <- rbind(ABC,A)
  }
  Reduced_Decklists$Avg_Copies <- ABC
  Rarity_Check <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
  colnames(Rarity_Check) <- c("Key","name","Set","Rarity","Foil")
  Reduced_Decklists$Rarity <- Rarity_Check$Rarity[match(Reduced_Decklists$CARD,Rarity_Check$name)]
  Reduced_Decklists$Rarity <- as.character(Reduced_Decklists$Rarity)
  Reduced_Decklists$Rarity[is.na(Reduced_Decklists$Rarity)] = "Unknown"
  Reduced_Decklists <- as.data.frame(Reduced_Decklists)
  Reduced_Decklists$QTY <- as.numeric(Reduced_Decklists$QTY)
  #View(Reduced_Decklists)
  Reduced_Decklists$QTY_Cards_1 <- round(Reduced_Decklists$QTY / 4,1)
  Reduced_Decklists$QTY_Cards_Aux <- round(Reduced_Decklists$QTY / 3,1)
  Reduced_Decklists$QTY_Cards_1 <- as.character(Reduced_Decklists$QTY_Cards_1)
  Reduced_Decklists$QTY_Cards_Aux <- as.character (Reduced_Decklists$QTY_Cards_Aux)
  Reduced_Decklists$QTY_Cards <- ifelse(grepl("\\.\\d*$",Reduced_Decklists$QTY_Cards_1), Reduced_Decklists$QTY_Cards_Aux, Reduced_Decklists$QTY_Cards_1)
  Reduced_Decklists$QTY_Cards <- as.numeric(Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY_Lands <- ifelse(Reduced_Decklists$Rarity == "L", round(Reduced_Decklists$QTY / 3,0), Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY <- ifelse(Reduced_Decklists$Rarity == "L", Reduced_Decklists$QTY_Lands, Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY_Cards_1 <- Reduced_Decklists$QTY_Cards
  #View(Reduced_Decklists)
  Final_Decklist_Results <- Reduced_Decklists[,1:4]
  Final_Decklist_Results$Avg_Copies <- Final_Decklist_Results$Avg_Copies[,1]
  Final_Decklist_Results$Date <- currentDate
  Final_Decklist_Results$Sample_Size <- Deck_number
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Avg_Copies, -Final_Decklist_Results$QTY),]
  Final_Decklist_Results$Potential_Decks <- round(Final_Decklist_Results$QTY/Final_Decklist_Results$Avg_Copies,0)
  Final_Decklist_Results$QTY <- ifelse(Final_Decklist_Results$Potential_Decks < 1, (Final_Decklist_Results$QTY + 1) ,Final_Decklist_Results$QTY)
  Final_Decklist_Results$Potential_Decks <- ifelse(Final_Decklist_Results$Potential_Decks < 1, Final_Decklist_Results$Potential_Decks + 1, Final_Decklist_Results$Potential_Decks)
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Potential_Decks),]
  Final_Decklist_Results <- Final_Decklist_Results[ which(Final_Decklist_Results$Rarity !='Unknown'),]
  Final_Decklist_Results$Meta_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results$QTY_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results <- Final_Decklist_Results[c(5,6,1,4,8,9,2,3,7)]
  Final_Decklist_Results <- Final_Decklist_Results[order(Final_Decklist_Results$Meta_Rank),]
  Legacy_Results <- Final_Decklist_Results
  
  setwd("/home/cujo253/Reports/Decklists/Legacy Decklists")
  if(dim(Legacy_Results)[1] != 0 ){
    ss <- drive_get("Decklists")
    #sheets_deauth()
    gs4_auth(email="pachun95@gmail.com", use_oob = T)
    sheet_write(
      Legacy_Results,
      ss = "/d/1PMVRwPJNPFnpxkksHrgcDaSZFYm5B20SnBsb2gboCEw",
      sheet = "Legacy_League"
    )
    
    csvFileName <- paste(currentDate,"_Legacy_Deck_Lists",".csv",sep="")
    write.csv(Legacy_Results, file=csvFileName, row.names = FALSE)
  } else {
    print("No Legacy Leagues Were logged Today")
  }
} else {
  print("No Legacy Leagues Release Today")
}

#Vintage League####
Premium_WD_Part_1 <- "/home/cujo253/Reports/High Confidence Reps/"
Premium_WD_Part_2 <-  currentDate
Premium_WD_Part_3 <- "_Premium.csv"
URL_Part_1 <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/Vintage-League-"
URL_Part_2 <- currentDate
URL_Part_3 <- paste(URL_Part_1,URL_Part_2,sep="")
url = URL_Part_3
tryCatch(
  data <- url %>% 
    read_html() %>% 
    html_nodes(".sorted-by-overview-container") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Deck_Number <- as.data.frame(data)
Deck_number <- nrow(Deck_Number)
Final_Export <- read_csv(paste(Premium_WD_Part_1,Premium_WD_Part_2,Premium_WD_Part_3,sep=""),col_types = cols(.default = "c"))

tryCatch(
  Card_Totals <- url %>% 
    read_html() %>% 
    html_nodes(".card-count") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Totals <- as.data.frame(Card_Totals)

tryCatch(
  Card_Name <- url %>% 
    read_html() %>% 
    html_nodes(".card-name") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Name <- as.data.frame(Card_Name)

Combined_Decklists <- tibble(Card_Totals,
                             Card_Name)
Combined_Decklists <- as.data.frame(Combined_Decklists)
names(Combined_Decklists) <- c("QTY","CARD")

Combined_Decklists <- as.data.frame(Combined_Decklists)
Combined_Decklists$QTY <- unlist(Combined_Decklists$QTY)
Combined_Decklists$QTY <- as.character(Combined_Decklists$QTY)
Combined_Decklists$QTY <- as.numeric(Combined_Decklists$QTY)
Combined_Decklists$CARD <- as.factor(unlist(Combined_Decklists$CARD))
Combined_Decklists <- as.data.frame(Combined_Decklists)

if(dim(Combined_Decklists)[1] != 0 ){
  Reduced_Decklists <- aggregate(. ~ CARD, data=Combined_Decklists, sum)
} else {
  Final_Decklist_Results <- 0
  Reduced_Decklists <- as.data.frame(0)
  print("No Results Were Found")
}
if(dim(Reduced_Decklists)[1] > 1){
  List <- Reduced_Decklists$CARD
  ABC <- NULL
  for(i in List){
    df_sub <- subset(Combined_Decklists, Combined_Decklists$CARD == i)
    df_sub<- as.data.frame(df_sub)
    A  <- round(mean(df_sub$QTY),2)
    ABC <- rbind(ABC,A)
  }
  Reduced_Decklists$Avg_Copies <- ABC
  Rarity_Check <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
  colnames(Rarity_Check) <- c("Key","name","Set","Rarity","Foil")
  Reduced_Decklists$Rarity <- Rarity_Check$Rarity[match(Reduced_Decklists$CARD,Rarity_Check$name)]
  Reduced_Decklists$Rarity <- as.character(Reduced_Decklists$Rarity)
  Reduced_Decklists$Rarity[is.na(Reduced_Decklists$Rarity)] = "Unknown"
  Reduced_Decklists <- as.data.frame(Reduced_Decklists)
  Reduced_Decklists$QTY <- as.numeric(Reduced_Decklists$QTY)
  #View(Reduced_Decklists)
  Reduced_Decklists$QTY_Cards_1 <- round(Reduced_Decklists$QTY / 4,1)
  Reduced_Decklists$QTY_Cards_Aux <- round(Reduced_Decklists$QTY / 3,1)
  Reduced_Decklists$QTY_Cards_1 <- as.character(Reduced_Decklists$QTY_Cards_1)
  Reduced_Decklists$QTY_Cards_Aux <- as.character (Reduced_Decklists$QTY_Cards_Aux)
  Reduced_Decklists$QTY_Cards <- ifelse(grepl("\\.\\d*$",Reduced_Decklists$QTY_Cards_1), Reduced_Decklists$QTY_Cards_Aux, Reduced_Decklists$QTY_Cards_1)
  Reduced_Decklists$QTY_Cards <- as.numeric(Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY_Lands <- ifelse(Reduced_Decklists$Rarity == "L", round(Reduced_Decklists$QTY / 3,0), Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY <- ifelse(Reduced_Decklists$Rarity == "L", Reduced_Decklists$QTY_Lands, Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY_Cards_1 <- Reduced_Decklists$QTY_Cards
  #View(Reduced_Decklists)
  Final_Decklist_Results <- Reduced_Decklists[,1:4]
  Final_Decklist_Results$Avg_Copies <- Final_Decklist_Results$Avg_Copies[,1]
  Final_Decklist_Results$Date <- currentDate
  Final_Decklist_Results$Sample_Size <- Deck_number
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Avg_Copies, -Final_Decklist_Results$QTY),]
  Final_Decklist_Results$Potential_Decks <- round(Final_Decklist_Results$QTY/Final_Decklist_Results$Avg_Copies,0)
  Final_Decklist_Results$QTY <- ifelse(Final_Decklist_Results$Potential_Decks < 1, (Final_Decklist_Results$QTY + 1) ,Final_Decklist_Results$QTY)
  Final_Decklist_Results$Potential_Decks <- ifelse(Final_Decklist_Results$Potential_Decks < 1, Final_Decklist_Results$Potential_Decks + 1, Final_Decklist_Results$Potential_Decks)
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Potential_Decks),]
  Final_Decklist_Results <- Final_Decklist_Results[ which(Final_Decklist_Results$Rarity !='Unknown'),]
  Final_Decklist_Results$Meta_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results$QTY_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results <- Final_Decklist_Results[c(5,6,1,4,8,9,2,3,7)]
  Final_Decklist_Results <- Final_Decklist_Results[order(Final_Decklist_Results$Meta_Rank),]
  Vintage_Results <- Final_Decklist_Results
  
  setwd("/home/cujo253/Reports/Decklists/Vintage Decklists")
  if(dim(Vintage_Results)[1] != 0 ){
    ss <- drive_get("Decklists")
    #sheets_deauth()
    gs4_auth(email="pachun95@gmail.com", use_oob = T)
    sheet_write(
      Vintage_Results,
      ss = "/d/1PMVRwPJNPFnpxkksHrgcDaSZFYm5B20SnBsb2gboCEw",
      sheet = "Vintage_League"
    )
    
    csvFileName <- paste(currentDate,"_Vintage_Deck_Lists",".csv",sep="")
    write.csv(Vintage_Results, file=csvFileName, row.names = FALSE)
  } else {
    print("No Vintage Leagues Were logged Today")
  }
} else {
  print("No Vintage Leagues Release Today")
}


#Pauper League####
Premium_WD_Part_1 <- "/home/cujo253/Reports/High Confidence Reps/"
Premium_WD_Part_2 <-  currentDate
Premium_WD_Part_3 <- "_Premium.csv"
URL_Part_1 <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/Pauper-League-"
URL_Part_2 <- currentDate
URL_Part_3 <- paste(URL_Part_1,URL_Part_2,sep="")
url = URL_Part_3
tryCatch(
  data <- url %>% 
    read_html() %>% 
    html_nodes(".sorted-by-overview-container") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Deck_Number <- as.data.frame(data)
Deck_number <- nrow(Deck_Number)
Final_Export <- read_csv(paste(Premium_WD_Part_1,Premium_WD_Part_2,Premium_WD_Part_3,sep=""),col_types = cols(.default = "c"))

tryCatch(
  Card_Totals <- url %>% 
    read_html() %>% 
    html_nodes(".card-count") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Totals <- as.data.frame(Card_Totals)

tryCatch(
  Card_Name <- url %>% 
    read_html() %>% 
    html_nodes(".card-name") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Name <- as.data.frame(Card_Name)

Combined_Decklists <- tibble(Card_Totals,
                             Card_Name)
Combined_Decklists <- as.data.frame(Combined_Decklists)
names(Combined_Decklists) <- c("QTY","CARD")

Combined_Decklists <- as.data.frame(Combined_Decklists)
Combined_Decklists$QTY <- unlist(Combined_Decklists$QTY)
Combined_Decklists$QTY <- as.character(Combined_Decklists$QTY)
Combined_Decklists$QTY <- as.numeric(Combined_Decklists$QTY)
Combined_Decklists$CARD <- as.factor(unlist(Combined_Decklists$CARD))
Combined_Decklists <- as.data.frame(Combined_Decklists)

if(dim(Combined_Decklists)[1] != 0 ){
  Reduced_Decklists <- aggregate(. ~ CARD, data=Combined_Decklists, sum)
} else {
  Final_Decklist_Results <- 0
  Reduced_Decklists <- as.data.frame(0)
  print("No Results Were Found")
}
if(dim(Reduced_Decklists)[1] > 1){
  List <- Reduced_Decklists$CARD
  ABC <- NULL
  for(i in List){
    df_sub <- subset(Combined_Decklists, Combined_Decklists$CARD == i)
    df_sub<- as.data.frame(df_sub)
    A  <- round(mean(df_sub$QTY),2)
    ABC <- rbind(ABC,A)
  }
  Reduced_Decklists$Avg_Copies <- ABC
  Rarity_Check <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
  colnames(Rarity_Check) <- c("Key","name","Set","Rarity","Foil")
  Reduced_Decklists$Rarity <- Rarity_Check$Rarity[match(Reduced_Decklists$CARD,Rarity_Check$name)]
  Reduced_Decklists$Rarity <- as.character(Reduced_Decklists$Rarity)
  Reduced_Decklists$Rarity[is.na(Reduced_Decklists$Rarity)] = "Unknown"
  Reduced_Decklists <- as.data.frame(Reduced_Decklists)
  Reduced_Decklists$QTY <- as.numeric(Reduced_Decklists$QTY)
  #View(Reduced_Decklists)
  Reduced_Decklists$QTY_Cards_1 <- round(Reduced_Decklists$QTY / 4,1)
  Reduced_Decklists$QTY_Cards_Aux <- round(Reduced_Decklists$QTY / 3,1)
  Reduced_Decklists$QTY_Cards_1 <- as.character(Reduced_Decklists$QTY_Cards_1)
  Reduced_Decklists$QTY_Cards_Aux <- as.character (Reduced_Decklists$QTY_Cards_Aux)
  Reduced_Decklists$QTY_Cards <- ifelse(grepl("\\.\\d*$",Reduced_Decklists$QTY_Cards_1), Reduced_Decklists$QTY_Cards_Aux, Reduced_Decklists$QTY_Cards_1)
  Reduced_Decklists$QTY_Cards <- as.numeric(Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY_Lands <- ifelse(Reduced_Decklists$Rarity == "L", round(Reduced_Decklists$QTY / 3,0), Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY <- ifelse(Reduced_Decklists$Rarity == "L", Reduced_Decklists$QTY_Lands, Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY_Cards_1 <- Reduced_Decklists$QTY_Cards
  #View(Reduced_Decklists)
  Final_Decklist_Results <- Reduced_Decklists[,1:4]
  Final_Decklist_Results$Avg_Copies <- Final_Decklist_Results$Avg_Copies[,1]
  Final_Decklist_Results$Date <- currentDate
  Final_Decklist_Results$Sample_Size <- Deck_number
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Avg_Copies, -Final_Decklist_Results$QTY),]
  Final_Decklist_Results$Potential_Decks <- round(Final_Decklist_Results$QTY/Final_Decklist_Results$Avg_Copies,0)
  Final_Decklist_Results$QTY <- ifelse(Final_Decklist_Results$Potential_Decks < 1, (Final_Decklist_Results$QTY + 1) ,Final_Decklist_Results$QTY)
  Final_Decklist_Results$Potential_Decks <- ifelse(Final_Decklist_Results$Potential_Decks < 1, Final_Decklist_Results$Potential_Decks + 1, Final_Decklist_Results$Potential_Decks)
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Potential_Decks),]
  Final_Decklist_Results <- Final_Decklist_Results[ which(Final_Decklist_Results$Rarity !='Unknown'),]
  Final_Decklist_Results$Meta_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results$QTY_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results <- Final_Decklist_Results[c(5,6,1,4,8,9,2,3,7)]
  Final_Decklist_Results <- Final_Decklist_Results[order(Final_Decklist_Results$Meta_Rank),]
  Pauper_Results <- Final_Decklist_Results
  
  setwd("/home/cujo253/Reports/Decklists/Pauper Decklists")
  if(dim(Pauper_Results)[1] != 0 ){
    ss <- drive_get("Decklists")
    #sheets_deauth()
    gs4_auth(email="pachun95@gmail.com", use_oob = T)
    sheet_write(
      Pauper_Results,
      ss = "/d/1PMVRwPJNPFnpxkksHrgcDaSZFYm5B20SnBsb2gboCEw",
      sheet = "Pauper_League"
    )
    
    csvFileName <- paste(currentDate,"_Pauper_Deck_Lists",".csv",sep="")
    write.csv(Pauper_Results, file=csvFileName, row.names = FALSE)
  } else {
    print("No Pauper Leagues Were logged Today")
  }
} else {
  print("No Pauper Leagues Release Today")
}




#Standard Preliminary####
Premium_WD_Part_1 <- "/home/cujo253/Reports/High Confidence Reps/"
Premium_WD_Part_2 <-  currentDate
Premium_WD_Part_3 <- "_Premium.csv"
URL_Part_1 <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/Standard-Preliminary-"
URL_Part_2 <- currentDate
URL_Part_3 <- paste(URL_Part_1,URL_Part_2,sep="")
url = URL_Part_3
tryCatch(
  data <- url %>% 
    read_html() %>% 
    html_nodes(".sorted-by-overview-container") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Deck_Number <- as.data.frame(data)
Deck_number <- nrow(Deck_Number)
Final_Export <- read_csv(paste(Premium_WD_Part_1,Premium_WD_Part_2,Premium_WD_Part_3,sep=""),col_types = cols(.default = "c"))

tryCatch(
  Card_Totals <- url %>% 
    read_html() %>% 
    html_nodes(".card-count") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Totals <- as.data.frame(Card_Totals)

tryCatch(
  Card_Name <- url %>% 
    read_html() %>% 
    html_nodes(".card-name") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Name <- as.data.frame(Card_Name)

Combined_Decklists <- tibble(Card_Totals,
                             Card_Name)
Combined_Decklists <- as.data.frame(Combined_Decklists)
names(Combined_Decklists) <- c("QTY","CARD")

Combined_Decklists <- as.data.frame(Combined_Decklists)
Combined_Decklists$QTY <- unlist(Combined_Decklists$QTY)
Combined_Decklists$QTY <- as.character(Combined_Decklists$QTY)
Combined_Decklists$QTY <- as.numeric(Combined_Decklists$QTY)
Combined_Decklists$CARD <- as.factor(unlist(Combined_Decklists$CARD))
Combined_Decklists <- as.data.frame(Combined_Decklists)

if(dim(Combined_Decklists)[1] != 0 ){
  Reduced_Decklists <- aggregate(. ~ CARD, data=Combined_Decklists, sum)
} else {
  Final_Decklist_Preliminaries <- 0
  Reduced_Decklists <- as.data.frame(0)
  print("No Results Were Found")
}
if(dim(Reduced_Decklists)[1] > 1){
  List <- Reduced_Decklists$CARD
  ABC <- NULL
  for(i in List){
    df_sub <- subset(Combined_Decklists, Combined_Decklists$CARD == i)
    df_sub<- as.data.frame(df_sub)
    A  <- round(mean(df_sub$QTY),2)
    ABC <- rbind(ABC,A)
  }
  Reduced_Decklists$Avg_Copies <- ABC
  Rarity_Check <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
  colnames(Rarity_Check) <- c("Key","name","Set","Rarity","Foil")
  Reduced_Decklists$Rarity <- Rarity_Check$Rarity[match(Reduced_Decklists$CARD,Rarity_Check$name)]
  Reduced_Decklists$Rarity <- as.character(Reduced_Decklists$Rarity)
  Reduced_Decklists$Rarity[is.na(Reduced_Decklists$Rarity)] = "Unknown"
  Reduced_Decklists <- as.data.frame(Reduced_Decklists)
  Reduced_Decklists$QTY <- as.numeric(Reduced_Decklists$QTY)
  #View(Reduced_Decklists)
  Reduced_Decklists$QTY_Cards_1 <- round(Reduced_Decklists$QTY / 4,1)
  Reduced_Decklists$QTY_Cards_Aux <- round(Reduced_Decklists$QTY / 3,1)
  Reduced_Decklists$QTY_Cards_1 <- as.character(Reduced_Decklists$QTY_Cards_1)
  Reduced_Decklists$QTY_Cards_Aux <- as.character (Reduced_Decklists$QTY_Cards_Aux)
  Reduced_Decklists$QTY_Cards <- ifelse(grepl("\\.\\d*$",Reduced_Decklists$QTY_Cards_1), Reduced_Decklists$QTY_Cards_Aux, Reduced_Decklists$QTY_Cards_1)
  Reduced_Decklists$QTY_Cards <- as.numeric(Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY_Lands <- ifelse(Reduced_Decklists$Rarity == "L", round(Reduced_Decklists$QTY / 3,0), Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY <- ifelse(Reduced_Decklists$Rarity == "L", Reduced_Decklists$QTY_Lands, Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY_Cards_1 <- Reduced_Decklists$QTY_Cards
  #View(Reduced_Decklists)
  Final_Decklist_Preliminaries <- Reduced_Decklists[,1:4]
  Final_Decklist_Preliminaries$Avg_Copies <- Final_Decklist_Preliminaries$Avg_Copies[,1]
  Final_Decklist_Preliminaries$Date <- currentDate
  Final_Decklist_Preliminaries$Sample_Size <- Deck_number
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(-Final_Decklist_Preliminaries$Avg_Copies, -Final_Decklist_Preliminaries$QTY),]
  Final_Decklist_Preliminaries$Potential_Decks <- round(Final_Decklist_Preliminaries$QTY/Final_Decklist_Preliminaries$Avg_Copies,0)
  Final_Decklist_Preliminaries$QTY <- ifelse(Final_Decklist_Preliminaries$Potential_Decks < 1, (Final_Decklist_Preliminaries$QTY + 1) ,Final_Decklist_Preliminaries$QTY)
  Final_Decklist_Preliminaries$Potential_Decks <- ifelse(Final_Decklist_Preliminaries$Potential_Decks < 1, Final_Decklist_Preliminaries$Potential_Decks + 1, Final_Decklist_Preliminaries$Potential_Decks)
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(-Final_Decklist_Preliminaries$Potential_Decks),]
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[ which(Final_Decklist_Preliminaries$Rarity !='Unknown'),]
  Final_Decklist_Preliminaries$Meta_Rank <- seq.int(nrow(Final_Decklist_Preliminaries))
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(-Final_Decklist_Preliminaries$QTY),]
  Final_Decklist_Preliminaries$QTY_Rank <- seq.int(nrow(Final_Decklist_Preliminaries))
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(-Final_Decklist_Preliminaries$QTY),]
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[c(5,6,1,4,8,9,2,3,7)]
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(Final_Decklist_Preliminaries$Meta_Rank),]
  Standard_Preliminaries <- Final_Decklist_Preliminaries
  
  setwd("/home/cujo253/Reports/Decklists/Standard Preliminary")
  if(dim(Standard_Preliminaries)[1] != 0 ){
    ss <- drive_get("Decklists")
    #sheets_deauth()
    gs4_auth(email="pachun95@gmail.com", use_oob = T)
    sheet_write(
      Standard_Preliminaries,
      ss = "/d/1PMVRwPJNPFnpxkksHrgcDaSZFYm5B20SnBsb2gboCEw",
      sheet = "Standard_Prelim"
    )
    
    csvFileName <- paste(currentDate,"_Standard_Preliminary_Lists",".csv",sep="")
    write.csv(Standard_Preliminaries, file=csvFileName, row.names = FALSE)
  } else {
    print("No Standard Preliminarys Were logged Today")
  }
} else {
  print("No Standard Preliminarys Release Today")
}

#Pioneer Preliminary####
Premium_WD_Part_1 <- "/home/cujo253/Reports/High Confidence Reps/"
Premium_WD_Part_2 <-  currentDate
Premium_WD_Part_3 <- "_Premium.csv"
URL_Part_1 <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/Pioneer-Preliminary-"
URL_Part_2 <- currentDate
URL_Part_3 <- paste(URL_Part_1,URL_Part_2,sep="")
url = URL_Part_3
tryCatch(
  data <- url %>% 
    read_html() %>% 
    html_nodes(".sorted-by-overview-container") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Deck_Number <- as.data.frame(data)
Deck_number <- nrow(Deck_Number)
Final_Export <- read_csv(paste(Premium_WD_Part_1,Premium_WD_Part_2,Premium_WD_Part_3,sep=""),col_types = cols(.default = "c"))

tryCatch(
  Card_Totals <- url %>% 
    read_html() %>% 
    html_nodes(".card-count") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Totals <- as.data.frame(Card_Totals)

tryCatch(
  Card_Name <- url %>% 
    read_html() %>% 
    html_nodes(".card-name") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Name <- as.data.frame(Card_Name)

Combined_Decklists <- tibble(Card_Totals,
                             Card_Name)
Combined_Decklists <- as.data.frame(Combined_Decklists)
names(Combined_Decklists) <- c("QTY","CARD")

Combined_Decklists <- as.data.frame(Combined_Decklists)
Combined_Decklists$QTY <- unlist(Combined_Decklists$QTY)
Combined_Decklists$QTY <- as.character(Combined_Decklists$QTY)
Combined_Decklists$QTY <- as.numeric(Combined_Decklists$QTY)
Combined_Decklists$CARD <- as.factor(unlist(Combined_Decklists$CARD))
Combined_Decklists <- as.data.frame(Combined_Decklists)

if(dim(Combined_Decklists)[1] != 0 ){
  Reduced_Decklists <- aggregate(. ~ CARD, data=Combined_Decklists, sum)
} else {
  Final_Decklist_Preliminaries <- 0
  Reduced_Decklists <- as.data.frame(0)
  print("No Results Were Found")
}
if(dim(Reduced_Decklists)[1] > 1){
  List <- Reduced_Decklists$CARD
  ABC <- NULL
  for(i in List){
    df_sub <- subset(Combined_Decklists, Combined_Decklists$CARD == i)
    df_sub<- as.data.frame(df_sub)
    A  <- round(mean(df_sub$QTY),2)
    ABC <- rbind(ABC,A)
  }
  Reduced_Decklists$Avg_Copies <- ABC
  Rarity_Check <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
  colnames(Rarity_Check) <- c("Key","name","Set","Rarity","Foil")
  Reduced_Decklists$Rarity <- Rarity_Check$Rarity[match(Reduced_Decklists$CARD,Rarity_Check$name)]
  Reduced_Decklists$Rarity <- as.character(Reduced_Decklists$Rarity)
  Reduced_Decklists$Rarity[is.na(Reduced_Decklists$Rarity)] = "Unknown"
  Reduced_Decklists <- as.data.frame(Reduced_Decklists)
  Reduced_Decklists$QTY <- as.numeric(Reduced_Decklists$QTY)
  #View(Reduced_Decklists)
  Reduced_Decklists$QTY_Cards_1 <- round(Reduced_Decklists$QTY / 4,1)
  Reduced_Decklists$QTY_Cards_Aux <- round(Reduced_Decklists$QTY / 3,1)
  Reduced_Decklists$QTY_Cards_1 <- as.character(Reduced_Decklists$QTY_Cards_1)
  Reduced_Decklists$QTY_Cards_Aux <- as.character (Reduced_Decklists$QTY_Cards_Aux)
  Reduced_Decklists$QTY_Cards <- ifelse(grepl("\\.\\d*$",Reduced_Decklists$QTY_Cards_1), Reduced_Decklists$QTY_Cards_Aux, Reduced_Decklists$QTY_Cards_1)
  Reduced_Decklists$QTY_Cards <- as.numeric(Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY_Lands <- ifelse(Reduced_Decklists$Rarity == "L", round(Reduced_Decklists$QTY / 3,0), Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY <- ifelse(Reduced_Decklists$Rarity == "L", Reduced_Decklists$QTY_Lands, Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY_Cards_1 <- Reduced_Decklists$QTY_Cards
  #View(Reduced_Decklists)
  Final_Decklist_Preliminaries <- Reduced_Decklists[,1:4]
  Final_Decklist_Preliminaries$Avg_Copies <- Final_Decklist_Preliminaries$Avg_Copies[,1]
  Final_Decklist_Preliminaries$Date <- currentDate
  Final_Decklist_Preliminaries$Sample_Size <- Deck_number
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(-Final_Decklist_Preliminaries$Avg_Copies, -Final_Decklist_Preliminaries$QTY),]
  Final_Decklist_Preliminaries$Potential_Decks <- round(Final_Decklist_Preliminaries$QTY/Final_Decklist_Preliminaries$Avg_Copies,0)
  Final_Decklist_Preliminaries$QTY <- ifelse(Final_Decklist_Preliminaries$Potential_Decks < 1, (Final_Decklist_Preliminaries$QTY + 1) ,Final_Decklist_Preliminaries$QTY)
  Final_Decklist_Preliminaries$Potential_Decks <- ifelse(Final_Decklist_Preliminaries$Potential_Decks < 1, Final_Decklist_Preliminaries$Potential_Decks + 1, Final_Decklist_Preliminaries$Potential_Decks)
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(-Final_Decklist_Preliminaries$Potential_Decks),]
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[ which(Final_Decklist_Preliminaries$Rarity !='Unknown'),]
  Final_Decklist_Preliminaries$Meta_Rank <- seq.int(nrow(Final_Decklist_Preliminaries))
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(-Final_Decklist_Preliminaries$QTY),]
  Final_Decklist_Preliminaries$QTY_Rank <- seq.int(nrow(Final_Decklist_Preliminaries))
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(-Final_Decklist_Preliminaries$QTY),]
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[c(5,6,1,4,8,9,2,3,7)]
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(Final_Decklist_Preliminaries$Meta_Rank),]
  Pioneer_Preliminaries <- Final_Decklist_Preliminaries
  
  setwd("/home/cujo253/Reports/Decklists/Pioneer Preliminary")
  if(dim(Pioneer_Preliminaries)[1] != 0 ){
    ss <- drive_get("Decklists")
    #sheets_deauth()
    gs4_auth(email="pachun95@gmail.com", use_oob = T)
    sheet_write(
      Pioneer_Preliminaries,
      ss = "/d/1PMVRwPJNPFnpxkksHrgcDaSZFYm5B20SnBsb2gboCEw",
      sheet = "Pioneer_Prelim"
    )
    csvFileName <- paste(currentDate,"_Pioneer_Preliminary_Lists",".csv",sep="")
    write.csv(Pioneer_Preliminaries, file=csvFileName, row.names = FALSE)
  } else {
    print("No Pioneer Preliminarys Were logged Today")
  }
} else {
  print("No Pioneer Preliminarys Release Today")
}

#Modern Preliminary####
Premium_WD_Part_1 <- "/home/cujo253/Reports/High Confidence Reps/"
Premium_WD_Part_2 <-  currentDate
Premium_WD_Part_3 <- "_Premium.csv"
URL_Part_1 <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/Modern-Preliminary-"
URL_Part_2 <- currentDate
URL_Part_3 <- paste(URL_Part_1,URL_Part_2,sep="")
url = URL_Part_3
tryCatch(
  data <- url %>% 
    read_html() %>% 
    html_nodes(".sorted-by-overview-container") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Deck_Number <- as.data.frame(data)
Deck_number <- nrow(Deck_Number)
Final_Export <- read_csv(paste(Premium_WD_Part_1,Premium_WD_Part_2,Premium_WD_Part_3,sep=""),col_types = cols(.default = "c"))

tryCatch(
  Card_Totals <- url %>% 
    read_html() %>% 
    html_nodes(".card-count") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Totals <- as.data.frame(Card_Totals)

tryCatch(
  Card_Name <- url %>% 
    read_html() %>% 
    html_nodes(".card-name") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Name <- as.data.frame(Card_Name)

Combined_Decklists <- tibble(Card_Totals,
                             Card_Name)
Combined_Decklists <- as.data.frame(Combined_Decklists)
names(Combined_Decklists) <- c("QTY","CARD")

Combined_Decklists <- as.data.frame(Combined_Decklists)
Combined_Decklists$QTY <- unlist(Combined_Decklists$QTY)
Combined_Decklists$QTY <- as.character(Combined_Decklists$QTY)
Combined_Decklists$QTY <- as.numeric(Combined_Decklists$QTY)
Combined_Decklists$CARD <- as.factor(unlist(Combined_Decklists$CARD))
Combined_Decklists <- as.data.frame(Combined_Decklists)

if(dim(Combined_Decklists)[1] != 0 ){
  Reduced_Decklists <- aggregate(. ~ CARD, data=Combined_Decklists, sum)
} else {
  Final_Decklist_Preliminaries <- 0
  Reduced_Decklists <- as.data.frame(0)
  print("No Results Were Found")
}
if(dim(Reduced_Decklists)[1] > 1){
  List <- Reduced_Decklists$CARD
  ABC <- NULL
  for(i in List){
    df_sub <- subset(Combined_Decklists, Combined_Decklists$CARD == i)
    df_sub<- as.data.frame(df_sub)
    A  <- round(mean(df_sub$QTY),2)
    ABC <- rbind(ABC,A)
  }
  Reduced_Decklists$Avg_Copies <- ABC
  Rarity_Check <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
  colnames(Rarity_Check) <- c("Key","name","Set","Rarity","Foil")
  Reduced_Decklists$Rarity <- Rarity_Check$Rarity[match(Reduced_Decklists$CARD,Rarity_Check$name)]
  Reduced_Decklists$Rarity <- as.character(Reduced_Decklists$Rarity)
  Reduced_Decklists$Rarity[is.na(Reduced_Decklists$Rarity)] = "Unknown"
  Reduced_Decklists <- as.data.frame(Reduced_Decklists)
  Reduced_Decklists$QTY <- as.numeric(Reduced_Decklists$QTY)
  #View(Reduced_Decklists)
  Reduced_Decklists$QTY_Cards_1 <- round(Reduced_Decklists$QTY / 4,1)
  Reduced_Decklists$QTY_Cards_Aux <- round(Reduced_Decklists$QTY / 3,1)
  Reduced_Decklists$QTY_Cards_1 <- as.character(Reduced_Decklists$QTY_Cards_1)
  Reduced_Decklists$QTY_Cards_Aux <- as.character (Reduced_Decklists$QTY_Cards_Aux)
  Reduced_Decklists$QTY_Cards <- ifelse(grepl("\\.\\d*$",Reduced_Decklists$QTY_Cards_1), Reduced_Decklists$QTY_Cards_Aux, Reduced_Decklists$QTY_Cards_1)
  Reduced_Decklists$QTY_Cards <- as.numeric(Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY_Lands <- ifelse(Reduced_Decklists$Rarity == "L", round(Reduced_Decklists$QTY / 3,0), Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY <- ifelse(Reduced_Decklists$Rarity == "L", Reduced_Decklists$QTY_Lands, Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY_Cards_1 <- Reduced_Decklists$QTY_Cards
  #View(Reduced_Decklists)
  Final_Decklist_Preliminaries <- Reduced_Decklists[,1:4]
  Final_Decklist_Preliminaries$Avg_Copies <- Final_Decklist_Preliminaries$Avg_Copies[,1]
  Final_Decklist_Preliminaries$Date <- currentDate
  Final_Decklist_Preliminaries$Sample_Size <- Deck_number
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(-Final_Decklist_Preliminaries$Avg_Copies, -Final_Decklist_Preliminaries$QTY),]
  Final_Decklist_Preliminaries$Potential_Decks <- round(Final_Decklist_Preliminaries$QTY/Final_Decklist_Preliminaries$Avg_Copies,0)
  Final_Decklist_Preliminaries$QTY <- ifelse(Final_Decklist_Preliminaries$Potential_Decks < 1, (Final_Decklist_Preliminaries$QTY + 1) ,Final_Decklist_Preliminaries$QTY)
  Final_Decklist_Preliminaries$Potential_Decks <- ifelse(Final_Decklist_Preliminaries$Potential_Decks < 1, Final_Decklist_Preliminaries$Potential_Decks + 1, Final_Decklist_Preliminaries$Potential_Decks)
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(-Final_Decklist_Preliminaries$Potential_Decks),]
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[ which(Final_Decklist_Preliminaries$Rarity !='Unknown'),]
  Final_Decklist_Preliminaries$Meta_Rank <- seq.int(nrow(Final_Decklist_Preliminaries))
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(-Final_Decklist_Preliminaries$QTY),]
  Final_Decklist_Preliminaries$QTY_Rank <- seq.int(nrow(Final_Decklist_Preliminaries))
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(-Final_Decklist_Preliminaries$QTY),]
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[c(5,6,1,4,8,9,2,3,7)]
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(Final_Decklist_Preliminaries$Meta_Rank),]
  Modern_Preliminaries <- Final_Decklist_Preliminaries
  
  setwd("/home/cujo253/Reports/Decklists/Modern Preliminary")
  if(dim(Modern_Preliminaries)[1] != 0 ){
    ss <- drive_get("Decklists")
    #sheets_deauth()
    gs4_auth(email="pachun95@gmail.com", use_oob = T)
    sheet_write(
      Modern_Preliminaries,
      ss = "/d/1PMVRwPJNPFnpxkksHrgcDaSZFYm5B20SnBsb2gboCEw",
      sheet = "Modern_Prelim"
    )
    csvFileName <- paste(currentDate,"_Modern _Preliminary_Lists",".csv",sep="")
    write.csv(Modern_Preliminaries, file=csvFileName, row.names = FALSE)
  } else {
    print("No Modern Preliminarys Were logged Today")
  }
} else {
  print("No Modern Preliminarys Release Today")
}

#Legacy Preliminary####
Premium_WD_Part_1 <- "/home/cujo253/Reports/High Confidence Reps/"
Premium_WD_Part_2 <-  currentDate
Premium_WD_Part_3 <- "_Premium.csv"
URL_Part_1 <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/Legacy-Preliminary-"
URL_Part_2 <- currentDate
URL_Part_3 <- paste(URL_Part_1,URL_Part_2,sep="")
url = URL_Part_3
tryCatch(
  data <- url %>% 
    read_html() %>% 
    html_nodes(".sorted-by-overview-container") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Deck_Number <- as.data.frame(data)
Deck_number <- nrow(Deck_Number)
Final_Export <- read_csv(paste(Premium_WD_Part_1,Premium_WD_Part_2,Premium_WD_Part_3,sep=""),col_types = cols(.default = "c"))

tryCatch(
  Card_Totals <- url %>% 
    read_html() %>% 
    html_nodes(".card-count") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Totals <- as.data.frame(Card_Totals)

tryCatch(
  Card_Name <- url %>% 
    read_html() %>% 
    html_nodes(".card-name") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Name <- as.data.frame(Card_Name)

Combined_Decklists <- tibble(Card_Totals,
                             Card_Name)
Combined_Decklists <- as.data.frame(Combined_Decklists)
names(Combined_Decklists) <- c("QTY","CARD")

Combined_Decklists <- as.data.frame(Combined_Decklists)
Combined_Decklists$QTY <- unlist(Combined_Decklists$QTY)
Combined_Decklists$QTY <- as.character(Combined_Decklists$QTY)
Combined_Decklists$QTY <- as.numeric(Combined_Decklists$QTY)
Combined_Decklists$CARD <- as.factor(unlist(Combined_Decklists$CARD))
Combined_Decklists <- as.data.frame(Combined_Decklists)

if(dim(Combined_Decklists)[1] != 0 ){
  Reduced_Decklists <- aggregate(. ~ CARD, data=Combined_Decklists, sum)
} else {
  Final_Decklist_Preliminaries <- 0
  Reduced_Decklists <- as.data.frame(0)
  print("No Results Were Found")
}
if(dim(Reduced_Decklists)[1] > 1){
  List <- Reduced_Decklists$CARD
  ABC <- NULL
  for(i in List){
    df_sub <- subset(Combined_Decklists, Combined_Decklists$CARD == i)
    df_sub<- as.data.frame(df_sub)
    A  <- round(mean(df_sub$QTY),2)
    ABC <- rbind(ABC,A)
  }
  Reduced_Decklists$Avg_Copies <- ABC
  Rarity_Check <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
  colnames(Rarity_Check) <- c("Key","name","Set","Rarity","Foil")
  Reduced_Decklists$Rarity <- Rarity_Check$Rarity[match(Reduced_Decklists$CARD,Rarity_Check$name)]
  Reduced_Decklists$Rarity <- as.character(Reduced_Decklists$Rarity)
  Reduced_Decklists$Rarity[is.na(Reduced_Decklists$Rarity)] = "Unknown"
  Reduced_Decklists <- as.data.frame(Reduced_Decklists)
  Reduced_Decklists$QTY <- as.numeric(Reduced_Decklists$QTY)
  #View(Reduced_Decklists)
  Reduced_Decklists$QTY_Cards_1 <- round(Reduced_Decklists$QTY / 4,1)
  Reduced_Decklists$QTY_Cards_Aux <- round(Reduced_Decklists$QTY / 3,1)
  Reduced_Decklists$QTY_Cards_1 <- as.character(Reduced_Decklists$QTY_Cards_1)
  Reduced_Decklists$QTY_Cards_Aux <- as.character (Reduced_Decklists$QTY_Cards_Aux)
  Reduced_Decklists$QTY_Cards <- ifelse(grepl("\\.\\d*$",Reduced_Decklists$QTY_Cards_1), Reduced_Decklists$QTY_Cards_Aux, Reduced_Decklists$QTY_Cards_1)
  Reduced_Decklists$QTY_Cards <- as.numeric(Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY_Lands <- ifelse(Reduced_Decklists$Rarity == "L", round(Reduced_Decklists$QTY / 3,0), Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY <- ifelse(Reduced_Decklists$Rarity == "L", Reduced_Decklists$QTY_Lands, Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY_Cards_1 <- Reduced_Decklists$QTY_Cards
  #View(Reduced_Decklists)
  Final_Decklist_Preliminaries <- Reduced_Decklists[,1:4]
  Final_Decklist_Preliminaries$Avg_Copies <- Final_Decklist_Preliminaries$Avg_Copies[,1]
  Final_Decklist_Preliminaries$Date <- currentDate
  Final_Decklist_Preliminaries$Sample_Size <- Deck_number
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(-Final_Decklist_Preliminaries$Avg_Copies, -Final_Decklist_Preliminaries$QTY),]
  Final_Decklist_Preliminaries$Potential_Decks <- round(Final_Decklist_Preliminaries$QTY/Final_Decklist_Preliminaries$Avg_Copies,0)
  Final_Decklist_Preliminaries$QTY <- ifelse(Final_Decklist_Preliminaries$Potential_Decks < 1, (Final_Decklist_Preliminaries$QTY + 1) ,Final_Decklist_Preliminaries$QTY)
  Final_Decklist_Preliminaries$Potential_Decks <- ifelse(Final_Decklist_Preliminaries$Potential_Decks < 1, Final_Decklist_Preliminaries$Potential_Decks + 1, Final_Decklist_Preliminaries$Potential_Decks)
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(-Final_Decklist_Preliminaries$Potential_Decks),]
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[ which(Final_Decklist_Preliminaries$Rarity !='Unknown'),]
  Final_Decklist_Preliminaries$Meta_Rank <- seq.int(nrow(Final_Decklist_Preliminaries))
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(-Final_Decklist_Preliminaries$QTY),]
  Final_Decklist_Preliminaries$QTY_Rank <- seq.int(nrow(Final_Decklist_Preliminaries))
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(-Final_Decklist_Preliminaries$QTY),]
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[c(5,6,1,4,8,9,2,3,7)]
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(Final_Decklist_Preliminaries$Meta_Rank),]
  Legacy_Preliminaries <- Final_Decklist_Preliminaries
  
  setwd("/home/cujo253/Reports/Decklists/Legacy Preliminary")
  if(dim(Legacy_Preliminaries)[1] != 0 ){
    ss <- drive_get("Decklists")
    #sheets_deauth()
    gs4_auth(email="pachun95@gmail.com", use_oob = T)
    sheet_write(
      Legacy_Preliminaries,
      ss = "/d/1PMVRwPJNPFnpxkksHrgcDaSZFYm5B20SnBsb2gboCEw",
      sheet = "Legacy_Prelim"
    )
    csvFileName <- paste(currentDate,"_Legacy_Preliminary_Lists",".csv",sep="")
    write.csv(Legacy_Preliminaries, file=csvFileName, row.names = FALSE)
  } else {
    print("No Legacy Preliminarys Were logged Today")
  }
} else {
  print("No Legacy Preliminarys Release Today")
}

#Vintage Preliminary####
Premium_WD_Part_1 <- "/home/cujo253/Reports/High Confidence Reps/"
Premium_WD_Part_2 <-  currentDate
Premium_WD_Part_3 <- "_Premium.csv"
URL_Part_1 <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/Vintage-Preliminary-"
URL_Part_2 <- currentDate
URL_Part_3 <- paste(URL_Part_1,URL_Part_2,sep="")
url = URL_Part_3
tryCatch(
  data <- url %>% 
    read_html() %>% 
    html_nodes(".sorted-by-overview-container") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Deck_Number <- as.data.frame(data)
Deck_number <- nrow(Deck_Number)
Final_Export <- read_csv(paste(Premium_WD_Part_1,Premium_WD_Part_2,Premium_WD_Part_3,sep=""),col_types = cols(.default = "c"))

tryCatch(
  Card_Totals <- url %>% 
    read_html() %>% 
    html_nodes(".card-count") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Totals <- as.data.frame(Card_Totals)

tryCatch(
  Card_Name <- url %>% 
    read_html() %>% 
    html_nodes(".card-name") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Name <- as.data.frame(Card_Name)

Combined_Decklists <- tibble(Card_Totals,
                             Card_Name)
Combined_Decklists <- as.data.frame(Combined_Decklists)
names(Combined_Decklists) <- c("QTY","CARD")

Combined_Decklists <- as.data.frame(Combined_Decklists)
Combined_Decklists$QTY <- unlist(Combined_Decklists$QTY)
Combined_Decklists$QTY <- as.character(Combined_Decklists$QTY)
Combined_Decklists$QTY <- as.numeric(Combined_Decklists$QTY)
Combined_Decklists$CARD <- as.factor(unlist(Combined_Decklists$CARD))
Combined_Decklists <- as.data.frame(Combined_Decklists)

if(dim(Combined_Decklists)[1] != 0 ){
  Reduced_Decklists <- aggregate(. ~ CARD, data=Combined_Decklists, sum)
} else {
  Final_Decklist_Preliminaries <- 0
  Reduced_Decklists <- as.data.frame(0)
  print("No Results Were Found")
}
if(dim(Reduced_Decklists)[1] > 1){
  List <- Reduced_Decklists$CARD
  ABC <- NULL
  for(i in List){
    df_sub <- subset(Combined_Decklists, Combined_Decklists$CARD == i)
    df_sub<- as.data.frame(df_sub)
    A  <- round(mean(df_sub$QTY),2)
    ABC <- rbind(ABC,A)
  }
  Reduced_Decklists$Avg_Copies <- ABC
  Rarity_Check <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
  colnames(Rarity_Check) <- c("Key","name","Set","Rarity","Foil")
  Reduced_Decklists$Rarity <- Rarity_Check$Rarity[match(Reduced_Decklists$CARD,Rarity_Check$name)]
  Reduced_Decklists$Rarity <- as.character(Reduced_Decklists$Rarity)
  Reduced_Decklists$Rarity[is.na(Reduced_Decklists$Rarity)] = "Unknown"
  Reduced_Decklists <- as.data.frame(Reduced_Decklists)
  Reduced_Decklists$QTY <- as.numeric(Reduced_Decklists$QTY)
  #View(Reduced_Decklists)
  Reduced_Decklists$QTY_Cards_1 <- round(Reduced_Decklists$QTY / 4,1)
  Reduced_Decklists$QTY_Cards_Aux <- round(Reduced_Decklists$QTY / 3,1)
  Reduced_Decklists$QTY_Cards_1 <- as.character(Reduced_Decklists$QTY_Cards_1)
  Reduced_Decklists$QTY_Cards_Aux <- as.character (Reduced_Decklists$QTY_Cards_Aux)
  Reduced_Decklists$QTY_Cards <- ifelse(grepl("\\.\\d*$",Reduced_Decklists$QTY_Cards_1), Reduced_Decklists$QTY_Cards_Aux, Reduced_Decklists$QTY_Cards_1)
  Reduced_Decklists$QTY_Cards <- as.numeric(Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY_Lands <- ifelse(Reduced_Decklists$Rarity == "L", round(Reduced_Decklists$QTY / 3,0), Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY <- ifelse(Reduced_Decklists$Rarity == "L", Reduced_Decklists$QTY_Lands, Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY_Cards_1 <- Reduced_Decklists$QTY_Cards
  #View(Reduced_Decklists)
  Final_Decklist_Preliminaries <- Reduced_Decklists[,1:4]
  Final_Decklist_Preliminaries$Avg_Copies <- Final_Decklist_Preliminaries$Avg_Copies[,1]
  Final_Decklist_Preliminaries$Date <- currentDate
  Final_Decklist_Preliminaries$Sample_Size <- Deck_number
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(-Final_Decklist_Preliminaries$Avg_Copies, -Final_Decklist_Preliminaries$QTY),]
  Final_Decklist_Preliminaries$Potential_Decks <- round(Final_Decklist_Preliminaries$QTY/Final_Decklist_Preliminaries$Avg_Copies,0)
  Final_Decklist_Preliminaries$QTY <- ifelse(Final_Decklist_Preliminaries$Potential_Decks < 1, (Final_Decklist_Preliminaries$QTY + 1) ,Final_Decklist_Preliminaries$QTY)
  Final_Decklist_Preliminaries$Potential_Decks <- ifelse(Final_Decklist_Preliminaries$Potential_Decks < 1, Final_Decklist_Preliminaries$Potential_Decks + 1, Final_Decklist_Preliminaries$Potential_Decks)
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(-Final_Decklist_Preliminaries$Potential_Decks),]
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[ which(Final_Decklist_Preliminaries$Rarity !='Unknown'),]
  Final_Decklist_Preliminaries$Meta_Rank <- seq.int(nrow(Final_Decklist_Preliminaries))
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(-Final_Decklist_Preliminaries$QTY),]
  Final_Decklist_Preliminaries$QTY_Rank <- seq.int(nrow(Final_Decklist_Preliminaries))
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(-Final_Decklist_Preliminaries$QTY),]
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[c(5,6,1,4,8,9,2,3,7)]
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(Final_Decklist_Preliminaries$Meta_Rank),]
  Vintage_Preliminaries <- Final_Decklist_Preliminaries
  
  setwd("/home/cujo253/Reports/Decklists/Vintage Preliminary")
  if(dim(Vintage_Preliminaries)[1] != 0 ){
    ss <- drive_get("Decklists")
    #sheets_deauth()
    gs4_auth(email="pachun95@gmail.com", use_oob = T)
    sheet_write(
      Vintage_Preliminaries,
      ss = "/d/1PMVRwPJNPFnpxkksHrgcDaSZFYm5B20SnBsb2gboCEw",
      sheet = "Vintage_Prelim"
    )
    csvFileName <- paste(currentDate,"_Vintage_Preliminary_Lists",".csv",sep="")
    write.csv(Vintage_Preliminaries, file=csvFileName, row.names = FALSE)
  } else {
    print("No Vintage Preliminarys Were logged Today")
  }
} else {
  print("No Vintage Preliminarys Release Today")
}


#Pauper Preliminary####
Premium_WD_Part_1 <- "/home/cujo253/Reports/High Confidence Reps/"
Premium_WD_Part_2 <-  currentDate
Premium_WD_Part_3 <- "_Premium.csv"
URL_Part_1 <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/Pauper-Preliminary-"
URL_Part_2 <- currentDate
URL_Part_3 <- paste(URL_Part_1,URL_Part_2,sep="")
url = URL_Part_3
tryCatch(
  data <- url %>% 
    read_html() %>% 
    html_nodes(".sorted-by-overview-container") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Deck_Number <- as.data.frame(data)
Deck_number <- nrow(Deck_Number)
Final_Export <- read_csv(paste(Premium_WD_Part_1,Premium_WD_Part_2,Premium_WD_Part_3,sep=""),col_types = cols(.default = "c"))

tryCatch(
  Card_Totals <- url %>% 
    read_html() %>% 
    html_nodes(".card-count") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Totals <- as.data.frame(Card_Totals)

tryCatch(
  Card_Name <- url %>% 
    read_html() %>% 
    html_nodes(".card-name") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Name <- as.data.frame(Card_Name)

Combined_Decklists <- tibble(Card_Totals,
                             Card_Name)
Combined_Decklists <- as.data.frame(Combined_Decklists)
names(Combined_Decklists) <- c("QTY","CARD")

Combined_Decklists <- as.data.frame(Combined_Decklists)
Combined_Decklists$QTY <- unlist(Combined_Decklists$QTY)
Combined_Decklists$QTY <- as.character(Combined_Decklists$QTY)
Combined_Decklists$QTY <- as.numeric(Combined_Decklists$QTY)
Combined_Decklists$CARD <- as.factor(unlist(Combined_Decklists$CARD))
Combined_Decklists <- as.data.frame(Combined_Decklists)

if(dim(Combined_Decklists)[1] != 0 ){
  Reduced_Decklists <- aggregate(. ~ CARD, data=Combined_Decklists, sum)
} else {
  Final_Decklist_Preliminaries <- 0
  Reduced_Decklists <- as.data.frame(0)
  print("No Results Were Found")
}
if(dim(Reduced_Decklists)[1] > 1){
  List <- Reduced_Decklists$CARD
  ABC <- NULL
  for(i in List){
    df_sub <- subset(Combined_Decklists, Combined_Decklists$CARD == i)
    df_sub<- as.data.frame(df_sub)
    A  <- round(mean(df_sub$QTY),2)
    ABC <- rbind(ABC,A)
  }
  Reduced_Decklists$Avg_Copies <- ABC
  Rarity_Check <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
  colnames(Rarity_Check) <- c("Key","name","Set","Rarity","Foil")
  Reduced_Decklists$Rarity <- Rarity_Check$Rarity[match(Reduced_Decklists$CARD,Rarity_Check$name)]
  Reduced_Decklists$Rarity <- as.character(Reduced_Decklists$Rarity)
  Reduced_Decklists$Rarity[is.na(Reduced_Decklists$Rarity)] = "Unknown"
  Reduced_Decklists <- as.data.frame(Reduced_Decklists)
  Reduced_Decklists$QTY <- as.numeric(Reduced_Decklists$QTY)
  #View(Reduced_Decklists)
  Reduced_Decklists$QTY_Cards_1 <- round(Reduced_Decklists$QTY / 4,1)
  Reduced_Decklists$QTY_Cards_Aux <- round(Reduced_Decklists$QTY / 3,1)
  Reduced_Decklists$QTY_Cards_1 <- as.character(Reduced_Decklists$QTY_Cards_1)
  Reduced_Decklists$QTY_Cards_Aux <- as.character (Reduced_Decklists$QTY_Cards_Aux)
  Reduced_Decklists$QTY_Cards <- ifelse(grepl("\\.\\d*$",Reduced_Decklists$QTY_Cards_1), Reduced_Decklists$QTY_Cards_Aux, Reduced_Decklists$QTY_Cards_1)
  Reduced_Decklists$QTY_Cards <- as.numeric(Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY_Lands <- ifelse(Reduced_Decklists$Rarity == "L", round(Reduced_Decklists$QTY / 3,0), Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY <- ifelse(Reduced_Decklists$Rarity == "L", Reduced_Decklists$QTY_Lands, Reduced_Decklists$QTY_Cards)
  Reduced_Decklists$QTY_Cards_1 <- Reduced_Decklists$QTY_Cards
  #View(Reduced_Decklists)
  Final_Decklist_Preliminaries <- Reduced_Decklists[,1:4]
  Final_Decklist_Preliminaries$Avg_Copies <- Final_Decklist_Preliminaries$Avg_Copies[,1]
  Final_Decklist_Preliminaries$Date <- currentDate
  Final_Decklist_Preliminaries$Sample_Size <- Deck_number
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(-Final_Decklist_Preliminaries$Avg_Copies, -Final_Decklist_Preliminaries$QTY),]
  Final_Decklist_Preliminaries$Potential_Decks <- round(Final_Decklist_Preliminaries$QTY/Final_Decklist_Preliminaries$Avg_Copies,0)
  Final_Decklist_Preliminaries$QTY <- ifelse(Final_Decklist_Preliminaries$Potential_Decks < 1, (Final_Decklist_Preliminaries$QTY + 1) ,Final_Decklist_Preliminaries$QTY)
  Final_Decklist_Preliminaries$Potential_Decks <- ifelse(Final_Decklist_Preliminaries$Potential_Decks < 1, Final_Decklist_Preliminaries$Potential_Decks + 1, Final_Decklist_Preliminaries$Potential_Decks)
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(-Final_Decklist_Preliminaries$Potential_Decks),]
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[ which(Final_Decklist_Preliminaries$Rarity !='Unknown'),]
  Final_Decklist_Preliminaries$Meta_Rank <- seq.int(nrow(Final_Decklist_Preliminaries))
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(-Final_Decklist_Preliminaries$QTY),]
  Final_Decklist_Preliminaries$QTY_Rank <- seq.int(nrow(Final_Decklist_Preliminaries))
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(-Final_Decklist_Preliminaries$QTY),]
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[c(5,6,1,4,8,9,2,3,7)]
  Final_Decklist_Preliminaries <- Final_Decklist_Preliminaries[order(Final_Decklist_Preliminaries$Meta_Rank),]
  Pauper_Preliminaries <- Final_Decklist_Preliminaries
  
  setwd("/home/cujo253/Reports/Decklists/Pauper Preliminary")
  if(dim(Pauper_Preliminaries)[1] != 0 ){
    ss <- drive_get("Decklists")
    #sheets_deauth()
    gs4_auth(email="pachun95@gmail.com", use_oob = T)
    sheet_write(
      Pauper_Preliminaries,
      ss = "/d/1PMVRwPJNPFnpxkksHrgcDaSZFYm5B20SnBsb2gboCEw",
      sheet = "Pauper_Prelim"
    )
    csvFileName <- paste(currentDate,"_Pauper_Preliminary_Lists",".csv",sep="")
    write.csv(Pauper_Preliminaries, file=csvFileName, row.names = FALSE)
  } else {
    print("No Pauper Preliminarys Were logged Today")
  }
} else {
  print("No Pauper Preliminarys Release Today")
}


Sys.sleep(10)
#Standard Event####
Premium_WD_Part_1 <- "/home/cujo253/Reports/High Confidence Reps/"
Premium_WD_Part_2 <-  currentDate
Premium_WD_Part_3 <- "_Premium.csv"
URL_Part_1 <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/Standard-Event-"
URL_Part_2 <- currentDate
URL_Part_3 <- paste(URL_Part_1,URL_Part_2,sep="")
url = URL_Part_3
tryCatch(
  data <- url %>% 
    read_html() %>% 
    html_nodes(".sorted-by-overview-container") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Deck_Number <- as.data.frame(data)
Deck_number <- nrow(Deck_Number)
Final_Export <- read_csv(paste(Premium_WD_Part_1,Premium_WD_Part_2,Premium_WD_Part_3,sep=""),col_types = cols(.default = "c"))

tryCatch(
  Card_Totals <- url %>% 
    read_html() %>% 
    html_nodes(".card-count") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Totals <- as.data.frame(Card_Totals)

tryCatch(
  Card_Name <- url %>% 
    read_html() %>% 
    html_nodes(".card-name") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Name <- as.data.frame(Card_Name)

Combined_Preliminary <- tibble(Card_Totals,
                             Card_Name)
Combined_Preliminary <- as.data.frame(Combined_Preliminary)
names(Combined_Preliminary) <- c("QTY","CARD")

Combined_Preliminary <- as.data.frame(Combined_Preliminary)
Combined_Preliminary$QTY <- unlist(Combined_Preliminary$QTY)
Combined_Preliminary$QTY <- as.character(Combined_Preliminary$QTY)
Combined_Preliminary$QTY <- as.numeric(Combined_Preliminary$QTY)
Combined_Preliminary$CARD <- as.factor(unlist(Combined_Preliminary$CARD))
Combined_Preliminary <- as.data.frame(Combined_Preliminary)

if(dim(Combined_Preliminary)[1] != 0 ){
  Reduced_Preliminary <- aggregate(. ~ CARD, data=Combined_Preliminary, sum)
} else {
  Final_Decklist_Results <- 0
  Reduced_Preliminary <- as.data.frame(0)
  print("No Results Were Found")
}
if(dim(Reduced_Preliminary)[1] > 1){
  List <- Reduced_Preliminary$CARD
  ABC <- NULL
  for(i in List){
    df_sub <- subset(Combined_Preliminary, Combined_Preliminary$CARD == i)
    df_sub<- as.data.frame(df_sub)
    A  <- round(mean(df_sub$QTY),2)
    ABC <- rbind(ABC,A)
  }
  Reduced_Preliminary$Avg_Copies <- ABC
  Rarity_Check <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
  colnames(Rarity_Check) <- c("Key","name","Set","Rarity","Foil")
  Reduced_Preliminary$Rarity <- Rarity_Check$Rarity[match(Reduced_Preliminary$CARD,Rarity_Check$name)]
  Reduced_Preliminary$Rarity <- as.character(Reduced_Preliminary$Rarity)
  Reduced_Preliminary$Rarity[is.na(Reduced_Preliminary$Rarity)] = "Unknown"
  Reduced_Preliminary <- as.data.frame(Reduced_Preliminary)
  Reduced_Preliminary$QTY <- as.numeric(Reduced_Preliminary$QTY)
  #View(Reduced_Preliminary)
  Reduced_Preliminary$QTY_Cards_1 <- round(Reduced_Preliminary$QTY / 4,1)
  Reduced_Preliminary$QTY_Cards_Aux <- round(Reduced_Preliminary$QTY / 3,1)
  Reduced_Preliminary$QTY_Cards_1 <- as.character(Reduced_Preliminary$QTY_Cards_1)
  Reduced_Preliminary$QTY_Cards_Aux <- as.character (Reduced_Preliminary$QTY_Cards_Aux)
  Reduced_Preliminary$QTY_Cards <- ifelse(grepl("\\.\\d*$",Reduced_Preliminary$QTY_Cards_1), Reduced_Preliminary$QTY_Cards_Aux, Reduced_Preliminary$QTY_Cards_1)
  Reduced_Preliminary$QTY_Cards <- as.numeric(Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY_Lands <- ifelse(Reduced_Preliminary$Rarity == "L", round(Reduced_Preliminary$QTY / 3,0), Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY <- ifelse(Reduced_Preliminary$Rarity == "L", Reduced_Preliminary$QTY_Lands, Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY_Cards_1 <- Reduced_Preliminary$QTY_Cards
  #View(Reduced_Preliminary)
  Final_Decklist_Results <- Reduced_Preliminary[,1:4]
  Final_Decklist_Results$Avg_Copies <- Final_Decklist_Results$Avg_Copies[,1]
  Final_Decklist_Results$Date <- currentDate
  Final_Decklist_Results$Sample_Size <- Deck_number
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Avg_Copies, -Final_Decklist_Results$QTY),]
  Final_Decklist_Results$Potential_Decks <- round(Final_Decklist_Results$QTY/Final_Decklist_Results$Avg_Copies,0)
  Final_Decklist_Results$QTY <- ifelse(Final_Decklist_Results$Potential_Decks < 1, (Final_Decklist_Results$QTY + 1) ,Final_Decklist_Results$QTY)
  Final_Decklist_Results$Potential_Decks <- ifelse(Final_Decklist_Results$Potential_Decks < 1, Final_Decklist_Results$Potential_Decks + 1, Final_Decklist_Results$Potential_Decks)
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Potential_Decks),]
  Final_Decklist_Results <- Final_Decklist_Results[ which(Final_Decklist_Results$Rarity !='Unknown'),]
  Final_Decklist_Results$Meta_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results$QTY_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results <- Final_Decklist_Results[c(5,6,1,4,8,9,2,3,7)]
  Final_Decklist_Results <- Final_Decklist_Results[order(Final_Decklist_Results$Meta_Rank),]
  Standard_Results <- Final_Decklist_Results
  
  setwd("/home/cujo253/Reports/Decklists/Standard Preliminary")
  if(dim(Standard_Results)[1] != 0 ){
    ss <- drive_get("Preliminary")
    #sheets_deauth()
    gs4_auth(email="pachun95@gmail.com", use_oob = T)
    sheet_write(
      Standard_Results,
      ss = "/d/1PMVRwPJNPFnpxkksHrgcDaSZFYm5B20SnBsb2gboCEw",
      sheet = "Standard_Event"
    )
    csvFileName <- paste(currentDate,"_Standard_Event_Lists",".csv",sep="")
    write.csv(Standard_Results, file=csvFileName, row.names = FALSE)
  } else {
    print("No Standard Events Were logged Today")
  }
} else {
  print("No Standard Events Release Today")
}

#Pioneer Event####
Premium_WD_Part_1 <- "/home/cujo253/Reports/High Confidence Reps/"
Premium_WD_Part_2 <-  currentDate
Premium_WD_Part_3 <- "_Premium.csv"
URL_Part_1 <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/Pioneer-Event-"
URL_Part_2 <- currentDate
URL_Part_3 <- paste(URL_Part_1,URL_Part_2,sep="")
url = URL_Part_3
tryCatch(
  data <- url %>% 
    read_html() %>% 
    html_nodes(".sorted-by-overview-container") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Deck_Number <- as.data.frame(data)
Deck_number <- nrow(Deck_Number)
Final_Export <- read_csv(paste(Premium_WD_Part_1,Premium_WD_Part_2,Premium_WD_Part_3,sep=""),col_types = cols(.default = "c"))

tryCatch(
  Card_Totals <- url %>% 
    read_html() %>% 
    html_nodes(".card-count") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Totals <- as.data.frame(Card_Totals)

tryCatch(
  Card_Name <- url %>% 
    read_html() %>% 
    html_nodes(".card-name") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Name <- as.data.frame(Card_Name)

Combined_Preliminary <- tibble(Card_Totals,
                             Card_Name)
Combined_Preliminary <- as.data.frame(Combined_Preliminary)
names(Combined_Preliminary) <- c("QTY","CARD")

Combined_Preliminary <- as.data.frame(Combined_Preliminary)
Combined_Preliminary$QTY <- unlist(Combined_Preliminary$QTY)
Combined_Preliminary$QTY <- as.character(Combined_Preliminary$QTY)
Combined_Preliminary$QTY <- as.numeric(Combined_Preliminary$QTY)
Combined_Preliminary$CARD <- as.factor(unlist(Combined_Preliminary$CARD))
Combined_Preliminary <- as.data.frame(Combined_Preliminary)

if(dim(Combined_Preliminary)[1] != 0 ){
  Reduced_Preliminary <- aggregate(. ~ CARD, data=Combined_Preliminary, sum)
} else {
  Final_Decklist_Results <- 0
  Reduced_Preliminary <- as.data.frame(0)
  print("No Results Were Found")
}
if(dim(Reduced_Preliminary)[1] > 1){
  List <- Reduced_Preliminary$CARD
  ABC <- NULL
  for(i in List){
    df_sub <- subset(Combined_Preliminary, Combined_Preliminary$CARD == i)
    df_sub<- as.data.frame(df_sub)
    A  <- round(mean(df_sub$QTY),2)
    ABC <- rbind(ABC,A)
  }
  Reduced_Preliminary$Avg_Copies <- ABC
  Rarity_Check <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
  colnames(Rarity_Check) <- c("Key","name","Set","Rarity","Foil")
  Reduced_Preliminary$Rarity <- Rarity_Check$Rarity[match(Reduced_Preliminary$CARD,Rarity_Check$name)]
  Reduced_Preliminary$Rarity <- as.character(Reduced_Preliminary$Rarity)
  Reduced_Preliminary$Rarity[is.na(Reduced_Preliminary$Rarity)] = "Unknown"
  Reduced_Preliminary <- as.data.frame(Reduced_Preliminary)
  Reduced_Preliminary$QTY <- as.numeric(Reduced_Preliminary$QTY)
  #View(Reduced_Preliminary)
  Reduced_Preliminary$QTY_Cards_1 <- round(Reduced_Preliminary$QTY / 4,1)
  Reduced_Preliminary$QTY_Cards_Aux <- round(Reduced_Preliminary$QTY / 3,1)
  Reduced_Preliminary$QTY_Cards_1 <- as.character(Reduced_Preliminary$QTY_Cards_1)
  Reduced_Preliminary$QTY_Cards_Aux <- as.character (Reduced_Preliminary$QTY_Cards_Aux)
  Reduced_Preliminary$QTY_Cards <- ifelse(grepl("\\.\\d*$",Reduced_Preliminary$QTY_Cards_1), Reduced_Preliminary$QTY_Cards_Aux, Reduced_Preliminary$QTY_Cards_1)
  Reduced_Preliminary$QTY_Cards <- as.numeric(Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY_Lands <- ifelse(Reduced_Preliminary$Rarity == "L", round(Reduced_Preliminary$QTY / 3,0), Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY <- ifelse(Reduced_Preliminary$Rarity == "L", Reduced_Preliminary$QTY_Lands, Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY_Cards_1 <- Reduced_Preliminary$QTY_Cards
  #View(Reduced_Preliminary)
  Final_Decklist_Results <- Reduced_Preliminary[,1:4]
  Final_Decklist_Results$Avg_Copies <- Final_Decklist_Results$Avg_Copies[,1]
  Final_Decklist_Results$Date <- currentDate
  Final_Decklist_Results$Sample_Size <- Deck_number
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Avg_Copies, -Final_Decklist_Results$QTY),]
  Final_Decklist_Results$Potential_Decks <- round(Final_Decklist_Results$QTY/Final_Decklist_Results$Avg_Copies,0)
  Final_Decklist_Results$QTY <- ifelse(Final_Decklist_Results$Potential_Decks < 1, (Final_Decklist_Results$QTY + 1) ,Final_Decklist_Results$QTY)
  Final_Decklist_Results$Potential_Decks <- ifelse(Final_Decklist_Results$Potential_Decks < 1, Final_Decklist_Results$Potential_Decks + 1, Final_Decklist_Results$Potential_Decks)
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Potential_Decks),]
  Final_Decklist_Results <- Final_Decklist_Results[ which(Final_Decklist_Results$Rarity !='Unknown'),]
  Final_Decklist_Results$Meta_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results$QTY_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results <- Final_Decklist_Results[c(5,6,1,4,8,9,2,3,7)]
  Final_Decklist_Results <- Final_Decklist_Results[order(Final_Decklist_Results$Meta_Rank),]
  Pioneer_Results <- Final_Decklist_Results
  
  setwd("/home/cujo253/Reports/Decklists/Pioneer Preliminary")
  if(dim(Pioneer_Results)[1] != 0 ){
    ss <- drive_get("Preliminary")
    #sheets_deauth()
    gs4_auth(email="pachun95@gmail.com", use_oob = T)
    sheet_write(
      Pioneer_Results,
      ss = "/d/1PMVRwPJNPFnpxkksHrgcDaSZFYm5B20SnBsb2gboCEw",
      sheet = "Pioneer_Event"
    )
    csvFileName <- paste(currentDate,"_Pioneer_Event_Lists",".csv",sep="")
    write.csv(Pioneer_Results, file=csvFileName, row.names = FALSE)
  } else {
    print("No Pioneer Events Were logged Today")
  }
} else {
  print("No Pioneer Events Release Today")
}

#Modern Event####
Premium_WD_Part_1 <- "/home/cujo253/Reports/High Confidence Reps/"
Premium_WD_Part_2 <-  currentDate
Premium_WD_Part_3 <- "_Premium.csv"
URL_Part_1 <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/Modern-Event-"
URL_Part_2 <- currentDate
URL_Part_3 <- paste(URL_Part_1,URL_Part_2,sep="")
url = URL_Part_3
tryCatch(
  data <- url %>% 
    read_html() %>% 
    html_nodes(".sorted-by-overview-container") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Deck_Number <- as.data.frame(data)
Deck_number <- nrow(Deck_Number)
Final_Export <- read_csv(paste(Premium_WD_Part_1,Premium_WD_Part_2,Premium_WD_Part_3,sep=""),col_types = cols(.default = "c"))

tryCatch(
  Card_Totals <- url %>% 
    read_html() %>% 
    html_nodes(".card-count") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Totals <- as.data.frame(Card_Totals)

tryCatch(
  Card_Name <- url %>% 
    read_html() %>% 
    html_nodes(".card-name") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Name <- as.data.frame(Card_Name)

Combined_Preliminary <- tibble(Card_Totals,
                             Card_Name)
Combined_Preliminary <- as.data.frame(Combined_Preliminary)
names(Combined_Preliminary) <- c("QTY","CARD")

Combined_Preliminary <- as.data.frame(Combined_Preliminary)
Combined_Preliminary$QTY <- unlist(Combined_Preliminary$QTY)
Combined_Preliminary$QTY <- as.character(Combined_Preliminary$QTY)
Combined_Preliminary$QTY <- as.numeric(Combined_Preliminary$QTY)
Combined_Preliminary$CARD <- as.factor(unlist(Combined_Preliminary$CARD))
Combined_Preliminary <- as.data.frame(Combined_Preliminary)

if(dim(Combined_Preliminary)[1] != 0 ){
  Reduced_Preliminary <- aggregate(. ~ CARD, data=Combined_Preliminary, sum)
} else {
  Final_Decklist_Results <- 0
  Reduced_Preliminary <- as.data.frame(0)
  print("No Results Were Found")
}
if(dim(Reduced_Preliminary)[1] > 1){
  List <- Reduced_Preliminary$CARD
  ABC <- NULL
  for(i in List){
    df_sub <- subset(Combined_Preliminary, Combined_Preliminary$CARD == i)
    df_sub<- as.data.frame(df_sub)
    A  <- round(mean(df_sub$QTY),2)
    ABC <- rbind(ABC,A)
  }
  Reduced_Preliminary$Avg_Copies <- ABC
  Rarity_Check <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
  colnames(Rarity_Check) <- c("Key","name","Set","Rarity","Foil")
  Reduced_Preliminary$Rarity <- Rarity_Check$Rarity[match(Reduced_Preliminary$CARD,Rarity_Check$name)]
  Reduced_Preliminary$Rarity <- as.character(Reduced_Preliminary$Rarity)
  Reduced_Preliminary$Rarity[is.na(Reduced_Preliminary$Rarity)] = "Unknown"
  Reduced_Preliminary <- as.data.frame(Reduced_Preliminary)
  Reduced_Preliminary$QTY <- as.numeric(Reduced_Preliminary$QTY)
  #View(Reduced_Preliminary)
  Reduced_Preliminary$QTY_Cards_1 <- round(Reduced_Preliminary$QTY / 4,1)
  Reduced_Preliminary$QTY_Cards_Aux <- round(Reduced_Preliminary$QTY / 3,1)
  Reduced_Preliminary$QTY_Cards_1 <- as.character(Reduced_Preliminary$QTY_Cards_1)
  Reduced_Preliminary$QTY_Cards_Aux <- as.character (Reduced_Preliminary$QTY_Cards_Aux)
  Reduced_Preliminary$QTY_Cards <- ifelse(grepl("\\.\\d*$",Reduced_Preliminary$QTY_Cards_1), Reduced_Preliminary$QTY_Cards_Aux, Reduced_Preliminary$QTY_Cards_1)
  Reduced_Preliminary$QTY_Cards <- as.numeric(Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY_Lands <- ifelse(Reduced_Preliminary$Rarity == "L", round(Reduced_Preliminary$QTY / 3,0), Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY <- ifelse(Reduced_Preliminary$Rarity == "L", Reduced_Preliminary$QTY_Lands, Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY_Cards_1 <- Reduced_Preliminary$QTY_Cards
  #View(Reduced_Preliminary)
  Final_Decklist_Results <- Reduced_Preliminary[,1:4]
  Final_Decklist_Results$Avg_Copies <- Final_Decklist_Results$Avg_Copies[,1]
  Final_Decklist_Results$Date <- currentDate
  Final_Decklist_Results$Sample_Size <- Deck_number
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Avg_Copies, -Final_Decklist_Results$QTY),]
  Final_Decklist_Results$Potential_Decks <- round(Final_Decklist_Results$QTY/Final_Decklist_Results$Avg_Copies,0)
  Final_Decklist_Results$QTY <- ifelse(Final_Decklist_Results$Potential_Decks < 1, (Final_Decklist_Results$QTY + 1) ,Final_Decklist_Results$QTY)
  Final_Decklist_Results$Potential_Decks <- ifelse(Final_Decklist_Results$Potential_Decks < 1, Final_Decklist_Results$Potential_Decks + 1, Final_Decklist_Results$Potential_Decks)
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Potential_Decks),]
  Final_Decklist_Results <- Final_Decklist_Results[ which(Final_Decklist_Results$Rarity !='Unknown'),]
  Final_Decklist_Results$Meta_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results$QTY_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results <- Final_Decklist_Results[c(5,6,1,4,8,9,2,3,7)]
  Final_Decklist_Results <- Final_Decklist_Results[order(Final_Decklist_Results$Meta_Rank),]
  Modern_Results <- Final_Decklist_Results
  
  setwd("/home/cujo253/Reports/Decklists/Modern Preliminary")
  if(dim(Modern_Results)[1] != 0 ){
    ss <- drive_get("Preliminary")
    #sheets_deauth()
    gs4_auth(email="pachun95@gmail.com", use_oob = T)
    sheet_write(
      Modern_Results,
      ss = "/d/1PMVRwPJNPFnpxkksHrgcDaSZFYm5B20SnBsb2gboCEw",
      sheet = "Modern_Event"
    )
    csvFileName <- paste(currentDate,"_Modern_Event_Lists",".csv",sep="")
    write.csv(Modern_Results, file=csvFileName, row.names = FALSE)
  } else {
    print("No Modern Events Were logged Today")
  }
} else {
  print("No Modern Events Release Today")
}

#Legacy Event####
Premium_WD_Part_1 <- "/home/cujo253/Reports/High Confidence Reps/"
Premium_WD_Part_2 <-  currentDate
Premium_WD_Part_3 <- "_Premium.csv"
URL_Part_1 <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/Legacy-Event-"
URL_Part_2 <- currentDate
URL_Part_3 <- paste(URL_Part_1,URL_Part_2,sep="")
url = URL_Part_3
tryCatch(
  data <- url %>% 
    read_html() %>% 
    html_nodes(".sorted-by-overview-container") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Deck_Number <- as.data.frame(data)
Deck_number <- nrow(Deck_Number)
Final_Export <- read_csv(paste(Premium_WD_Part_1,Premium_WD_Part_2,Premium_WD_Part_3,sep=""),col_types = cols(.default = "c"))

tryCatch(
  Card_Totals <- url %>% 
    read_html() %>% 
    html_nodes(".card-count") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Totals <- as.data.frame(Card_Totals)

tryCatch(
  Card_Name <- url %>% 
    read_html() %>% 
    html_nodes(".card-name") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Name <- as.data.frame(Card_Name)

Combined_Preliminary <- tibble(Card_Totals,
                             Card_Name)
Combined_Preliminary <- as.data.frame(Combined_Preliminary)
names(Combined_Preliminary) <- c("QTY","CARD")

Combined_Preliminary <- as.data.frame(Combined_Preliminary)
Combined_Preliminary$QTY <- unlist(Combined_Preliminary$QTY)
Combined_Preliminary$QTY <- as.character(Combined_Preliminary$QTY)
Combined_Preliminary$QTY <- as.numeric(Combined_Preliminary$QTY)
Combined_Preliminary$CARD <- as.factor(unlist(Combined_Preliminary$CARD))
Combined_Preliminary <- as.data.frame(Combined_Preliminary)

if(dim(Combined_Preliminary)[1] != 0 ){
  Reduced_Preliminary <- aggregate(. ~ CARD, data=Combined_Preliminary, sum)
} else {
  Final_Decklist_Results <- 0
  Reduced_Preliminary <- as.data.frame(0)
  print("No Results Were Found")
}
if(dim(Reduced_Preliminary)[1] > 1){
  List <- Reduced_Preliminary$CARD
  ABC <- NULL
  for(i in List){
    df_sub <- subset(Combined_Preliminary, Combined_Preliminary$CARD == i)
    df_sub<- as.data.frame(df_sub)
    A  <- round(mean(df_sub$QTY),2)
    ABC <- rbind(ABC,A)
  }
  Reduced_Preliminary$Avg_Copies <- ABC
  Rarity_Check <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
  colnames(Rarity_Check) <- c("Key","name","Set","Rarity","Foil")
  Reduced_Preliminary$Rarity <- Rarity_Check$Rarity[match(Reduced_Preliminary$CARD,Rarity_Check$name)]
  Reduced_Preliminary$Rarity <- as.character(Reduced_Preliminary$Rarity)
  Reduced_Preliminary$Rarity[is.na(Reduced_Preliminary$Rarity)] = "Unknown"
  Reduced_Preliminary <- as.data.frame(Reduced_Preliminary)
  Reduced_Preliminary$QTY <- as.numeric(Reduced_Preliminary$QTY)
  #View(Reduced_Preliminary)
  Reduced_Preliminary$QTY_Cards_1 <- round(Reduced_Preliminary$QTY / 4,1)
  Reduced_Preliminary$QTY_Cards_Aux <- round(Reduced_Preliminary$QTY / 3,1)
  Reduced_Preliminary$QTY_Cards_1 <- as.character(Reduced_Preliminary$QTY_Cards_1)
  Reduced_Preliminary$QTY_Cards_Aux <- as.character (Reduced_Preliminary$QTY_Cards_Aux)
  Reduced_Preliminary$QTY_Cards <- ifelse(grepl("\\.\\d*$",Reduced_Preliminary$QTY_Cards_1), Reduced_Preliminary$QTY_Cards_Aux, Reduced_Preliminary$QTY_Cards_1)
  Reduced_Preliminary$QTY_Cards <- as.numeric(Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY_Lands <- ifelse(Reduced_Preliminary$Rarity == "L", round(Reduced_Preliminary$QTY / 3,0), Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY <- ifelse(Reduced_Preliminary$Rarity == "L", Reduced_Preliminary$QTY_Lands, Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY_Cards_1 <- Reduced_Preliminary$QTY_Cards
  #View(Reduced_Preliminary)
  Final_Decklist_Results <- Reduced_Preliminary[,1:4]
  Final_Decklist_Results$Avg_Copies <- Final_Decklist_Results$Avg_Copies[,1]
  Final_Decklist_Results$Date <- currentDate
  Final_Decklist_Results$Sample_Size <- Deck_number
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Avg_Copies, -Final_Decklist_Results$QTY),]
  Final_Decklist_Results$Potential_Decks <- round(Final_Decklist_Results$QTY/Final_Decklist_Results$Avg_Copies,0)
  Final_Decklist_Results$QTY <- ifelse(Final_Decklist_Results$Potential_Decks < 1, (Final_Decklist_Results$QTY + 1) ,Final_Decklist_Results$QTY)
  Final_Decklist_Results$Potential_Decks <- ifelse(Final_Decklist_Results$Potential_Decks < 1, Final_Decklist_Results$Potential_Decks + 1, Final_Decklist_Results$Potential_Decks)
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Potential_Decks),]
  Final_Decklist_Results <- Final_Decklist_Results[ which(Final_Decklist_Results$Rarity !='Unknown'),]
  Final_Decklist_Results$Meta_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results$QTY_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results <- Final_Decklist_Results[c(5,6,1,4,8,9,2,3,7)]
  Final_Decklist_Results <- Final_Decklist_Results[order(Final_Decklist_Results$Meta_Rank),]
  Legacy_Results <- Final_Decklist_Results
  
  setwd("/home/cujo253/Reports/Decklists/Legacy Preliminary")
  if(dim(Legacy_Results)[1] != 0 ){
    ss <- drive_get("Preliminary")
    #sheets_deauth()
    gs4_auth(email="pachun95@gmail.com", use_oob = T)
    sheet_write(
      Legacy_Results,
      ss = "/d/1PMVRwPJNPFnpxkksHrgcDaSZFYm5B20SnBsb2gboCEw",
      sheet = "Legacy_Event"
    )
    
    csvFileName <- paste(currentDate,"_Legacy_Event_Lists",".csv",sep="")
    write.csv(Legacy_Results, file=csvFileName, row.names = FALSE)
  } else {
    print("No Legacy Events Were logged Today")
  }
} else {
  print("No Legacy Events Release Today")
}

#Vintage Event####
Premium_WD_Part_1 <- "/home/cujo253/Reports/High Confidence Reps/"
Premium_WD_Part_2 <-  currentDate
Premium_WD_Part_3 <- "_Premium.csv"
URL_Part_1 <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/Vintage-Event-"
URL_Part_2 <- currentDate
URL_Part_3 <- paste(URL_Part_1,URL_Part_2,sep="")
url = URL_Part_3
tryCatch(
  data <- url %>% 
    read_html() %>% 
    html_nodes(".sorted-by-overview-container") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Deck_Number <- as.data.frame(data)
Deck_number <- nrow(Deck_Number)
Final_Export <- read_csv(paste(Premium_WD_Part_1,Premium_WD_Part_2,Premium_WD_Part_3,sep=""),col_types = cols(.default = "c"))

tryCatch(
  Card_Totals <- url %>% 
    read_html() %>% 
    html_nodes(".card-count") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Totals <- as.data.frame(Card_Totals)

tryCatch(
  Card_Name <- url %>% 
    read_html() %>% 
    html_nodes(".card-name") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Name <- as.data.frame(Card_Name)

Combined_Preliminary <- tibble(Card_Totals,
                             Card_Name)
Combined_Preliminary <- as.data.frame(Combined_Preliminary)
names(Combined_Preliminary) <- c("QTY","CARD")

Combined_Preliminary <- as.data.frame(Combined_Preliminary)
Combined_Preliminary$QTY <- unlist(Combined_Preliminary$QTY)
Combined_Preliminary$QTY <- as.character(Combined_Preliminary$QTY)
Combined_Preliminary$QTY <- as.numeric(Combined_Preliminary$QTY)
Combined_Preliminary$CARD <- as.factor(unlist(Combined_Preliminary$CARD))
Combined_Preliminary <- as.data.frame(Combined_Preliminary)

if(dim(Combined_Preliminary)[1] != 0 ){
  Reduced_Preliminary <- aggregate(. ~ CARD, data=Combined_Preliminary, sum)
} else {
  Final_Decklist_Results <- 0
  Reduced_Preliminary <- as.data.frame(0)
  print("No Results Were Found")
}
if(dim(Reduced_Preliminary)[1] > 1){
  List <- Reduced_Preliminary$CARD
  ABC <- NULL
  for(i in List){
    df_sub <- subset(Combined_Preliminary, Combined_Preliminary$CARD == i)
    df_sub<- as.data.frame(df_sub)
    A  <- round(mean(df_sub$QTY),2)
    ABC <- rbind(ABC,A)
  }
  Reduced_Preliminary$Avg_Copies <- ABC
  Rarity_Check <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
  colnames(Rarity_Check) <- c("Key","name","Set","Rarity","Foil")
  Reduced_Preliminary$Rarity <- Rarity_Check$Rarity[match(Reduced_Preliminary$CARD,Rarity_Check$name)]
  Reduced_Preliminary$Rarity <- as.character(Reduced_Preliminary$Rarity)
  Reduced_Preliminary$Rarity[is.na(Reduced_Preliminary$Rarity)] = "Unknown"
  Reduced_Preliminary <- as.data.frame(Reduced_Preliminary)
  Reduced_Preliminary$QTY <- as.numeric(Reduced_Preliminary$QTY)
  #View(Reduced_Preliminary)
  Reduced_Preliminary$QTY_Cards_1 <- round(Reduced_Preliminary$QTY / 4,1)
  Reduced_Preliminary$QTY_Cards_Aux <- round(Reduced_Preliminary$QTY / 3,1)
  Reduced_Preliminary$QTY_Cards_1 <- as.character(Reduced_Preliminary$QTY_Cards_1)
  Reduced_Preliminary$QTY_Cards_Aux <- as.character (Reduced_Preliminary$QTY_Cards_Aux)
  Reduced_Preliminary$QTY_Cards <- ifelse(grepl("\\.\\d*$",Reduced_Preliminary$QTY_Cards_1), Reduced_Preliminary$QTY_Cards_Aux, Reduced_Preliminary$QTY_Cards_1)
  Reduced_Preliminary$QTY_Cards <- as.numeric(Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY_Lands <- ifelse(Reduced_Preliminary$Rarity == "L", round(Reduced_Preliminary$QTY / 3,0), Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY <- ifelse(Reduced_Preliminary$Rarity == "L", Reduced_Preliminary$QTY_Lands, Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY_Cards_1 <- Reduced_Preliminary$QTY_Cards
  #View(Reduced_Preliminary)
  Final_Decklist_Results <- Reduced_Preliminary[,1:4]
  Final_Decklist_Results$Avg_Copies <- Final_Decklist_Results$Avg_Copies[,1]
  Final_Decklist_Results$Date <- currentDate
  Final_Decklist_Results$Sample_Size <- Deck_number
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Avg_Copies, -Final_Decklist_Results$QTY),]
  Final_Decklist_Results$Potential_Decks <- round(Final_Decklist_Results$QTY/Final_Decklist_Results$Avg_Copies,0)
  Final_Decklist_Results$QTY <- ifelse(Final_Decklist_Results$Potential_Decks < 1, (Final_Decklist_Results$QTY + 1) ,Final_Decklist_Results$QTY)
  Final_Decklist_Results$Potential_Decks <- ifelse(Final_Decklist_Results$Potential_Decks < 1, Final_Decklist_Results$Potential_Decks + 1, Final_Decklist_Results$Potential_Decks)
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Potential_Decks),]
  Final_Decklist_Results <- Final_Decklist_Results[ which(Final_Decklist_Results$Rarity !='Unknown'),]
  Final_Decklist_Results$Meta_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results$QTY_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results <- Final_Decklist_Results[c(5,6,1,4,8,9,2,3,7)]
  Final_Decklist_Results <- Final_Decklist_Results[order(Final_Decklist_Results$Meta_Rank),]
  Vintage_Results <- Final_Decklist_Results
  
  setwd("/home/cujo253/Reports/Decklists/Vintage Preliminary")
  if(dim(Vintage_Results)[1] != 0 ){
    ss <- drive_get("Preliminary")
    #sheets_deauth()
    gs4_auth(email="pachun95@gmail.com", use_oob = T)
    sheet_write(
      Vintage_Results,
      ss = "/d/1PMVRwPJNPFnpxkksHrgcDaSZFYm5B20SnBsb2gboCEw",
      sheet = "Vintage_Event"
    )
    
    csvFileName <- paste(currentDate,"_Vintage_Event_Lists",".csv",sep="")
    write.csv(Vintage_Results, file=csvFileName, row.names = FALSE)
  } else {
    print("No Vintage Events Were logged Today")
  }
} else {
  print("No Vintage Events Release Today")
}


#Pauper Event####
Premium_WD_Part_1 <- "/home/cujo253/Reports/High Confidence Reps/"
Premium_WD_Part_2 <-  currentDate
Premium_WD_Part_3 <- "_Premium.csv"
URL_Part_1 <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/Pauper-Event-"
URL_Part_2 <- currentDate
URL_Part_3 <- paste(URL_Part_1,URL_Part_2,sep="")
url = URL_Part_3
tryCatch(
  data <- url %>% 
    read_html() %>% 
    html_nodes(".sorted-by-overview-container") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Deck_Number <- as.data.frame(data)
Deck_number <- nrow(Deck_Number)
Final_Export <- read_csv(paste(Premium_WD_Part_1,Premium_WD_Part_2,Premium_WD_Part_3,sep=""),col_types = cols(.default = "c"))

tryCatch(
  Card_Totals <- url %>% 
    read_html() %>% 
    html_nodes(".card-count") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Totals <- as.data.frame(Card_Totals)

tryCatch(
  Card_Name <- url %>% 
    read_html() %>% 
    html_nodes(".card-name") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Name <- as.data.frame(Card_Name)

Combined_Preliminary <- tibble(Card_Totals,
                             Card_Name)
Combined_Preliminary <- as.data.frame(Combined_Preliminary)
names(Combined_Preliminary) <- c("QTY","CARD")

Combined_Preliminary <- as.data.frame(Combined_Preliminary)
Combined_Preliminary$QTY <- unlist(Combined_Preliminary$QTY)
Combined_Preliminary$QTY <- as.character(Combined_Preliminary$QTY)
Combined_Preliminary$QTY <- as.numeric(Combined_Preliminary$QTY)
Combined_Preliminary$CARD <- as.factor(unlist(Combined_Preliminary$CARD))
Combined_Preliminary <- as.data.frame(Combined_Preliminary)

if(dim(Combined_Preliminary)[1] != 0 ){
  Reduced_Preliminary <- aggregate(. ~ CARD, data=Combined_Preliminary, sum)
} else {
  Final_Decklist_Results <- 0
  Reduced_Preliminary <- as.data.frame(0)
  print("No Results Were Found")
}
if(dim(Reduced_Preliminary)[1] > 1){
  List <- Reduced_Preliminary$CARD
  ABC <- NULL
  for(i in List){
    df_sub <- subset(Combined_Preliminary, Combined_Preliminary$CARD == i)
    df_sub<- as.data.frame(df_sub)
    A  <- round(mean(df_sub$QTY),2)
    ABC <- rbind(ABC,A)
  }
  Reduced_Preliminary$Avg_Copies <- ABC
  Rarity_Check <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
  colnames(Rarity_Check) <- c("Key","name","Set","Rarity","Foil")
  Reduced_Preliminary$Rarity <- Rarity_Check$Rarity[match(Reduced_Preliminary$CARD,Rarity_Check$name)]
  Reduced_Preliminary$Rarity <- as.character(Reduced_Preliminary$Rarity)
  Reduced_Preliminary$Rarity[is.na(Reduced_Preliminary$Rarity)] = "Unknown"
  Reduced_Preliminary <- as.data.frame(Reduced_Preliminary)
  Reduced_Preliminary$QTY <- as.numeric(Reduced_Preliminary$QTY)
  #View(Reduced_Preliminary)
  Reduced_Preliminary$QTY_Cards_1 <- round(Reduced_Preliminary$QTY / 4,1)
  Reduced_Preliminary$QTY_Cards_Aux <- round(Reduced_Preliminary$QTY / 3,1)
  Reduced_Preliminary$QTY_Cards_1 <- as.character(Reduced_Preliminary$QTY_Cards_1)
  Reduced_Preliminary$QTY_Cards_Aux <- as.character (Reduced_Preliminary$QTY_Cards_Aux)
  Reduced_Preliminary$QTY_Cards <- ifelse(grepl("\\.\\d*$",Reduced_Preliminary$QTY_Cards_1), Reduced_Preliminary$QTY_Cards_Aux, Reduced_Preliminary$QTY_Cards_1)
  Reduced_Preliminary$QTY_Cards <- as.numeric(Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY_Lands <- ifelse(Reduced_Preliminary$Rarity == "L", round(Reduced_Preliminary$QTY / 3,0), Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY <- ifelse(Reduced_Preliminary$Rarity == "L", Reduced_Preliminary$QTY_Lands, Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY_Cards_1 <- Reduced_Preliminary$QTY_Cards
  #View(Reduced_Preliminary)
  Final_Decklist_Results <- Reduced_Preliminary[,1:4]
  Final_Decklist_Results$Avg_Copies <- Final_Decklist_Results$Avg_Copies[,1]
  Final_Decklist_Results$Date <- currentDate
  Final_Decklist_Results$Sample_Size <- Deck_number
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Avg_Copies, -Final_Decklist_Results$QTY),]
  Final_Decklist_Results$Potential_Decks <- round(Final_Decklist_Results$QTY/Final_Decklist_Results$Avg_Copies,0)
  Final_Decklist_Results$QTY <- ifelse(Final_Decklist_Results$Potential_Decks < 1, (Final_Decklist_Results$QTY + 1) ,Final_Decklist_Results$QTY)
  Final_Decklist_Results$Potential_Decks <- ifelse(Final_Decklist_Results$Potential_Decks < 1, Final_Decklist_Results$Potential_Decks + 1, Final_Decklist_Results$Potential_Decks)
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Potential_Decks),]
  Final_Decklist_Results <- Final_Decklist_Results[ which(Final_Decklist_Results$Rarity !='Unknown'),]
  Final_Decklist_Results$Meta_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results$QTY_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results <- Final_Decklist_Results[c(5,6,1,4,8,9,2,3,7)]
  Final_Decklist_Results <- Final_Decklist_Results[order(Final_Decklist_Results$Meta_Rank),]
  Pauper_Results <- Final_Decklist_Results
  
  setwd("/home/cujo253/Reports/Decklists/Pauper Preliminary")
  if(dim(Pauper_Results)[1] != 0 ){
    ss <- drive_get("Preliminary")
    #sheets_deauth()
    gs4_auth(email="pachun95@gmail.com", use_oob = T)
    sheet_write(
      Pauper_Results,
      ss = "/d/1PMVRwPJNPFnpxkksHrgcDaSZFYm5B20SnBsb2gboCEw",
      sheet = "Pauper_Event"
    )
    
    csvFileName <- paste(currentDate,"_Pauper_Event_Lists",".csv",sep="")
    write.csv(Pauper_Results, file=csvFileName, row.names = FALSE)
  } else {
    print("No Pauper Events Were logged Today")
  }
} else {
  print("No Pauper Events Release Today")
}









Sys.sleep(10)
#Standard Challenge####
Premium_WD_Part_1 <- "/home/cujo253/Reports/High Confidence Reps/"
Premium_WD_Part_2 <-  currentDate
Premium_WD_Part_3 <- "_Premium.csv"
URL_Part_1 <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/Standard-Challenge-"
URL_Part_2 <- currentDate
URL_Part_3 <- paste(URL_Part_1,URL_Part_2,sep="")
url = URL_Part_3
tryCatch(
  data <- url %>% 
    read_html() %>% 
    html_nodes(".sorted-by-overview-container") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Deck_Number <- as.data.frame(data)
Deck_number <- nrow(Deck_Number)
Final_Export <- read_csv(paste(Premium_WD_Part_1,Premium_WD_Part_2,Premium_WD_Part_3,sep=""),col_types = cols(.default = "c"))

tryCatch(
  Card_Totals <- url %>% 
    read_html() %>% 
    html_nodes(".card-count") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Totals <- as.data.frame(Card_Totals)

tryCatch(
  Card_Name <- url %>% 
    read_html() %>% 
    html_nodes(".card-name") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Name <- as.data.frame(Card_Name)

Combined_Preliminary <- tibble(Card_Totals,
                               Card_Name)
Combined_Preliminary <- as.data.frame(Combined_Preliminary)
names(Combined_Preliminary) <- c("QTY","CARD")

Combined_Preliminary <- as.data.frame(Combined_Preliminary)
Combined_Preliminary$QTY <- unlist(Combined_Preliminary$QTY)
Combined_Preliminary$QTY <- as.character(Combined_Preliminary$QTY)
Combined_Preliminary$QTY <- as.numeric(Combined_Preliminary$QTY)
Combined_Preliminary$CARD <- as.factor(unlist(Combined_Preliminary$CARD))
Combined_Preliminary <- as.data.frame(Combined_Preliminary)

if(dim(Combined_Preliminary)[1] != 0 ){
  Reduced_Preliminary <- aggregate(. ~ CARD, data=Combined_Preliminary, sum)
} else {
  Final_Decklist_Results <- 0
  Reduced_Preliminary <- as.data.frame(0)
  print("No Results Were Found")
}
if(dim(Reduced_Preliminary)[1] > 1){
  List <- Reduced_Preliminary$CARD
  ABC <- NULL
  for(i in List){
    df_sub <- subset(Combined_Preliminary, Combined_Preliminary$CARD == i)
    df_sub<- as.data.frame(df_sub)
    A  <- round(mean(df_sub$QTY),2)
    ABC <- rbind(ABC,A)
  }
  Reduced_Preliminary$Avg_Copies <- ABC
  Rarity_Check <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
  colnames(Rarity_Check) <- c("Key","name","Set","Rarity","Foil")
  Reduced_Preliminary$Rarity <- Rarity_Check$Rarity[match(Reduced_Preliminary$CARD,Rarity_Check$name)]
  Reduced_Preliminary$Rarity <- as.character(Reduced_Preliminary$Rarity)
  Reduced_Preliminary$Rarity[is.na(Reduced_Preliminary$Rarity)] = "Unknown"
  Reduced_Preliminary <- as.data.frame(Reduced_Preliminary)
  Reduced_Preliminary$QTY <- as.numeric(Reduced_Preliminary$QTY)
  #View(Reduced_Preliminary)
  Reduced_Preliminary$QTY_Cards_1 <- round(Reduced_Preliminary$QTY / 4,1)
  Reduced_Preliminary$QTY_Cards_Aux <- round(Reduced_Preliminary$QTY / 3,1)
  Reduced_Preliminary$QTY_Cards_1 <- as.character(Reduced_Preliminary$QTY_Cards_1)
  Reduced_Preliminary$QTY_Cards_Aux <- as.character (Reduced_Preliminary$QTY_Cards_Aux)
  Reduced_Preliminary$QTY_Cards <- ifelse(grepl("\\.\\d*$",Reduced_Preliminary$QTY_Cards_1), Reduced_Preliminary$QTY_Cards_Aux, Reduced_Preliminary$QTY_Cards_1)
  Reduced_Preliminary$QTY_Cards <- as.numeric(Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY_Lands <- ifelse(Reduced_Preliminary$Rarity == "L", round(Reduced_Preliminary$QTY / 3,0), Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY <- ifelse(Reduced_Preliminary$Rarity == "L", Reduced_Preliminary$QTY_Lands, Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY_Cards_1 <- Reduced_Preliminary$QTY_Cards
  #View(Reduced_Preliminary)
  Final_Decklist_Results <- Reduced_Preliminary[,1:4]
  Final_Decklist_Results$Avg_Copies <- Final_Decklist_Results$Avg_Copies[,1]
  Final_Decklist_Results$Date <- currentDate
  Final_Decklist_Results$Sample_Size <- Deck_number
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Avg_Copies, -Final_Decklist_Results$QTY),]
  Final_Decklist_Results$Potential_Decks <- round(Final_Decklist_Results$QTY/Final_Decklist_Results$Avg_Copies,0)
  Final_Decklist_Results$QTY <- ifelse(Final_Decklist_Results$Potential_Decks < 1, (Final_Decklist_Results$QTY + 1) ,Final_Decklist_Results$QTY)
  Final_Decklist_Results$Potential_Decks <- ifelse(Final_Decklist_Results$Potential_Decks < 1, Final_Decklist_Results$Potential_Decks + 1, Final_Decklist_Results$Potential_Decks)
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Potential_Decks),]
  Final_Decklist_Results <- Final_Decklist_Results[ which(Final_Decklist_Results$Rarity !='Unknown'),]
  Final_Decklist_Results$Meta_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results$QTY_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results <- Final_Decklist_Results[c(5,6,1,4,8,9,2,3,7)]
  Final_Decklist_Results <- Final_Decklist_Results[order(Final_Decklist_Results$Meta_Rank),]
  Standard_Results <- Final_Decklist_Results
  
  setwd("/home/cujo253/Reports/Decklists/Standard Preliminary")
  if(dim(Standard_Results)[1] != 0 ){
    ss <- drive_get("Preliminary")
    #sheets_deauth()
    gs4_auth(email="pachun95@gmail.com", use_oob = T)
    sheet_write(
      Standard_Results,
      ss = "/d/1PMVRwPJNPFnpxkksHrgcDaSZFYm5B20SnBsb2gboCEw",
      sheet = "Standard_Challenge"
    )
    csvFileName <- paste(currentDate,"_Standard_Challenge_Lists",".csv",sep="")
    write.csv(Standard_Results, file=csvFileName, row.names = FALSE)
  } else {
    print("No Standard Challenges Were logged Today")
  }
} else {
  print("No Standard Challenges Release Today")
}

#Pioneer Challenge####
Premium_WD_Part_1 <- "/home/cujo253/Reports/High Confidence Reps/"
Premium_WD_Part_2 <-  currentDate
Premium_WD_Part_3 <- "_Premium.csv"
URL_Part_1 <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/Pioneer-Challenge-"
URL_Part_2 <- currentDate
URL_Part_3 <- paste(URL_Part_1,URL_Part_2,sep="")
url = URL_Part_3
tryCatch(
  data <- url %>% 
    read_html() %>% 
    html_nodes(".sorted-by-overview-container") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Deck_Number <- as.data.frame(data)
Deck_number <- nrow(Deck_Number)
Final_Export <- read_csv(paste(Premium_WD_Part_1,Premium_WD_Part_2,Premium_WD_Part_3,sep=""),col_types = cols(.default = "c"))

tryCatch(
  Card_Totals <- url %>% 
    read_html() %>% 
    html_nodes(".card-count") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Totals <- as.data.frame(Card_Totals)

tryCatch(
  Card_Name <- url %>% 
    read_html() %>% 
    html_nodes(".card-name") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Name <- as.data.frame(Card_Name)

Combined_Preliminary <- tibble(Card_Totals,
                               Card_Name)
Combined_Preliminary <- as.data.frame(Combined_Preliminary)
names(Combined_Preliminary) <- c("QTY","CARD")

Combined_Preliminary <- as.data.frame(Combined_Preliminary)
Combined_Preliminary$QTY <- unlist(Combined_Preliminary$QTY)
Combined_Preliminary$QTY <- as.character(Combined_Preliminary$QTY)
Combined_Preliminary$QTY <- as.numeric(Combined_Preliminary$QTY)
Combined_Preliminary$CARD <- as.factor(unlist(Combined_Preliminary$CARD))
Combined_Preliminary <- as.data.frame(Combined_Preliminary)

if(dim(Combined_Preliminary)[1] != 0 ){
  Reduced_Preliminary <- aggregate(. ~ CARD, data=Combined_Preliminary, sum)
} else {
  Final_Decklist_Results <- 0
  Reduced_Preliminary <- as.data.frame(0)
  print("No Results Were Found")
}
if(dim(Reduced_Preliminary)[1] > 1){
  List <- Reduced_Preliminary$CARD
  ABC <- NULL
  for(i in List){
    df_sub <- subset(Combined_Preliminary, Combined_Preliminary$CARD == i)
    df_sub<- as.data.frame(df_sub)
    A  <- round(mean(df_sub$QTY),2)
    ABC <- rbind(ABC,A)
  }
  Reduced_Preliminary$Avg_Copies <- ABC
  Rarity_Check <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
  colnames(Rarity_Check) <- c("Key","name","Set","Rarity","Foil")
  Reduced_Preliminary$Rarity <- Rarity_Check$Rarity[match(Reduced_Preliminary$CARD,Rarity_Check$name)]
  Reduced_Preliminary$Rarity <- as.character(Reduced_Preliminary$Rarity)
  Reduced_Preliminary$Rarity[is.na(Reduced_Preliminary$Rarity)] = "Unknown"
  Reduced_Preliminary <- as.data.frame(Reduced_Preliminary)
  Reduced_Preliminary$QTY <- as.numeric(Reduced_Preliminary$QTY)
  #View(Reduced_Preliminary)
  Reduced_Preliminary$QTY_Cards_1 <- round(Reduced_Preliminary$QTY / 4,1)
  Reduced_Preliminary$QTY_Cards_Aux <- round(Reduced_Preliminary$QTY / 3,1)
  Reduced_Preliminary$QTY_Cards_1 <- as.character(Reduced_Preliminary$QTY_Cards_1)
  Reduced_Preliminary$QTY_Cards_Aux <- as.character (Reduced_Preliminary$QTY_Cards_Aux)
  Reduced_Preliminary$QTY_Cards <- ifelse(grepl("\\.\\d*$",Reduced_Preliminary$QTY_Cards_1), Reduced_Preliminary$QTY_Cards_Aux, Reduced_Preliminary$QTY_Cards_1)
  Reduced_Preliminary$QTY_Cards <- as.numeric(Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY_Lands <- ifelse(Reduced_Preliminary$Rarity == "L", round(Reduced_Preliminary$QTY / 3,0), Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY <- ifelse(Reduced_Preliminary$Rarity == "L", Reduced_Preliminary$QTY_Lands, Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY_Cards_1 <- Reduced_Preliminary$QTY_Cards
  #View(Reduced_Preliminary)
  Final_Decklist_Results <- Reduced_Preliminary[,1:4]
  Final_Decklist_Results$Avg_Copies <- Final_Decklist_Results$Avg_Copies[,1]
  Final_Decklist_Results$Date <- currentDate
  Final_Decklist_Results$Sample_Size <- Deck_number
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Avg_Copies, -Final_Decklist_Results$QTY),]
  Final_Decklist_Results$Potential_Decks <- round(Final_Decklist_Results$QTY/Final_Decklist_Results$Avg_Copies,0)
  Final_Decklist_Results$QTY <- ifelse(Final_Decklist_Results$Potential_Decks < 1, (Final_Decklist_Results$QTY + 1) ,Final_Decklist_Results$QTY)
  Final_Decklist_Results$Potential_Decks <- ifelse(Final_Decklist_Results$Potential_Decks < 1, Final_Decklist_Results$Potential_Decks + 1, Final_Decklist_Results$Potential_Decks)
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Potential_Decks),]
  Final_Decklist_Results <- Final_Decklist_Results[ which(Final_Decklist_Results$Rarity !='Unknown'),]
  Final_Decklist_Results$Meta_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results$QTY_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results <- Final_Decklist_Results[c(5,6,1,4,8,9,2,3,7)]
  Final_Decklist_Results <- Final_Decklist_Results[order(Final_Decklist_Results$Meta_Rank),]
  Pioneer_Results <- Final_Decklist_Results
  
  setwd("/home/cujo253/Reports/Decklists/Pioneer Preliminary")
  if(dim(Pioneer_Results)[1] != 0 ){
    ss <- drive_get("Preliminary")
    #sheets_deauth()
    gs4_auth(email="pachun95@gmail.com", use_oob = T)
    sheet_write(
      Pioneer_Results,
      ss = "/d/1PMVRwPJNPFnpxkksHrgcDaSZFYm5B20SnBsb2gboCEw",
      sheet = "Pioneer_Challenge"
    )
    csvFileName <- paste(currentDate,"_Pioneer_Deck_Lists",".csv",sep="")
    write.csv(Pioneer_Results, file=csvFileName, row.names = FALSE)
  } else {
    print("No Pioneer Challenges Were logged Today")
  }
} else {
  print("No Pioneer Challenges Release Today")
}

#Modern Challenge####
Premium_WD_Part_1 <- "/home/cujo253/Reports/High Confidence Reps/"
Premium_WD_Part_2 <-  currentDate
Premium_WD_Part_3 <- "_Premium.csv"
URL_Part_1 <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/Modern-Challenge-"
URL_Part_2 <- currentDate
URL_Part_3 <- paste(URL_Part_1,URL_Part_2,sep="")
url = URL_Part_3
tryCatch(
  data <- url %>% 
    read_html() %>% 
    html_nodes(".sorted-by-overview-container") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Deck_Number <- as.data.frame(data)
Deck_number <- nrow(Deck_Number)
Final_Export <- read_csv(paste(Premium_WD_Part_1,Premium_WD_Part_2,Premium_WD_Part_3,sep=""),col_types = cols(.default = "c"))

tryCatch(
  Card_Totals <- url %>% 
    read_html() %>% 
    html_nodes(".card-count") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Totals <- as.data.frame(Card_Totals)

tryCatch(
  Card_Name <- url %>% 
    read_html() %>% 
    html_nodes(".card-name") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Name <- as.data.frame(Card_Name)

Combined_Preliminary <- tibble(Card_Totals,
                               Card_Name)
Combined_Preliminary <- as.data.frame(Combined_Preliminary)
names(Combined_Preliminary) <- c("QTY","CARD")

Combined_Preliminary <- as.data.frame(Combined_Preliminary)
Combined_Preliminary$QTY <- unlist(Combined_Preliminary$QTY)
Combined_Preliminary$QTY <- as.character(Combined_Preliminary$QTY)
Combined_Preliminary$QTY <- as.numeric(Combined_Preliminary$QTY)
Combined_Preliminary$CARD <- as.factor(unlist(Combined_Preliminary$CARD))
Combined_Preliminary <- as.data.frame(Combined_Preliminary)

if(dim(Combined_Preliminary)[1] != 0 ){
  Reduced_Preliminary <- aggregate(. ~ CARD, data=Combined_Preliminary, sum)
} else {
  Final_Decklist_Results <- 0
  Reduced_Preliminary <- as.data.frame(0)
  print("No Results Were Found")
}
if(dim(Reduced_Preliminary)[1] > 1){
  List <- Reduced_Preliminary$CARD
  ABC <- NULL
  for(i in List){
    df_sub <- subset(Combined_Preliminary, Combined_Preliminary$CARD == i)
    df_sub<- as.data.frame(df_sub)
    A  <- round(mean(df_sub$QTY),2)
    ABC <- rbind(ABC,A)
  }
  Reduced_Preliminary$Avg_Copies <- ABC
  Rarity_Check <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
  colnames(Rarity_Check) <- c("Key","name","Set","Rarity","Foil")
  Reduced_Preliminary$Rarity <- Rarity_Check$Rarity[match(Reduced_Preliminary$CARD,Rarity_Check$name)]
  Reduced_Preliminary$Rarity <- as.character(Reduced_Preliminary$Rarity)
  Reduced_Preliminary$Rarity[is.na(Reduced_Preliminary$Rarity)] = "Unknown"
  Reduced_Preliminary <- as.data.frame(Reduced_Preliminary)
  Reduced_Preliminary$QTY <- as.numeric(Reduced_Preliminary$QTY)
  #View(Reduced_Preliminary)
  Reduced_Preliminary$QTY_Cards_1 <- round(Reduced_Preliminary$QTY / 4,1)
  Reduced_Preliminary$QTY_Cards_Aux <- round(Reduced_Preliminary$QTY / 3,1)
  Reduced_Preliminary$QTY_Cards_1 <- as.character(Reduced_Preliminary$QTY_Cards_1)
  Reduced_Preliminary$QTY_Cards_Aux <- as.character (Reduced_Preliminary$QTY_Cards_Aux)
  Reduced_Preliminary$QTY_Cards <- ifelse(grepl("\\.\\d*$",Reduced_Preliminary$QTY_Cards_1), Reduced_Preliminary$QTY_Cards_Aux, Reduced_Preliminary$QTY_Cards_1)
  Reduced_Preliminary$QTY_Cards <- as.numeric(Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY_Lands <- ifelse(Reduced_Preliminary$Rarity == "L", round(Reduced_Preliminary$QTY / 3,0), Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY <- ifelse(Reduced_Preliminary$Rarity == "L", Reduced_Preliminary$QTY_Lands, Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY_Cards_1 <- Reduced_Preliminary$QTY_Cards
  #View(Reduced_Preliminary)
  Final_Decklist_Results <- Reduced_Preliminary[,1:4]
  Final_Decklist_Results$Avg_Copies <- Final_Decklist_Results$Avg_Copies[,1]
  Final_Decklist_Results$Date <- currentDate
  Final_Decklist_Results$Sample_Size <- Deck_number
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Avg_Copies, -Final_Decklist_Results$QTY),]
  Final_Decklist_Results$Potential_Decks <- round(Final_Decklist_Results$QTY/Final_Decklist_Results$Avg_Copies,0)
  Final_Decklist_Results$QTY <- ifelse(Final_Decklist_Results$Potential_Decks < 1, (Final_Decklist_Results$QTY + 1) ,Final_Decklist_Results$QTY)
  Final_Decklist_Results$Potential_Decks <- ifelse(Final_Decklist_Results$Potential_Decks < 1, Final_Decklist_Results$Potential_Decks + 1, Final_Decklist_Results$Potential_Decks)
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Potential_Decks),]
  Final_Decklist_Results <- Final_Decklist_Results[ which(Final_Decklist_Results$Rarity !='Unknown'),]
  Final_Decklist_Results$Meta_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results$QTY_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results <- Final_Decklist_Results[c(5,6,1,4,8,9,2,3,7)]
  Final_Decklist_Results <- Final_Decklist_Results[order(Final_Decklist_Results$Meta_Rank),]
  Modern_Results <- Final_Decklist_Results
  
  setwd("/home/cujo253/Reports/Decklists/Modern Preliminary")
  if(dim(Modern_Results)[1] != 0 ){
    ss <- drive_get("Preliminary")
    #sheets_deauth()
    gs4_auth(email="pachun95@gmail.com", use_oob = T)
    sheet_write(
      Modern_Results,
      ss = "/d/1PMVRwPJNPFnpxkksHrgcDaSZFYm5B20SnBsb2gboCEw",
      sheet = "Modern_Challenge"
    )
    csvFileName <- paste(currentDate,"_Modern_Deck_Lists",".csv",sep="")
    write.csv(Modern_Results, file=csvFileName, row.names = FALSE)
  } else {
    print("No Modern Challenges Were logged Today")
  }
} else {
  print("No Modern Challenges Release Today")
}

#Legacy Challenge####
Premium_WD_Part_1 <- "/home/cujo253/Reports/High Confidence Reps/"
Premium_WD_Part_2 <-  currentDate
Premium_WD_Part_3 <- "_Premium.csv"
URL_Part_1 <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/Legacy-Challenge-"
URL_Part_2 <- currentDate
URL_Part_3 <- paste(URL_Part_1,URL_Part_2,sep="")
url = URL_Part_3
tryCatch(
  data <- url %>% 
    read_html() %>% 
    html_nodes(".sorted-by-overview-container") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Deck_Number <- as.data.frame(data)
Deck_number <- nrow(Deck_Number)
Final_Export <- read_csv(paste(Premium_WD_Part_1,Premium_WD_Part_2,Premium_WD_Part_3,sep=""),col_types = cols(.default = "c"))

tryCatch(
  Card_Totals <- url %>% 
    read_html() %>% 
    html_nodes(".card-count") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Totals <- as.data.frame(Card_Totals)

tryCatch(
  Card_Name <- url %>% 
    read_html() %>% 
    html_nodes(".card-name") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Name <- as.data.frame(Card_Name)

Combined_Preliminary <- tibble(Card_Totals,
                               Card_Name)
Combined_Preliminary <- as.data.frame(Combined_Preliminary)
names(Combined_Preliminary) <- c("QTY","CARD")

Combined_Preliminary <- as.data.frame(Combined_Preliminary)
Combined_Preliminary$QTY <- unlist(Combined_Preliminary$QTY)
Combined_Preliminary$QTY <- as.character(Combined_Preliminary$QTY)
Combined_Preliminary$QTY <- as.numeric(Combined_Preliminary$QTY)
Combined_Preliminary$CARD <- as.factor(unlist(Combined_Preliminary$CARD))
Combined_Preliminary <- as.data.frame(Combined_Preliminary)

if(dim(Combined_Preliminary)[1] != 0 ){
  Reduced_Preliminary <- aggregate(. ~ CARD, data=Combined_Preliminary, sum)
} else {
  Final_Decklist_Results <- 0
  Reduced_Preliminary <- as.data.frame(0)
  print("No Results Were Found")
}
if(dim(Reduced_Preliminary)[1] > 1){
  List <- Reduced_Preliminary$CARD
  ABC <- NULL
  for(i in List){
    df_sub <- subset(Combined_Preliminary, Combined_Preliminary$CARD == i)
    df_sub<- as.data.frame(df_sub)
    A  <- round(mean(df_sub$QTY),2)
    ABC <- rbind(ABC,A)
  }
  Reduced_Preliminary$Avg_Copies <- ABC
  Rarity_Check <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
  colnames(Rarity_Check) <- c("Key","name","Set","Rarity","Foil")
  Reduced_Preliminary$Rarity <- Rarity_Check$Rarity[match(Reduced_Preliminary$CARD,Rarity_Check$name)]
  Reduced_Preliminary$Rarity <- as.character(Reduced_Preliminary$Rarity)
  Reduced_Preliminary$Rarity[is.na(Reduced_Preliminary$Rarity)] = "Unknown"
  Reduced_Preliminary <- as.data.frame(Reduced_Preliminary)
  Reduced_Preliminary$QTY <- as.numeric(Reduced_Preliminary$QTY)
  #View(Reduced_Preliminary)
  Reduced_Preliminary$QTY_Cards_1 <- round(Reduced_Preliminary$QTY / 4,1)
  Reduced_Preliminary$QTY_Cards_Aux <- round(Reduced_Preliminary$QTY / 3,1)
  Reduced_Preliminary$QTY_Cards_1 <- as.character(Reduced_Preliminary$QTY_Cards_1)
  Reduced_Preliminary$QTY_Cards_Aux <- as.character (Reduced_Preliminary$QTY_Cards_Aux)
  Reduced_Preliminary$QTY_Cards <- ifelse(grepl("\\.\\d*$",Reduced_Preliminary$QTY_Cards_1), Reduced_Preliminary$QTY_Cards_Aux, Reduced_Preliminary$QTY_Cards_1)
  Reduced_Preliminary$QTY_Cards <- as.numeric(Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY_Lands <- ifelse(Reduced_Preliminary$Rarity == "L", round(Reduced_Preliminary$QTY / 3,0), Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY <- ifelse(Reduced_Preliminary$Rarity == "L", Reduced_Preliminary$QTY_Lands, Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY_Cards_1 <- Reduced_Preliminary$QTY_Cards
  #View(Reduced_Preliminary)
  Final_Decklist_Results <- Reduced_Preliminary[,1:4]
  Final_Decklist_Results$Avg_Copies <- Final_Decklist_Results$Avg_Copies[,1]
  Final_Decklist_Results$Date <- currentDate
  Final_Decklist_Results$Sample_Size <- Deck_number
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Avg_Copies, -Final_Decklist_Results$QTY),]
  Final_Decklist_Results$Potential_Decks <- round(Final_Decklist_Results$QTY/Final_Decklist_Results$Avg_Copies,0)
  Final_Decklist_Results$QTY <- ifelse(Final_Decklist_Results$Potential_Decks < 1, (Final_Decklist_Results$QTY + 1) ,Final_Decklist_Results$QTY)
  Final_Decklist_Results$Potential_Decks <- ifelse(Final_Decklist_Results$Potential_Decks < 1, Final_Decklist_Results$Potential_Decks + 1, Final_Decklist_Results$Potential_Decks)
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Potential_Decks),]
  Final_Decklist_Results <- Final_Decklist_Results[ which(Final_Decklist_Results$Rarity !='Unknown'),]
  Final_Decklist_Results$Meta_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results$QTY_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results <- Final_Decklist_Results[c(5,6,1,4,8,9,2,3,7)]
  Final_Decklist_Results <- Final_Decklist_Results[order(Final_Decklist_Results$Meta_Rank),]
  Legacy_Results <- Final_Decklist_Results
  
  setwd("/home/cujo253/Reports/Decklists/Legacy Preliminary")
  if(dim(Legacy_Results)[1] != 0 ){
    ss <- drive_get("Preliminary")
    #sheets_deauth()
    gs4_auth(email="pachun95@gmail.com", use_oob = T)
    sheet_write(
      Legacy_Results,
      ss = "/d/1PMVRwPJNPFnpxkksHrgcDaSZFYm5B20SnBsb2gboCEw",
      sheet = "Legacy_Challenge"
    )
    
    csvFileName <- paste(currentDate,"_Legacy_Deck_Lists",".csv",sep="")
    write.csv(Legacy_Results, file=csvFileName, row.names = FALSE)
  } else {
    print("No Legacy Challenges Were logged Today")
  }
} else {
  print("No Legacy Challenges Release Today")
}

#Vintage Challenge####
Premium_WD_Part_1 <- "/home/cujo253/Reports/High Confidence Reps/"
Premium_WD_Part_2 <-  currentDate
Premium_WD_Part_3 <- "_Premium.csv"
URL_Part_1 <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/Vintage-Challenge-"
URL_Part_2 <- currentDate
URL_Part_3 <- paste(URL_Part_1,URL_Part_2,sep="")
url = URL_Part_3
tryCatch(
  data <- url %>% 
    read_html() %>% 
    html_nodes(".sorted-by-overview-container") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Deck_Number <- as.data.frame(data)
Deck_number <- nrow(Deck_Number)
Final_Export <- read_csv(paste(Premium_WD_Part_1,Premium_WD_Part_2,Premium_WD_Part_3,sep=""),col_types = cols(.default = "c"))

tryCatch(
  Card_Totals <- url %>% 
    read_html() %>% 
    html_nodes(".card-count") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Totals <- as.data.frame(Card_Totals)

tryCatch(
  Card_Name <- url %>% 
    read_html() %>% 
    html_nodes(".card-name") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Name <- as.data.frame(Card_Name)

Combined_Preliminary <- tibble(Card_Totals,
                               Card_Name)
Combined_Preliminary <- as.data.frame(Combined_Preliminary)
names(Combined_Preliminary) <- c("QTY","CARD")

Combined_Preliminary <- as.data.frame(Combined_Preliminary)
Combined_Preliminary$QTY <- unlist(Combined_Preliminary$QTY)
Combined_Preliminary$QTY <- as.character(Combined_Preliminary$QTY)
Combined_Preliminary$QTY <- as.numeric(Combined_Preliminary$QTY)
Combined_Preliminary$CARD <- as.factor(unlist(Combined_Preliminary$CARD))
Combined_Preliminary <- as.data.frame(Combined_Preliminary)

if(dim(Combined_Preliminary)[1] != 0 ){
  Reduced_Preliminary <- aggregate(. ~ CARD, data=Combined_Preliminary, sum)
} else {
  Final_Decklist_Results <- 0
  Reduced_Preliminary <- as.data.frame(0)
  print("No Results Were Found")
}
if(dim(Reduced_Preliminary)[1] > 1){
  List <- Reduced_Preliminary$CARD
  ABC <- NULL
  for(i in List){
    df_sub <- subset(Combined_Preliminary, Combined_Preliminary$CARD == i)
    df_sub<- as.data.frame(df_sub)
    A  <- round(mean(df_sub$QTY),2)
    ABC <- rbind(ABC,A)
  }
  Reduced_Preliminary$Avg_Copies <- ABC
  Rarity_Check <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
  colnames(Rarity_Check) <- c("Key","name","Set","Rarity","Foil")
  Reduced_Preliminary$Rarity <- Rarity_Check$Rarity[match(Reduced_Preliminary$CARD,Rarity_Check$name)]
  Reduced_Preliminary$Rarity <- as.character(Reduced_Preliminary$Rarity)
  Reduced_Preliminary$Rarity[is.na(Reduced_Preliminary$Rarity)] = "Unknown"
  Reduced_Preliminary <- as.data.frame(Reduced_Preliminary)
  Reduced_Preliminary$QTY <- as.numeric(Reduced_Preliminary$QTY)
  #View(Reduced_Preliminary)
  Reduced_Preliminary$QTY_Cards_1 <- round(Reduced_Preliminary$QTY / 4,1)
  Reduced_Preliminary$QTY_Cards_Aux <- round(Reduced_Preliminary$QTY / 3,1)
  Reduced_Preliminary$QTY_Cards_1 <- as.character(Reduced_Preliminary$QTY_Cards_1)
  Reduced_Preliminary$QTY_Cards_Aux <- as.character (Reduced_Preliminary$QTY_Cards_Aux)
  Reduced_Preliminary$QTY_Cards <- ifelse(grepl("\\.\\d*$",Reduced_Preliminary$QTY_Cards_1), Reduced_Preliminary$QTY_Cards_Aux, Reduced_Preliminary$QTY_Cards_1)
  Reduced_Preliminary$QTY_Cards <- as.numeric(Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY_Lands <- ifelse(Reduced_Preliminary$Rarity == "L", round(Reduced_Preliminary$QTY / 3,0), Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY <- ifelse(Reduced_Preliminary$Rarity == "L", Reduced_Preliminary$QTY_Lands, Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY_Cards_1 <- Reduced_Preliminary$QTY_Cards
  #View(Reduced_Preliminary)
  Final_Decklist_Results <- Reduced_Preliminary[,1:4]
  Final_Decklist_Results$Avg_Copies <- Final_Decklist_Results$Avg_Copies[,1]
  Final_Decklist_Results$Date <- currentDate
  Final_Decklist_Results$Sample_Size <- Deck_number
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Avg_Copies, -Final_Decklist_Results$QTY),]
  Final_Decklist_Results$Potential_Decks <- round(Final_Decklist_Results$QTY/Final_Decklist_Results$Avg_Copies,0)
  Final_Decklist_Results$QTY <- ifelse(Final_Decklist_Results$Potential_Decks < 1, (Final_Decklist_Results$QTY + 1) ,Final_Decklist_Results$QTY)
  Final_Decklist_Results$Potential_Decks <- ifelse(Final_Decklist_Results$Potential_Decks < 1, Final_Decklist_Results$Potential_Decks + 1, Final_Decklist_Results$Potential_Decks)
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Potential_Decks),]
  Final_Decklist_Results <- Final_Decklist_Results[ which(Final_Decklist_Results$Rarity !='Unknown'),]
  Final_Decklist_Results$Meta_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results$QTY_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results <- Final_Decklist_Results[c(5,6,1,4,8,9,2,3,7)]
  Final_Decklist_Results <- Final_Decklist_Results[order(Final_Decklist_Results$Meta_Rank),]
  Vintage_Results <- Final_Decklist_Results
  
  setwd("/home/cujo253/Reports/Decklists/Vintage Preliminary")
  if(dim(Vintage_Results)[1] != 0 ){
    ss <- drive_get("Preliminary")
    #sheets_deauth()
    gs4_auth(email="pachun95@gmail.com", use_oob = T)
    sheet_write(
      Vintage_Results,
      ss = "/d/1PMVRwPJNPFnpxkksHrgcDaSZFYm5B20SnBsb2gboCEw",
      sheet = "Vintage_Challenge"
    )
    
    csvFileName <- paste(currentDate,"_Vintage_Deck_Lists",".csv",sep="")
    write.csv(Vintage_Results, file=csvFileName, row.names = FALSE)
  } else {
    print("No Vintage Challenges Were logged Today")
  }
} else {
  print("No Vintage Challenges Release Today")
}


#Pauper Challenge####
Premium_WD_Part_1 <- "/home/cujo253/Reports/High Confidence Reps/"
Premium_WD_Part_2 <-  currentDate
Premium_WD_Part_3 <- "_Premium.csv"
URL_Part_1 <- "https://magic.wizards.com/en/articles/archive/mtgo-standings/Pauper-Challenge-"
URL_Part_2 <- currentDate
URL_Part_3 <- paste(URL_Part_1,URL_Part_2,sep="")
url = URL_Part_3
tryCatch(
  data <- url %>% 
    read_html() %>% 
    html_nodes(".sorted-by-overview-container") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Deck_Number <- as.data.frame(data)
Deck_number <- nrow(Deck_Number)
Final_Export <- read_csv(paste(Premium_WD_Part_1,Premium_WD_Part_2,Premium_WD_Part_3,sep=""),col_types = cols(.default = "c"))

tryCatch(
  Card_Totals <- url %>% 
    read_html() %>% 
    html_nodes(".card-count") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Totals <- as.data.frame(Card_Totals)

tryCatch(
  Card_Name <- url %>% 
    read_html() %>% 
    html_nodes(".card-name") %>% 
    html_text(), 
  error = function(e){NA}    # a function that returns NA regardless of what it's passed
)
Card_Name <- as.data.frame(Card_Name)

Combined_Preliminary <- tibble(Card_Totals,
                               Card_Name)
Combined_Preliminary <- as.data.frame(Combined_Preliminary)
names(Combined_Preliminary) <- c("QTY","CARD")

Combined_Preliminary <- as.data.frame(Combined_Preliminary)
Combined_Preliminary$QTY <- unlist(Combined_Preliminary$QTY)
Combined_Preliminary$QTY <- as.character(Combined_Preliminary$QTY)
Combined_Preliminary$QTY <- as.numeric(Combined_Preliminary$QTY)
Combined_Preliminary$CARD <- as.factor(unlist(Combined_Preliminary$CARD))
Combined_Preliminary <- as.data.frame(Combined_Preliminary)

if(dim(Combined_Preliminary)[1] != 0 ){
  Reduced_Preliminary <- aggregate(. ~ CARD, data=Combined_Preliminary, sum)
} else {
  Final_Decklist_Results <- 0
  Reduced_Preliminary <- as.data.frame(0)
  print("No Results Were Found")
}
if(dim(Reduced_Preliminary)[1] > 1){
  List <- Reduced_Preliminary$CARD
  ABC <- NULL
  for(i in List){
    df_sub <- subset(Combined_Preliminary, Combined_Preliminary$CARD == i)
    df_sub<- as.data.frame(df_sub)
    A  <- round(mean(df_sub$QTY),2)
    ABC <- rbind(ABC,A)
  }
  Reduced_Preliminary$Avg_Copies <- ABC
  Rarity_Check <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO.csv", col_types = cols(`F/NF` = col_character()))
  colnames(Rarity_Check) <- c("Key","name","Set","Rarity","Foil")
  Reduced_Preliminary$Rarity <- Rarity_Check$Rarity[match(Reduced_Preliminary$CARD,Rarity_Check$name)]
  Reduced_Preliminary$Rarity <- as.character(Reduced_Preliminary$Rarity)
  Reduced_Preliminary$Rarity[is.na(Reduced_Preliminary$Rarity)] = "Unknown"
  Reduced_Preliminary <- as.data.frame(Reduced_Preliminary)
  Reduced_Preliminary$QTY <- as.numeric(Reduced_Preliminary$QTY)
  #View(Reduced_Preliminary)
  Reduced_Preliminary$QTY_Cards_1 <- round(Reduced_Preliminary$QTY / 4,1)
  Reduced_Preliminary$QTY_Cards_Aux <- round(Reduced_Preliminary$QTY / 3,1)
  Reduced_Preliminary$QTY_Cards_1 <- as.character(Reduced_Preliminary$QTY_Cards_1)
  Reduced_Preliminary$QTY_Cards_Aux <- as.character (Reduced_Preliminary$QTY_Cards_Aux)
  Reduced_Preliminary$QTY_Cards <- ifelse(grepl("\\.\\d*$",Reduced_Preliminary$QTY_Cards_1), Reduced_Preliminary$QTY_Cards_Aux, Reduced_Preliminary$QTY_Cards_1)
  Reduced_Preliminary$QTY_Cards <- as.numeric(Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY_Lands <- ifelse(Reduced_Preliminary$Rarity == "L", round(Reduced_Preliminary$QTY / 3,0), Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY <- ifelse(Reduced_Preliminary$Rarity == "L", Reduced_Preliminary$QTY_Lands, Reduced_Preliminary$QTY_Cards)
  Reduced_Preliminary$QTY_Cards_1 <- Reduced_Preliminary$QTY_Cards
  #View(Reduced_Preliminary)
  Final_Decklist_Results <- Reduced_Preliminary[,1:4]
  Final_Decklist_Results$Avg_Copies <- Final_Decklist_Results$Avg_Copies[,1]
  Final_Decklist_Results$Date <- currentDate
  Final_Decklist_Results$Sample_Size <- Deck_number
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Avg_Copies, -Final_Decklist_Results$QTY),]
  Final_Decklist_Results$Potential_Decks <- round(Final_Decklist_Results$QTY/Final_Decklist_Results$Avg_Copies,0)
  Final_Decklist_Results$QTY <- ifelse(Final_Decklist_Results$Potential_Decks < 1, (Final_Decklist_Results$QTY + 1) ,Final_Decklist_Results$QTY)
  Final_Decklist_Results$Potential_Decks <- ifelse(Final_Decklist_Results$Potential_Decks < 1, Final_Decklist_Results$Potential_Decks + 1, Final_Decklist_Results$Potential_Decks)
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$Potential_Decks),]
  Final_Decklist_Results <- Final_Decklist_Results[ which(Final_Decklist_Results$Rarity !='Unknown'),]
  Final_Decklist_Results$Meta_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results$QTY_Rank <- seq.int(nrow(Final_Decklist_Results))
  Final_Decklist_Results <- Final_Decklist_Results[order(-Final_Decklist_Results$QTY),]
  Final_Decklist_Results <- Final_Decklist_Results[c(5,6,1,4,8,9,2,3,7)]
  Final_Decklist_Results <- Final_Decklist_Results[order(Final_Decklist_Results$Meta_Rank),]
  Pauper_Results <- Final_Decklist_Results
  
  setwd("/home/cujo253/Reports/Decklists/Pauper Preliminary")
  if(dim(Pauper_Results)[1] != 0 ){
    ss <- drive_get("Preliminary")
    #sheets_deauth()
    gs4_auth(email="pachun95@gmail.com", use_oob = T)
    sheet_write(
      Pauper_Results,
      ss = "/d/1PMVRwPJNPFnpxkksHrgcDaSZFYm5B20SnBsb2gboCEw",
      sheet = "Pauper_Challenge"
    )
    
    csvFileName <- paste(currentDate,"_Pauper_Deck_Lists",".csv",sep="")
    write.csv(Pauper_Results, file=csvFileName, row.names = FALSE)
  } else {
    print("No Pauper Challenges Were logged Today")
  }
} else {
  print("No Pauper Challenges Release Today")
}











#End Bracket####
# Sys.sleep(10)
# }

