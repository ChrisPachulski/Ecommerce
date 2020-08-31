#Packages####
install.packages("readr")
library(reshape2)
library(zoo)
library(plyr)
library(rlist)
library(fpp2)
library(forecast)
library(stats)
library(tseries)
library(data.table)
library(gtools)
library(tidyverse) # Data Manipulation
library(reshape2)
library(readr)
library(rvest)
library(taRifx)
library(knitr)     # Pretty HTML Tables
library(purrr)     # Allows for the replacement of loops and suite of apply functions
library(tibble) 
library(dplyr)# Breakdown further elements
#library(svMisc)    # Progress bar on loops to make me feel good - Outsourced
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
library(devtools)
#devtools::install_github("tidyverse/googlesheets4", force = TRUE)
library(googlesheets4)
library(googledrive)
library(googlesheets)
library(gargle)
library(httr)
options(httr_oob_default=TRUE) 
options(gargle_oauth_email = "pachun95@gmail.com")

drive_auth(email = "pachun95@gmail.com", use_oob = T)
gs4_auth(email = "pachun95@gmail.com", use_oob = T)

#Format Reviews & Graph Generation####
currentDate = Sys.Date()
Updated_Tracking_Keys <- read_csv("/home/cujo253/Essential_Referential_CSVS/All_Cards_IKO_Legalities.csv", col_types = cols(`F/NF` = col_character()))
colnames(Updated_Tracking_Keys) <- c("Key","name","Set","Rarity","Foil","Duel","Penny","Commander","Legacy","Modern","Vintage","Historic","Pioneer","Standard","Brawl","Pauper","Future","Oldschool")
Updated_Tracking_Keys <- data.frame(Updated_Tracking_Keys, stringsAsFactors=TRUE)
#Market Values####
setwd("/home/cujo253/Reports/High Confidence Reps/")
currentDate <- Sys.Date()
temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp)
Format_Tracker <- Updated_Tracking_Keys
New_Info <- NULL
for (i in 1:Number_Of_Files){
  Desired_Date <- currentDate - (Number_Of_Files - i)
  tmp  <- read_csv(temp[i], col_types = cols(.default = "c"))
  tmp$Key <- as.factor(tmp$Key)
  New_Info <- as.data.frame(tmp$MKT[match(Format_Tracker$Key,tmp$Key)])
  New_Info <- as.data.frame(New_Info)
  colnames(New_Info) <- c(as.Date(Desired_Date))
  Format_Tracker <- cbind(Format_Tracker, New_Info[1])
}
desired_Column <- which(colnames(Format_Tracker)== "2020-02-13")
Format_Tracker <- Format_Tracker[-desired_Column]

Duel_Tracker <- Format_Tracker[which(Format_Tracker$Duel == "Legal"),]
Penny_Tracker <- Format_Tracker[which(Format_Tracker$Penny == "Legal"),]
Commander_Tracker <- Format_Tracker[which(Format_Tracker$Duel == "Legal"),]
Legacy_Tracker <- Format_Tracker[which(Format_Tracker$Legacy == "Legal"),]
Modern_Tracker <- Format_Tracker[which(Format_Tracker$Modern == "Legal"),]
Vintage_Tracker <- Format_Tracker[which(Format_Tracker$Vintage == "Legal"),]
Historic_Tracker <- Format_Tracker[which(Format_Tracker$Historic == "Legal"),]
Pioneer_Tracker <- Format_Tracker[which(Format_Tracker$Pioneer == "Legal"),]
Standard_Tracker <- Format_Tracker[which(Format_Tracker$Standard == "Legal"),]
Brawl_Tracker <- Format_Tracker[which(Format_Tracker$Brawl == "Legal"),]
Pauper_Tracker <- Format_Tracker[which(Format_Tracker$Pauper == "Legal"),]
Future_Tracker <- Format_Tracker[which(Format_Tracker$Future == "Legal"),]
Oldschool_Tracker <- Format_Tracker[which(Format_Tracker$Oldschool == "Legal"),]


Duel_Tracker <- Duel_Tracker[-c(6:18)]
Penny_Tracker <- Penny_Tracker[-c(6:18)]
Commander_Tracker <- Commander_Tracker[-c(6:18)]
Legacy_Tracker <- Legacy_Tracker[-c(6:18)]
Modern_Tracker <- Modern_Tracker[-c(6:18)]
Vintage_Tracker <- Vintage_Tracker[-c(6:18)]
Historic_Tracker <- Historic_Tracker[-c(6:18)]
Pioneer_Tracker <- Pioneer_Tracker[-c(6:18)]
Standard_Tracker <- Standard_Tracker[-c(6:18)]
Brawl_Tracker <- Brawl_Tracker[-c(6:18)]
Pauper_Tracker <- Pauper_Tracker[-c(6:18)]
Future_Tracker <- Future_Tracker[-c(6:18)]
Oldschool_Tracker <- Oldschool_Tracker[-c(6:18)]

Analyzed_Tracker <- Modern_Tracker
Analyzed_Track <- Analyzed_Tracker[-c(1:5)]
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="factor"), as.character)
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="character"), as.numeric)
Analyzed_Tracker <- data.frame(Analyzed_Tracker[c(1:5)], Analyzed_Track)
data <- Analyzed_Tracker
data$Foil <- ifelse(is.na(data$Foil) == TRUE, "NF", data$Foil)
trans_data <- t(data)
removed_data <- as.data.frame(trans_data)
headers <- removed_data[c(1:5),]
headers <- sapply(headers,as.character)
headers[5,][is.na(headers[5,])==TRUE]<- "Non-Foil"
headers <- as.data.frame(headers)
meaned_data <- as.data.frame(apply(removed_data[c(6:nrow(trans_data)),], 2, as.numeric))
meaned_data[is.na(meaned_data) == T] <- 0
meaned_data <- sapply(meaned_data, as.factor)
Recombined_data <- rbind(headers, meaned_data)
Recombined_data <- as.data.frame(Recombined_data, stringsAsFactors = FALSE)
Cleaned_Recombined_data <- Filter(function(Recombined_data) !any(Recombined_data=="NaN"), Recombined_data)
names(Cleaned_Recombined_data) <- as.matrix(Cleaned_Recombined_data[1,])
Titless_Recombined_data <- Cleaned_Recombined_data[-c(1:5),]
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
Titless_Recombined_data$Dates <- values
col_idx <- grep("Dates", names(Titless_Recombined_data))
Titless_Recombined_data <- Titless_Recombined_data[, c(col_idx, (1:ncol(Titless_Recombined_data))[-col_idx])]
Buy_List_ts <- Titless_Recombined_data[,-1]
rownames(Buy_List_ts) <- Titless_Recombined_data[,1]
Buy_List_ts[] <- lapply( Buy_List_ts, function(x) as.numeric(as.character(x)))
Buy_List_ts <- as.data.frame(Buy_List_ts)
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
Buy_List_ts$Dates <- values
Buy_List_ts <- Buy_List_ts[moveme(names(Buy_List_ts), "Dates first")]
Reformatted_Keys <- as.data.frame(t(Cleaned_Recombined_data[1,]))
Reformatted_Keys <- Reformatted_Keys[,1]
Reformatted_Keys <- as.data.frame(Reformatted_Keys)
nrow(Reformatted_Keys)

UnDated_Info <- Buy_List_ts[,-1]
UnDated_Info <- data.frame(Buy_List_ts[1], Sums = rowSums(UnDated_Info),Means =rowMeans(UnDated_Info))
UnDated_Info$Means <- round(UnDated_Info$Means,2)
UnDated_Info$Sums <-  round(UnDated_Info$Sums,0)
Format_Market_Means <- data.frame("Dates" = Buy_List_ts[,1], "Modern" = UnDated_Info$Means)
Format_Market_Total <- data.frame("Dates" = Buy_List_ts[,1], "Modern" = UnDated_Info$Sums)
GUI_Dataframe <- as.data.frame(UnDated_Info)
library(scales)
library(ggplot2)
theme_set(theme_dark())

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Means, label = Means)) + 
  geom_line(aes(y=Means), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_y = 0.01, size = 3.25,check_overlap = TRUE)+
  labs(title="Mean Modern Legal Card - Market Value", 
       subtitle="Via TCG Market Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Average Modern Card Market Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Sums, label = Sums)) + 
  geom_line(aes(y=Sums), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_x = .025, nudge_y = 0.08, size = 3.25,check_overlap = TRUE)+
  labs(title="Sum Modern Legal Cards - Market Value", 
       subtitle="Via TCG Market Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Sum Modern Card Market Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)

Analyzed_Tracker <- Standard_Tracker
Analyzed_Track <- Analyzed_Tracker[-c(1:5)]
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="factor"), as.character)
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="character"), as.numeric)
Analyzed_Tracker <- data.frame(Analyzed_Tracker[c(1:5)], Analyzed_Track)
data <- Analyzed_Tracker
data$Foil <- ifelse(is.na(data$Foil) == TRUE, "NF", data$Foil)
trans_data <- t(data)
removed_data <- as.data.frame(trans_data)
headers <- removed_data[c(1:5),]
headers <- sapply(headers,as.character)
headers[5,][is.na(headers[5,])==TRUE]<- "Non-Foil"
headers <- as.data.frame(headers)
meaned_data <- as.data.frame(apply(removed_data[c(6:nrow(trans_data)),], 2, as.numeric))
meaned_data[is.na(meaned_data) == T] <- 0
meaned_data <- sapply(meaned_data, as.factor)
Recombined_data <- rbind(headers, meaned_data)
Recombined_data <- as.data.frame(Recombined_data, stringsAsFactors = FALSE)
Cleaned_Recombined_data <- Filter(function(Recombined_data) !any(Recombined_data=="NaN"), Recombined_data)
names(Cleaned_Recombined_data) <- as.matrix(Cleaned_Recombined_data[1,])
Titless_Recombined_data <- Cleaned_Recombined_data[-c(1:5),]
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
Titless_Recombined_data$Dates <- values
col_idx <- grep("Dates", names(Titless_Recombined_data))
Titless_Recombined_data <- Titless_Recombined_data[, c(col_idx, (1:ncol(Titless_Recombined_data))[-col_idx])]
Buy_List_ts <- Titless_Recombined_data[,-1]
rownames(Buy_List_ts) <- Titless_Recombined_data[,1]
Buy_List_ts[] <- lapply( Buy_List_ts, function(x) as.numeric(as.character(x)))
Buy_List_ts <- as.data.frame(Buy_List_ts)
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
Buy_List_ts$Dates <- values
Buy_List_ts <- Buy_List_ts[moveme(names(Buy_List_ts), "Dates first")]
Reformatted_Keys <- as.data.frame(t(Cleaned_Recombined_data[1,]))
Reformatted_Keys <- Reformatted_Keys[,1]
Reformatted_Keys <- as.data.frame(Reformatted_Keys)
nrow(Reformatted_Keys)

UnDated_Info <- Buy_List_ts[,-1]
UnDated_Info <- data.frame(Buy_List_ts[1], Sums = rowSums(UnDated_Info),Means =rowMeans(UnDated_Info))
UnDated_Info$Means <- round(UnDated_Info$Means,2)
UnDated_Info$Sums <-  round(UnDated_Info$Sums,0)
Format_Market_Means <- data.frame(Format_Market_Means, "Standard" = UnDated_Info$Means)
Format_Market_Total <- data.frame(Format_Market_Total, "Standard" = UnDated_Info$Sums)

GUI_Dataframe <- as.data.frame(UnDated_Info)

library(scales)
library(ggplot2)
theme_set(theme_dark())

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Means, label = Means)) + 
  geom_line(aes(y=Means), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_y = 0.01, size = 3.25,check_overlap = TRUE)+
  labs(title="Mean Standard Legal Card - Market Value", 
       subtitle="Via TCG Market Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Average Standard Card Market Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Sums, label = Sums)) + 
  geom_line(aes(y=Sums), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_x = .025, nudge_y = 0.08, size = 3.25,check_overlap = TRUE)+
  labs(title="Sum Standard Legal Cards - Market Value", 
       subtitle="Via TCG Market Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Sum Standard Card Market Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)

Analyzed_Tracker <- Commander_Tracker
Analyzed_Track <- Analyzed_Tracker[-c(1:5)]
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="factor"), as.character)
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="character"), as.numeric)
Analyzed_Tracker <- data.frame(Analyzed_Tracker[c(1:5)], Analyzed_Track)
data <- Analyzed_Tracker
data$Foil <- ifelse(is.na(data$Foil) == TRUE, "NF", data$Foil)
trans_data <- t(data)
removed_data <- as.data.frame(trans_data)
headers <- removed_data[c(1:5),]
headers <- sapply(headers,as.character)
headers[5,][is.na(headers[5,])==TRUE]<- "Non-Foil"
headers <- as.data.frame(headers)
meaned_data <- as.data.frame(apply(removed_data[c(6:nrow(trans_data)),], 2, as.numeric))
meaned_data[is.na(meaned_data) == T] <- 0
meaned_data <- sapply(meaned_data, as.factor)
Recombined_data <- rbind(headers, meaned_data)
Recombined_data <- as.data.frame(Recombined_data, stringsAsFactors = FALSE)
Cleaned_Recombined_data <- Filter(function(Recombined_data) !any(Recombined_data=="NaN"), Recombined_data)
names(Cleaned_Recombined_data) <- as.matrix(Cleaned_Recombined_data[1,])
Titless_Recombined_data <- Cleaned_Recombined_data[-c(1:5),]
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
Titless_Recombined_data$Dates <- values
col_idx <- grep("Dates", names(Titless_Recombined_data))
Titless_Recombined_data <- Titless_Recombined_data[, c(col_idx, (1:ncol(Titless_Recombined_data))[-col_idx])]
Buy_List_ts <- Titless_Recombined_data[,-1]
rownames(Buy_List_ts) <- Titless_Recombined_data[,1]
Buy_List_ts[] <- lapply( Buy_List_ts, function(x) as.numeric(as.character(x)))
Buy_List_ts <- as.data.frame(Buy_List_ts)
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
Buy_List_ts$Dates <- values
Buy_List_ts <- Buy_List_ts[moveme(names(Buy_List_ts), "Dates first")]
Reformatted_Keys <- as.data.frame(t(Cleaned_Recombined_data[1,]))
Reformatted_Keys <- Reformatted_Keys[,1]
Reformatted_Keys <- as.data.frame(Reformatted_Keys)
nrow(Reformatted_Keys)

UnDated_Info <- Buy_List_ts[,-1]
UnDated_Info <- data.frame(Buy_List_ts[1], Sums = rowSums(UnDated_Info),Means =rowMeans(UnDated_Info))
UnDated_Info$Means <- round(UnDated_Info$Means,2)
UnDated_Info$Sums <-  round(UnDated_Info$Sums,0)
Format_Market_Means <- data.frame(Format_Market_Means, "Commander" = UnDated_Info$Means)
Format_Market_Total <- data.frame(Format_Market_Total, "Commander" = UnDated_Info$Sums)
GUI_Dataframe <- as.data.frame(UnDated_Info)

library(scales)
library(ggplot2)
theme_set(theme_dark())

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Means, label = Means)) + 
  geom_line(aes(y=Means), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_y = 0.01, size = 3.25,check_overlap = TRUE)+
  labs(title="Mean Commander Legal Card - Market Value", 
       subtitle="Via TCG Market Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Average Commander Card Market Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Sums, label = Sums)) + 
  geom_line(aes(y=Sums), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_x = .025, nudge_y = 0.08, size = 3.25,check_overlap = TRUE)+
  labs(title="Sum Commander Legal Cards - Market Value", 
       subtitle="Via TCG Market Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Sum Commander Card Market Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)

Analyzed_Tracker <- Legacy_Tracker
Analyzed_Track <- Analyzed_Tracker[-c(1:5)]
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="factor"), as.character)
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="character"), as.numeric)
Analyzed_Tracker <- data.frame(Analyzed_Tracker[c(1:5)], Analyzed_Track)
data <- Analyzed_Tracker
data$Foil <- ifelse(is.na(data$Foil) == TRUE, "NF", data$Foil)
trans_data <- t(data)
removed_data <- as.data.frame(trans_data)
headers <- removed_data[c(1:5),]
headers <- sapply(headers,as.character)
headers[5,][is.na(headers[5,])==TRUE]<- "Non-Foil"
headers <- as.data.frame(headers)
meaned_data <- as.data.frame(apply(removed_data[c(6:nrow(trans_data)),], 2, as.numeric))
meaned_data[is.na(meaned_data) == T] <- 0
meaned_data <- sapply(meaned_data, as.factor)
Recombined_data <- rbind(headers, meaned_data)
Recombined_data <- as.data.frame(Recombined_data, stringsAsFactors = FALSE)
Cleaned_Recombined_data <- Filter(function(Recombined_data) !any(Recombined_data=="NaN"), Recombined_data)
names(Cleaned_Recombined_data) <- as.matrix(Cleaned_Recombined_data[1,])
Titless_Recombined_data <- Cleaned_Recombined_data[-c(1:5),]
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
Titless_Recombined_data$Dates <- values
col_idx <- grep("Dates", names(Titless_Recombined_data))
Titless_Recombined_data <- Titless_Recombined_data[, c(col_idx, (1:ncol(Titless_Recombined_data))[-col_idx])]
Buy_List_ts <- Titless_Recombined_data[,-1]
rownames(Buy_List_ts) <- Titless_Recombined_data[,1]
Buy_List_ts[] <- lapply( Buy_List_ts, function(x) as.numeric(as.character(x)))
Buy_List_ts <- as.data.frame(Buy_List_ts)
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
Buy_List_ts$Dates <- values
Buy_List_ts <- Buy_List_ts[moveme(names(Buy_List_ts), "Dates first")]
Reformatted_Keys <- as.data.frame(t(Cleaned_Recombined_data[1,]))
Reformatted_Keys <- Reformatted_Keys[,1]
Reformatted_Keys <- as.data.frame(Reformatted_Keys)
nrow(Reformatted_Keys)

UnDated_Info <- Buy_List_ts[,-1]
UnDated_Info <- data.frame(Buy_List_ts[1], Sums = rowSums(UnDated_Info),Means =rowMeans(UnDated_Info))
UnDated_Info$Means <- round(UnDated_Info$Means,2)
UnDated_Info$Sums <-  round(UnDated_Info$Sums,0)
Format_Market_Means <- data.frame(Format_Market_Means, "Legacy" = UnDated_Info$Means)
Format_Market_Total <- data.frame(Format_Market_Total, "Legacy" = UnDated_Info$Sums)
GUI_Dataframe <- as.data.frame(UnDated_Info)
library(scales)
library(ggplot2)
theme_set(theme_dark())

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Means, label = Means)) + 
  geom_line(aes(y=Means), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_y = 0.01, size = 3.25,check_overlap = TRUE)+
  labs(title="Mean Legacy Legal Card - Market Value", 
       subtitle="Via TCG Market Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Average Legacy Card Market Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Sums, label = Sums)) + 
  geom_line(aes(y=Sums), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_x = .025, nudge_y = 0.08, size = 3.25,check_overlap = TRUE)+
  labs(title="Sum Legacy Legal Cards - Market Value", 
       subtitle="Via TCG Market Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Sum Legacy Card Market Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)

Analyzed_Tracker <- Vintage_Tracker
Analyzed_Track <- Analyzed_Tracker[-c(1:5)]
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="factor"), as.character)
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="character"), as.numeric)
Analyzed_Tracker <- data.frame(Analyzed_Tracker[c(1:5)], Analyzed_Track)
data <- Analyzed_Tracker
data$Foil <- ifelse(is.na(data$Foil) == TRUE, "NF", data$Foil)
trans_data <- t(data)
removed_data <- as.data.frame(trans_data)
headers <- removed_data[c(1:5),]
headers <- sapply(headers,as.character)
headers[5,][is.na(headers[5,])==TRUE]<- "Non-Foil"
headers <- as.data.frame(headers)
meaned_data <- as.data.frame(apply(removed_data[c(6:nrow(trans_data)),], 2, as.numeric))
meaned_data[is.na(meaned_data) == T] <- 0
meaned_data <- sapply(meaned_data, as.factor)
Recombined_data <- rbind(headers, meaned_data)
Recombined_data <- as.data.frame(Recombined_data, stringsAsFactors = FALSE)
Cleaned_Recombined_data <- Filter(function(Recombined_data) !any(Recombined_data=="NaN"), Recombined_data)
names(Cleaned_Recombined_data) <- as.matrix(Cleaned_Recombined_data[1,])
Titless_Recombined_data <- Cleaned_Recombined_data[-c(1:5),]
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
Titless_Recombined_data$Dates <- values
col_idx <- grep("Dates", names(Titless_Recombined_data))
Titless_Recombined_data <- Titless_Recombined_data[, c(col_idx, (1:ncol(Titless_Recombined_data))[-col_idx])]
Buy_List_ts <- Titless_Recombined_data[,-1]
rownames(Buy_List_ts) <- Titless_Recombined_data[,1]
Buy_List_ts[] <- lapply( Buy_List_ts, function(x) as.numeric(as.character(x)))
Buy_List_ts <- as.data.frame(Buy_List_ts)
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
Buy_List_ts$Dates <- values
Buy_List_ts <- Buy_List_ts[moveme(names(Buy_List_ts), "Dates first")]
Reformatted_Keys <- as.data.frame(t(Cleaned_Recombined_data[1,]))
Reformatted_Keys <- Reformatted_Keys[,1]
Reformatted_Keys <- as.data.frame(Reformatted_Keys)
nrow(Reformatted_Keys)

UnDated_Info <- Buy_List_ts[,-1]
UnDated_Info <- data.frame(Buy_List_ts[1], Sums = rowSums(UnDated_Info),Means =rowMeans(UnDated_Info))
UnDated_Info$Means <- round(UnDated_Info$Means,2)
UnDated_Info$Sums <-  round(UnDated_Info$Sums,0)
Format_Market_Means <- data.frame(Format_Market_Means, "Vintage" = UnDated_Info$Means)
Format_Market_Total <- data.frame(Format_Market_Total, "Vintage" = UnDated_Info$Sums)
GUI_Dataframe <- as.data.frame(UnDated_Info)

library(scales)
library(ggplot2)
theme_set(theme_dark())

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Means, label = Means)) + 
  geom_line(aes(y=Means), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_y = 0.01, size = 3.25,check_overlap = TRUE)+
  labs(title="Mean Vintage Legal Card - Market Value", 
       subtitle="Via TCG Market Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Average Vintage Card Market Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Sums, label = Sums)) + 
  geom_line(aes(y=Sums), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_x = .025, nudge_y = 0.08, size = 3.25,check_overlap = TRUE)+
  labs(title="Sum Vintage Legal Cards - Market Value", 
       subtitle="Via TCG Market Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Sum Vintage Card Market Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)

Analyzed_Tracker <- Pioneer_Tracker
Analyzed_Track <- Analyzed_Tracker[-c(1:5)]
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="factor"), as.character)
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="character"), as.numeric)
Analyzed_Tracker <- data.frame(Analyzed_Tracker[c(1:5)], Analyzed_Track)
data <- Analyzed_Tracker
data$Foil <- ifelse(is.na(data$Foil) == TRUE, "NF", data$Foil)
trans_data <- t(data)
removed_data <- as.data.frame(trans_data)
headers <- removed_data[c(1:5),]
headers <- sapply(headers,as.character)
headers[5,][is.na(headers[5,])==TRUE]<- "Non-Foil"
headers <- as.data.frame(headers)
meaned_data <- as.data.frame(apply(removed_data[c(6:nrow(trans_data)),], 2, as.numeric))
meaned_data[is.na(meaned_data) == T] <- 0
meaned_data <- sapply(meaned_data, as.factor)
Recombined_data <- rbind(headers, meaned_data)
Recombined_data <- as.data.frame(Recombined_data, stringsAsFactors = FALSE)
Cleaned_Recombined_data <- Filter(function(Recombined_data) !any(Recombined_data=="NaN"), Recombined_data)
names(Cleaned_Recombined_data) <- as.matrix(Cleaned_Recombined_data[1,])
Titless_Recombined_data <- Cleaned_Recombined_data[-c(1:5),]
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
Titless_Recombined_data$Dates <- values
col_idx <- grep("Dates", names(Titless_Recombined_data))
Titless_Recombined_data <- Titless_Recombined_data[, c(col_idx, (1:ncol(Titless_Recombined_data))[-col_idx])]
Buy_List_ts <- Titless_Recombined_data[,-1]
rownames(Buy_List_ts) <- Titless_Recombined_data[,1]
Buy_List_ts[] <- lapply( Buy_List_ts, function(x) as.numeric(as.character(x)))
Buy_List_ts <- as.data.frame(Buy_List_ts)
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
Buy_List_ts$Dates <- values
Buy_List_ts <- Buy_List_ts[moveme(names(Buy_List_ts), "Dates first")]
Reformatted_Keys <- as.data.frame(t(Cleaned_Recombined_data[1,]))
Reformatted_Keys <- Reformatted_Keys[,1]
Reformatted_Keys <- as.data.frame(Reformatted_Keys)
nrow(Reformatted_Keys)

UnDated_Info <- Buy_List_ts[,-1]
UnDated_Info <- data.frame(Buy_List_ts[1], Sums = rowSums(UnDated_Info),Means =rowMeans(UnDated_Info))
UnDated_Info$Means <- round(UnDated_Info$Means,2)
UnDated_Info$Sums <-  round(UnDated_Info$Sums,0)
Format_Market_Means <- data.frame(Format_Market_Means, "Pioneer" = UnDated_Info$Means)
Format_Market_Total <- data.frame(Format_Market_Total, "Pioneer" = UnDated_Info$Sums)
GUI_Dataframe <- as.data.frame(UnDated_Info)
library(scales)
library(ggplot2)
theme_set(theme_dark())

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Means, label = Means)) + 
  geom_line(aes(y=Means), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_y = 0.01, size = 3.25,check_overlap = TRUE)+
  labs(title="Mean Pioneer Legal Card - Market Value", 
       subtitle="Via TCG Market Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Average Pioneer Card Market Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Sums, label = Sums)) + 
  geom_line(aes(y=Sums), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_x = .025, nudge_y = 0.08, size = 3.25,check_overlap = TRUE)+
  labs(title="Sum Pioneer Legal Cards - Market Value", 
       subtitle="Via TCG Market Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Sum Pioneer Card Market Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)

#Buy list Values####
setwd("/home/cujo253/Reports/High Confidence Reps/")
currentDate <- Sys.Date()
temp <- list.files(pattern="*.csv")
Number_Of_Files <- length(temp)
Format_Tracker <- Updated_Tracking_Keys
New_Info <- NULL
for (i in 1:Number_Of_Files){
  Desired_Date <- currentDate - (Number_Of_Files - i)
  tmp  <- read_csv(temp[i], col_types = cols(.default = "c"))
  tmp$Key <- as.factor(tmp$Key)
  New_Info <- as.data.frame(tmp$BL[match(Format_Tracker$Key,tmp$Key)])
  New_Info <- as.data.frame(New_Info)
  colnames(New_Info) <- c(as.Date(Desired_Date))
  Format_Tracker <- cbind(Format_Tracker, New_Info[1])
}
desired_Column <- which(colnames(Format_Tracker)== "2020-02-13")
Format_Tracker <- Format_Tracker[-desired_Column]

Duel_Tracker <- Format_Tracker[which(Format_Tracker$Duel == "Legal"),]
Penny_Tracker <- Format_Tracker[which(Format_Tracker$Penny == "Legal"),]
Commander_Tracker <- Format_Tracker[which(Format_Tracker$Duel == "Legal"),]
Legacy_Tracker <- Format_Tracker[which(Format_Tracker$Legacy == "Legal"),]
Modern_Tracker <- Format_Tracker[which(Format_Tracker$Modern == "Legal"),]
Vintage_Tracker <- Format_Tracker[which(Format_Tracker$Vintage == "Legal"),]
Historic_Tracker <- Format_Tracker[which(Format_Tracker$Historic == "Legal"),]
Pioneer_Tracker <- Format_Tracker[which(Format_Tracker$Pioneer == "Legal"),]
Standard_Tracker <- Format_Tracker[which(Format_Tracker$Standard == "Legal"),]
Brawl_Tracker <- Format_Tracker[which(Format_Tracker$Brawl == "Legal"),]
Pauper_Tracker <- Format_Tracker[which(Format_Tracker$Pauper == "Legal"),]
Future_Tracker <- Format_Tracker[which(Format_Tracker$Future == "Legal"),]
Oldschool_Tracker <- Format_Tracker[which(Format_Tracker$Oldschool == "Legal"),]


Duel_Tracker <- Duel_Tracker[-c(6:18)]
Penny_Tracker <- Penny_Tracker[-c(6:18)]
Commander_Tracker <- Commander_Tracker[-c(6:18)]
Legacy_Tracker <- Legacy_Tracker[-c(6:18)]
Modern_Tracker <- Modern_Tracker[-c(6:18)]
Vintage_Tracker <- Vintage_Tracker[-c(6:18)]
Historic_Tracker <- Historic_Tracker[-c(6:18)]
Pioneer_Tracker <- Pioneer_Tracker[-c(6:18)]
Standard_Tracker <- Standard_Tracker[-c(6:18)]
Brawl_Tracker <- Brawl_Tracker[-c(6:18)]
Pauper_Tracker <- Pauper_Tracker[-c(6:18)]
Future_Tracker <- Future_Tracker[-c(6:18)]
Oldschool_Tracker <- Oldschool_Tracker[-c(6:18)]

Analyzed_Tracker <- Modern_Tracker
Analyzed_Track <- Analyzed_Tracker[-c(1:5)]
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="factor"), as.character)
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="character"), as.numeric)
Analyzed_Tracker <- data.frame(Analyzed_Tracker[c(1:5)], Analyzed_Track)
data <- Analyzed_Tracker
data$Foil <- ifelse(is.na(data$Foil) == TRUE, "NF", data$Foil)
trans_data <- t(data)
removed_data <- as.data.frame(trans_data)
headers <- removed_data[c(1:5),]
headers <- sapply(headers,as.character)
headers[5,][is.na(headers[5,])==TRUE]<- "Non-Foil"
headers <- as.data.frame(headers)
meaned_data <- as.data.frame(apply(removed_data[c(6:nrow(trans_data)),], 2, as.numeric))
meaned_data[is.na(meaned_data) == T] <- 0
meaned_data <- sapply(meaned_data, as.factor)
Recombined_data <- rbind(headers, meaned_data)
Recombined_data <- as.data.frame(Recombined_data, stringsAsFactors = FALSE)
Cleaned_Recombined_data <- Filter(function(Recombined_data) !any(Recombined_data=="NaN"), Recombined_data)
names(Cleaned_Recombined_data) <- as.matrix(Cleaned_Recombined_data[1,])
Titless_Recombined_data <- Cleaned_Recombined_data[-c(1:5),]
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
Titless_Recombined_data$Dates <- values
col_idx <- grep("Dates", names(Titless_Recombined_data))
Titless_Recombined_data <- Titless_Recombined_data[, c(col_idx, (1:ncol(Titless_Recombined_data))[-col_idx])]
Buy_List_ts <- Titless_Recombined_data[,-1]
rownames(Buy_List_ts) <- Titless_Recombined_data[,1]
Buy_List_ts[] <- lapply( Buy_List_ts, function(x) as.numeric(as.character(x)))
Buy_List_ts <- as.data.frame(Buy_List_ts)
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
Buy_List_ts$Dates <- values
Buy_List_ts <- Buy_List_ts[moveme(names(Buy_List_ts), "Dates first")]
Reformatted_Keys <- as.data.frame(t(Cleaned_Recombined_data[1,]))
Reformatted_Keys <- Reformatted_Keys[,1]
Reformatted_Keys <- as.data.frame(Reformatted_Keys)
nrow(Reformatted_Keys)

UnDated_Info <- Buy_List_ts[,-1]
UnDated_Info <- data.frame(Buy_List_ts[1], Sums = rowSums(UnDated_Info),Means =rowMeans(UnDated_Info))
UnDated_Info$Means <- round(UnDated_Info$Means,2)
UnDated_Info$Sums <-  round(UnDated_Info$Sums,0)
Format_Buylist_Means <- data.frame("Dates" = Buy_List_ts[,1], "Modern" = UnDated_Info$Means)
Format_Buylist_Total <- data.frame("Dates" = Buy_List_ts[,1], "Modern" = UnDated_Info$Sums)
GUI_Dataframe <- as.data.frame(UnDated_Info)
library(scales)
library(ggplot2)
theme_set(theme_dark())

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Means, label = Means)) + 
  geom_line(aes(y=Means), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_y = 0.01, size = 3.25,check_overlap = TRUE)+
  labs(title="Mean Modern Legal Card - Buylist Value", 
       subtitle="Via CK Buylist Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Average Modern Card Buylist Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Sums, label = Sums)) + 
  geom_line(aes(y=Sums), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_x = .025, nudge_y = 0.08, size = 3.25,check_overlap = TRUE)+
  labs(title="Sum Modern Legal Cards - Buylist Value", 
       subtitle="Via CK Buylist Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Sum Modern Card Buylist Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)

Analyzed_Tracker <- Standard_Tracker
Analyzed_Track <- Analyzed_Tracker[-c(1:5)]
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="factor"), as.character)
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="character"), as.numeric)
Analyzed_Tracker <- data.frame(Analyzed_Tracker[c(1:5)], Analyzed_Track)
data <- Analyzed_Tracker
data$Foil <- ifelse(is.na(data$Foil) == TRUE, "NF", data$Foil)
trans_data <- t(data)
removed_data <- as.data.frame(trans_data)
headers <- removed_data[c(1:5),]
headers <- sapply(headers,as.character)
headers[5,][is.na(headers[5,])==TRUE]<- "Non-Foil"
headers <- as.data.frame(headers)
meaned_data <- as.data.frame(apply(removed_data[c(6:nrow(trans_data)),], 2, as.numeric))
meaned_data[is.na(meaned_data) == T] <- 0
meaned_data <- sapply(meaned_data, as.factor)
Recombined_data <- rbind(headers, meaned_data)
Recombined_data <- as.data.frame(Recombined_data, stringsAsFactors = FALSE)
Cleaned_Recombined_data <- Filter(function(Recombined_data) !any(Recombined_data=="NaN"), Recombined_data)
names(Cleaned_Recombined_data) <- as.matrix(Cleaned_Recombined_data[1,])
Titless_Recombined_data <- Cleaned_Recombined_data[-c(1:5),]
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
Titless_Recombined_data$Dates <- values
col_idx <- grep("Dates", names(Titless_Recombined_data))
Titless_Recombined_data <- Titless_Recombined_data[, c(col_idx, (1:ncol(Titless_Recombined_data))[-col_idx])]
Buy_List_ts <- Titless_Recombined_data[,-1]
rownames(Buy_List_ts) <- Titless_Recombined_data[,1]
Buy_List_ts[] <- lapply( Buy_List_ts, function(x) as.numeric(as.character(x)))
Buy_List_ts <- as.data.frame(Buy_List_ts)
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
Buy_List_ts$Dates <- values
Buy_List_ts <- Buy_List_ts[moveme(names(Buy_List_ts), "Dates first")]
Reformatted_Keys <- as.data.frame(t(Cleaned_Recombined_data[1,]))
Reformatted_Keys <- Reformatted_Keys[,1]
Reformatted_Keys <- as.data.frame(Reformatted_Keys)
nrow(Reformatted_Keys)

UnDated_Info <- Buy_List_ts[,-1]
UnDated_Info <- data.frame(Buy_List_ts[1], Sums = rowSums(UnDated_Info),Means =rowMeans(UnDated_Info))
UnDated_Info$Means <- round(UnDated_Info$Means,2)
UnDated_Info$Sums <-  round(UnDated_Info$Sums,0)
Format_Buylist_Means <- data.frame(Format_Buylist_Means, "Standard" = UnDated_Info$Means)
Format_Buylist_Total <- data.frame(Format_Buylist_Total, "Standard" = UnDated_Info$Sums)
GUI_Dataframe <- as.data.frame(UnDated_Info)
library(scales)
library(ggplot2)
theme_set(theme_dark())

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Means, label = Means)) + 
  geom_line(aes(y=Means), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_y = 0.01, size = 3.25,check_overlap = TRUE)+
  labs(title="Mean Standard Legal Card - Buylist Value", 
       subtitle="Via CK Buylist Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Average Standard Card Buylist Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Sums, label = Sums)) + 
  geom_line(aes(y=Sums), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_x = .025, nudge_y = 0.08, size = 3.25,check_overlap = TRUE)+
  labs(title="Sum Standard Legal Cards - Buylist Value", 
       subtitle="Via CK Buylist Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Sum Standard Card Buylist Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)

Analyzed_Tracker <- Commander_Tracker
Analyzed_Track <- Analyzed_Tracker[-c(1:5)]
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="factor"), as.character)
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="character"), as.numeric)
Analyzed_Tracker <- data.frame(Analyzed_Tracker[c(1:5)], Analyzed_Track)
data <- Analyzed_Tracker
data$Foil <- ifelse(is.na(data$Foil) == TRUE, "NF", data$Foil)
trans_data <- t(data)
removed_data <- as.data.frame(trans_data)
headers <- removed_data[c(1:5),]
headers <- sapply(headers,as.character)
headers[5,][is.na(headers[5,])==TRUE]<- "Non-Foil"
headers <- as.data.frame(headers)
meaned_data <- as.data.frame(apply(removed_data[c(6:nrow(trans_data)),], 2, as.numeric))
meaned_data[is.na(meaned_data) == T] <- 0
meaned_data <- sapply(meaned_data, as.factor)
Recombined_data <- rbind(headers, meaned_data)
Recombined_data <- as.data.frame(Recombined_data, stringsAsFactors = FALSE)
Cleaned_Recombined_data <- Filter(function(Recombined_data) !any(Recombined_data=="NaN"), Recombined_data)
names(Cleaned_Recombined_data) <- as.matrix(Cleaned_Recombined_data[1,])
Titless_Recombined_data <- Cleaned_Recombined_data[-c(1:5),]
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
Titless_Recombined_data$Dates <- values
col_idx <- grep("Dates", names(Titless_Recombined_data))
Titless_Recombined_data <- Titless_Recombined_data[, c(col_idx, (1:ncol(Titless_Recombined_data))[-col_idx])]
Buy_List_ts <- Titless_Recombined_data[,-1]
rownames(Buy_List_ts) <- Titless_Recombined_data[,1]
Buy_List_ts[] <- lapply( Buy_List_ts, function(x) as.numeric(as.character(x)))
Buy_List_ts <- as.data.frame(Buy_List_ts)
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
Buy_List_ts$Dates <- values
Buy_List_ts <- Buy_List_ts[moveme(names(Buy_List_ts), "Dates first")]
Reformatted_Keys <- as.data.frame(t(Cleaned_Recombined_data[1,]))
Reformatted_Keys <- Reformatted_Keys[,1]
Reformatted_Keys <- as.data.frame(Reformatted_Keys)
nrow(Reformatted_Keys)

UnDated_Info <- Buy_List_ts[,-1]
UnDated_Info <- data.frame(Buy_List_ts[1], Sums = rowSums(UnDated_Info),Means =rowMeans(UnDated_Info))
UnDated_Info$Means <- round(UnDated_Info$Means,2)
UnDated_Info$Sums <-  round(UnDated_Info$Sums,0)

Format_Buylist_Means <- data.frame(Format_Buylist_Means, "Commander" = UnDated_Info$Means)
Format_Buylist_Total <- data.frame(Format_Buylist_Total, "Commander" = UnDated_Info$Sums)
GUI_Dataframe <- as.data.frame(UnDated_Info)

library(scales)
library(ggplot2)
theme_set(theme_dark())

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Means, label = Means)) + 
  geom_line(aes(y=Means), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_y = 0.01, size = 3.25,check_overlap = TRUE)+
  labs(title="Mean Commander Legal Card - Buylist Value", 
       subtitle="Via CK Buylist Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Average Commander Card Buylist Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Sums, label = Sums)) + 
  geom_line(aes(y=Sums), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_x = .025, nudge_y = 0.08, size = 3.25,check_overlap = TRUE)+
  labs(title="Sum Commander Legal Cards - Buylist Value", 
       subtitle="Via CK Buylist Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Sum Commander Card Buylist Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)

Analyzed_Tracker <- Legacy_Tracker
Analyzed_Track <- Analyzed_Tracker[-c(1:5)]
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="factor"), as.character)
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="character"), as.numeric)
Analyzed_Tracker <- data.frame(Analyzed_Tracker[c(1:5)], Analyzed_Track)
data <- Analyzed_Tracker
data$Foil <- ifelse(is.na(data$Foil) == TRUE, "NF", data$Foil)
trans_data <- t(data)
removed_data <- as.data.frame(trans_data)
headers <- removed_data[c(1:5),]
headers <- sapply(headers,as.character)
headers[5,][is.na(headers[5,])==TRUE]<- "Non-Foil"
headers <- as.data.frame(headers)
meaned_data <- as.data.frame(apply(removed_data[c(6:nrow(trans_data)),], 2, as.numeric))
meaned_data[is.na(meaned_data) == T] <- 0
meaned_data <- sapply(meaned_data, as.factor)
Recombined_data <- rbind(headers, meaned_data)
Recombined_data <- as.data.frame(Recombined_data, stringsAsFactors = FALSE)
Cleaned_Recombined_data <- Filter(function(Recombined_data) !any(Recombined_data=="NaN"), Recombined_data)
names(Cleaned_Recombined_data) <- as.matrix(Cleaned_Recombined_data[1,])
Titless_Recombined_data <- Cleaned_Recombined_data[-c(1:5),]
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
Titless_Recombined_data$Dates <- values
col_idx <- grep("Dates", names(Titless_Recombined_data))
Titless_Recombined_data <- Titless_Recombined_data[, c(col_idx, (1:ncol(Titless_Recombined_data))[-col_idx])]
Buy_List_ts <- Titless_Recombined_data[,-1]
rownames(Buy_List_ts) <- Titless_Recombined_data[,1]
Buy_List_ts[] <- lapply( Buy_List_ts, function(x) as.numeric(as.character(x)))
Buy_List_ts <- as.data.frame(Buy_List_ts)
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
Buy_List_ts$Dates <- values
Buy_List_ts <- Buy_List_ts[moveme(names(Buy_List_ts), "Dates first")]
Reformatted_Keys <- as.data.frame(t(Cleaned_Recombined_data[1,]))
Reformatted_Keys <- Reformatted_Keys[,1]
Reformatted_Keys <- as.data.frame(Reformatted_Keys)
nrow(Reformatted_Keys)

UnDated_Info <- Buy_List_ts[,-1]
UnDated_Info <- data.frame(Buy_List_ts[1], Sums = rowSums(UnDated_Info),Means =rowMeans(UnDated_Info))
UnDated_Info$Means <- round(UnDated_Info$Means,2)
UnDated_Info$Sums <-  round(UnDated_Info$Sums,0)
Format_Buylist_Means <- data.frame(Format_Buylist_Means, "Legacy" = UnDated_Info$Means)
Format_Buylist_Total <- data.frame(Format_Buylist_Total, "Legacy" = UnDated_Info$Sums)

GUI_Dataframe <- as.data.frame(UnDated_Info)

library(scales)
library(ggplot2)
theme_set(theme_dark())

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Means, label = Means)) + 
  geom_line(aes(y=Means), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_y = 0.01, size = 3.25,check_overlap = TRUE)+
  labs(title="Mean Legacy Legal Card - Buylist Value", 
       subtitle="Via CK Buylist Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Average Legacy Card Buylist Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Sums, label = Sums)) + 
  geom_line(aes(y=Sums), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_x = .025, nudge_y = 0.08, size = 3.25,check_overlap = TRUE)+
  labs(title="Sum Legacy Legal Cards - Buylist Value", 
       subtitle="Via CK Buylist Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Sum Legacy Card Buylist Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)

Analyzed_Tracker <- Vintage_Tracker
Analyzed_Track <- Analyzed_Tracker[-c(1:5)]
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="factor"), as.character)
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="character"), as.numeric)
Analyzed_Tracker <- data.frame(Analyzed_Tracker[c(1:5)], Analyzed_Track)
data <- Analyzed_Tracker
data$Foil <- ifelse(is.na(data$Foil) == TRUE, "NF", data$Foil)
trans_data <- t(data)
removed_data <- as.data.frame(trans_data)
headers <- removed_data[c(1:5),]
headers <- sapply(headers,as.character)
headers[5,][is.na(headers[5,])==TRUE]<- "Non-Foil"
headers <- as.data.frame(headers)
meaned_data <- as.data.frame(apply(removed_data[c(6:nrow(trans_data)),], 2, as.numeric))
meaned_data[is.na(meaned_data) == T] <- 0
meaned_data <- sapply(meaned_data, as.factor)
Recombined_data <- rbind(headers, meaned_data)
Recombined_data <- as.data.frame(Recombined_data, stringsAsFactors = FALSE)
Cleaned_Recombined_data <- Filter(function(Recombined_data) !any(Recombined_data=="NaN"), Recombined_data)
names(Cleaned_Recombined_data) <- as.matrix(Cleaned_Recombined_data[1,])
Titless_Recombined_data <- Cleaned_Recombined_data[-c(1:5),]
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
Titless_Recombined_data$Dates <- values
col_idx <- grep("Dates", names(Titless_Recombined_data))
Titless_Recombined_data <- Titless_Recombined_data[, c(col_idx, (1:ncol(Titless_Recombined_data))[-col_idx])]
Buy_List_ts <- Titless_Recombined_data[,-1]
rownames(Buy_List_ts) <- Titless_Recombined_data[,1]
Buy_List_ts[] <- lapply( Buy_List_ts, function(x) as.numeric(as.character(x)))
Buy_List_ts <- as.data.frame(Buy_List_ts)
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
Buy_List_ts$Dates <- values
Buy_List_ts <- Buy_List_ts[moveme(names(Buy_List_ts), "Dates first")]
Reformatted_Keys <- as.data.frame(t(Cleaned_Recombined_data[1,]))
Reformatted_Keys <- Reformatted_Keys[,1]
Reformatted_Keys <- as.data.frame(Reformatted_Keys)
nrow(Reformatted_Keys)

UnDated_Info <- Buy_List_ts[,-1]
UnDated_Info <- data.frame(Buy_List_ts[1], Sums = rowSums(UnDated_Info),Means =rowMeans(UnDated_Info))
UnDated_Info$Means <- round(UnDated_Info$Means,2)
UnDated_Info$Sums <-  round(UnDated_Info$Sums,0)
Format_Buylist_Means <- data.frame(Format_Buylist_Means, "Vintage" = UnDated_Info$Means)
Format_Buylist_Total <- data.frame(Format_Buylist_Total, "Vintage" = UnDated_Info$Sums)
GUI_Dataframe <- as.data.frame(UnDated_Info)
library(scales)
library(ggplot2)
theme_set(theme_dark())

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Means, label = Means)) + 
  geom_line(aes(y=Means), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_y = 0.01, size = 3.25,check_overlap = TRUE)+
  labs(title="Mean Vintage Legal Card - Buylist Value", 
       subtitle="Via CK Buylist Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Average Vintage Card Buylist Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Sums, label = Sums)) + 
  geom_line(aes(y=Sums), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_x = .025, nudge_y = 0.08, size = 3.25,check_overlap = TRUE)+
  labs(title="Sum Vintage Legal Cards - Buylist Value", 
       subtitle="Via CK Buylist Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Sum Vintage Card Buylist Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)

Analyzed_Tracker <- Pioneer_Tracker
Analyzed_Track <- Analyzed_Tracker[-c(1:5)]
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="factor"), as.character)
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="character"), as.numeric)
Analyzed_Tracker <- data.frame(Analyzed_Tracker[c(1:5)], Analyzed_Track)
data <- Analyzed_Tracker
data$Foil <- ifelse(is.na(data$Foil) == TRUE, "NF", data$Foil)
trans_data <- t(data)
removed_data <- as.data.frame(trans_data)
headers <- removed_data[c(1:5),]
headers <- sapply(headers,as.character)
headers[5,][is.na(headers[5,])==TRUE]<- "Non-Foil"
headers <- as.data.frame(headers)
meaned_data <- as.data.frame(apply(removed_data[c(6:nrow(trans_data)),], 2, as.numeric))
meaned_data[is.na(meaned_data) == T] <- 0
meaned_data <- sapply(meaned_data, as.factor)
Recombined_data <- rbind(headers, meaned_data)
Recombined_data <- as.data.frame(Recombined_data, stringsAsFactors = FALSE)
Cleaned_Recombined_data <- Filter(function(Recombined_data) !any(Recombined_data=="NaN"), Recombined_data)
names(Cleaned_Recombined_data) <- as.matrix(Cleaned_Recombined_data[1,])
Titless_Recombined_data <- Cleaned_Recombined_data[-c(1:5),]
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
Titless_Recombined_data$Dates <- values
col_idx <- grep("Dates", names(Titless_Recombined_data))
Titless_Recombined_data <- Titless_Recombined_data[, c(col_idx, (1:ncol(Titless_Recombined_data))[-col_idx])]
Buy_List_ts <- Titless_Recombined_data[,-1]
rownames(Buy_List_ts) <- Titless_Recombined_data[,1]
Buy_List_ts[] <- lapply( Buy_List_ts, function(x) as.numeric(as.character(x)))
Buy_List_ts <- as.data.frame(Buy_List_ts)
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
Buy_List_ts$Dates <- values
Buy_List_ts <- Buy_List_ts[moveme(names(Buy_List_ts), "Dates first")]
Reformatted_Keys <- as.data.frame(t(Cleaned_Recombined_data[1,]))
Reformatted_Keys <- Reformatted_Keys[,1]
Reformatted_Keys <- as.data.frame(Reformatted_Keys)

UnDated_Info <- Buy_List_ts[,-1]
UnDated_Info <- data.frame(Buy_List_ts[1], Sums = rowSums(UnDated_Info),Means =rowMeans(UnDated_Info))
UnDated_Info$Means <- round(UnDated_Info$Means,2)
UnDated_Info$Sums <-  round(UnDated_Info$Sums,0)
Format_Buylist_Means <- data.frame(Format_Buylist_Means, "Pioneer" = UnDated_Info$Means)
Format_Buylist_Total <- data.frame(Format_Buylist_Total, "Pioneer" = UnDated_Info$Sums)
GUI_Dataframe <- as.data.frame(UnDated_Info)
library(scales)
library(ggplot2)
theme_set(theme_dark())

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Means, label = Means)) + 
  geom_line(aes(y=Means), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_y = 0.01, size = 3.25,check_overlap = TRUE)+
  labs(title="Mean Pioneer Legal Card - Buylist Value", 
       subtitle="Via CK Buylist Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Average Pioneer Card Buylist Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Sums, label = Sums)) + 
  geom_line(aes(y=Sums), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_x = .025, nudge_y = 0.08, size = 3.25,check_overlap = TRUE)+
  labs(title="Sum Pioneer Legal Cards - Buylist Value", 
       subtitle="Via CK Buylist Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Sum Pioneer Card Buylist Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)


Analyzed_Tracker <- Format_Tracker
Analyzed_Track <- Analyzed_Tracker[-c(1:5)]
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="factor"), as.character)
Analyzed_Track <- japply(Analyzed_Track, which(sapply(Analyzed_Track, class)=="character"), as.numeric)
Analyzed_Tracker <- data.frame(Analyzed_Tracker[c(1:5)], Analyzed_Track)
data <- Analyzed_Tracker
data$Foil <- ifelse(is.na(data$Foil) == TRUE, "NF", data$Foil)
trans_data <- t(data)
removed_data <- as.data.frame(trans_data)
headers <- removed_data[c(1:5),]
headers <- sapply(headers,as.character)
headers[5,][is.na(headers[5,])==TRUE]<- "Non-Foil"
headers <- as.data.frame(headers)
meaned_data <- as.data.frame(apply(removed_data[c(6:nrow(trans_data)),], 2, as.numeric))
meaned_data[is.na(meaned_data) == T] <- 0
meaned_data <- sapply(meaned_data, as.factor)
Recombined_data <- rbind(headers, meaned_data)
Recombined_data <- as.data.frame(Recombined_data, stringsAsFactors = FALSE)
Cleaned_Recombined_data <- Filter(function(Recombined_data) !any(Recombined_data=="NaN"), Recombined_data)
names(Cleaned_Recombined_data) <- as.matrix(Cleaned_Recombined_data[1,])
Titless_Recombined_data <- Cleaned_Recombined_data[-c(1:5),]
values = seq(from = as.Date("2019-12-08"), to = currentDate, by = 'day')
Titless_Recombined_data$Dates <- values
col_idx <- grep("Dates", names(Titless_Recombined_data))
Titless_Recombined_data <- Titless_Recombined_data[, c(col_idx, (1:ncol(Titless_Recombined_data))[-col_idx])]
Buy_List_ts <- Titless_Recombined_data[,-1]
rownames(Buy_List_ts) <- Titless_Recombined_data[,1]
Buy_List_ts[] <- lapply( Buy_List_ts, function(x) as.numeric(as.character(x)))
Buy_List_ts <- as.data.frame(Buy_List_ts)
values = seq(from = as.Date("2019-12-08"), to = currentDate, by = 'day')
Buy_List_ts$Dates <- values
Buy_List_ts <- Buy_List_ts[moveme(names(Buy_List_ts), "Dates first")]
Reformatted_Keys <- as.data.frame(t(Cleaned_Recombined_data[1,]))
Reformatted_Keys <- Reformatted_Keys[,1]
Reformatted_Keys <- as.data.frame(Reformatted_Keys)
nrow(Reformatted_Keys)

UnDated_Info <- Buy_List_ts[,-1]
UnDated_Info <- data.frame(Buy_List_ts[1], Sums = rowSums(UnDated_Info),Means =rowMeans(UnDated_Info))
UnDated_Info$Means <- round(UnDated_Info$Means,2)
UnDated_Info$Sums <-  round(UnDated_Info$Sums,0)
UnDated_Info <- UnDated_Info[14:nrow(UnDated_Info),]
Format_Buylist_Means <- data.frame(Format_Buylist_Means, "All" = UnDated_Info$Means)
Format_Buylist_Total <- data.frame(Format_Buylist_Total, "All" = UnDated_Info$Sums)
GUI_Dataframe <- as.data.frame(UnDated_Info)
library(scales)
library(ggplot2)
theme_set(theme_dark())

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Means, label = Means)) + 
  geom_line(aes(y=Means), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_y = 0.01, size = 3.25,check_overlap = TRUE)+
  labs(title="Mean All Format Legal Card - Market Value", 
       subtitle="Via TCG Market Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Average Format Card Market Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)

desired_Graph <- ggplot(GUI_Dataframe, 
                        aes(x=Dates, y= Sums, label = Sums)) + 
  geom_line(aes(y=Sums), colour="green", size = 1.25) + 
  #geom_text(colour = "white",vjust = 0.00, nudge_x = .025, nudge_y = 0.08, size = 3.25,check_overlap = TRUE)+
  labs(title="Sum All Format Legal Cards - Market Value", 
       subtitle="Via TCG Market Pricing", 
       caption="Source: WolfofTinStreet", 
       y="$ Offers") +
  scale_x_date(breaks = "7 day", labels = date_format("%m-%d")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0, face = "italic", size = 10)) +
  theme(panel.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(plot.background = element_rect(fill = "black", color = "black", size = .5, linetype = 'solid')) +
  theme(axis.text.x = element_text(color = "#4DBBD5B2",angle = 45, vjust=0.5),
        axis.text.y = element_text(color = "#4DBBD5B2", vjust=0.5)) +
  theme(axis.title.x = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        axis.title.y = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.subtitle = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5),
        plot.caption = element_text(face = "bold", color = "Dark Grey", vjust=0.5),
        plot.title  = element_text(face = "bold", color = "#4DBBD5B2", vjust=0.5))+
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "white"))
Title <- paste("Sum Format Card Market Value",".png",sep="")
#ggsave(Title, plot = last_plot(), path = "/home/cujo253/Plots/Coronavirus_Format_Effect/", width = 15,height = 5 , limitsize = T)

# my_dfs <- list(Format_Market_Means, Format_Buylist_Means, Format_Market_Total, Format_Buylist_Total)
# gs4_auth()
# sheets_create(
#   paste("Format_Total",sep=""),
#   sheets = my_dfs
# )

ss <- drive_get("Format_Total")
#sheets_deauth()
gs4_auth(email = "pachun95@gmail.com")
sheet_write(
  Format_Market_Means,
  ss = ss,
  sheet = "Market_Mean"
)
sheet_write(
  Format_Buylist_Means,
  ss = ss,
  sheet = "Buylist_Mean"
)
sheet_write(
  Format_Market_Total,
  ss = ss,
  sheet = "Market_Total"
)
sheet_write(
  Format_Buylist_Total,
  ss = ss,
  sheet = "Buylist_Total"
)


