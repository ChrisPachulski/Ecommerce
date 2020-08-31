#Packages####
setwd("/home/cujo253/cronR/")
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
library(tibble)    # Breakdown further elements
library(tictoc)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
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

#Time Series Analysis for BL####
Sets <- read.csv("/home/cujo253/Essential_Referential_CSVS/Sets.csv",stringsAsFactors = TRUE)
Exclusion <- data.frame(Sets$Set_Excl,Sets$Excl_Excl)
colnames(Exclusion) <- c("Set_Excl","Excl_Excl")
currentDate <- Sys.Date()
#Set Today's Date

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
New_Roster[is.na(New_Roster)==T] <- 0

setwd("/home/cujo253/Metrics/TBD Updated Roster")
csvFileName <- paste("3_BuyList_History",".csv",sep="")
write.csv(New_Roster, file=csvFileName, row.names = FALSE) 

Current_Buylist_Tracker <- paste("/home/cujo253/Metrics/TBD Updated Roster/3_BuyList_History.csv", sep ="")
#Make it automate-able
Current_Buylist_Tracker <- as.data.frame(read_csv(Current_Buylist_Tracker, 
                                                  col_types = cols(Foil = col_character())))



#Three_Week_Movers <- read_csv(Three_Week_Movers)
#Keys_Of_Interest <- Three_Week_Movers$Key
Keys_Of_Interest <- Current_Buylist_Tracker$Key
Keys_Of_Interest <- as.data.frame(Keys_Of_Interest)
colnames(Keys_Of_Interest) <- c("Key")
Keys_Of_Interest <- merge(Keys_Of_Interest,Current_Buylist_Tracker, by="Key")
#Pull it into a variable (ensure foil is as character)
data <- Keys_Of_Interest
data$Exclude <- Exclusion$Excl_Excl[match(data$Set, Exclusion$Set_Excl)]
data <- data[which(data$Exclude != "Exclude"),]
data <- data[-ncol(data)]
#Convert to "data" for simplicity
nrow(data)
data$Foil <- ifelse(is.na(data$Foil) == TRUE, "NF", data$Foil)

data <- data[which(data$Foil == "NF"),]
#data$Foil <- ifelse(is.na(data$Foil) == TRUE, "NF", data$Foil)
#Remove the Foils bc foils are gross
data <- data[which(data$Rarity != "U"),] #<- Rarity Selection Tool
data <- data[which(data$Rarity != "C"),] 
trans_data <- t(data)
#Flip the data so the keys are on top
removed_data <- as.data.frame(trans_data)

headers <- removed_data[c(1:5),]
headers <- sapply(headers,as.character)
headers[5,][is.na(headers[5,])==TRUE]<- "Non-Foil"
headers <- as.data.frame(headers)
#Store the headers away to be repulled back in after transformation is complete
meaned_data <- as.data.frame(apply(removed_data[c(6:nrow(trans_data)),], 2, as.numeric))

#Ensure all Buy List values are numerical for calculation
meaned_data <- round(na.aggregate(meaned_data),2)
#If* cards have NA's, if there was no offer, the NA's will be filled in with the mean offer it has had in the time period selected
meaned_data <- sapply(meaned_data, as.factor)

Recombined_data <- rbind(headers, meaned_data)
Recombined_data <- as.data.frame(Recombined_data, stringsAsFactors = FALSE)

#Rejoined headers with numeric Values
Cleaned_Recombined_data <- Filter(function(Recombined_data) !any(Recombined_data=="NaN"), Recombined_data)
#Removed any lingering NaN's (potential for Inf & -Inf that would also need to be addressed should they arise)
names(Cleaned_Recombined_data) <- as.matrix(Cleaned_Recombined_data[1,])
ncol(Cleaned_Recombined_data)
Titless_Recombined_data <- Cleaned_Recombined_data[-c(1:5),]
ncol(Cleaned_Recombined_data)
ncol(Titless_Recombined_data)
#Once again, ax those headers for future ts conversion
values = seq(from = as.Date("2019-12-21"), to = currentDate, by = 'day')
length(values)
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
Buy_List_ts <- Buy_List_ts[-nrow(Buy_List_ts),]
training_rows <- round(nrow(Buy_List_ts)*.70,0)
test_rows <- round(training_rows:nrow(Buy_List_ts),0)

Reformatted_Keys <- as.data.frame(t(Cleaned_Recombined_data[1,]))
Reformatted_Keys <- Reformatted_Keys[,1]
Reformatted_Keys <- as.data.frame(Reformatted_Keys)
nrow(Reformatted_Keys)

for (i in 1:1000) {
  Frequent_Value <- round((nrow(Buy_List_ts)/i),0)
  if(Frequent_Value == 7){ts_frequency_value <- i}#else{ts_frequency_value <- print("Increase Range")}
}


Time_Series_Training_Dates <- read_csv("/home/cujo253/cronR/Time_Series_Training_Dates.csv")
Training_Week <- Time_Series_Training_Dates$Training_Week[match(currentDate,Time_Series_Training_Dates$Date)]
Training_Day <- Time_Series_Training_Dates$Training_Day[match(currentDate,Time_Series_Training_Dates$Date)]
Test_Week <- Time_Series_Training_Dates$Test_Week[match(currentDate,Time_Series_Training_Dates$Date)]
Test_Day <- Time_Series_Training_Dates$Test_Day[match(currentDate,Time_Series_Training_Dates$Date)]


ts_frequency_value <- ts_frequency_value-1
Buy_List_ts_final <- ts(Buy_List_ts, start=(1),frequency = 7)

#Holts-Winter Analysis#
#All unique Keys to cycle through for time Series Analysis

Forecast_DF <- (1:(nrow(Buy_List_ts)+9))
Forecast_DF <- as.data.frame(Forecast_DF)

total = nrow(Reformatted_Keys) #For loading bar, match up this number for the pages to be run through
pb <- txtProgressBar(min=0, max = total, style = 3)
XYZ <- 1
Start_Time <- Sys.time()
#for(Key in Reformatted_Keys[c(1:nrow(Reformatted_Keys)),])
for(Key in Reformatted_Keys[c(1:total),]){
  desired_Column <- which( colnames(Buy_List_ts)== Key)
  #Buy_List_ts_plot <- plot(Buy_List_ts_final[,desired_Column], xlab="Days",ylab="Card Offers",main= Key)
  bl_train_data <- window(Buy_List_ts_final[,desired_Column], start=c(1), end=c(Training_Week,Training_Day), freq=7)
  bl_test_data <- window(Buy_List_ts_final[,desired_Column], start=c(Test_Week,Test_Day),freq=7)
  bl_data_all <- window(Buy_List_ts_final[,desired_Column], start=c(1),freq=7)
  bl_train_data[bl_train_data == 0] <- .01
  bl_train_data_hw <- hw(bl_train_data, seasonal = "multiplicative", h = 7)
  df <- as.data.frame(bl_train_data_hw)
  
  HW_Vec2 <- cbind(bl_test_data,df[,1])
  #ts.plot(HW_Vec2, col=c("blue","red"), main = "Actual (Blue) vs Forecast (Red)")
  RMSE2 <- round(sqrt(sum(((HW_Vec2[,1]-HW_Vec2[,2])^2)/length(HW_Vec2[,1]))),4)
  MAPE2 <- round(mean(abs((HW_Vec2[,1]-HW_Vec2[,2])/HW_Vec2[,1])),4)
  
  #HW_CYA <- paste("When this prediction is incorrect, it will be within $",RMSE2," 68% of the time, or, within ",MAPE2*100,"% of what the true Buylist offer will be",sep="")
  bl_data_all[bl_data_all == 0] <- .01
  bl_data_all_hw <- hw(bl_data_all, seasonal = "multiplicative", h = 7)
  df <- as.data.frame(bl_data_all_hw)
  
  Future_Forecast_Values <- rbind(as.data.frame(df[,1]),RMSE2)
  Future_Forecast_Values <- rbind(Future_Forecast_Values, MAPE2)
  Future_Forecast_Values <- as.data.frame(Future_Forecast_Values)
  
  Seperated_Column <- as.list(Buy_List_ts_final[,desired_Column])
  Future_Forecast_Values <- as.list(Future_Forecast_Values)
  Present_Future <- append(Seperated_Column, Future_Forecast_Values)
  New_Forecast_Column <-round(data.frame(unlist(Present_Future)),2)
  colnames(New_Forecast_Column) <- c(Key)
  Forecast_DF <- cbind(Forecast_DF, New_Forecast_Column)
  
  setTxtProgressBar(pb, XYZ)
  XYZ <- XYZ + 1
}

End_Time <- Sys.time()
End_Time - Start_Time

#View(Forecast_DF)
FC_DF <- Forecast_DF[,-1]
FC_DF <- t(FC_DF)
FC_DF <- as.data.frame(FC_DF)
All_Cards <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(`hasFoil` = col_character()))
Reformatted_Keys$Name <- All_Cards$card[match(Reformatted_Keys$Reformatted_Keys,All_Cards$Key)]
Reformatted_Keys$Set <- All_Cards$set[match(Reformatted_Keys$Reformatted_Keys,All_Cards$Key)]
Reformatted_Keys$Rarity <- All_Cards$rarity[match(Reformatted_Keys$Reformatted_Keys,All_Cards$Key)]
Reformatted_Keys$'F/NF' <- All_Cards$hasFoil[match(Reformatted_Keys$Reformatted_Keys,All_Cards$Key)]
ncol(FC_DF)

format(currentDate, format = "%Y-%m-%d")

Final_Touches <- data.frame(Reformatted_Keys, FC_DF[c((ncol(FC_DF)-9):ncol(FC_DF))])
colnames(Final_Touches) <- c("Key","Name","Set","Rarity","F/NF",format(currentDate,format = "%Y-%m-%d"),format(currentDate+1,format = "%Y-%m-%d"),format(currentDate+2,format = "%Y-%m-%d"),format(currentDate+3,format = "%Y-%m-%d"),format(currentDate+4,format = "%Y-%m-%d"),format(currentDate+5,format = "%Y-%m-%d"),format(currentDate+6,format = "%Y-%m-%d"),format(currentDate+7,format = "%Y-%m-%d"),"RMSE","MAPES")
Final_Touches$`F/NF`[is.na(Final_Touches$`F/NF`)==TRUE]<- ""
Filtered_Angle <- Final_Touches
Filtered_Angle$Diff <- Filtered_Angle[,12]-Filtered_Angle[,6]
Filtered_Angle <- Filtered_Angle[order(-Filtered_Angle$Diff),]
rownames(Filtered_Angle) <- seq(nrow(Filtered_Angle))

#Neural Network Time Series#
Reformatted_Keys <- as.data.frame(t(Cleaned_Recombined_data[1,]))
Reformatted_Keys <- Reformatted_Keys[,1]
Reformatted_Keys <- as.data.frame(Reformatted_Keys)
nrow(Reformatted_Keys)

for (i in 1:1000) {
  Frequent_Value <- round((nrow(Buy_List_ts)/i),0)
  if(Frequent_Value == 7){ts_frequency_value <- i}#else{ts_frequency_value <- print("Increase Range")}
}

ts_frequency_value
Buy_List_ts_final <- ts(Buy_List_ts, start=(1),frequency = 7)
NNETAR_DF <- (1:(nrow(Buy_List_ts)+9))
NNETAR_DF <- as.data.frame(NNETAR_DF)

total = nrow(Reformatted_Keys) #For loading bar, match up this number for the pages to be run through
pb <- txtProgressBar(min=0, max = total, style = 3)
XYZ <- 1
Start_Time <- Sys.time()
#(Key in Reformatted_Keys[c(1:total),])
for(Key in Reformatted_Keys[c(1:total),]){
  desired_Column <- which( colnames(Buy_List_ts)== Key)
  #Buy_List_ts_plot <- plot(Buy_List_ts_final[,desired_Column], xlab="Days",ylab="Card Offers",main= Key)
  bl_train_data <- window(Buy_List_ts_final[,desired_Column], start=c(1), end=c(Training_Week,Training_Day), freq=7)
  bl_test_data <- window(Buy_List_ts_final[,desired_Column], start=c(Test_Week,Test_Day),freq=7)
  bl_data_all <- window(Buy_List_ts_final[,desired_Column], start=c(1),freq=7)
  set.seed(253)
  fit = nnetar(bl_train_data)
  nnetforecast <- forecast(fit, h=7, PI = F)
  #autoplot(bl_test_data,series="Test Set")+autolayer(nnetforecast, series = "Forecast")+ggtitle("Test vs Forecast")+xlab("Weeks")+ylab("Offer Amount")+guides(colour=guide_legend(title="Forecast"))
  
  Vec2<- (cbind(bl_test_data, as.data.frame(forecast(fit, h=7, PI = F))[,1]))
  #ts.plot(Vec2, col=c("blue","red"), main = "Actual vs Forecast")
  RMSE2 <- round(sqrt(sum(((Vec2[,1]-Vec2[,2])^2)/length(Vec2[,1]))),4)
  MAPE2 <- round(mean(abs((Vec2[,1]-Vec2[,2])/Vec2[,1])),4)
  
  fit_all <- nnetar(bl_data_all)
  nnetforecast_all <- forecast(fit, h=7, PI = F)
  
  df <- as.data.frame(nnetforecast_all)
  Future_Forecast_Values <- rbind(as.data.frame(df[,1]),RMSE2)
  Future_Forecast_Values <- rbind(Future_Forecast_Values, MAPE2)
  Future_Forecast_Values <- as.data.frame(Future_Forecast_Values)
  
  Seperated_Column <- as.list(Buy_List_ts_final[,desired_Column])
  Future_Forecast_Values <- as.list(Future_Forecast_Values)
  Present_Future <- append(Seperated_Column, Future_Forecast_Values)
  NN_New_Forecast_Column <-round(data.frame(unlist(Present_Future)),2)
  colnames(NN_New_Forecast_Column) <- c(Key)
  NNETAR_DF <- cbind(NNETAR_DF, NN_New_Forecast_Column)
  
  setTxtProgressBar(pb, XYZ)
  XYZ <- XYZ + 1
}

End_Time <- Sys.time()
End_Time - Start_Time

FC_DF <- NNETAR_DF[,-1]
FC_DF <- t(FC_DF)
FC_DF <- as.data.frame(FC_DF)
All_Cards <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(`hasFoil` = col_character()))
Reformatted_Keys$Name <- All_Cards$card[match(Reformatted_Keys$Reformatted_Keys,All_Cards$Key)]
Reformatted_Keys$Set <- All_Cards$set[match(Reformatted_Keys$Reformatted_Keys,All_Cards$Key)]
Reformatted_Keys$Rarity <- All_Cards$rarity[match(Reformatted_Keys$Reformatted_Keys,All_Cards$Key)]
Reformatted_Keys$'F/NF' <- All_Cards$hasFoil[match(Reformatted_Keys$Reformatted_Keys,All_Cards$Key)]

NN_Final_Touches <- data.frame(Reformatted_Keys, FC_DF[c((ncol(FC_DF)-9):ncol(FC_DF))])
colnames(NN_Final_Touches) <- c("Key","Name","Set","Rarity","F/NF",format(currentDate,format = "%Y-%m-%d"),format(currentDate+1,format = "%Y-%m-%d"),format(currentDate+2,format = "%Y-%m-%d"),format(currentDate+3,format = "%Y-%m-%d"),format(currentDate+4,format = "%Y-%m-%d"),format(currentDate+5,format = "%Y-%m-%d"),format(currentDate+6,format = "%Y-%m-%d"),format(currentDate+7,format = "%Y-%m-%d"),"RMSE","MAPES")
NN_Final_Touches$`F/NF`[is.na(NN_Final_Touches$`F/NF`)==TRUE]<- ""
NN_Filtered_Angle <- NN_Final_Touches
NN_Filtered_Angle$Diff <- NN_Filtered_Angle[,12]-NN_Filtered_Angle[,6]
NN_Filtered_Angle <- NN_Filtered_Angle[order(-NN_Filtered_Angle$Diff),]
rownames(NN_Filtered_Angle) <- seq(nrow(NN_Filtered_Angle))
#View(NN_Filtered_Angle)
#Arima#
Reformatted_Keys <- as.data.frame(t(Cleaned_Recombined_data[1,]))
Reformatted_Keys <- Reformatted_Keys[,1]
Reformatted_Keys <- as.data.frame(Reformatted_Keys)

library(MASS)
library(tseries)
library(forecast)
library(ggplot2)
library(urca)
nrow(Reformatted_Keys)

Arima_Forecast_DF <- (1:(nrow(Buy_List_ts)+9))
Arima_Forecast_DF <- as.data.frame(Arima_Forecast_DF)


total = nrow(Reformatted_Keys) #For loading bar, match up this number for the pages to be run through
pb <- txtProgressBar(min=0, max = total, style = 3)
XYZ <- 1
Start_Time <- Sys.time()
set.seed(253)
#for(Key in Reformatted_Keys[c(1:total),])
for(Key in Reformatted_Keys[c(1:total),]){
  desired_Column <- which( colnames(Buy_List_ts)== Key)
  bl_train_data <- window(Buy_List_ts_final[,desired_Column], start=c(1), end=c(Training_Week,Training_Day), freq=7)
  bl_test_data <- window(Buy_List_ts_final[,desired_Column], start=c(Test_Week,Test_Day),freq=7)
  bl_data_all <- window(Buy_List_ts_final[,desired_Column], start=c(1),freq=7)
  MyArima <- auto.arima(bl_train_data, ic = "aic",stepwise = T, seasonal = T, approximation = F)
  MyArima_Forecast <- forecast(MyArima, h = 7)
  training_guardrails <- as.data.frame(bl_train_data)
  Arima_Vector <- cbind(bl_test_data,training_guardrails[c((nrow(training_guardrails)-6):nrow(training_guardrails)),1])
  RMSE2 <- round(sqrt(sum(((Arima_Vector[,1]-Arima_Vector[,2])^2)/length(Arima_Vector[,1]))),4)
  MAPE2 <- round(mean(abs((Arima_Vector[,1]-Arima_Vector[,2])/Arima_Vector[,1])),4)
  
  MyArima <- auto.arima(bl_data_all,max.D = 0, max.order = 5, nmodels = 5, allowmean = T, allowdrift = T, ic = "aic",stepwise = T, seasonal = T, approximation = F)
  bl_data_all_Arima <- forecast(MyArima, h = 7)
  Arima_Final_Forecast <- as.data.frame(bl_data_all_Arima)
  
  
  Future_Forecast_Values <- rbind(as.data.frame(Arima_Final_Forecast[,1]),RMSE2)
  Future_Forecast_Values <- rbind(Future_Forecast_Values, MAPE2)
  Future_Forecast_Values <- as.data.frame(Future_Forecast_Values)
  
  Seperated_Column <- as.list(Buy_List_ts_final[,desired_Column])
  Future_Forecast_Values <- as.list(Future_Forecast_Values)
  Present_Future <- append(Seperated_Column, Future_Forecast_Values)
  New_Forecast_Column <-round(data.frame(unlist(Present_Future)),2)
  colnames(New_Forecast_Column) <- c(Key)
  Arima_Forecast_DF <- cbind(Arima_Forecast_DF, New_Forecast_Column)
  
  setTxtProgressBar(pb, XYZ)
  XYZ <- XYZ + 1
  
}

End_Time <- Sys.time()
End_Time - Start_Time

#View(Arima_Forecast_DF)
ARIMA_DF <- Arima_Forecast_DF[,-1]
ARIMA_DF <- t(ARIMA_DF)
ARIMA_DF <- as.data.frame(ARIMA_DF)
All_Cards <- read_csv("/home/cujo253/Essential_Referential_CSVS/C20_Addition.csv", col_types = cols(`hasFoil` = col_character()))
Reformatted_Keys$Name <- All_Cards$card[match(Reformatted_Keys$Reformatted_Keys,All_Cards$Key)]
Reformatted_Keys$Set <- All_Cards$set[match(Reformatted_Keys$Reformatted_Keys,All_Cards$Key)]
Reformatted_Keys$Rarity <- All_Cards$rarity[match(Reformatted_Keys$Reformatted_Keys,All_Cards$Key)]
Reformatted_Keys$'F/NF' <- All_Cards$hasFoil[match(Reformatted_Keys$Reformatted_Keys,All_Cards$Key)]

format(currentDate, format = "%Y-%m-%d")

Final_Touches_Arima <- data.frame(Reformatted_Keys, ARIMA_DF[c((ncol(ARIMA_DF)-9):ncol(ARIMA_DF))])
colnames(Final_Touches_Arima) <- c("Key","Name","Set","Rarity","F/NF",format(currentDate,format = "%Y-%m-%d"),format(currentDate+1,format = "%Y-%m-%d"),format(currentDate+2,format = "%Y-%m-%d"),format(currentDate+3,format = "%Y-%m-%d"),format(currentDate+4,format = "%Y-%m-%d"),format(currentDate+5,format = "%Y-%m-%d"),format(currentDate+6,format = "%Y-%m-%d"),format(currentDate+7,format = "%Y-%m-%d"),"RMSE","MAPES")
Final_Touches_Arima$`F/NF`[is.na(Final_Touches_Arima$`F/NF`)==TRUE]<- ""
Filtered_Angle_ARIMA <- Final_Touches_Arima
Filtered_Angle_ARIMA$Diff <- Filtered_Angle_ARIMA[,12]-Filtered_Angle_ARIMA[,6]
Filtered_Angle_ARIMA <- Filtered_Angle_ARIMA[order(-Filtered_Angle_ARIMA$Diff),]
rownames(Filtered_Angle_ARIMA) <- seq(nrow(Filtered_Angle_ARIMA))


#Final Model Aggregation#
Filtered_Angle_ARIMA[Filtered_Angle_ARIMA == "Inf"] <- ""
Filtered_Angle_ARIMA[Filtered_Angle_ARIMA == "-Inf"] <- ""
Filtered_Angle_ARIMA[Filtered_Angle_ARIMA == "NaN"] <- ""

Filtered_Angle[Filtered_Angle == "Inf"] <- ""
Filtered_Angle[Filtered_Angle == "-Inf"] <- ""
Filtered_Angle[Filtered_Angle == "NaN"] <- ""

NN_Filtered_Angle[NN_Filtered_Angle == "Inf"] <- ""
NN_Filtered_Angle[NN_Filtered_Angle == "-Inf"] <- ""
NN_Filtered_Angle[NN_Filtered_Angle == "NaN"] <- ""

# Final_Compilation[Final_Compilation == "Inf"] <- ""
# Final_Compilation[Final_Compilation == "-Inf"] <- ""
# Final_Compilation[Final_Compilation == "NaN"] <- ""

Time_Series_keys <- as.list(Filtered_Angle_ARIMA$Key,NN_Filtered_Angle$Key,Filtered_Angle$Key)
Time_Series_keys <- as.data.frame((Time_Series_keys))
#Time_Series_keys <- unique(Time_Series_keys$Key)
Time_Series_keys <- t(Time_Series_keys)
Time_Series_keys <- as.data.frame(Time_Series_keys)
#rownames(Time_Series_keys)<- seq(nrow(Time_Series_keys))
colnames(Time_Series_keys)<-"Key"
Time_Series_keys <- as.data.frame(Time_Series_keys)
colnames(Time_Series_keys)<-"Key"
rownames(Time_Series_keys) <- seq(nrow(Time_Series_keys))
Time_Series_keys$name <- All_Cards$card[match(Time_Series_keys$Key,All_Cards$Key)]
Time_Series_keys$Set <- All_Cards$set[match(Time_Series_keys$Key,All_Cards$Key)]
Time_Series_keys$Rarity <- All_Cards$rarity[match(Time_Series_keys$Key,All_Cards$Key)]
Time_Series_keys$'F/NF' <- All_Cards$hasFoil[match(Time_Series_keys$Key,All_Cards$Key)]
Time_Series_keys$Todays_BL <- Filtered_Angle[,6][match(Time_Series_keys$Key,Filtered_Angle$Key)]
Time_Series_keys$`F/NF`[is.na(Time_Series_keys$`F/NF`)==TRUE]<-""

Final_Compilation <- na.omit(Time_Series_keys)
Final_Compilation$HW_RMSE <- Filtered_Angle[,14][match(Final_Compilation$Key,Filtered_Angle$Key)]
Final_Compilation$NN_RMSE <- NN_Filtered_Angle[,14][match(Final_Compilation$Key,NN_Filtered_Angle$Key)]
Final_Compilation$ARIMA_RMSE <- Filtered_Angle_ARIMA[,14][match(Final_Compilation$Key,Filtered_Angle_ARIMA$Key)]
rownames(Final_Compilation)<- seq(nrow(Final_Compilation))
Final_Compilation <- as.data.frame(Final_Compilation)
Filtered_Angle_ARIMA <- as.data.frame(Filtered_Angle_ARIMA)

Final_Compilation$Chosen_Model <- ifelse((Final_Compilation$HW_RMSE <= Final_Compilation$NN_RMSE & Final_Compilation$HW_RMSE <= Final_Compilation$ARIMA_RMSE), "Holts-Winter",
                                         ifelse((Final_Compilation$NN_RMSE <= Final_Compilation$HW_RMSE & Final_Compilation$NN_RMSE <= Final_Compilation$ARIMA_RMSE), "Neural Net",
                                                ifelse((Final_Compilation$ARIMA_RMSE <= Final_Compilation$HW_RMSE & Final_Compilation$ARIMA_RMSE <= Final_Compilation$NN_RMSE),"ARIMA",
                                                       "Game Over Man!")))
Final_Compilation$Day_1 <- ifelse(Final_Compilation$Chosen_Model == "Holts-Winter", Filtered_Angle[,7][match(Final_Compilation$Key,Filtered_Angle$Key)],
                                  ifelse(Final_Compilation$Chosen_Model == "Neural Net", NN_Filtered_Angle[,7][match(Final_Compilation$Key,NN_Filtered_Angle$Key)],
                                         ifelse(Final_Compilation$Chosen_Model == "ARIMA", Filtered_Angle_ARIMA[,7][match(Final_Compilation$Key,Filtered_Angle_ARIMA$Key)],
                                                "Game Over Man!")))

Final_Compilation$Day_2 <- ifelse(Final_Compilation$Chosen_Model == "Holts-Winter", Filtered_Angle[,8][match(Final_Compilation$Key,Filtered_Angle$Key)],
                                  ifelse(Final_Compilation$Chosen_Model == "Neural Net", NN_Filtered_Angle[,8][match(Final_Compilation$Key,NN_Filtered_Angle$Key)],
                                         ifelse(Final_Compilation$Chosen_Model == "ARIMA", Filtered_Angle_ARIMA[,8][match(Final_Compilation$Key,Filtered_Angle_ARIMA$Key)],
                                                "Game Over Man!")))
Final_Compilation$Day_3 <- ifelse(Final_Compilation$Chosen_Model == "Holts-Winter", Filtered_Angle[,9][match(Final_Compilation$Key,Filtered_Angle$Key)],
                                  ifelse(Final_Compilation$Chosen_Model == "Neural Net", NN_Filtered_Angle[,9][match(Final_Compilation$Key,NN_Filtered_Angle$Key)],
                                         ifelse(Final_Compilation$Chosen_Model == "ARIMA", Filtered_Angle_ARIMA[,9][match(Final_Compilation$Key,Filtered_Angle_ARIMA$Key)],
                                                "Game Over Man!")))
Final_Compilation$Day_4 <- ifelse(Final_Compilation$Chosen_Model == "Holts-Winter", Filtered_Angle[,10][match(Final_Compilation$Key,Filtered_Angle$Key)],
                                  ifelse(Final_Compilation$Chosen_Model == "Neural Net", NN_Filtered_Angle[,10][match(Final_Compilation$Key,NN_Filtered_Angle$Key)],
                                         ifelse(Final_Compilation$Chosen_Model == "ARIMA", Filtered_Angle_ARIMA[,10][match(Final_Compilation$Key,Filtered_Angle_ARIMA$Key)],
                                                "Game Over Man!")))
Final_Compilation$Day_5 <- ifelse(Final_Compilation$Chosen_Model == "Holts-Winter", Filtered_Angle[,11][match(Final_Compilation$Key,Filtered_Angle$Key)],
                                  ifelse(Final_Compilation$Chosen_Model == "Neural Net", NN_Filtered_Angle[,11][match(Final_Compilation$Key,NN_Filtered_Angle$Key)],
                                         ifelse(Final_Compilation$Chosen_Model == "ARIMA", Filtered_Angle_ARIMA[,11][match(Final_Compilation$Key,Filtered_Angle_ARIMA$Key)],
                                                "Game Over Man!")))
Final_Compilation$Day_6 <- ifelse(Final_Compilation$Chosen_Model == "Holts-Winter", Filtered_Angle[,12][match(Final_Compilation$Key,Filtered_Angle$Key)],
                                  ifelse(Final_Compilation$Chosen_Model == "Neural Net", NN_Filtered_Angle[,12][match(Final_Compilation$Key,NN_Filtered_Angle$Key)],
                                         ifelse(Final_Compilation$Chosen_Model == "ARIMA", Filtered_Angle_ARIMA[,12][match(Final_Compilation$Key,Filtered_Angle_ARIMA$Key)],
                                                "Game Over Man!")))
Final_Compilation$Day_7 <- ifelse(Final_Compilation$Chosen_Model == "Holts-Winter", Filtered_Angle[,13][match(Final_Compilation$Key,Filtered_Angle$Key)],
                                  ifelse(Final_Compilation$Chosen_Model == "Neural Net", NN_Filtered_Angle[,13][match(Final_Compilation$Key,NN_Filtered_Angle$Key)],
                                         ifelse(Final_Compilation$Chosen_Model == "ARIMA", Filtered_Angle_ARIMA[,13][match(Final_Compilation$Key,Filtered_Angle_ARIMA$Key)],
                                                "Game Over Man!")))
Final_Compilation$Best_RMSE <- ifelse(Final_Compilation$Chosen_Model == "Holts-Winter", Filtered_Angle[,14][match(Final_Compilation$Key,Filtered_Angle$Key)],
                                      ifelse(Final_Compilation$Chosen_Model == "Neural Net", NN_Filtered_Angle[,14][match(Final_Compilation$Key,NN_Filtered_Angle$Key)],
                                             ifelse(Final_Compilation$Chosen_Model == "ARIMA", Filtered_Angle_ARIMA[,14][match(Final_Compilation$Key,Filtered_Angle_ARIMA$Key)],
                                                    "Game Over Man!")))
Final_Compilation$Best_MAPE <- ifelse(Final_Compilation$Chosen_Model == "Holts-Winter", Filtered_Angle[,15][match(Final_Compilation$Key,Filtered_Angle$Key)],
                                      ifelse(Final_Compilation$Chosen_Model == "Neural Net", NN_Filtered_Angle[,15][match(Final_Compilation$Key,NN_Filtered_Angle$Key)],
                                             ifelse(Final_Compilation$Chosen_Model == "ARIMA", Filtered_Angle_ARIMA[,15][match(Final_Compilation$Key,Filtered_Angle_ARIMA$Key)],
                                                    "Game Over Man!")))
Final_Compilation$Best_Diff <- ifelse(Final_Compilation$Chosen_Model == "Holts-Winter", Filtered_Angle[,16][match(Final_Compilation$Key,Filtered_Angle$Key)],
                                      ifelse(Final_Compilation$Chosen_Model == "Neural Net", NN_Filtered_Angle[,16][match(Final_Compilation$Key,NN_Filtered_Angle$Key)],
                                             ifelse(Final_Compilation$Chosen_Model == "ARIMA", Filtered_Angle_ARIMA[,16][match(Final_Compilation$Key,Filtered_Angle_ARIMA$Key)],
                                                    "Game Over Man!")))


Final_Compilation <- Final_Compilation[-c(7:9)]
Final_Compilation[,c(8:17)] <- sapply(Final_Compilation[,c(8:17)],as.character)
Final_Compilation[,c(8:17)] <- sapply(Final_Compilation[,c(8:17)],as.numeric)
Final_Compilation[,c(8:17)] <- round(Final_Compilation[,c(8:17)],1)
#Final_Compilation[is.na(Final_Compilation)== T] <- ""
Final_Compilation$RMSE_Pct_Volatility <- round(Final_Compilation$Best_RMSE/ Final_Compilation$Todays_BL,3)
Final_Compilation$Dollar_Pct_Chg <-  round(Final_Compilation$Best_Diff/Final_Compilation$Todays_BL,3)
Final_Compilation[Final_Compilation == "Inf"] <- .5
Final_Compilation[Final_Compilation == "-Inf"] <- .5
Final_Compilation[Final_Compilation == "NaN"] <- .5

RMSE_Cutoff <- mean(Final_Compilation$RMSE_Pct_Volatility)
Dollar_Cutoff <- mean(Final_Compilation$Dollar_Pct_Chg)
Final_Compilation <- unique(Final_Compilation)

Confidant_Volatility <- Final_Compilation[which(Final_Compilation$RMSE_Pct_Volatility < (RMSE_Cutoff) & Final_Compilation$Todays_BL > 3.00),]
Confidant_Volatility <- Confidant_Volatility[which(Confidant_Volatility$Dollar_Pct_Chg > (Dollar_Cutoff * 2) | Confidant_Volatility$Dollar_Pct_Chg > ((Dollar_Cutoff*-1) * 2)),]
Confidant_Volatility <- Confidant_Volatility[which(Confidant_Volatility$Dollar_Pct_Chg != 0),]
Confidant_Volatility <- Confidant_Volatility[order(-Confidant_Volatility$Dollar_Pct_Chg),]


Volatile_Growth <- Confidant_Volatility[which(Confidant_Volatility$Dollar_Pct_Chg > 0),]
Volatile_Growth <- Volatile_Growth[order(-Volatile_Growth$Dollar_Pct_Chg),]
Volatile_Decline <- Confidant_Volatility[which(Confidant_Volatility$Dollar_Pct_Chg < 0),]
Volatile_Decline <- Volatile_Decline[order(Volatile_Decline$Dollar_Pct_Chg),]


setwd("/home/cujo253/Metrics/Time_Series_Results/Buylist")
csvFileName <- paste(currentDate,"_Times_Series",".csv",sep="")
write.csv(Final_Compilation, file=csvFileName, row.names = FALSE)

library(devtools)
library(googlesheets4)
library(googledrive)
library(gargle)
library(httr)
options(httr_oob_default=TRUE) 
options(gargle_oauth_email = "pachun95@gmail.com")

# my_dfs <- list(Volatile_Growth,Volatile_Decline,Confidant_Volatility,Final_Compilation)
# gs4_auth()
# sheets_create(
#   paste(currentDate,"TS_Results",sep=""),
#   sheets = my_dfs
# )
drive_auth(email = "pachun95@gmail.com", use_oob = T)
ss <- drive_get("Time_Series_Results")
#sheets_deauth()
gs4_auth(email = "pachun95@gmail.com", use_oob = T)
sheet_write(
  Volatile_Growth,
  ss = ss,
  sheet = "Growth"
)
sheet_write(
  Volatile_Decline,
  ss = ss,
  sheet = "Decline"
)
sheet_write(
  Confidant_Volatility,
  ss = ss,
  sheet = "All_Confident_Volatility"
)
sheet_write(
  Final_Compilation,
  ss = ss,
  sheet = "All_Cards"
)


summary(as.factor(Confidant_Volatility$Chosen_Model))

setwd("/home/cujo253/cronR/")