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
pacman::p_load(tidyverse,rvest,jsonlite,devtools,googlesheets4,googledrive,googlesheets,readr,dplyr,gargle,httr,bigrquery,RSelenium)
options(httr_oob_default=TRUE) 
options(gargle_oauth_email = "pachun95@gmail.com")
drive_auth(email = "pachun95@gmail.com",use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com",use_oob=TRUE)

specs <- range_read(drive_get("BAN Spec Performance"),"DataEntry")

compiled_results= NULL
for(i in 1:nrow(specs)){
    cardofinterest = specs[i,] %>% select(Card)
    setofinterest = specs[i,] %>% select(Set)
    period_length = specs[i,] %>% select(`Days Since Call`)
    
    con <- gaeas_cradle("wolfoftinstreet@gmail.com")
    
    statement <- paste(
       'DECLARE cardofinterest DEFAULT "',cardofinterest,'"; ',
        'DECLARE setofinterest DEFAULT "',setofinterest,'"; ',
        'With t1 as (Select *,round(BL_chg/BL_avg*100,2) as BL_Perc_Growth ,round((c.BL_chg/c.MKT_chg) *100,2) as BL_MKT_Perc_Ratio ',
        'FROM ',
        '(Select b.Card, b.Set,b.rarity,number, ',
        'round(avg(BL),1) as BL_avg, round(sum(BL_change),2) as BL_chg, ',
        'round(AVG(MKT),1) as MKT_avg,round(sum(MKT_change),1) as MKT_chg, ',
        'round(AVG(Sellers),0) as Sellers_avg, round(sum(Seller_change),0) as Seller_chg ',
        'FROM ',
        '(SELECT Card,a.Set,rarity,Foil_status,number, Date, BL, round(BL - lag(BL) over (order by Date),2) as BL_change,MKT, round(MKT - lag(MKT) over (order by Date),2) as MKT_change, Sellers, round(Sellers - lag(Sellers) over (order by Date),0) as Seller_change ',
        'FROM `gaeas-cradle.premiums.*` a ',
        'WHERE Foil_Status like "" and Card like cardofinterest and a.Set like setofinterest ',
        'Order By Date desc ',
        'Limit ',period_length,' ) b ',
        'group by 1,2,3,4) c), ',
        't2 as ( ',
        'Select BL as Current_BL, MKT as Current_MKT, Sellers as Current_Sellers  ',
        'From `gaeas-cradle.premiums.*` a ',
        'Where Foil_Status like "" and number is not null and Card like cardofinterest and a.Set like setofinterest ',
        'Order By Date Desc ',
        'Limit 1), ',
        't3 as ( ',
        'SELECT q.CK_MKT, q.MKT_Diff, q.CK_Backing, q.TCG_Backing, q.Group ',
        'FROM `gaeas-cradle.ck_funny_money.*` q ',
        'Where Foil_Status like "" and Card like cardofinterest and q.Set like setofinterest ',
        'Order By Date Desc ',
        'Limit 1), ',
        't4 as (  ',
        'SELECT q.BL as Origin_BL,q.BL_QTY Origin_BL_QTY,q.TCG_MKT Origin_TCG_Mkt, q.CK_MKT Origin_CK_MKT, q.MKT_Diff Origin_MKT_Diff, q.CK_Backing Orign_CK_Backing, q.TCG_Backing Orign_TCG_Backing ',
        'FROM `gaeas-cradle.ck_funny_money.*` q ',
        ' Where Foil_Status like "" and Card like cardofinterest and q.Set like setofinterest AND  ',
        '_TABLE_SUFFIX BETWEEN ',
        'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',period_length + 1,' DAY)) AND ',
        'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL ',period_length,' DAY)) ',
        'Order By Date Desc ',
        'Limit 1) ',
       'Select * ',
        'From t1 cross join t2 cross join t3 cross join t4; ',
        sep = ""
    )
    individual_result <- dbSendQuery(con, statement = statement) %>% dbFetch(n = 1)
    
    compiled_results = rbind(compiled_results,individual_result)
}

compiled_results = compiled_results %>% mutate(Speculator = specs$Speculator[match(paste(compiled_results$Card,compiled_results$Set), paste(specs$Card,specs$Set))],
                            Days_Passed = specs$`Days Since Call`[match(paste(compiled_results$Card,compiled_results$Set), paste(specs$Card,specs$Set))],
                            Date_Called = specs$`Date Called`[match(paste(compiled_results$Card,compiled_results$Set), paste(specs$Card,specs$Set))]) %>%
    select(Speculator,Days_Passed, Date_Called, everything())

sheet_write(compiled_results, drive_get("BAN Spec Performance"),"Performance")
