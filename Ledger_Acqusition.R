pacman::p_load(fs)
setwd("/home/cujo253/cronR")
source("ledger_functions.R")
# Initial Ledger Acquisition ----------------------------------------------

drive_auth(email = "pachun95@gmail.com",use_oob=TRUE)
gs4_auth(email = "pachun95@gmail.com",use_oob=TRUE)

Sold_Ledger <- ovr_sold_ledger("Bills & MTG 2020","Inventory Sold")
r_inv <- ovr_inventory_ledger("Bills & MTG 2020","Raw Inventory")

remDr <- chrome("Your_IP")


# TCG Sales History -------------------------------------------------------

Recent_Sales <- tcg_login_sales()
combined_attributes <- scryfall_api(Recent_Sales)
Recent_Sales <- tcg_sales_scryfall_marriage(Recent_Sales,combined_attributes)


# Card Kingdom Buylist Sales ----------------------------------------------

ck_sell_order <- ck_login_sales()
combined_attributes <- scryfall_api(ck_sell_order)
ck_sell_order <- cbind(ck_sell_order[-ncol(ck_sell_order)],(combined_attributes                                  %>% 
                                                                as.data.frame()                                  %>% 
                                                                select(type_line,commander,final_color,rarity))) %>% 
    as.data.frame()


# TCG Purchases -----------------------------------------------------------

#option[1] = 30 days
#option[2] = 90 days
#options[3] = 120 days
#options[4] = All Time
TCG_Full_History <- tcg_purchases(n=3)
combined_attributes <- scryfall_api(TCG_Full_History)
TCG_Full_History <- cbind(TCG_Full_History[-ncol(TCG_Full_History)],combined_attributes)

# Card Kingdom Purchases --------------------------------------------------

ck_order <- ck_purchases()
combined_attributes <- scryfall_api(ck_order)
ck_order <- cbind(ck_order[-ncol(ck_order)],(combined_attributes                            %>% 
                                                 as.data.frame()                            %>% 
                                                 select(type_line,commander,final_color)) ) %>% 
    as.data.frame()                                                                         %>% 
    relocate(order_id,.after = last_col())                                                  %>% 
    relocate(Purchased_Via,.after = last_col())


# Merged Purchases --------------------------------------------------------

ref_TCG <- TCG_Full_History[c(1,2,3,4,5,6,7,8,9,10,14,18,19,17,15,16)]

ref_TCG$Purchase_Dates =  mdy(gsub("\\/","-",ref_TCG$Purchase_Dates))

colnames(ck_order) <- colnames(ref_TCG)
Purchased_Ledger <- rbind(ck_order,ref_TCG)

Purchased_Ledger <- Purchased_Ledger %>% group_by(Key,name,set,hasFoil,abbrev,rarity,type_line,commander,final_color,Purchased_Via) %>% summarize(
                                                 qty = sum(as.numeric(Quantity)),
                                                 total = sum(total)/qty,
                                                 dop = mean(Purchase_Dates)) %>% ungroup() %>%
    mutate(Blank1 = "",Blank2 = "",Blank3 = "", Blank4 = "", Blank5 = "", Blank6 = "", Blank7 = "",Blank8 = "", Blank9 = "", Blank10 = "")  %>%
    mutate(Purchased_Via = gsub("CardKindom","CK",Purchased_Via),
           dop = gsub("-","/",dop),
           hasFoil = gsub("Foil","FOIL",hasFoil)) %>%
    select(name,qty,Blank1,Blank2,hasFoil,set,Key,abbrev,Purchased_Via,dop,Blank3,Blank4,Blank5,Blank6,Blank7,total,Blank8,Blank9,Blank10,final_color,type_line,commander,rarity) %>%
    ungroup()



ss <- drive_get("Bills & MTG 2020")
sheet_write(Purchased_Ledger,ss,"New Purchases")





