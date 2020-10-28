pacman::p_load(tidyverse,timetk,lubridate,bigrquery,modeltime,recipes,rsample,kknn,earth,tune)

gaeas_cradle <- function(email){
    con <- dbConnect(
        bigrquery::bigquery(),
        project = "gaeas-cradle",
        dataset = "premiums",
        billing = "gaeas-cradle"
    )
    bq_auth(email = email, use_oob = TRUE)
    options(scipen = 20)
    con
}

tcg_sr_rr <- function(email){
    con <- dbConnect(
        bigrquery::bigquery(),
        project = "tcg-sr-rr",
        dataset = "aether_revolt",
        billing = "tcg-sr-rr"
    )
    bq_auth(email = email, use_oob = TRUE)
    options(scipen = 20)
    con
}
con <- gaeas_cradle("wolfoftinstreet@gmail.com")
statement <- paste("SELECT DISTINCT rdate, a.set FROM `gaeas-cradle.roster.mtgjson`a ORDER BY rdate desc")
set_dates_xreg = dbSendQuery(con, statement = statement) %>% dbFetch(., n = -1) %>% distinct() %>% 
    mutate(set_release = 1, rdate = ymd(rdate)) %>% pad_by_time(.date_var =rdate) %>%
    select(-set) %>% replace(is.na(.),0) 

statement <- paste(
    "SELECT a.Key,a.Card,a.MKT,a.Sellers,a.BL,a.BL_QTY,a.Arb,a.TCG_Rank,a.CK_ADJ_Rank,a.Date ",
    "FROM `gaeas-cradle.premiums.*` a ",
    'WHERE Foil_Status not like "%FOIL%" and (Rarity like "R" or Rarity like "M" or Rarity like "U") and a.Set is not NULL and param is not NULL ',
    'and _TABLE_SUFFIX BETWEEN ',
    'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 300 DAY)) AND ',
    'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL -1 DAY)) ',
    "Order By Date asc; ",
    sep = ""
)

raw_query <- dbSendQuery(con, statement = statement) %>% dbFetch(., n = -1) %>% distinct()
raw_query %>% filter(grepl("Ghoulcaller Gisa",Key))

the_graph_title = "Ghoulcaller GisaCommander 2014M"

unique_card = raw_query[which(raw_query$Key == the_graph_title),] %>% mutate(Arb = ifelse(is.na(Arb), (MKT*(-1)),Arb ),
                                                                                                    BL = ifelse(is.na(BL), (0), BL ),
                                                                                                    BL_QTY = ifelse(is.na(BL_QTY), (0), BL_QTY ))

statement <- paste(
    "SELECT DISTINCT rdate, card, count(*) added_printings FROM `gaeas-cradle.roster.mtgjson`a WHERE card like '",unique(unique_card$Card),"' GROUP BY 1,2  ORDER BY rdate desc ",
    sep = ""
)
printings_xreg = dbSendQuery(con, statement = statement) %>% dbFetch(., n = -1) %>% distinct() %>% mutate(rdate = ymd(rdate))


# modeltime ---------------------------------------------------------------

unique_card_bl <- unique_card %>% select(Date,BL) %>% pivot_longer(-Date) %>% select(-name) %>% na.omit()

unique_card_external_analytics_tbl = unique_card %>% select(-Arb,-Card) %>%
    fill(., .direction = "up")%>% summarise_by_time(Date,.by = "day", across(MKT:CK_ADJ_Rank,.fns =sum)) %>%
    mutate(across(MKT:CK_ADJ_Rank, .fns = log1p)) %>% fill(MKT,Sellers,BL,BL_QTY,TCG_Rank,CK_ADJ_Rank) %>% 
    mutate(across(MKT:CK_ADJ_Rank, .fns = standardize_vec)) %>% select(-BL)

#unique_card_bl %>% View()
bl_prepared_tbl = unique_card_bl %>% summarise_by_time(.date_var = Date, .by = "day",value = sum(value)
) %>% pad_by_time(.date_var = Date,.pad_value = 0
) %>% mutate(bl_trans = log_interval_vec(value,limit_lower = 0, offset = 1)
) %>% mutate(bl_trans = standardize_vec(bl_trans)
) %>% mutate(bl_trans_clean = ts_clean_vec(bl_trans, period = 7)
) %>% select(-value, -bl_trans
) %>% pivot_longer(contains("trans")
) %>% select(-name
) 

# bl_prepared_tbl %>%
    # plot_acf_diagnostics(Date,value,.ccf_vars = MKT:CK_ADJ_Rank,.show_ccf_vars_only=T)
    

lower_limit = 0
upper_limit = 16.4
offset = 1
std_mean = 0.139616246925689
standard_deviation = 0.921417012014815

horizon = 30
lag_period = 30
rolling_periods = c(7,14,21)

#data_prepared_full_tbl %>% view()
data_prepared_full_tbl = bl_prepared_tbl                       %>%
    bind_rows(
        future_frame(.data = .,.date_var = Date,.length_out = horizon)
    )                                                          %>% 
    # tk_augment_fourier(Date,.periods = c(15,40,60,130), .K =2) %>%
    tk_augment_lags(value,.lags = lag_period)                  %>%
    tk_augment_slidify(.value   = value_lag30,
                       .f       = median, 
                       .period  = rolling_periods,
                       .align   = "center",
                       .partial = T) %>% 
    left_join(set_dates_xreg, by = c("Date"="rdate")) %>%
    left_join(printings_xreg %>% select(-card), by = c("Date"="rdate")) %>% 
    distinct() %>% 
    mutate(printings = ifelse(min(printings_xreg$rdate)<Date[1], 
        sum(added_printings[1], sum(printings_xreg %>% 
                                        filter(rdate<min(bl_prepared_tbl$Date)) %>% 
                                     select(added_printings)),na.rm=T))) 
data_prepared_full_tbl = data_prepared_full_tbl %>% 
    mutate(added_printings = ifelse(is.na(added_printings),0,added_printings),
           new_printings = ifelse( (added_printings==0) & (printings > added_printings), printings, added_printings + printings) ) %>%
    select(-added_printings,-printings)

for(i in 1:nrow(data_prepared_full_tbl)){
    if(data_prepared_full_tbl$new_printings[i+1]<data_prepared_full_tbl$new_printings[i]){data_prepared_full_tbl$new_printings[i+1]=data_prepared_full_tbl$new_printings[i]}
    if(i+1 == nrow(data_prepared_full_tbl)){break}
}



data_prepared_tbl = data_prepared_full_tbl %>% filter(!is.na(value))
forecast_tbl = data_prepared_full_tbl %>% filter(is.na(value))

splits = time_series_split(data_prepared_tbl, assess = horizon, cumulative = T)


best_lm_model$terms %>% formula()

recipe_spec_base = recipe(value ~ ., data = training(splits)) %>%
    #Time series signature
    step_timeseries_signature(Date) %>%
    step_rm(matches("(iso)|(hour)|(minute)|(second)|(am.pm)|(xts)|(year)")) %>%
    #stnadardization
    step_normalize(matches("(index.num)|(yday)")) %>%
    #dummy Encoding
    step_dummy(all_nominal(),one_hot = T) %>%
    step_interact(~matches("mday7") * matches("wday.lbl")) %>%
    step_fourier(Date, period = c(15,40,60), K =2)

recipe_spec_base %>% prep() %>% juice() %>% glimpse() 
 

# Spline Model ------------------------------------------------------------
# lm
model_spec_lm = linear_reg() %>% set_engine("lm")
model_spec_rf = rand_forest(mode = "regression") %>% set_engine("randomForest")
model_spec_exp_smooth = exp_smoothing(mode = "regression") %>% set_engine("ets")
model_spec_arima = arima_boost(mode = "regression") %>% set_engine("auto_arima_xgboost")
model_spec_glmnet = linear_reg(penalty = 0.1,mixture   = 0.5) %>% set_engine("glmnet") 
#model_spec_arima_2 = arima_reg() %>% set_engine("auto_arima") 
model_spec_kknn = nearest_neighbor() %>% set_engine("kknn") 
model_spec_prophet = prophet_reg() %>% set_engine("prophet") 

# spline model    
recipe_spec_base_1 = recipe_spec_base %>% step_rm(Date,contains("_lag")) %>% step_ns(contains("index.num"),deg_free = 2) 

workflow_fit_lm_1_spline = workflow() %>% add_model(model_spec_lm) %>% add_recipe(recipe_spec_base_1) %>% fit(training(splits))

calibration_tbl = modeltime_table(workflow_fit_lm_1_spline) %>% modeltime_calibrate(new_data = testing(splits))

# * Lag Recipe 

recipe_spec_base_2 = recipe_spec_base %>% step_rm(Date) %>% step_naomit(starts_with("value_lag"))

workflow_fit_lm_2_lag = workflow() %>% add_model(model_spec_lm) %>% add_recipe(recipe_spec_base_2) %>% fit(training(splits))

calibration_tbl_2 = modeltime_table(workflow_fit_lm_2_lag) %>% modeltime_calibrate(new_data = testing(splits))

# * random forest base
recipe_spec_base_3 = recipe_spec_base  %>% step_naomit(starts_with("value_lag"))

workflow_fit_rf_3_lag = workflow() %>% add_model(model_spec_rf) %>% add_recipe(recipe_spec_base_3) %>% fit(training(splits))

calibration_tbl_3 = modeltime_table(workflow_fit_rf_3_lag) %>% modeltime_calibrate(new_data = testing(splits))

# * Exp Smoothing
recipe_spec_base_4 = recipe_spec_base  %>% step_rm(set_release,new_printings) %>% step_naomit(starts_with("value_lag"))

workflow_fit_expsm_4_lag = workflow() %>% add_model(model_spec_exp_smooth) %>% add_recipe(recipe_spec_base_4) %>% fit(training(splits))

calibration_tbl_4 = modeltime_table(workflow_fit_expsm_4_lag) %>% modeltime_calibrate(new_data = testing(splits))

# * arima base
recipe_spec_base_5 = recipe_spec_base  %>% step_naomit(starts_with("value_lag"))

workflow_fit_ari_5_lag = workflow() %>% add_model(model_spec_arima) %>% add_recipe(recipe_spec_base_5) %>% fit(training(splits))

calibration_tbl_5 = modeltime_table(workflow_fit_ari_5_lag) %>% modeltime_calibrate(new_data = testing(splits))

# * arima base
recipe_spec_base_8 = recipe_spec_base %>% step_rm(Date,contains("_lag")) %>% step_ns(contains("index.num"),deg_free = 2) 

workflow_fit_glm_8_lag = workflow() %>% add_model(model_spec_glmnet) %>% add_recipe(recipe_spec_base_8) %>% fit(training(splits))

calibration_tbl_8 = modeltime_table(workflow_fit_ari_8_lag) %>% modeltime_calibrate(new_data = testing(splits))

# * arima base
recipe_spec_base_6 = recipe_spec_base  %>% step_naomit(starts_with("value_lag"))

workflow_fit_ffnn_6_lag = workflow() %>% add_model(model_spec_kknn) %>% add_recipe(recipe_spec_base_6) %>%fit(training(splits))

calibration_tbl_6 = modeltime_table(workflow_fit_ffnn_6_lag) %>% modeltime_calibrate(new_data = testing(splits))

# * Multivariate adaptive regression
# * arima base
recipe_spec_base_7 = recipe_spec_base  %>% step_naomit(starts_with("value_lag"))

workflow_fit_proph_7_lag = workflow() %>% add_model(model_spec_prophet) %>% add_recipe(recipe_spec_base_7) %>%fit(training(splits))

calibration_tbl_7 = modeltime_table(workflow_fit_proph_7_lag) %>% modeltime_calibrate(new_data = testing(splits))


# Compare workflow --------------------------------------------------------

calibration_tbl_compare = modeltime_table(
    workflow_fit_lm_1_spline,
    workflow_fit_lm_2_lag,
    workflow_fit_rf_3_lag,
    workflow_fit_expsm_4_lag,
    workflow_fit_ari_5_lag,
    workflow_fit_ffnn_6_lag,
    workflow_fit_proph_7_lag,
    workflow_fit_glm_8_lag
) %>%
    modeltime_calibrate(new_data    = testing(splits))

calibration_tbl_compare %>% modeltime_forecast(new_data= testing(splits),
                                               actual_data = training(splits))%>% 
    #Inversion
    mutate(across(.value:.conf_hi, .fns = ~standardize_inv_vec(x=.,
                                                               mean = std_mean,
                                                               sd = standard_deviation))) %>%
    mutate(across(.value:.conf_hi,.fns = ~log_interval_inv_vec(
        x=.,
        limit_lower = lower_limit,
        limit_upper = upper_limit,
        offset = offset
    ))) %>%
    
    plot_modeltime_forecast(.title = the_graph_title)

calibration_tbl_compare %>% modeltime_accuracy() %>% mutate(smape = round(smape/2,2) )

refit_tbl = calibration_tbl_compare %>% modeltime_refit(data = data_prepared_tbl)

refit_tbl %>% modeltime_forecast(new_data= forecast_tbl,
                                 actual_data = data_prepared_tbl,
                                 conf_interval = .95)%>% 
    #Inversion
    mutate(across(.value:.conf_hi, .fns = ~standardize_inv_vec(x=.,
                                                               mean = std_mean,
                                                               sd = standard_deviation))) %>%
    mutate(across(.value:.conf_hi,.fns = ~log_interval_inv_vec(
        x=.,
        limit_lower = lower_limit,
        limit_upper = upper_limit,
        offset = offset
    ))) %>%
    
    plot_modeltime_forecast(.title = the_graph_title)

refit_tbl %>% modeltime_accuracy() %>% mutate(smape = round(smape/2,2) )
 


model_fit_smoothing = exp_smoothing(
    error = "additive",
    trend = "none",
    season = "none"
) %>% set_engine("ets") %>%
    fit(value ~ ., training(splits)%>% select(-set_release,-new_printings))


model_fit_seasonal = seasonal_reg(seasonal_period_1 = 7,
             seasonal_period_2 = 21,
             seasonal_period_3 = 40) %>%
    set_engine("tbats")  %>%
    fit(value ~ ., (training(splits) %>% select(-set_release,-new_printings)) )

modeltime_table(
    model_fit_smoothing,
    model_fit_seasonal
) %>% 
    modeltime_calibrate(testing(splits)%>% select(-set_release,-new_printings)) %>%
    modeltime_forecast(
        new_data = testing(splits) %>% select(-set_release,-new_printings),
        actual_data = data_prepared_tbl %>% select(-set_release,-new_printings)
    ) %>%
    plot_modeltime_forecast()
