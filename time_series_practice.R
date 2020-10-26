pacman::p_load(tidyverse,timetk,lubridate,bigrquery,modeltime,recipes,rsample)

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
con <- tcg_sr_rr("wolfoftinstreet@gmail.com")
statement <- "With t1 as (SELECT DISTINCT a.Key, a.Card_name,a.Set,a.Rarity,a.MKT_EST,a.MKT,a.Vendor_Listings,a.Set_Rank,a.Date 
    FROM `tcg-sr-rr.rivals_of_ixalan.*` a 
    WHERE a._TABLE_SUFFIX BETWEEN 
        FORMAT_DATE('%Y_%m_%d', DATE_SUB(CURRENT_DATE(), INTERVAL 30 DAY)) AND  
        FORMAT_DATE('%Y_%m_%d', DATE_SUB(CURRENT_DATE(), INTERVAL -1 DAY)) AND 
    a.Rarity like 'M' AND Methodology like 'SR'), 
    t2 as( 
    SELECT * 
    FROM `gaeas-cradle.premiums.*` b 
    WHERE b._TABLE_SUFFIX BETWEEN 
        FORMAT_DATE('%Y_%m_%d', DATE_SUB(CURRENT_DATE(), INTERVAL 30 DAY)) AND  
        FORMAT_DATE('%Y_%m_%d', DATE_SUB(CURRENT_DATE(), INTERVAL -1 DAY)) AND 
    b.Rarity like 'M') 
    SELECT a.Key, a.Card_name,a.Set,a.Rarity,a.MKT_EST,a.MKT,b.BL,b.BL_QTY,a.Vendor_Listings,a.Set_Rank,a.Date 
    from t1 a 
    Left join t2 b on CONCAT(a.Key,' ',a.Date) = CONCAT(b.Key,' ',b.Date) 
    ORDER BY Card_name,Date "
raw_query <- dbSendQuery(con, statement = statement) %>% dbFetch(., n = -1) %>% distinct() %>% mutate(Arb = BL - MKT)
raw_query %>% glimpse()
raw_query %>% mutate(Date = format(Date, "%m/%d/%Y"))


con <- gaeas_cradle("wolfoftinstreet@gmail.com")
statement <- paste(
    "SELECT a.Key,a.MKT,a.Sellers,a.BL,a.BL_QTY,a.Arb,a.TCG_Rank,a.CK_ADJ_Rank,a.Date ",
    "FROM `gaeas-cradle.premiums.*` a ",
    'WHERE Foil_Status not like "%FOIL%" and (Rarity like "R" or Rarity like "M" or Rarity like "U") and a.Set is not NULL and param is not NULL ',
    'and _TABLE_SUFFIX BETWEEN ',
    'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL 180 DAY)) AND ',
    'FORMAT_DATE("%Y_%m_%d", DATE_SUB(CURRENT_DATE(), INTERVAL -1 DAY)) ',
    "Order By Date asc; ",
    sep = ""
)
raw_query <- dbSendQuery(con, statement = statement) %>% dbFetch(., n = -1) %>% distinct()
raw_query %>% glimpse()
raw_query %>% mutate(Date = format(Date, "%m/%d/%Y"))
raw_query %>% filter(grepl("Taigam, Ojutai Master",Key))
unique_card = raw_query[which(raw_query$Key == "Taigam, Ojutai MasterCommander 2017R"),] %>% mutate(Arb = ifelse(is.na(Arb), (MKT*(-1)),Arb ),
                                                                                                    BL = ifelse(is.na(BL), (0), BL ),
                                                                                                    BL_QTY = ifelse(is.na(BL_QTY), (0), BL_QTY ))

unique_card_summary <- unique_card %>% select(Date,everything()) %>% select(-Key) %>% pivot_longer(-Date) %>% group_by(name)

unique_card_summary %>% plot_time_series(Date,log1p(abs(value)),name)

unique_card_summary %>% plot_time_series(Date,expm1(log1p(abs(value))),name)


unique_card_summary %>% plot_time_series(Date,box_cox_vec(value + 1, lambda = "auto"),name)

box_coxed <- unique_card_summary %>% mutate(value_trans = box_cox_vec(value)) %>% group_split() %>%
    map2(.y=1:6, .f =  function(df,idx){
        if(idx==1) lambda = 1.99995900720725
        if(idx==2) lambda = -0.234680344678062
        if(idx==3) lambda = -0.99992424816297
        if(idx==4) lambda = -0.99992424816297
        if(idx==5) lambda = 1.99992424816297
        if(idx==6) lambda = -0.00371706262469994
        
        df %>%
            mutate(value_trans_inv = box_cox_inv_vec(value_trans, lambda = lambda))
    }) %>%
    bind_rows() 

box_coxed %>%
    group_by(name) %>%
    plot_time_series(Date, value_trans)

unique_card %>% select(Date,everything()) %>% select(-Key,-Arb)


# Rolling Median/Averages -------------------------------------------------

unique_card %>% select(Date,everything()) %>% select(-Key) %>% 
    mutate(MKT = MKT %>% slidify_vec(.f = mean,.period=7,.align = "right",.partial =T),
           Vendor_Listings = Vendor_Listings %>% slidify_vec(.f = mean,.period=7,.align = "right",.partial =T),
           BL = BL %>% slidify_vec(.f = mean,.period=7,.align = "right",.partial =T),
           BL_QTY = BL_QTY %>% slidify_vec(.f = mean,.period=7,.align = "right",.partial =T),
           Set_Rank = Set_Rank %>% slidify_vec(.f = mean,.period=7,.align = "right",.partial =T),
           Arb = Arb %>% slidify_vec(.f = mean,.period=7,.align = "right",.partial =T),
           MKT_EST = MKT_EST %>% slidify_vec(.f = mean,.period=7,.align = "right",.partial =T)) %>%
    #slice(-c(1:6)) %>%
    select(-Set,-Rarity,-Card_name) %>% 
    pivot_longer(-Date) %>% group_by(name) %>% plot_time_series(Date,value)


unique_card %>% select(Date,everything()) %>% select(-Key) %>% 
    mutate(MKT = MKT %>% slidify_vec(.f = median,.period=7,.align = "center",.partial =T),
           Vendor_Listings = Vendor_Listings %>% slidify_vec(.f = median,.period=7,.align = "center",.partial =T),
           BL = BL %>% slidify_vec(.f = median,.period=7,.align = "center",.partial =T),
           BL_QTY = BL_QTY %>% slidify_vec(.f = median,.period=7,.align = "center",.partial =T),
           Set_Rank = Set_Rank %>% slidify_vec(.f = median,.period=7,.align = "center",.partial =T),
           Arb = Arb %>% slidify_vec(.f = mean,.period=7,.align = "center",.partial =T),
           MKT_EST = MKT_EST %>% slidify_vec(.f = median,.period=7,.align = "center",.partial =T)) %>%
    #slice(-c(1:6)) %>%
    select(-Set,-Rarity,-Card_name) %>% 
    pivot_longer(-Date) %>% group_by(name) %>% plot_time_series(Date,value)

# Loess Smoother ----------------------------------------------------------
#uses a proportion of the data to model the non-linear fit & is combined with a K-nearest neighbor meta model
# Loess comes up in STL Decomposition and is commonly used to plot a trend

unique_card %>% select(Date,everything()) %>% select(-Key) %>%
    mutate(MKT = smooth_vec(MKT,period = 7),
           Vendor_Listings = smooth_vec(Vendor_Listings,period = 7),
           BL = smooth_vec(BL,period = 7),
           BL_QTY = smooth_vec(BL_QTY,period = 7),
           Set_Rank = smooth_vec(Set_Rank,period = 7),
           Arb = smooth_vec(Arb,period = 7),
           MKT_EST = smooth_vec(MKT_EST,period = 7)) %>%
    select(-Set,-Rarity,-Card_name) %>% 
    pivot_longer(-Date) %>% group_by(name) %>% plot_time_series(Date,value)


# Rolling Correlation -----------------------------------------------------
rolling_cor_week <- slidify(
    .f = ~ cor(.x,.y,use = "pairwise.complete.obs"),
    .period = 7,
    .align = "center",
    .partial = T
) 
unique_card %>% select(Date,everything()) %>% select(-Key)  %>%
    select(Date,MKT,Vendor_Listings) %>%
    mutate(rolling_cor_VL_MKT = rolling_cor_week(Vendor_Listings,MKT)) %>%
    pivot_longer(-Date) %>%
    group_by(name) %>% na.omit() %>%
    plot_time_series(Date, value)


unique_card %>% select(Date,everything()) %>% select(-Key,-Set,-Rarity,-Card_name) %>% pivot_longer(-Date) %>%
    group_by(name)   


# Cleaning Outliers -------------------------------------------------------

unique_card %>% 
    select(Date,everything()) %>% 
    select(-Key) %>% 
    mutate(Arb = ts_clean_vec(Arb, period = 7),
           BL = ts_clean_vec(BL, period = 7),
           BL_QTY = ts_clean_vec(BL_QTY, period = 7),
           TCG_Rank = ts_impute_vec(TCG_Rank, period = 7),
           Sellers = ts_impute_vec(Sellers, period = 7)) %>%
    pivot_longer(-Date) %>%
    plot_anomaly_diagnostics(Date, value)


#Before Cleaning
unique_card %>% 
    select(Date,everything()) %>% 
    select(-Key) %>% 
    pivot_longer(-Date) %>%
    plot_time_series_regression(Date,
                                .formula = value ~ as.numeric(Date) + wday(Date, label = T) + month(Date, label = T), .show_summary = T)

#After Cleaning
unique_card %>% 
    select(Date,everything()) %>% 
    select(-Key) %>%
    mutate(Arb = ts_clean_vec(Arb, period = 7),
           BL = ts_clean_vec(BL, period = 7),
           BL_QTY = ts_clean_vec(BL_QTY, period = 7),
           TCG_Rank = ts_impute_vec(TCG_Rank, period = 7),
           Sellers = ts_impute_vec(Sellers, period = 7)) %>%
    pivot_longer(-Date) %>%
    plot_time_series_regression(Date,
                                .formula = value ~ as.numeric(Date) + wday(Date, label = T) + month(Date, label = T), .show_summary = T)


# lags --------------------------------------------------------------------

unique_card %>% 
    select(Date,everything()) %>% 
    select(Date, BL)  %>%
    pivot_longer(-Date) %>%
    plot_acf_diagnostics(Date, log1p(value) )


unique_card %>% 
    select(Date,everything()) %>% 
    select(Date, BL)  %>%
    pivot_longer(-Date) %>%
    tk_augment_lags(.value = value, .lags = c(28,41,74)) %>%
    drop_na() %>%
    plot_time_series_regression(
        Date,
        .formula = log1p(value) ~ log1p(value_lag28) +log1p(value_lag41) +log1p(value_lag74), .show_summary = T
    )


# Time Series Growth - cumsum ---------------------------------------------

unique_card %>% 
    select(Date,everything()) %>% 
    select(Date, BL)  %>%
    mutate(BL_lag = lag(BL), BL = BL - BL_lag) %>% slice(-1) %>%
    mutate(BL_growth = cumsum(BL)) %>%
    plot_time_series(Date, BL_growth, .smooth = F, .legend_show = T, .title = "Corsair Captain - CK BL Growth")

unique_card %>% 
    select(Date,everything()) %>% 
    select(Date, Sellers)  %>%
    mutate(Sellers = ts_impute_vec(Sellers, period = 7),BL_lag = lag(Sellers), Sellers_lag = Sellers - BL_lag) %>% slice(-1) %>%
    mutate(Sellers_growth = cumsum(Sellers_lag),
           Seller_velocity = diff_vec(Sellers_growth, lag = 1),
           Seller_acceleration = diff_vec(Sellers_growth, lag = 2))%>% select(-BL_lag) %>%
    pivot_longer(-Date) %>%
    group_by(name) %>%
    plot_time_series(Date, value, name, .smooth = F, .legend_show = T, .title = "Corsair Captain - TCG Sellers Growth")

unique_card %>% 
    select(Date,everything()) %>% 
    mutate(Sellers = ts_impute_vec(Sellers, period = 7),
           TCG_Rank = ts_impute_vec(TCG_Rank, period = 7),
           CK_ADJ_Rank = ts_impute_vec(CK_ADJ_Rank,period = 7))%>%
    mutate(across(MKT:CK_ADJ_Rank, .fns = diff_vec)) %>% select(-Key, - TCG_Rank, -CK_ADJ_Rank) %>%
    pivot_longer(-Date) %>%
    plot_time_series(Date,value,.smooth = F)


# Fourier Analysis --------------------------------------------------------

model_formula <- as.formula(value ~ as.numeric(Date) + . - Date + 
                                wday(Date, label = T) + 
                                ceiling(day(Date) / 7))

transformed_unique <- unique_card %>% 
    select(Date,everything()) %>%
    select(-Key & -Arb) %>%
    mutate(Sellers = ts_impute_vec(Sellers, period = 7),
           TCG_Rank = ts_impute_vec(TCG_Rank, period = 7),
           CK_ADJ_Rank = ts_impute_vec(CK_ADJ_Rank,period = 7)) %>%
    mutate(across(MKT:CK_ADJ_Rank, .fns = log_interval_vec)) %>%
    tk_augment_fourier(Date, .period = c(1,3,7,14,21,30), .K = 1) 

transformed_unique %>%pivot_longer(-Date) %>% plot_time_series_regression(Date,
                                                                          .formula = model_formula,
                                                                          .show_summary = T)

lm_model_fit <- lm(formula = model_formula, data = transformed_unique%>%pivot_longer(-Date))
summary(lm_model_fit)


future_tbl <- transformed_unique %>% future_frame(.date_var = Date,.length_out = "2 months") %>%
    mutate(MKT=NA, Sellers=NA, BL=NA,BL_QTY=NA,TCG_Rank=NA,CK_ADJ_Rank=NA) %>%
    tk_augment_fourier(Date, .period = c(1,3,7,14,21,30), .K = 1)  #%>% pivot_longer(-Date)

palantir = predict(lm_model_fit,newdata = future_tbl %>% pivot_longer(-Date)) %>% as.vector()

conf_interval = .75
residuals = lm_model_fit$residuals %>% as.vector()
alpha = (1- conf_interval)/2

abs_error_range = abs(qnorm(alpha) * sd(residuals))

forecast_tbl <- transformed_unique %>% pivot_longer(-Date)%>% 
    add_column(type = "actual") %>%
    bind_rows(
        future_tbl %>% pivot_longer(-Date) %>%
            mutate(value = palantir,
                   type = "prediction")
    ) %>% mutate(
        conf_lo = value - abs_error_range,
        conf_high = value + abs_error_range)

forecast_tbl %>% filter(name == "Sellers") %>%  na.omit() %>%
    rename(Sellers = name) %>%
    pivot_longer(cols = c(value,conf_lo,conf_high)) %>% 
    plot_time_series(Date, 
                     log_interval_inv_vec(x = value,
                                          limit_lower = 0,
                                          limit_upper = 416.8,
                                          offset = 0),
                     .color_var = name, .smooth = F)


# tk_augment --------------------------------------------------------------


unique_card_bl <- unique_card %>% select(Date,MKT) %>% pivot_longer(-Date) %>% select(-name) 

bl_prepared_tbl = unique_card_bl %>% summarise_by_time(Date, .by = "day",value = sum(value)
) %>% pad_by_time(.pad_value = 0
) %>% na.omit(
) %>% mutate(bl_trans = log_interval_vec(value,limit_lower = 0, offset = 1)
) %>% mutate(bl_trans = standardize_vec(bl_trans)
) %>% mutate(bl_trans_clean = ts_clean_vec(bl_trans, period = 7)
) %>% select(-value, -bl_trans
) %>% pivot_longer(contains("trans")
) 

bl_prepared_tbl %>%
    plot_time_series(Date,value,name)

unique_card %>% select(Date,MKT) %>% pivot_longer(-Date) %>% select(-name) %>%plot_time_series(Date,value)

bl_signature_tbl = bl_prepared_tbl %>%
    tk_augment_timeseries_signature(.date_var = Date
    ) %>% select(-diff, -contains("hour"),-contains("minute"),-contains("second"),-contains("name"),-contains("am.pm"),-ends_with("iso"),-ends_with(".xts")
    ) 

bl_signature_tbl %>% glimpse()

bl_signature_seasonality_formula = as.formula(
    value ~ splines::ns(index.num, knots = quantile(index.num, probs = c(.33,.66))) 
    + .
    + (as.factor(mday7) * wday.lbl)
)

bl_signature_tbl %>%
    plot_time_series_regression(
        Date,
        .formula = bl_signature_seasonality_formula,
        .show_summary = T
    )


bl_signature_tbl %>% plot_acf_diagnostics(Date,value)

bl_fourier_tbl = bl_signature_tbl %>%
    tk_augment_fourier(Date,.periods = c(15,40,60,130), .K =2)

bl_fourier_seasonality_formula = as.formula(
    value ~ splines::ns(index.num, knots = quantile(index.num, probs = c(.33,.66))) 
    + .
    + (as.factor(mday7) * wday.lbl)
)

bl_fourier_tbl %>%
    plot_time_series_regression(
        Date,
        .formula = bl_fourier_seasonality_formula,
        .show_summary = T
    )

bl_fourier_tbl %>% plot_time_series(Date,value,.interactive=F) +
    geom_point(color="red",data = . %>% filter(mday7 == 3))


# Chosen Model ------------------------------------------------------------
unique_card_bl <- unique_card %>% select(Date,BL) %>% pivot_longer(-Date) %>% select(-name) 


bl_prepared_tbl = unique_card_bl %>% summarise_by_time(Date, .by = "day",value = sum(value)
) %>% pad_by_time(.pad_value = 0
) %>% mutate(bl_trans = log_interval_vec(value,limit_lower = 0, offset = 1)
) %>% mutate(bl_trans = standardize_vec(bl_trans)
) %>% mutate(bl_trans_clean = ts_clean_vec(bl_trans, period = 7)
) %>% select(-value, -bl_trans
) %>% pivot_longer(contains("trans")
) %>% select(-name)

lower_limit = 0
upper_limit = 5.95
offset = 1
mean = -0.197600649674408
standard_deviation = 0.585606035392674

horizon = 30
lag_period = 30
rolling_periods = c(15,30,45)

bl_prepared_tbl %>% plot_time_series(Date,value)






bl_signature_tbl = bl_prepared_tbl %>%
    tk_augment_timeseries_signature(.date_var = Date
    ) %>% select(-diff, -contains("hour"),-contains("minute"),-contains("second"),-contains("name"),-contains("am.pm"),-ends_with("iso"),-ends_with(".xts")
    ) 

bl_fourier_tbl = bl_signature_tbl %>%
    tk_augment_fourier(Date,.periods = c(15,40,60,130), .K =2)

bl_fourier_tbl %>% glimpse()

bl_fourier_seasonality_formula = as.formula(
    value ~ splines::ns(index.num, knots = quantile(index.num, probs = c(.33,.66))) 
    + .
    + (as.factor(mday7) * wday.lbl)
    + (as.factor(week) * mday)
)

bl_fourier_tbl %>%
    plot_time_series_regression(
        Date,
        .formula = bl_fourier_seasonality_formula,
        .show_summary = T
    )

#Linear Regression
best_lm_model = lm(bl_fourier_seasonality_formula, data = bl_fourier_tbl)
#in theory save this but Im excited for actual modeling



# Modeling ----------------------------------------------------------------

unique_card_bl <- unique_card %>% select(Date,MKT) %>% pivot_longer(-Date) %>% select(-name) 


bl_prepared_tbl = unique_card_bl %>% summarise_by_time(Date, .by = "day",value = sum(value)
) %>% pad_by_time(.pad_value = 0
) %>% mutate(bl_trans = log_interval_vec(value,limit_lower = 0, offset = 1)
) %>% mutate(bl_trans = standardize_vec(bl_trans)
) %>% mutate(bl_trans_clean = ts_clean_vec(bl_trans, period = 7)
) %>% select(-value, -bl_trans
) %>% pivot_longer(contains("trans")
) %>% select(-name)

lower_limit = 0
upper_limit = 9.646
offset = 1
std_mean = -0.0226959514803494
standard_deviation = 0.69247676649603

horizon = 30
lag_period = 30
rolling_periods = c(3,7,14,21,28)

bl_prepared_tbl %>%
    #tk_augment_lags(value,.lags = lag_period) %>%
    tk_augment_slidify(.value = value,
                       .f = median, 
                       .period = rolling_periods)

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
                        .partial = T)


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
    step_fourier(Date, period = c(15,40,60,130), K =2)

recipe_spec_base %>% prep() %>% juice() %>% glimpse()


# Spline Model ------------------------------------------------------------
# lm
model_spec_lm = linear_reg() %>% set_engine("lm")
# spline model    

recipe_spec_base %>% prep() %>% juice() %>% glimpse()

recipe_spec_base_1 = recipe_spec_base %>% step_rm(Date,contains("_lag")) %>% step_ns(contains("index.num"),deg_free = 2) 

recipe_spec_base_1 %>% prep() %>% juice() %>% glimpse()

#spline workflow
workflow_fit_lm_1_spline = workflow() %>%
    add_model(model_spec_lm) %>%
    add_recipe(recipe_spec_base_1) %>%
    fit(training(splits))

workflow_fit_lm_1_spline %>% pull_workflow_fit() %>% pluck("fit") %>% summary()
# Modeltime workflow ------------------------------------------------------

calibration_tbl = modeltime_table(workflow_fit_lm_1_spline) %>%
    modeltime_calibrate(new_data = testing(splits))

calibration_tbl %>% modeltime_forecast(new_data    = testing(splits),
                                       actual_data = data_prepared_tbl) %>% plot_modeltime_forecast()

calibration_tbl %>% modeltime_accuracy()

# * Lag Recipe 
recipe_spec_base %>% prep() %>% juice() %>% glimpse()

recipe_spec_base_2 = recipe_spec_base %>% step_rm(Date) %>% step_naomit(starts_with("value_lag"))

recipe_spec_base_2 %>% prep() %>% juice() %>% glimpse()

workflow_fit_lm_2_lag = workflow() %>%
    add_model(model_spec_lm) %>%
    add_recipe(recipe_spec_base_2) %>%
    fit(training(splits))

workflow_fit_lm_2_lag %>% pull_workflow_fit() %>% pluck("fit") %>% summary()

calibration_tbl_2 = modeltime_table(workflow_fit_lm_2_lag) %>%
    modeltime_calibrate(new_data = testing(splits))

calibration_tbl_2 %>% modeltime_forecast(new_data    = testing(splits),
                                       actual_data = data_prepared_tbl) %>% plot_modeltime_forecast()

calibration_tbl_2 %>% modeltime_accuracy()

# Compare workflow --------------------------------------------------------

calibration_tbl_compare = modeltime_table(
    workflow_fit_lm_1_spline,
    workflow_fit_lm_2_lag
) %>%
    modeltime_calibrate(new_data    = testing(splits))

calibration_tbl_compare %>% modeltime_forecast(new_data= testing(splits),
                                               actual_data = data_prepared_tbl)%>% plot_modeltime_forecast()

calibration_tbl_compare %>% modeltime_accuracy()

refit_tbl = calibration_tbl_compare %>% modeltime_refit(data = data_prepared_tbl)

refit_tbl %>% modeltime_forecast(new_data= forecast_tbl,
                                 actual_data = data_prepared_tbl)%>% 
    #Inversion
    mutate(across(.value:.conf_hi, .fns = ~standardize_inv_vec(x=.,
                                                               mean = mean,
                                                               sd = standard_deviation))) %>%
    mutate(across(.value:.conf_hi,.fns = ~log_interval_inv_vec(
        x=.,
        limit_lower = lower_limit,
        limit_upper = upper_limit,
        offset = offset
    ))) %>%
    
    plot_modeltime_forecast()

refit_tbl %>% modeltime_accuracy()
