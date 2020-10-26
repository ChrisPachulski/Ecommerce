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
raw_query %>% filter(grepl("Taigam, Ojutai Master",Key))
unique_card = raw_query[which(raw_query$Key == "Taigam, Ojutai MasterCommander 2017R"),] %>% mutate(Arb = ifelse(is.na(Arb), (MKT*(-1)),Arb ),
                                                                                                    BL = ifelse(is.na(BL), (0), BL ),
                                                                                                    BL_QTY = ifelse(is.na(BL_QTY), (0), BL_QTY ))


# modeltime ---------------------------------------------------------------

unique_card_bl <- unique_card %>% select(Date,Sellers) %>% pivot_longer(-Date) %>% select(-name) %>% na.omit()


bl_prepared_tbl = unique_card_bl %>% summarise_by_time(.date_var = Date, .by = "day",value = sum(value)
) %>% pad_by_time(.date_var = Date,.pad_value = 0
) %>% mutate(bl_trans = log_interval_vec(value,limit_lower = 0, offset = 1)
) %>% mutate(bl_trans = standardize_vec(bl_trans)
) %>% mutate(bl_trans_clean = ts_clean_vec(bl_trans, period = 7)
) %>% select(-value, -bl_trans
) %>% pivot_longer(contains("trans")
) %>% select(-name)

lower_limit = 0
upper_limit = 148.4
offset = 1
std_mean = -0.976317929294176
standard_deviation = 2.00433131043194

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
    step_fourier(Date, period = c(15,40,60), K =2)

recipe_spec_base %>% prep() %>% juice() %>% glimpse()


# Spline Model ------------------------------------------------------------
# lm
model_spec_lm = linear_reg() %>% set_engine("lm")
model_spec_rf = rand_forest(mode = "regression") %>% set_engine("randomForest")
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

# * random forest base

recipe_spec_base_3 = recipe_spec_base  %>% step_naomit(starts_with("value_lag"))

recipe_spec_base_3 %>% prep() %>% juice() %>% glimpse()

workflow_fit_rf_3_lag = workflow() %>%
    add_model(model_spec_rf) %>%
    add_recipe(recipe_spec_base_3) %>%
    fit(training(splits))

workflow_fit_rf_3_lag %>% pull_workflow_fit() %>% pluck("fit") %>% summary()

calibration_tbl_3 = modeltime_table(workflow_fit_rf_3_lag) %>%
    modeltime_calibrate(new_data = testing(splits))

calibration_tbl_3 %>% modeltime_forecast(new_data    = testing(splits),
                                         actual_data = data_prepared_tbl) %>% plot_modeltime_forecast()

calibration_tbl_3 %>% modeltime_accuracy()
# * Exp Smoothing
model_spec_exp_smooth = exp_smoothing(mode = "regression") %>% set_engine("ets")
recipe_spec_base_4 = recipe_spec_base  %>% step_naomit(starts_with("value_lag"))

recipe_spec_base_4 %>% prep() %>% juice() %>% glimpse()

workflow_fit_expsm_4_lag = workflow() %>%
    add_model(model_spec_exp_smooth) %>%
    add_recipe(recipe_spec_base_4) %>%
    fit(training(splits))

workflow_fit_expsm_4_lag %>% pull_workflow_fit() %>% pluck("fit") %>% summary()

calibration_tbl_4 = modeltime_table(workflow_fit_expsm_4_lag) %>%
    modeltime_calibrate(new_data = testing(splits))

calibration_tbl_4 %>% modeltime_forecast(new_data    = testing(splits),
                                         actual_data = data_prepared_tbl) %>% plot_modeltime_forecast()

calibration_tbl_4 %>% modeltime_accuracy()
# * arima base
model_spec_arima = arima_boost(mode = "regression") %>% set_engine("auto_arima_xgboost")
recipe_spec_base_5 = recipe_spec_base  %>% step_naomit(starts_with("value_lag"))

recipe_spec_base_5 %>% prep() %>% juice() %>% glimpse()

workflow_fit_ari_5_lag = workflow() %>%
    add_model(model_spec_arima) %>%
    add_recipe(recipe_spec_base_5) %>%
    fit(training(splits))

workflow_fit_ari_5_lag %>% pull_workflow_fit() %>% pluck("fit") %>% summary()

calibration_tbl_5 = modeltime_table(workflow_fit_ari_5_lag) %>%
    modeltime_calibrate(new_data = testing(splits))

calibration_tbl_5 %>% modeltime_forecast(new_data    = testing(splits),
                                         actual_data = data_prepared_tbl) %>% plot_modeltime_forecast()

calibration_tbl_5 %>% modeltime_accuracy()

# * arima base
model_spec_kknn = nearest_neighbor() %>% set_engine("kknn") 
recipe_spec_base_6 = recipe_spec_base  %>% step_naomit(starts_with("value_lag"))

recipe_spec_base_6 %>% prep() %>% juice() %>% glimpse()

workflow_fit_ffnn_6_lag = workflow() %>%
    add_model(model_spec_kknn) %>%
    add_recipe(recipe_spec_base_6) %>%
    fit(training(splits))

workflow_fit_ffnn_6_lag %>% pull_workflow_fit() %>% pluck("fit") %>% summary()

calibration_tbl_6 = modeltime_table(workflow_fit_ffnn_6_lag) %>%
    modeltime_calibrate(new_data = testing(splits))

calibration_tbl_6 %>% modeltime_forecast(new_data    = testing(splits),
                                         actual_data = data_prepared_tbl) %>% plot_modeltime_forecast()

calibration_tbl_6 %>% modeltime_accuracy()

# Compare workflow --------------------------------------------------------

calibration_tbl_compare = modeltime_table(
    workflow_fit_lm_1_spline,
    workflow_fit_lm_2_lag,
    workflow_fit_rf_3_lag,
    workflow_fit_expsm_4_lag,
    workflow_fit_ari_5_lag,
    workflow_fit_ffnn_6_lag
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

