# devtools::install_github("tidymodels/tune")
# devtools::install_github("tidymodels/recipes", force = T, INSTALL_opts = '--no-lock')
# devtools::install_github("tidymodels/workflows")
# devtools::install_github("tidymodels/parsnip")
# 
# # Modeltime & Timetk Development Versions
# # ----------------------------------------
# devtools::install_github("business-science/modeltime")
# devtools::install_github("business-science/timetk")
pacman::p_load(tidyverse,timetk,lubridate,bigrquery,modeltime,recipes,rsample,kernlab,glmnet,kknn,earth,tidymodels,rules,doFuture,future,tune,plotly)

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
raw_query %>% filter(grepl("Doubling Season",Key))

the_graph_title = "Doubling SeasonBattlebondM"

unique_card = raw_query[which(raw_query$Key == the_graph_title),] %>% mutate(Arb = ifelse(is.na(Arb), (MKT*(-1)),Arb ),
                                                                             BL = ifelse(is.na(BL), (0), BL ),
                                                                             BL_QTY = ifelse(is.na(BL_QTY), (0), BL_QTY ))

statement <- paste(
    'SELECT DISTINCT rdate, card, count(*) added_printings FROM `gaeas-cradle.roster.mtgjson`a WHERE card like "',unique(unique_card$Card),'" GROUP BY 1,2  ORDER BY rdate desc ',
    sep = ""
)
printings_xreg = dbSendQuery(con, statement = statement) %>% dbFetch(., n = -1) %>% distinct() %>% mutate(rdate = ymd(rdate))


# modeltime ---------------------------------------------------------------

unique_card_bl <- unique_card %>% select(Date,BL) %>% pivot_longer(-Date) %>% select(-name) %>% na.omit()

unique_card_bl %>% plot_time_series(Date,value)

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
upper_limit = 73.6
offset = 1
std_mean = -0.689071324948861
standard_deviation = 1.07334670400685

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


#best_lm_model$terms %>% formula()

recipe_spec_base = recipe(value ~ ., data = training(splits)) %>%
    #Time series signature
    step_timeseries_signature(Date) %>%
    step_rm(matches("(iso)|(hour)|(minute)|(second)|(am.pm)|(xts)|(year)")) %>%
    #stnadardization
    step_normalize(matches("(index.num)|(yday)")) %>%
    #dummy Encoding
    step_dummy(all_nominal(),one_hot = T) %>%
    step_interact(~matches("mday7") * matches("wday.lbl")) %>%
    step_fourier(Date, period = c(4,10,20,40), K =2)

# recipe_spec_base %>% prep() %>% juice() %>% glimpse() 
recipe_spec_base_spline = recipe_spec_base %>% step_rm(Date,contains("_lag")) %>% step_ns(contains("index.num"),deg_free = 4)
recipe_spec_base_lag = recipe_spec_base %>% step_rm(Date) %>% step_naomit(starts_with("value_lag"))
recipe_spec_base_dated_lag = recipe_spec_base  %>% step_naomit(starts_with("value_lag"))
# Models ------------------------------------------------------------------
# * 1
model_spec_lm = linear_reg(mode = "regression",
                           penalty = 50, 
                           mixture = .5) %>% 
    set_engine("lm")

workflow_fit_lm_spline = workflow() %>% add_model(model_spec_lm) %>% add_recipe(recipe_spec_base_spline) %>% fit(training(splits))


# * 2
model_spec_arima = arima_boost(mode = "regression") %>% 
    set_engine("auto_arima_xgboost")

workflow_fit_arima_lag = workflow() %>% add_model(model_spec_arima) %>% add_recipe(recipe_spec_base_dated_lag) %>% fit(training(splits))

# * 3
model_spec_prophet = prophet_boost(mode="regression", 
                                   min_n              = 7, 
                                   tree_depth         = 10, 
                                   trees              = 30,
                                   changepoint_num    = 10, 
                                   changepoint_range  = .95, 
                                   learn_rate         = .01,
                                   seasonality_yearly = F,
                                   seasonality_weekly = F,
                                   seasonality_daily  = F) %>% 
    set_engine("prophet_xgboost") 

set.seed(253)
workflow_fit_prophet_lag = workflow() %>% add_model(model_spec_prophet) %>% add_recipe(recipe_spec_base_dated_lag) %>%fit(training(splits))

# * 4
model_spec_glmnet = linear_reg(penalty = 0.75,
                               mixture   = 0.15) %>% 
    set_engine("glmnet")

workflow_fit_glmnet_spline = workflow() %>% add_model(model_spec_glmnet) %>% add_recipe(recipe_spec_base_spline) %>% fit(training(splits))

# * 5
model_spec_mars = mars(mode = "regression", 
                       num_terms = round(horizon * .33,0)) %>% 
    set_engine("earth")

workflow_fit_mars_spline = workflow() %>% add_model(model_spec_mars) %>% add_recipe(recipe_spec_base_spline) %>% fit(training(splits))

# * 6
model_spec_svm = svm_poly(mode = "regression", 
                          cost = 25) %>% 
    set_engine("kernlab")

set.seed(253)

workflow_fit_svm_spline = workflow() %>% add_model(model_spec_svm) %>% add_recipe(recipe_spec_base_spline) %>% fit(training(splits))

# * NNETAR
model_spec_nnetar = nnetar_reg() %>% set_engine("nnetar")

workflow_fit_nnetar = workflow() %>% add_model(model_spec_nnetar)%>% add_recipe(recipe_spec_base) %>% fit(training(splits)%>% drop_na())

# Refit, Calibrate, & Plot ------------------------------------------------

calibrate_and_plot = function(..., type = "testing"){
    if( type == "testing"){
        new_data = testing(splits)
    }else {
        new_data = training(splits) %>% drop_na()
    }
    
    calibration_tbl = modeltime_table(...) %>%
        modeltime_calibrate(new_data)
    
    print(calibration_tbl %>% modeltime_accuracy() %>% mutate(smape = round(smape/2,2) ) %>% drop_na())
    
    chosen = calibration_tbl %>% modeltime_accuracy() %>% filter(mae <= min(mae)*2) %>% select(.model_desc)
    
    calibration_tbl %>% filter(.model_desc == chosen$.model_desc[1] | .model_desc == chosen$.model_desc[2]) %>% 
        modeltime_forecast(new_data= forecast_tbl,actual_data = data_prepared_tbl)%>% 
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
        
        plot_modeltime_forecast(.conf_interval_show = F, .title = the_graph_title, .interactive = F)
    
}


suppressWarnings(calibrate_and_plot(workflow_fit_lm_spline,
                                    workflow_fit_arima_lag,
                                    workflow_fit_prophet_lag,
                                    workflow_fit_glmnet_spline,
                                    workflow_fit_mars_spline,
                                    workflow_fit_svm_spline,
                                    workflow_fit_nnetar
))
combined_model_times = modeltime_table(
    workflow_fit_lm_spline,
    workflow_fit_arima_lag,
    workflow_fit_prophet_lag,
    workflow_fit_glmnet_spline,
    workflow_fit_mars_spline,
    workflow_fit_svm_spline,
    workflow_fit_nnetar
)


calibration_tbl = suppressWarnings(combined_model_times %>% modeltime_calibrate(testing(splits)))

# Sequential Cross Validation ---------------------------------------------

wflw_fit_nnetar = calibration_tbl %>% pluck_modeltime_model(7)

resamples_tscv_lag = time_series_cv(data       = training(splits) %>% drop_na(),
                                    cumulative  = T,
                                    assess      = "1 month",
                                    skip        = "15 days",
                                    slice_limit = 6)

# resamples_tscv_lag %>% tk_time_series_cv_plan() %>% plot_time_series_cv_plan(.date_var = Date,value)

recipe_spec_cv_lag = wflw_fit_nnetar %>% pull_workflow_preprocessor() %>% step_naomit(starts_with("value_lag"))

model_spec_nnetar_tune = nnetar_reg(
    non_seasonal_ar = tune(),
    seasonal_ar = tune(),
    hidden_units = tune(),
    num_networks = 5,
    penalty = tune(),
    epochs = tune()
) %>% set_engine("nnetar")

#parameters(model_spec_nnetar_tune)

set.seed(253)
grid_spec_nnetar_1 = grid_latin_hypercube(parameters(model_spec_nnetar_tune),size = 10)

wflw_nnetar_tune = wflw_fit_nnetar %>% update_recipe(recipe_spec_cv_lag) %>% update_model(model_spec_nnetar_tune)

set.seed(253)

#parallel::detectCores()

tune_results_nnetar_1 = wflw_nnetar_tune %>% tune_grid(resamples = resamples_tscv_lag,
                                                       grid      = grid_spec_nnetar_1,
                                                       metrics   = default_forecast_accuracy_metric_set(),
                                                       control   = control_grid(verbose = T, save_pred = T))

first_round = tune_results_nnetar_1 %>% show_best(metric = "rmse") %>% slice(1:3)

set.seed(253)
grid_spec_nnetar_2 = grid_latin_hypercube(
    non_seasonal_ar(range = c(first_round$non_seasonal_ar[1],first_round$non_seasonal_ar[3])),
    seasonal_ar(range = c(first_round$seasonal_ar[1],first_round$seasonal_ar[3])),
    hidden_units(range = c(first_round$hidden_units[1],first_round$hidden_units[3])),
    penalty(range = c(log10(first_round$penalty[1]),log10(first_round$penalty[3]))),
    epochs(range = c(first_round$epochs[1],first_round$epochs[3])),
    size = 10)


set.seed(253)
tune_results_nnetar_2 = wflw_nnetar_tune %>% tune_grid(resamples = resamples_tscv_lag,
                                                       grid      = grid_spec_nnetar_2,
                                                       metrics   = default_forecast_accuracy_metric_set(),
                                                       control   = control_grid(verbose = T, save_pred = T))



set.seed(253)
wflw_nnetar_tune_fitted = wflw_nnetar_tune            %>% 
    finalize_workflow(tune_results_nnetar_2        %>% 
                          show_best(metric = "rmse") %>% 
                          slice(1))                       %>%
    fit(training(splits))
#Calibrate ARIMA

wflw_fit_arima = calibration_tbl %>% pluck_modeltime_model(2)

resamples_tscv_lag = time_series_cv(data       = training(splits) %>% drop_na(),
                                    cumulative  = T,
                                    assess      = "1 month",
                                    skip        = "15 days",
                                    slice_limit = 6)

# resamples_tscv_lag %>% tk_time_series_cv_plan() %>% plot_time_series_cv_plan(.date_var = Date,value)

recipe_spec_cv_lag = wflw_fit_arima %>% pull_workflow_preprocessor() %>% step_naomit(starts_with("value_lag"))

model_spec_arima_tune = arima_boost(
    mode = "regression",
    seasonal_period = tune(),
    non_seasonal_ar = tune(),
    non_seasonal_differences = tune(),
    non_seasonal_ma = tune(),
    seasonal_ar = tune(),
    seasonal_differences = tune(),
    seasonal_ma = tune()
) %>% set_engine("auto_arima_xgboost")

#parameters(model_spec_nnetar_tune)

set.seed(253)
grid_spec_arima_1 = grid_latin_hypercube(parameters(model_spec_arima_tune),size = 10)

wflw_arima_tune = wflw_fit_arima %>% update_recipe(recipe_spec_cv_lag) %>% update_model(model_spec_arima_tune)

set.seed(253)
tune_results_arima_1 = wflw_arima_tune %>% tune_grid(resamples = resamples_tscv_lag,
                                                     grid      = grid_spec_arima_1,
                                                     metrics   = default_forecast_accuracy_metric_set(),
                                                     control   = control_grid(verbose = T, save_pred = T))

first_round = tune_results_arima_1 %>% show_best(metric = "rmse") %>% slice(1:3)

set.seed(253)
grid_spec_arima_2 = grid_latin_hypercube(
    seasonal_period("daily"),
    non_seasonal_ar(range = c(first_round$non_seasonal_ar[1],first_round$non_seasonal_ar[3])),
    non_seasonal_differences(range = c(first_round$non_seasonal_differences[1],first_round$non_seasonal_differences[3])),
    non_seasonal_ma(range = c(first_round$non_seasonal_ma[1],first_round$non_seasonal_ma[3])),
    seasonal_ar(range = c(first_round$seasonal_ar[1],first_round$seasonal_ar[3])),
    seasonal_differences(range = c(first_round$seasonal_differences[1],first_round$seasonal_differences[3])),
    seasonal_ma(range = c(first_round$seasonal_ma[1],first_round$seasonal_ma[3])),
    size = 10) %>% rename("seasonal_period" = "period")


set.seed(253)
tune_results_arima_2 = wflw_arima_tune %>% tune_grid(resamples = resamples_tscv_lag,
                                                     grid      = grid_spec_arima_2,
                                                     metrics   = default_forecast_accuracy_metric_set(),
                                                     control   = control_grid(verbose = T, save_pred = T))



set.seed(253)
wflw_arima_tune_fitted = wflw_arima_tune            %>% 
    finalize_workflow(tune_results_arima_2        %>% 
                          show_best(metric = "rmse") %>% 
                          slice(1))                       %>%
    fit(training(splits))

# Non Sequential K-Folds --------------------------------------------------

# prophet -----------------------------------------------------------------

wflw_fit_prophet = calibration_tbl %>% pluck_modeltime_model(3)

set.seed(253)
resamples_kfold = vfold_cv(training(splits) %>% drop_na(),v = 10)

#resamples_kfold %>% tk_time_series_cv_plan() %>% plot_time_series_cv_plan(Date, value, .facet_ncol = 2)

wflw_fit_prophet %>% pull_workflow_spec()

model_spec_prophet_boost = prophet_boost(
    changepoint_num     = tune(),
    changepoint_range   = tune(),
    seasonality_yearly  = F,
    seasonality_weekly  = F,
    seasonality_daily   = F,
    mtry                = tune(),
    trees               = 150,
    min_n               = tune(),
    tree_depth          = tune(),
    learn_rate          = tune(),
    loss_reduction      = tune()
)%>% 
    set_engine("prophet_xgboost") 

set.seed(253)
grid_spec_prophet_1 = grid_latin_hypercube(
    parameters(model_spec_prophet_boost) %>%
        update(mtry = mtry(range = c(1,65))),
    size = 10
)

set.seed(253)
tune_results_prophet_1 = wflw_fit_prophet %>% 
    update_model(model_spec_prophet_boost) %>% 
    tune_grid(resamples = resamples_kfold,
              grid      = grid_spec_prophet_1,
              metrics   = default_forecast_accuracy_metric_set(),
              control   = control_grid(verbose = F, save_pred = T))

tune_results_prophet_1 %>% show_best(metric = "rmse", n = Inf)

first_round = tune_results_prophet_1 %>% show_best(metric = "rmse") %>% slice(1:3)

wflw_fit_prophet %>% pull_workflow_spec()

set.seed(253)
grid_spec_prophet_2 = grid_latin_hypercube(
    changepoint_num(range = c(first_round$changepoint_num[1],first_round$changepoint_num[3])),
    changepoint_range(range = c(first_round$changepoint_range[1],first_round$changepoint_range[3])),
    mtry(range = c(first_round$mtry[1],first_round$mtry[3])),
    min_n(range = c(first_round$min_n[1],first_round$min_n[3])),
    tree_depth(range = c(first_round$tree_depth[1],first_round$tree_depth[3])),
    learn_rate(range = c(first_round$learn_rate[1],first_round$learn_rate[3])),
    loss_reduction(range = c(first_round$loss_reduction[1],first_round$loss_reduction[3])),
    size = 10) 


set.seed(253)
tune_results_prophet_2 = wflw_fit_prophet  %>% 
    update_model(model_spec_prophet_boost)%>% tune_grid(resamples = resamples_kfold,
                                                        grid      = grid_spec_prophet_2,
                                                        metrics   = default_forecast_accuracy_metric_set(),
                                                        control   = control_grid(verbose = T, save_pred = T))

set.seed(253)
wflw_prophet_tune_fitted = wflw_fit_prophet  %>% 
    update_model(model_spec_prophet_boost)            %>% 
    finalize_workflow(tune_results_prophet_2        %>% 
                          show_best(metric = "rmse") %>% 
                          slice(1))                       %>%
    fit(training(splits))

# MARS --------------------------------------------------------------------
wflw_fit_mars = calibration_tbl %>% pluck_modeltime_model(5)

set.seed(253)
resamples_kfold = vfold_cv(training(splits) %>% drop_na(),v = 10)

resamples_kfold %>% tk_time_series_cv_plan() %>% plot_time_series_cv_plan(Date, value, .facet_ncol = 2)

wflw_fit_mars %>% pull_workflow_spec()

model_spec_mars = mars(
    mode     = "regression",
    num_terms = tune(),
    prod_degree = tune())%>% 
    set_engine("earth") 

set.seed(253)
grid_spec_mars_1 = grid_latin_hypercube(
    parameters(model_spec_mars),
    size = 10
)


set.seed(253)
registerDoFuture()

plan(strategy = cluster,
     workers  = parallel::makeCluster(6))

tune_results_mars_1 = wflw_fit_mars %>% 
    update_model(model_spec_mars) %>% 
    tune_grid(resamples = resamples_kfold,
              grid      = grid_spec_mars_1,
              metrics   = default_forecast_accuracy_metric_set(),
              control   = control_grid(verbose = F, save_pred = T))

tune_results_mars_1 %>% show_best(metric = "rmse", n = Inf)

first_round = tune_results_prophet_1 %>% show_best(metric = "rmse") %>% slice(1:3)

set.seed(253)
grid_spec_mars_2 = grid_latin_hypercube(
    num_terms(range = c(first_round$num_terms[1],first_round$num_terms[3])),
    prod_degree(range = c(first_round$prod_degree[1],first_round$prod_degree[3])),
    size = 10) 

registerDoFuture()
plan(strategy = cluster,
     workers  = parallel::makeCluster(6))

set.seed(253)
tune_results_mars_2 = wflw_fit_mars  %>% 
    update_model(model_spec_mars)%>% tune_grid(resamples = resamples_kfold,
                                               grid      = grid_spec_mars_2,
                                               metrics   = default_forecast_accuracy_metric_set(),
                                               control   = control_grid(verbose = T, save_pred = T))

set.seed(253)
wflw_mars_tune_fitted = wflw_fit_mars  %>% 
    update_model(model_spec_mars)            %>% 
    finalize_workflow(tune_results_mars_2        %>% 
                          show_best(metric = "rmse") %>% 
                          slice(1))                       %>%
    fit(training(splits))



calibrate_and_plot(wflw_nnetar_tune_fitted,
                   wflw_arima_tune_fitted,
                   wflw_prophet_tune_fitted,
                   wflw_mars_tune_fitted)
