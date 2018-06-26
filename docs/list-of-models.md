List of models
================

| Model          | Function                 | Basis\_function                              | Notes                             |
|:---------------|:-------------------------|:---------------------------------------------|:----------------------------------|
| auto ARIMA     | `auto_arima_forecast`    | `forecast::auto.arima`                       |                                   |
| constant mean  | `constan_mean_forecast`  | `forecast::Arima(c(0,0,0))`                  |                                   |
| ETS            | `ets_forecast`           | `forecast::ets()`                            | auto add/mult order, auto damped  |
| RW             | `rw_forecast`            | `forecast::rwf()`                            | flexible lambda                   |
| RW-DRIFT       | `rw_drift_forecast`      | `forecast::rwf(drift = TRUE)`                |                                   |
| seasonal RW    |                          |                                              |                                   |
| arithmetic RW  | `arithmetic_rw_forecast` | `forecast::Arima(c(0, 1, 0), lambda = NULL)` |                                   |
| geometric RW   | `geometric_rw_forecast`  | `forecast::Arima(c(0, 1, 0), lambda = 0)`    | Values &gt;0 only                 |
| NNAR           |                          |                                              |                                   |
| TBATS          |                          |                                              |                                   |
| STLM-AR        |                          |                                              |                                   |
| DS-ses         |                          |                                              | same as ets("ANN")                |
| DS-holt        |                          |                                              | same as ets("AAN")                |
| DS-holt-damped |                          |                                              | same as ets("AAN", damped = TRUE) |
| THETAF         |                          |                                              |                                   |
| M4\_Comp       |                          |                                              | M4 benchmark composite            |
| M4\_Meta       |                          |                                              | M4 meta learning composite        |

M4 competition
--------------

The benchmark and second place winner in the M4 competition were ensemble models that would not be too hard to integrate. And the benchmark is very simple, would not be difficult at all.

Competition benchmark: raw code at <https://github.com/M4Competition/M4-methods/blob/master/Benchmarks%20and%20Evaluation.R>, it is a combination f4, 5, and 6, which are plain exponential smoothing models on deseasoned data.

Hyndman et all have a M4 meta learning model, basically 9 plain time series models whose ensemble weights are determined by xgboost using a broad range of extra time series features extracted as features for the meta model. See here <https://github.com/robjhyndman/M4metalearning/blob/master/R/forec_methods_list.R> and here <https://github.com/M4Competition/M4-methods/blob/master/245%20-%20pmontman/M4-Method-Description.pdf>.

The 3 and 9 component models for the benchmark and M4\_meta model are already included in the list above.
