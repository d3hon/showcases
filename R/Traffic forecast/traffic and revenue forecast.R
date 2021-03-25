#monthly forecst
require("forecast")
require("tseries")
require('dlm')
require(lubridate)

########get all data
traffic = read.csv('traffic.csv')


period = 12
last_month = month(as.Date(Sys.time()) - 30)
current_year = ifelse(last_month == 12, year(as.Date(Sys.time()) - 30), year(Sys.time()))


###########testing various arima, minimizing one-step ahead forecast error, as it is the most important forecast
########### for 6 most recent months
arima_rmse = function(timeseries, p,d,q,sp,sd,sq, drift) {
  error = data.frame()
  k = 2*tsp(timeseries)[3] + 1 
  n = length(timeseries) 
  st = tsp(timeseries)[1]+(k-2)/tsp(timeseries)[3]
  test_period_max = length(window(timeseries, start=st + 2/tsp(timeseries)[3]))
  for (i in (n-k-5): (n-k)){    
    train = window(timeseries, end=st + i/tsp(timeseries)[3])
    test = window(timeseries, start=st + (i+1)/tsp(timeseries)[3], end = st + (i+1)/tsp(timeseries)[3])
    horizon_month = length(test) 
    prediction = try(forecast(
      Arima(train, order = c(p,d,q), seasonal = list(order = c(sp,sd,sq), period), include.drift = drift), 
      horizon_month, level = 95), TRUE)
    if (class(prediction) == 'try-error') {
      rmse_arima = NA } else {
      rmse_arima = accuracy(prediction, test)[2,2]
    }
    error = rbind(error, data.frame(rmse = rmse_arima))    
  }
  rmse_mean = mean(subset(error, !(is.na(rmse)))$rmse)
  rmse_median = quantile(subset(error, !(is.na(rmse)))$rmse, 0.5)
  test_period = nrow(subset(error, !(is.na(rmse))))
  out = data.frame(p = p,d = d,q = q,sp = sp,sd = sd,sq= sq, drift = drift, rmse_mean = rmse_mean, 
                   rmse_median = rmse_median, test_period = test_period, test_period_max = test_period_max)
  return(out)
}

##### grid search
arima_rmse_values = function(timeseries, is_seasonal){
  out = data.frame()
  param = expand.grid(p = c(0,1), d = c(0,1),q = c(0,1), sp = c(0,1), sd = c(0,1),sq = c(0,1), drift = c(0,1))
  param = subset(param, !(d == 0 & sd == 0 & drift == 1))
  param = subset(param, !(d == 1 & sd == 1 & drift == 1)) 
  param = subset(param, !(d+p+q+sp+sd+sq == 0)) 
  if (is_seasonal == T) {param = subset(param, !(sp+sd+sq == 0)) }
  for (i in 1:nrow(param)){
    print(param[i,])
    out = rbind(out, arima_rmse(timeseries, p = param$p[i] ,d = param$d[i],q = param$q[i],
                                sp = param$sp[i], sd = param$sd[i],sq = param$sq[i], drift = param$drift[i]))
  }
  out= out[order(out$rmse_mean),]
  out = subset(out, test_period == out$test_period_max[1])  #to exclude cases where it was not possible to run Arima for several times
  out= out[,1:(ncol(out)-2)]
  return(out)
}

########testing various ets, minimizing two-steps ahead forecast error
ets_rmse = function(timeseries, error,trend,seasonality) {
  error_df = data.frame()
  if (trend == 'Ad') {
    trend1 = 'A'
    damped = 1
  } else if (trend == 'Md') {
    trend1 = 'M'
    damped = 1
  } else {damped = 0
          trend1= trend}
  
  k = 2*tsp(timeseries)[3] + 1 #minimum length of training data
  n = length(timeseries) #length of data set
  st = tsp(timeseries)[1]+(k-2)/tsp(timeseries)[3]
  test_period_max = length(window(timeseries, start=st + 2/tsp(timeseries)[3]))
  for (i in 1:(n-k)){
    train = window(timeseries, end=st + i/tsp(timeseries)[3])
    test = window(timeseries, start=st + (i+1)/tsp(timeseries)[3], end=st + (i+1)/tsp(timeseries)[3])
    horizon_month = length(test) 
      
    prediction = try(forecast(
      ets(train, model = paste(paste(error,trend1, sep = ''),seasonality, sep = ''),damped = damped), 
      horizon_month, level = 95), TRUE)
    if (class(prediction) == 'try-error') {
      rmse_ets = NA     
    } else {
      rmse_ets = accuracy(prediction, test)[2,2]
    }
    error_df = rbind(error_df, data.frame(rmse = rmse_ets))    
    
  }
  rmse_mean = mean(subset(error_df, !(is.na(rmse)))$rmse)
  rmse_median = quantile(subset(error_df, !(is.na(rmse)))$rmse, 0.5)
  test_period = nrow(subset(error_df, !(is.na(rmse))))
  out = data.frame(error = error,trend = trend1, seasonality = seasonality, damped = damped, rmse_mean = rmse_mean, 
                   rmse_median = rmse_median, test_period = test_period, test_period_max = test_period_max)
  return(out)
}

ets_rmse_values = function(timeseries, is_seasonal){
  out = data.frame()
  param = expand.grid(error = c('A', 'M'), trend = c('N', 'A', 'Ad', 'M', 'Md'), seasonality = c('N', 'A', 'M'))
  if (is_seasonal == T) {param = subset(param, seasonality != 'N')}
  for (i in 1:nrow(param)){
    print(param[i,])
    out = rbind(out, ets_rmse(timeseries, error = param$error[i] ,trend = param$trend[i],seasonality = param$seasonality[i]))
  }
  out = out[order(out$rmse_mean),]
  out = subset(out, test_period == out$test_period_max[1])
  out= out[,1:(ncol(out)-2)]
  return(out)
}

stl_rmse = function(timeseries){
  error = data.frame()
  k = 2*tsp(timeseries)[3] + 1 #minimum length of training data
  n = length(timeseries) #length of data set
  st = tsp(timeseries)[1]+(k-2)/tsp(timeseries)[3]
  #to define how many times when forecast function didn't work
  test_period_max = length(window(timeseries, start=st + 2/tsp(timeseries)[3]))
  for (i in 1:(n-k)){
    train = window(timeseries, end=st + i/tsp(timeseries)[3])
    test = window(timeseries, start=st + (i+1)/tsp(timeseries)[3], end=st + (i+1)/tsp(timeseries)[3])
    horizon_month = length(test) 
    
    prediction = try(forecast(stl(train, s.window="periodic", robust=TRUE), h=horizon_month), TRUE)
    if (class(prediction) == 'try-error') {
      rmse_ets = NA     
    } else {
      rmse_ets = accuracy(prediction, test)[2,2]
    }
    error = rbind(error, data.frame(rmse = rmse_ets))    
    
  }
  rmse_mean = mean(subset(error, !(is.na(rmse)))$rmse)
  rmse_median = quantile(subset(error, !(is.na(rmse)))$rmse, 0.5)
  test_period = nrow(subset(error, !(is.na(rmse))))
  out = data.frame(rmse_mean = rmse_mean, 
                   rmse_median = rmse_median, test_period = test_period, test_period_max = test_period_max)
  return(out)
}




########converting data to ts format
traffic.ts <- ts(traffic$visits, frequency=12, start=c(2013,1), end = c(current_year, last_month))
#choosing timeseires to build forecast 
timeseries = traffic.ts
# specifying if it should be seasonal, optional, basically everything is seasonal
is_seasonal = T

arima_model_error = arima_rmse_values(timeseries, is_seasonal) 
ets_model_error = ets_rmse_values(timeseries, is_seasonal) 
stl_model_error = stl_rmse(timeseries)


#12 month ahead forecast
horizon_forward = 12
fit_ets1_full = ets(timeseries,  model = paste(paste(ets_model_error$error[1],ets_model_error$trend[1], sep = ''),ets_model_error$seasonality[1], sep = ''),damped = as.logical(ets_model_error$damped[1]))
fit_ets2_full =  ets(timeseries,  model = paste(paste(ets_model_error$error[2],ets_model_error$trend[2], sep = ''),ets_model_error$seasonality[2], sep = ''),damped = as.logical(ets_model_error$damped[2]))
fcast_ets1_full = forecast(fit_ets1_full, h=horizon_forward)
fcast_ets2_full = forecast(fit_ets2_full, h=horizon_forward)
fit_arima1_full = Arima(timeseries, order=c(arima_model_error$p[1],arima_model_error$d[1],arima_model_error$q[1]), seasonal=list(order=c(arima_model_error$sp[1],
                                                                                    arima_model_error$sd[1],arima_model_error$sq[1]), period=12),   include.drift=as.logical(arima_model_error$drift[1]))
fit_arima2_full = Arima(timeseries, order=c(arima_model_error$p[2],arima_model_error$d[2],arima_model_error$q[2]), seasonal=list(order=c(arima_model_error$sp[2],
                                                                                    arima_model_error$sd[2],arima_model_error$sq[2]), period=12),   include.drift=as.logical(arima_model_error$drift[2]))

fcast_arima1_full = forecast(fit_arima1_full, h=horizon_forward)
fcast_arima2_full = forecast(fit_arima2_full, h=horizon_forward) 
          
fit_stl_full = stl(timeseries, s.window="periodic", robust=TRUE)
fcast_stl_full = forecast(fit_stl_full, h=horizon_forward) 
              
          
model_list =  cbind(rbind(
      data.frame(type = 'ets1',onestep_mae = ets_model_error$rmse_mean[1]),
      data.frame(type = 'ets2',onestep_mae = ets_model_error$rmse_mean[2]),
      data.frame(type = 'stl',onestep_mae = stl_model_error$rmse_mean[1]), 
      data.frame(type = 'arima1',onestep_mae = arima_model_error$rmse_mean[1]),
      data.frame(type = 'arima2',onestep_mae = arima_model_error$rmse_mean[2])), 
      as.data.frame(rbind(fcast_ets1_full$mean, fcast_ets2_full$mean, #fcast_autoets$mean,
                fcast_stl_full$mean, fcast_arima1_full$mean, fcast_arima2_full$mean
                )))
          

model_list = cbind(model_list, data.frame(p = '', d = '', q = '', sp = '', sd = '', sq = '', drift = '', ETS_model = '', stringsAsFactors = F))  
model_list[model_list$type == 'ets1',]$ETS_model = paste(paste(paste(ets_model_error$error[1],ets_model_error$trend[1], sep = ''),ets_model_error$seasonality[1], sep = ''), 
                                                                                          ifelse(ets_model_error$damped[1] == 1, 'damped trend', 'not damped trend'))
model_list[model_list$type == 'ets2',]$ETS_model = paste(paste(paste(ets_model_error$error[2],ets_model_error$trend[2], sep = ''),ets_model_error$seasonality[2], sep = ''), 
                                                         ifelse(ets_model_error$damped[2] == 1, 'damped trend', 'not damped trend'))
model_list[model_list$type == 'arima1',][,(horizon_forward+3):(horizon_forward+9)]  = arima_model_error[,1:7][1,]
model_list[model_list$type == 'arima2',][,(horizon_forward+3):(horizon_forward+9)]  = arima_model_error[,1:7][2,]
model_list = model_list[order(model_list$onestep_mae),]
model_list
