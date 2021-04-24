library(USgas)
library(dplyr)
library(plotly)
library(TSstudio)

data("us_monthly")

str(us_monthly)

us.ts <- ts(us_monthly$y, start = c(2001, 1), frequency = 12)


ts_plot(us.ts,
        title = "US Monthly Demand for Natural Gas",
        Ytitle = "Million Cubic Feet",
        slider = TRUE)

ts_heatmap(us.ts)
ts_seasonal(us.ts, type = "all")
ts_cor(us.ts, lag.max = 36)


methods <- list(ets1 = list(method = "ets",
                            method_arg = list(opt.crit = "lik"),
                            notes = "ETS model opt.crit=lik"),
                ets2 = list(method = "ets",
                            method_arg = list(opt.crit = "amse"),
                            notes = "ETS model opt.crit=amse"),
                ets3 = list(method = "ets",
                            method_arg = list(opt.crit = "mse"),
                            notes = "ETS model opt.crit=mse"),
                auto_arima1 = list(method = "auto.arima",
                                   method_arg = list(ic = "aicc"),
                                   notes = "Auto ARIMA with ic = aicc"),
                auto_arima2 = list(method = "auto.arima",
                                   method_arg = list(ic = "aic"),
                                   notes = "Auto ARIMA"),
                hw1 = list(method = "HoltWinters",
                           method_arg = NULL,
                           notes = "HoltWinters Model"),
                hw2 = list(method = "HoltWinters",
                           method_arg = list(optim.start = c(alpha = 0.1, beta = 0.1, gamma = 0.1)),
                           notes = "HoltWinters with wide optim range"),
                tslm1 = list(method = "tslm",
                             method_arg = list(formula = input ~ trend + season),
                             notes = "tslm with trend and seasonal"),
                tslm2 = list(method = "tslm",
                             method_arg = list(formula = input ~ trend),
                             notes = "tslm with trend"))


train_method = list(partitions = 8,
                    sample.out = 12,
                    space = 3)

md <- train_model(input = ts.obj,
                  methods = methods,
                  train_method = train_method,
                  horizon = nrow(post_covid),
                  error = "MAPE")

plot_error(md)

plot_model(md) %>%
  layout(title = "Time Series Models Horse Racing with Backtesting",
         xaxis = list(range = c(2016, 2020.5)),
         yaxis = list(title = "Number of Passengers"),
         plot_bgcolor = "black",
         paper_bgcolor = "black",
         font = list(color = "white"),
         xaxis = list(zerolinecolor = "white"))
