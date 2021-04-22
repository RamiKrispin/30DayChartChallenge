library(sfo)
library(dplyr)
library(plotly)
library(TSstudio)

data("sfo_passengers")

str(sfo_passengers)


df <- sfo_passengers %>%
  mutate(date = as.Date(paste(substr(sfo_passengers$activity_period, 1,4),
                              substr(sfo_passengers$activity_period, 5,6),
                              "01", sep ="/")))

df <- df %>%
  group_by(date) %>%
  summarise(y = sum(passenger_count), .groups = "drop")

head(df)


plot_ly(data = df,
        x = ~ date,
        y = ~ y,
        type = "scatter",
        mode = "line",
        name = "Total Passengers") %>%
  add_segments(x = as.Date("2020-02-01"),
               xend = as.Date("2020-02-01"),
               y = min(df$y),
               yend = max(df$y) * 1.05,
               line = list(color = "black", dash = "dash"),
               showlegend = FALSE) %>%
  add_annotations(text = "Pre-Covid19",
                  x = as.Date("2018-09-01"),
                  y = max(df$y) * 1.05,
                  showarrow = FALSE) %>%
  add_annotations(text = "Post-Covid19",
                  x = as.Date("2021-08-01"),
                  y = max(df$y) * 1.05,
                  showarrow = FALSE) %>%
  layout(title = "Total Number of Air Passengers - SFO Airport",
         yaxis = list(title = "Number of Passengers"),
         xaxis = list(title = "Source: San Francisco Open Data Portal"))

pre_covid <- df %>%
  dplyr::filter(date < as.Date("2020-03-01")) %>%
  dplyr::arrange(date)

post_covid <- df %>%
  dplyr::filter(date >= as.Date("2020-03-01")) %>%
  dplyr::arrange(date)

ts.obj <- ts(pre_covid$y, start = c(2005, 7), frequency = 12)

ts_plot(ts.obj,
        title = "Total Number of Air Passengers - SFO Airport",
        Ytitle = "Number of Passengers",
        slider = TRUE)

ts_seasonal(ts.obj = ts.obj, type = "all")

ts_cor(ts.obj = ts.obj, lag.max = 36)


methods <- list(ets1 = list(method = "ets",
                            method_arg = list(opt.crit = "lik"),
                            notes = "ETS model opt.crit=lik"),
                ets2 = list(method = "ets",
                            method_arg = list(opt.crit = "amse"),
                            notes = "ETS model opt.crit=amse"),
                ets3 = list(method = "ets",
                            method_arg = list(opt.crit = "mse"),
                            notes = "ETS model opt.crit=mse"),
                auto_arima = list(method = "auto.arima",
                                  notes = "Auto ARIMA"),
                hw1 = list(method = "HoltWinters",
                           method_arg = NULL,
                           notes = "HoltWinters Model"),
                hw2 = list(method = "HoltWinters",
                           method_arg = list(seasonal = "multiplicative"),
                           notes = "HW with multip. seasonality"),
                tslm = list(method = "tslm",
                            method_arg = list(formula = input ~ trend + season),
                            notes = "tslm with trend and seasonal"))


train_method = list(partitions = 6,
                    sample.out = 12,
                    space = 3)

md <- train_model(input = ts.obj,
                  methods = methods,
                  train_method = train_method,
                  horizon = nrow(post_covid),
                  error = "MAPE")

plot_error(md)

plot_model(md)

post_covid$yhat <- as.numeric(md$forecast$hw1$forecast$mean)
post_covid$upper95 <- as.numeric(md$forecast$hw1$forecast$upper[,2])
post_covid$lower95 <- as.numeric(md$forecast$hw1$forecast$lower[,2])


post_covid$passengers_loss <- post_covid$y - post_covid$yhat

post_covid
sum(post_covid$passengers_loss)
plot_ly() %>%
  add_ribbons(x = post_covid$date,
              ymin = post_covid$y,
              ymax = post_covid$yhat,
              line = list(color = 'rgba(255, 0, 0, 0.05)'),
              fillcolor = 'rgba(255, 0, 0, 0.6)',
              name = "Estimated Loss") %>%
  add_segments(x = as.Date("2020-02-01"),
               xend = as.Date("2020-02-01"),
               y = min(df$y),
               yend = max(df$y) * 1.05,
               line = list(color = "black", dash = "dash"),
               showlegend = FALSE) %>%
  add_annotations(text = "Pre-Covid19",
                  font = list(family = "Ariel",
                              size = 20),
                  x = as.Date("2017-09-01"),
                  y = max(df$y) * 1.05,
                  showarrow = FALSE) %>%
  add_annotations(text = "Post-Covid19",
                  font = list(family = "Ariel",
                              size = 20),
                  x = as.Date("2020-09-01"),
                  y = max(df$y) * 1.05,
                  showarrow = FALSE) %>%
  add_annotations(text = paste("Estimated decrease in", "<br>",
                               "passengers volume: ~41M",
                               sep = ""),
                  font = list(family = "Ariel",
                              size = 20),
                  x = as.Date("2020-04-01"),
                  y = 2 * 10 ^ 6,
                  arrowhead = 1,
                  ax = -180,
                  ay = -40,
                  showarrow = TRUE) %>%
  add_lines(x = df$date,
            y = df$y,
            line = list(color = "#1f77b4"),
            name = "Actual") %>%
  layout(title = "Covid19 Impact on SFO Air Passenger Traffic",
         yaxis = list(title = "Number of Passengers"),
         xaxis = list(title = "",
                      range = c(as.Date("2015-01-01"), as.Date("2021-01-01"))),
         font = list(size = 18,
                     family = "Ariel"),
         legend = list(x = 0.05, y = 0.97),
         margin = list(t = 50, b = 120, l = 80, r = 30)) %>%
  add_annotations(text = paste("Time Series Model - Holt-Winters",
                    paste("#30DayChartChallenge Day 21",
                                     "Viz: Rami Krispin",
                                     "Data: San Francisco Open Data",
                                     sep = " | "),
                    sep = "<br>"),
                  font = list(size = 18,
                              family = "Ariel"),
                  align = "left",
                  x = -0.03,
                  y = -0.15,
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE)
