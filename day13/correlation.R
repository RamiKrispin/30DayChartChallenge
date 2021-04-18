# libraries
library(USgas)
library(plotly)
library(dplyr)


hex_to_rgb <- function(hex){
  rgb <- paste0(as.numeric(grDevices::col2rgb(hex) %>%
                             base::t()), collapse = ",")
  return(rgb)
}

# Data
data("us_monthly")

head(us_monthly)


df <- us_monthly

# Calculate the ACF
max.lag <- 72
ci <- 0.95
na.rm <- FALSE

s <- NULL
y_mean <- base::mean(df$y, na.rm = na.rm)

for(k in 0:max.lag){
  s <- c(s,
         base::sum((df$y[1:(base::nrow(df) - k)] - y_mean) *
                     (df$y[(1 + k):(base::nrow(df))] - y_mean), na.rm = na.rm) /
           base::sum((df$y - y_mean)^2, na.rm = na.rm))


}


ci_value <- stats::qnorm((1 + ci)/2)/sqrt(base::nrow(df))

acf <-  base::data.frame(lag = 0:max.lag ,
                         acf = s,
                         ci_lower = - ci_value,
                         ci_upper = ci_value)

acf$seasonal <- ifelse(acf$lag %% 12 == 0 & acf$lag != 0, acf$acf, NA)
acf$non_seasonal <- ifelse(acf$lag %% 12 != 0 & acf$lag != 0, acf$acf, NA)
acf$lag1 <- ifelse(acf$lag == 0 , acf$acf, NA)



# Plot setting
background <- "black"
fontcolor <- "white"
seasonal_color <- "#2ec4b6"
non_seasonal_color <- "#e0fbfc"
lag1_color <- "#f7aef8"
grid_color <- "#343a40"
actual_c <- "#ffbe0b"
ci_color <- "#ee4266"
font_family <- "Ariel"
title_size <- 30
text_size <- 16
sub_text_size <- 14
arrowhead <- 4
arrowsize <- 1
arrowwidth <- 1

# Series plot
p1 <- plot_ly(data = df,
              x = ~ date,
              y = ~ y,
              type = "scatter",
              mode = "line",
              line = list(color = actual_c),
              showlegend = FALSE) %>%
  layout(yaxis = list(title = "Million Cubic Feet"),
         xaxis = list(title = "")) %>%
  add_annotations(text = paste("The Auto-Correlation Function"),
                  font = list(color = fontcolor,
                              size = title_size,
                              family = font_family),
                  y = 1.65,
                  x = 0,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper") %>%
  add_annotations(text = paste("By the nature of regular time series data, continues and equally spaced over time, often you should expect to have a high correlation between a series and its past lags (e.g., most recent and/or",
                               "seasonal lags). One of the main tools in time series analysis for exploring, quantifying, and visualizing the relationship between a series and its lags is the Auto-Correlation Function (ACF).",
                               "The ACF measures the correlation between a series and its past lags. Those insights can be leveraged to tune forecasting models such as ARIMA or set features for regression-based models.",
                               "The example below demonstrates the usage of the ACF for estimating the correlation for the monthly demand for natural gas in the US (with respect to its past lags).",
                               sep = "<br>"),
                  align = "left",
                  font = list(color = fontcolor,
                              size = text_size,
                              family = font_family),
                  y = 1.5,
                  x = 0,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper") %>%
  add_annotations(text = paste("US Monthly Total Natural Gas Consumption",
                               sep = " "),
                  font = list(color = fontcolor,
                              size = 18,
                              family = font_family),
                  align = "left",
                  y = 1.04,
                  x = 0.5,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper") %>%
  add_annotations(text = paste("The monthly consumption of ",
                               "natural gas in the US is fairly",
                               "cyclical, with strong seasonality.",
                               "This seasonality pattern can be ",
                               "seen in the ACF plot below, with",
                               "a strong correlation of the series",
                               "with its seasonal lags.",
                               sep = "<br>"),
                  font = list(color = fontcolor,
                              size = 12,
                              family = family),
                  align = "left",
                  y = 0.92,
                  x = 1.195,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper")

# ACF plot
p2 <- plot_ly(data = acf) %>%
  add_trace(x = ~ lag,
            y = ~ lag1,
            type = "bar", width = 0.1,
            marker = list(color = lag1_color,
                          line = list(color = lag1_color)),
            name = "Lag 0") %>%
  add_trace(x = ~ lag,
            y = ~ non_seasonal,
            type = "bar", width = 0.1,
            marker = list(color = non_seasonal_color,
                          line = list(color = non_seasonal_color)),
            name = "Non Seasonal Lags") %>%
  add_trace(x = ~ lag,
            y = ~ seasonal,
            type = "bar", width = 0.1,
            marker = list(color = seasonal_color,
                          line = list(color = seasonal_color)),
            name = "Seasonal Lags") %>%
  add_lines(x = ~ lag,
            y = ~ ci_lower,
            name = "95% CI",
            showlegend = TRUE,
            line = list(color = ci_color, dash = "dash")) %>%
  add_lines(x = ~ lag,
            y = ~ ci_upper,
            name = "95% CI",
            showlegend = FALSE,
            line = list(color = ci_color, dash = "dash")) %>%

  layout(
    yaxis = list(title = "Correlation",
                 zeroline = FALSE,
                 showline = FALSE,
                 showticklabels = TRUE,
                 gridcolor = grid_color,
                 showgrid = TRUE),
    font = list(color = fontcolor),
    xaxis = list(zerolinecolor = fontcolor,
                 title = "Lag",
                 zeroline = FALSE,
                 showline = FALSE,
                 dtick = 12,
                 showticklabels = TRUE,
                 gridcolor = grid_color,
                 linecolor = "black",
                 showgrid = FALSE),
    showlegend = TRUE,
    legend = list(x = 1, y = 0.2)) %>%
  add_annotations(text = paste("The ACF formula for series x:",
                               sep = " "),
                  font = list(color = fontcolor,
                              size = sub_text_size,
                              family = font_family),
                  align = "left",
                  y = 1.8,
                  x = 1.17,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper") %>%
  add_annotations(text = TeX("$$r_{k} = \\frac{\\sum_{t = k+1}^{n-k}(x_{t-k} - \\overline{x})(x_t-\\overline{x})}{\\sum_{t = 1}^{n}(x_t - \\overline{x})^2}$$"),
                  font = list(color = fontcolor,
                              size = sub_text_size,
                              family = font_family),
                  y = 1.6,
                  x = 1.2,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper") %>%
  add_annotations(text = paste("Where:",
                               sep = " "),
                  font = list(color = fontcolor,
                              size = sub_text_size,
                              family = font_family),
                  align = "left",
                  y = 1.4,
                  x = 1.05,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper") %>%
  add_annotations(text = TeX("r_{k}  \\text{ - represents the correlation}"),
                  font = list(color = fontcolor,
                              size = sub_text_size,
                              family = font_family),
                  align = "left",
                  y = 1.32,
                  x = 1.19,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper") %>%
  add_annotations(text = TeX("\\text{between the series and its k lag}"),
                  font = list(color = fontcolor,
                              size = sub_text_size,
                              family = font_family),
                  align = "left",
                  y = 1.24,
                  x = 1.2,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper") %>%
  add_annotations(text = TeX("x_{t}  \\text{ - the t observation of series x}"),
                  font = list(color = fontcolor,
                              size = sub_text_size,
                              family = font_family),
                  align = "left",
                  y = 1.16,
                  x = 1.2,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper")  %>%
  add_annotations(text = TeX("\\overline{x}  \\text{ - the mean of series x}"),
                  font = list(color = fontcolor,
                              size = sub_text_size,
                              family = font_family),
                  align = "left",
                  y = 1.08,
                  x = 1.15,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper") %>%
  add_annotations(text = paste("The Auto-Correlation Plot",
                               sep = " "),
                  font = list(color = fontcolor,
                              size = 18,
                              family = font_family),
                  align = "left",
                  y = 1.1,
                  x = 0.5,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper") %>%
  add_annotations(text = paste("Lag 0 represents the",
                               "series correlation",
                               "with itself",
                               sep = "<br>"),
                  font = list(color = lag1_color,
                              size = sub_text_size,
                              family = family),
                  y = 0.92,
                  x = 0.01,
                  align = "center",
                  showarrow = TRUE,
                  arrowhead = arrowhead,
                  arrowsize = arrowsize,
                  arrowwidth = arrowwidth,
                  arrowcolor = lag1_color,
                  xanchor = 'left',
                  yanchor = 'bottom',
                  ax = 30,
                  ay = -25,
                  yref = "paper",
                  xref = "paper") %>%
  add_annotations(text = paste("Lag 12 - the first",
                               "seasonal lag and the",
                               "most correlated with",
                               "the series",
                               sep = "<br>"),
                  font = list(color = seasonal_color,
                              size = sub_text_size,
                              family = family),
                  y = 0.9,
                  x = 0.18,
                  align = "center",
                  showarrow = TRUE,
                  arrowhead = arrowhead,
                  arrowsize = arrowsize,
                  arrowwidth = arrowwidth,
                  arrowcolor = seasonal_color,
                  xanchor = 'left',
                  yanchor = 'middle',
                  ax = 25,
                  ay = -5,
                  yref = "paper",
                  xref = "paper") %>%
  add_annotations(text = paste("95% confidence interval -",
                               "lags outside the two",
                               "dashed lines are",
                               "statistically significant",
                               sep = "<br>"),
                  font = list(color = ci_color,
                              size = sub_text_size,
                              family = family),
                  y = 0.35,
                  x = 0.75,
                  align = "center",
                  showarrow = TRUE,
                  arrowhead = arrowhead,
                  arrowsize = arrowsize,
                  arrowwidth = arrowwidth,
                  arrowcolor = ci_color,
                  xanchor = 'center',
                  yanchor = 'bottom',
                  ax = 0,
                  ay = -60,
                  yref = "paper",
                  xref = "paper")

# Subplot
subplot(p1, p2, nrows = 2,
        titleX = TRUE,
        titleY = TRUE,
        margin = 0.1) %>%
  layout(plot_bgcolor = background,
         paper_bgcolor = background,
         margin = list(t = 150, b = 90, l = 10, r = 230, pad = 4)
  ) %>%
  add_annotations(text = paste("#30DayChartChallenge Day 13",
                               "Viz: Rami Krispin",
                               "Data: US Energy Information Administration",
                               sep = " | "),
                  font = list(color = "#dee2e6",
                              size = 15,
                              family = font_family),
                  y = -0.15,
                  x = 0,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper") %>%
  config(mathjax = "cdn")

