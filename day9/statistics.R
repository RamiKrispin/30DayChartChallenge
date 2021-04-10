library(USgas)
library(plotly)
library(dplyr)
library(lubridate)
library(tsibble)
library(fabletools)
library(feasts)

data("us_monthly")
head(us_monthly)


df <- us_monthly %>%
  mutate(month = month(date, label = TRUE),
         index = yearmonth(date)) %>%
  as_tsibble(index = index)


df$trend <- (df %>%
               # model(feasts::STL(formula(paste(field, "~ season(window = 'periodic')")))) %>%
               model(STL(y ~ season(window = 10))) %>%
               components())$trend

df$detrend <- df$y - df$trend
head(df)
#V1
actual_c <- "#0466c8"
trend_c <- "#90e0ef"
detrend_c <- "#f2b5d4"
dash <- NULL
background <- "black"
title_c <- "white"
family <- "Arial"
arrowhead <- 4
arrowsize <- 1
arrowwidth <- 1
size <- 11

#V2
# actual_c <- "#0466c8"
# trend_c <- "#90e0ef"
# detrend_c <- "#f2b5d4"
# dash <- NULL
# background <- "white"
# title_c <- "black"
# family <- "Arial"
# arrowhead <- 4
# arrowsize <- 1
# arrowwidth <- 1
# size <- 11

# The series
p1 <- plot_ly(data = df,
              x = ~ date,
              y = ~ y,
              type = "scatter",
              mode = "line",
              line = list(color = actual_c),
              showlegend = FALSE) %>%
  add_lines(x = ~ date,
            y = ~ trend,
            line = list(color = trend_c, dash = dash),
            showlegend = FALSE) %>%
  add_lines(x = ~ date,
            y = ~ detrend,
            line = list(color = detrend_c, dash =dash),
            showlegend = FALSE) %>%
  add_annotations(text = paste("Monthly Demand",
                               "for Natural Gas", sep = "<br>"),
                  font = list(color = actual_c,
                              size = size,
                              family = family),
                  y = df$y[which(df$date == as.Date("2020-07-01"))] * 1.1,
                  x = df$date[which(df$date == as.Date("2020-06-01"))],
                  showarrow = TRUE,
                  arrowhead = arrowhead,
                  arrowsize = arrowsize,
                  arrowwidth = arrowwidth,
                  arrowcolor = actual_c,
                  xanchor = 'left',
                  yanchor = 'middle',
                  ax = 20,
                  ay = 0,
                  yref = "y",
                  xref = "x") %>%
  add_annotations(text = paste("Series Trend",
                               "Calculated with STL Model",
                               sep = "<br>"),
                  font = list(color = trend_c,
                              size = size,
                              family = family),
                  y = df$trend[which(df$date == as.Date("2019-01-01"))] * 0.95,
                  x = df$date[which(df$date == as.Date("2019-01-01"))],
                  showarrow = TRUE,
                  arrowhead = arrowhead,
                  arrowsize = arrowsize,
                  arrowwidth = arrowwidth,
                  arrowcolor = trend_c,
                  ax = -60,
                  ay = 40,
                  yref = "y",
                  xref = "x") %>%
  add_annotations(text = paste("De-Trend Series",
                               "(Series - Trend)",
                               sep = "<br>"),
                  font = list(color = detrend_c,
                              size = size,
                              family = family),
                  y = df$detrend[which(df$date == as.Date("2020-01-01"))] * 0.6,
                  x = df$date[which(df$date == as.Date("2020-05-01"))],
                  showarrow = TRUE,
                  arrowhead = arrowhead,
                  arrowsize = arrowsize,
                  arrowwidth = arrowwidth,
                  arrowcolor = detrend_c,
                  ax = 80,
                  ay = -20,
                  xanchor = 'right',
                  yanchor = 'middle',
                  yref = "y",
                  xref = "x") %>%
  add_annotations(text = "Seasonal Analysis - Should I De-Trend or Should I Not?",
                  font = list(color = title_c,
                              size = 24,
                              family = family),
                  y = 1.7,
                  x = 0,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper") %>%
  add_annotations(text = paste("Seasonal plots are one of the most common methods in time series analysis to explore and identify the characteristics of the series seasonal component. A common method for seasonal analysis",
                               "is plotting the series observations according to their frequency units. This enables us to visually inference the seasonal components' properties. Yet, one thing to be aware of when using seasonal plots on",
                               "a series is that the seasonal distribution may be wider and skew due to the trend effect. A more accurate method to review the true nature of the series seasonal components is by de-trend (series - trend)",
                               "first and then using a seasonal plot. The following example demonstrates the differences in seasonal distribution between the series and its detrend version using the US monthly demand for natural gas.",
                               sep = "<br>"),
                  align = "left",
                  font = list(color = title_c,
                              size = 14,
                              family = family),
                  y = 1.52,
                  x = 0,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper")


# Regular seasonal plot
p2 <- plot_ly(data = df,
              x = ~ month,
              y = ~ y,
              type = "box",
              boxpoints = "all",
              jitter = 0.3,
              pointpos = -1.8,
              color = ~ month,
              colors = "Spectral",
              showlegend = FALSE)  %>%
  add_annotations(text = "Seasonal Dist - Normal Series",
                  font = list(color = title_c,
                              size = 16,
                              family = family),
                  y = 0.95,
                  x = 0.5,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper") %>%
  add_annotations(text = paste("The monthly distribution of",
                               "the US's natural gas",
                               "consumption is significantly",
                               "wider when plotting the series",
                               "without de-trend it. After",
                               "de-trend the series distribution",
                               "is narrowing down, where it is",
                               "more pronounced during the",
                               "summer months.",
                               sep = "<br>"),
                  align = "left",
                  font = list(color = title_c,
                              size = size,
                              family = family),
                  y = 0.5,
                  x = 1.08,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper")

# Detrend seasonal plot
p3 <- plot_ly(data = df,
              x = ~ month,
              y = ~ y - trend,
              type = "box",
              boxpoints = "all",
              jitter = 0.3,
              pointpos = -1.8,
              color = ~ month,
              colors = "Spectral",
              showlegend = FALSE) %>%
  add_annotations(text = "Seasonal Dist - De-Trend Series",
                  font = list(color = title_c,
                              size = 16,
                              family = family),
                  y = 0.95,
                  x = 0.5,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper") %>%
  add_annotations(text = paste("#30DayChartChallenge Day 9",
                               "Viz: Rami Krispin",
                               "Data: US Energy Information Administration",
                               sep = " | "),
                  font = list(color = "#dee2e6",
                              size = 12,
                              family = family),
                  y = -0.37,
                  x = 0,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper")

# Merging the plots
p <- subplot(p1, p2, p3,
        nrows = 3,
        shareX = FALSE,
        shareY = FALSE,
        margin = 0.05) %>%
  layout(plot_bgcolor = background,
         paper_bgcolor = background,
         font = list(color = "white"),
         xaxis = list(zerolinecolor = "white"),
         margin = list(t = 120, b = 70, l = 10, r = 100, pad = 20))

