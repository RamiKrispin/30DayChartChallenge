library(readr)
library(plotly)
library(dplyr)


# Loading the data
df <- read_csv("day5/TTLCON.csv") %>%
  setNames(c("date","y"))

head(df)
str(df)


# Plot the data
plot_ly(data = df) %>%
  add_lines(x = ~ date,
            y = ~ y)



# Create features
df <- df %>%
  mutate(seg1 = 1:nrow(df),
         seg2 = pmax(0, seg1 - min(seg1[seg1[which(date > as.Date("2007-08-01"))]])),
         seg3 = pmax(0, seg1 - min(seg1[seg1[which(date > as.Date("2011-01-01"))]])))


head(df)
tail(df)

# Fit normal trend
md1 <- lm(y ~ seg1, data = df)
summary(md1)
# Fit piecewise trend
md2 <- lm(y ~ seg1 + seg2 + seg3, data = df)
summary(md2)

df$yhat1 <- predict(md1)
df$yhat2 <- predict(md2)


# Plot the data with the fitted trend
plot_ly(data = df) %>%
  add_markers(x = ~ date,
              y = ~ y,
              marker = list(
                opacity = 0.6,
                color = "#90e0ef",
                size = 8),
              name = "Construction Spending") %>%
add_lines(x = ~ date,
          y = ~ yhat1,
          line = list(color = "red",
                      dash = "dash",
                      width = 4),
          name = "Simple Trend") %>%
  add_lines(x = ~ date,
            y = ~ yhat2,
            line = list(color = "#fca311",
                        dash = "dot",
                        width = 6),
            name = "Piecewise Trend") %>%
  layout(title = "The US Total Construction Spending - Fitted Trends",
         plot_bgcolor = "black",
         paper_bgcolor = "black",
         font = list(color = "white"),
         yaxis = list(title = "Millions of Dollars"),
         xaxis = list(title = "Source: U.S. Census Bureau, Total Construction Spending [TTLCON], retrieved from FRED"),
         margin = list(t = 50, b = 80),
         legend = list(x = 0.05, y = 0.95))


