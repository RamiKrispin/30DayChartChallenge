library(dplyr)
library(plotly)
library(lubridate)
library(tsibble)
library(fabletools)
library(feasts)


# loading the data and reformat it
df <- read.csv("day20/S4248SM144NCEN.csv",
               stringsAsFactors=FALSE) %>%
  setNames(c("date_temp", "y")) %>%
  mutate(date = as.Date(date_temp)) %>%
  select(date, y) %>%
  mutate(month = factor(month(date, label = TRUE), ordered = FALSE),
         year = year(date),
         yearmon = yearmonth(date),
         index = 1:n(),
         index_0.5 = (1:n()) ^ 0.5,
         index_2 = (1:n()) ^ 2,
         index_log = log(1:n()),
         lag1 = lag(y, n = 1),
         lag12 = lag(y, n = 12),
         mon_growth = y/lag1 - 1,
         yoy_growth = y/lag12 - 1,
         yoy_usd_growth = y - lag12)



head(df)
tail(df)

# claculate the yoy growth
df_yr <- df %>% group_by(year) %>%
  summarise(total = sum(y)) %>%
  mutate(total_lag = lag(total, n = 1),
         yoy = round(100 * (total / total_lag - 1), 1))

head(df_yr)
tail(df_yr)

# Convert to tsibble object
df.ts <- df %>%
  dplyr::select(yearmon, date, y) %>%
  as_tsibble(index = yearmon)

head(df.ts)

# Decompose the series
stl_obj <- df.ts %>%
  fabletools::model(STL(y ~ season(window = 5))) %>%
  fabletools::components()

# Merge the trend to the main table
df <- df %>%
  left_join(stl_obj %>% select(yearmon, trend), by = "yearmon")


# Exploring the data
range <- c(as.Date("2010-01-01"), max(df$date))
range <- c(min(df$date), max(df$date))
p1 <- plot_ly(data = df) %>%
  add_lines(x = ~date,
            y = ~ y,
            line = list(color = "#22223b")) %>%
  add_lines(x = ~ date,
            y = ~ trend) %>%
  layout(xaxis = list(range = range))

p2 <- plot_ly(data = df,
        x = ~ date,
        y = ~ yoy_usd_growth,
        type = "bar") %>%
  layout(xaxis = list(range = range))


p3 <- plot_ly(data = df %>% filter(year >= 2005) ,
        x = ~ date,
        y = ~ yoy_growth,
        type = "bar") %>%
  layout(xaxis = list(title = ""))


subplot(p1,p2, p3, nrows = 3,
        shareX = TRUE)



# Final plot

# Plot setting
arrowhead <- 4
arrowsize <- 1
arrowwidth <- 1

d <- df %>% filter(year >= 2005)
p4 <- plot_ly()

for(i in unique(d$year)){
  d_yr <- d %>% filter(year == i)
  if(i < 2020){
  p4 <- p4 %>%
    add_lines(x = d_yr$month,
              y = d_yr$y / 1000, name = i,
              showlegend = FALSE,
              line = list(color = "gray", width = 1))

  } else {
    p4 <- p4 %>%
      add_lines(x = d_yr$month,
                y = d_yr$y / 1000, name = i,
                showlegend = FALSE,
                line = list(color = "red", width = 2))
  }
}

p4 <- p4 %>%
  layout(yaxis = list(title = "Billion of Dollars"),
         margin = list(t = 100)) %>%
  add_annotations(text = 2020,
                  font = list(size = 13,
                              color = "red",
                              family = "Ariel"),
                  align = "left",
                  x = 1.03,
                  y = 0.97,
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE) %>%
  add_annotations(text = 2015,
                  font = list(size = 13,
                              color = "gray",
                              family = "Ariel"),
                  align = "left",
                  x = 1.03,
                  y = 0.73,
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE) %>%
  add_annotations(text = 2010,
                  font = list(size = 13,
                              color = "gray",
                              family = "Ariel"),
                  align = "left",
                  x = 1.03,
                  y = 0.5,
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE) %>%
  add_annotations(text = 2005,
                  font = list(size = 13,
                              color = "gray",
                              family = "Ariel"),
                  align = "left",
                  x = 1.03,
                  y = 0.3,
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE) %>%
  add_annotations(text = 2021,
                  font = list(size = 13,
                              color = "red",
                              family = "Ariel"),
                  align = "left",
                  x = 0.09,
                  y = 0.63,
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE) %>%
  add_annotations(text = "Covid19 Effect",
                  font = list(size = 17,
                              family = "Ariel"),
                  align = "left",
                  x = 0.4,
                  y = 1.07,
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE) %>%
  add_annotations(text = paste("Covid19 hit", "the US", sep = "<br>"),
                  font = list(size = 12,
                              family = "Ariel"),
                  align = "center",
                  x = 0.175,
                  y = 0.74,
                  xref = "paper",
                  yref = "paper",
                  showarrow = TRUE,
                  arrowhead = arrowhead,
                  arrowsize = arrowsize,
                  arrowwidth = arrowwidth,
                  arrowcolor = "black",
                  xanchor = "right",
                  yanchor = "bottom",
                  ax = -15,
                  ay = -15) %>%
  add_annotations(text = paste("Seasonal drop in",
                               "alcohol sales,",
                               "potentially as",
                               "result of init",
                               "local lockdown",
                               sep = "<br>"),
                  font = list(size = 12,
                              family = "Ariel"),
                  align = "center",
                  x = 0.27,
                  y = 0.65,
                  xref = "paper",
                  yref = "paper",
                  showarrow = TRUE,
                  arrowhead = arrowhead,
                  arrowsize = arrowsize,
                  arrowwidth = arrowwidth,
                  arrowcolor = "black",
                  xanchor = "center",
                  yanchor = "bottom",
                  ax = 0,
                  ay = -30) %>%
  add_annotations(text = paste("Non-seasonal increase since June",
                               "to the level of holiday season",
                               sep = "<br>"),
                  font = list(size = 12,
                              family = "Ariel"),
                  align = "center",
                  x = 0.47,
                  y = 0.94,
                  xref = "paper",
                  yref = "paper",
                  showarrow = TRUE,
                  arrowhead = arrowhead,
                  arrowsize = arrowsize,
                  arrowwidth = arrowwidth,
                  arrowcolor = "black",
                  xanchor = "left",
                  yanchor = "bottom",
                  ax = 90,
                  ay = -7)



p5 <- plot_ly(data = df_yr %>% dplyr::filter(year >= 2005 & year < 2021),
              x = ~ year,
              y = ~ yoy,
              type = "bar",
              text = ~yoy, textposition = "auto",
              showlegend = FALSE,
              marker = list(color = "#3d405b")) %>%
  layout(yaxis = list(title = "YoY Growth (%)",
                      ticksuffix = '%',
                      range = c(-1.1,8)),
         xaxis = list(automargin = TRUE),
         margin = list(b = 30)) %>%
  add_annotations(text = "Year Over Year Growth (%)",
                  font = list(size = 17,
                              # color = "red",
                              family = "Ariel"),
                  align = "center",
                  x = 0.6,
                  y = 1.08,
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE)

p1 <- plot_ly(data = d) %>%
  add_lines(x = ~date,
            y = ~ y / 1000,
            name = "Alcohol Sells",
            line = list(color = "#343a40")) %>%
  add_lines(x = ~ date,
            y = ~ trend / 1000,
            name = "Trend",
            line = list(color = "#457b9d", dash = NULL, width = 2.5)) %>%
  layout(xaxis = list(title = ""),
         yaxis = list(title = "Billion of Dollars"),
         legend = list(x = 0.05, y = 0.95))  %>%
  add_annotations(text = "US Monthly Beer, Wine, and Distilled Alcoholic Beverages Sales (Billion of USD)",
                  font = list(size = 20,
                              # color = "red",
                              family = "Ariel"),
                  align = "center",
                  x = 0.5,
                  y = 1,
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE)





subplot(p1, subplot(p4,p5, nrows = 1, margin = 0.04,  titleY = TRUE),
        nrows = 2,
        shareX = FALSE,
        titleY = TRUE,
        heights = c(0.6, 0.4),
        margin = 0.04) %>%
  layout(plot_bgcolor = bgcolor,
         paper_bgcolor = bgcolor,
         font = list(color = "black"),
         margin = list(t = 20, b = 100, l = 60, r = 50),
         xaxis = list(zerolinecolor = "black")) %>%
  add_annotations(text = paste(paste("#30DayChartChallenge Day 20",
                               "Viz: Rami Krispin", sep = " | "),
                               paste("Data: U.S. Census Bureau, Merchant Wholesalers, Except Manufacturers' Sales Branches and Offices: Nondurable Goods: Beer, Wine, and Distilled Alcoholic Beverages Sales",
                                     "[S4248SM144NCEN], retrieved from FRED, Federal Reserve Bank of St. Louis; https://fred.stlouisfed.org/series/S4248SM144NCEN, April 24, 2021.",
                                     sep = "<br>"),
                               sep = "<br>"),
                  font = list(size = 13,
                              # color = "red",
                              family = "Ariel"),
                  align = "left",
                  x = -0.02,
                  y = -0.12,
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE)

