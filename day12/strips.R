# Packages
library(coronavirus)
library(countrycode)
library(dplyr)
library(plotly)

# loading the Covid19 data
raw <- coronavirus::refresh_coronavirus_jhu()
head(raw)


df <- raw %>%
  as.data.frame() %>%
  mutate(continent = countrycode(sourcevar = location,
                                 origin = "country.name",
                                 destination = "continent"))

df %>% select(location, continent) %>% distinct()

# Fixing continent label
x <- df[which(is.na(df$continent)),]
l <- data.frame(location = unique(x$location),
                stringsAsFactors = FALSE)


for(i in unique(x$location)){
  l <- substr(i, start = 1, stop = (gregexpr(pattern = ",", text = i)[[1]][1]) - 1)
  c <- NULL
  c <-  countrycode(sourcevar = l,
                    origin = "country.name",
                    destination = "continent")

  if(!is.null(c)){
    df$continent[which(df$location == i)] <- c
  }
}


df$continent <- ifelse(df$location == "Kosovo", "Europe", df$continent)
df$continent <- ifelse(df$location == "Micronesia", "Oceania", df$continent)


df <- df %>%
  mutate(continent = ifelse(continent == "Americas", "Latin America", continent),
         continent = ifelse(location %in% c("Canada", "US", "Mexico", "Greenland"), "N America", continent))


table(is.na(df$continent))
max(df$date)

df1 <- df %>% filter(!is.na(continent),
                     data_type == "cases_new") %>%
  group_by(date,continent) %>%
  summarise(daily = sum(value),
            .groups = "drop") %>%
  # pivot_wider(names_from = continent,
  #             values_from = daily) %>%
  left_join(df %>%
              as.data.frame %>%
              filter(!is.na(continent),
                     data_type == "cases_new") %>%
              group_by(date) %>%
              summarise(total = sum(value),
                        .groups = "drop"),
            by = "date") %>%
  mutate(perc = 100 * daily / total)

head(df1)
tail(df1)

# Setting the plot parameters
background <- "rgb(253, 241, 230)"
title_c <- "black"
size <- 12
family <- "Arial"
arrowhead <- 4
arrowsize <- 1
arrowwidth <- 1
size <- 11

# Plot
plot_ly(data = df1,
                x = ~ date,
                y = ~ perc,
                color = ~ continent,
                # Africa, Asia, Europe, Latin America, N America, Oceania
                # colors = c("#e85d04", "#cbc0d3", "#a8dadc","#457b9d", "#1d3557", "yellow"),
                type = "bar") %>%
  layout(barmode = "stack",
         plot_bgcolor = background,
         paper_bgcolor = background,
         yaxis = list(title = "",
                      ticksuffix = "%",
                      ticks = "inside",
                      tickcolor = "gray",
                      gridcolor = "gray",
                      nticks = 6),
         xaxis = list(title = "",
                      showgrid = FALSE,
                      dtick="M1",
                      type = 'date',
                      ticks = "inside",
                      tickcolor = "gray",
                      gridcolor = "gray",
                      tickformat = "%b",
                      ticklabelmode="period"
                      #nticks = 1
         ),
         showlegend  = FALSE,
         margin = list(b = 90)
  ) %>%
  add_annotations(text = c("Asia","Europe", "Latin America", "N America"),
                  font = list(color = title_c,
                              size = size,
                              family = family),
                  y = c(50, 35, 60, 85),
                  x = c(as.Date("2020-02-01"),
                        as.Date("2020-03-25"),
                        as.Date("2020-06-01"),
                        as.Date("2020-07-15")),
                  showarrow = FALSE,
                  bgcolor = background,
                  yref = "y",
                  xref = "x") %>%
  add_annotations(text = c("Oceania"),
                  font = list(color = title_c,
                              size = size,
                              family = family),
                  y = c(99),
                  x = c(as.Date("2020-03-25")),
                  showarrow = TRUE,
                  arrowhead = arrowhead,
                  arrowsize = arrowsize,
                  arrowwidth = arrowwidth,
                  arrowcolor = "black",
                  xanchor = 'left',
                  yanchor = 'middle',
                  ax = 13,
                  ay = 13,
                  bgcolor = background,
                  yref = "y",
                  xref = "x") %>%
  add_annotations(text = c("Africa"),
                  font = list(color = title_c,
                              size = size,
                              family = family),
                  y = c(5),
                  x = c(as.Date("2020-07-10")),
                  showarrow = TRUE,
                  arrowhead = arrowhead,
                  arrowsize = arrowsize,
                  arrowwidth = arrowwidth,
                  arrowcolor = "black",
                  xanchor = 'left',
                  yanchor = 'middle',
                  ax = 13,
                  ay = -13,
                  bgcolor = background,
                  yref = "y",
                  xref = "x") %>%
  add_annotations(text = c("Distribution of Covid19 Daily New Confirmed Cases by Region"),
                  font = list(color = title_c,
                              size = 14,
                              family = family),
                  y = 1.1,
                  x = -0.01,
                  showarrow = FALSE,
                  bgcolor = background,
                  yref = "paper",
                  xref = "paper") %>%
  add_annotations(text = c("Inspired by Financial Times graph on Covid19 death rate by continent by Steven Bernard / @sdbernard"),
                  font = list(color = title_c,
                              size = 11,
                              family = family),
                  y = -0.28,
                  x = -0.04,
                  showarrow = FALSE,
                  bgcolor = background,
                  yref = "paper",
                  xref = "paper") %>%
  add_annotations(text = paste("#30DayChartChallenge Day 12",
                               "Viz: Rami Krispin",
                               "Data: Johns Hopkins University Center for Systems Science and Engineering",
                               sep = " | "),
                  font = list(color = title_c,
                              size = 11,
                              family = family),
                  align = "left",
                  y = -0.35,
                  x = -0.04,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper") %>%
  add_annotations(text = c("2021"),
                  font = list(color = "gray",
                              size = 12,
                              family = family),
                  align = "center",
                  y = -0.15,
                  x = 0.82,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper")

