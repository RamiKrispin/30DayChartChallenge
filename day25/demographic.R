library(fable)
library(tsibble)
library(dplyr)
library(plotly)


hex_to_rgb <- function(hex){
  rgb <- paste0(as.numeric(grDevices::col2rgb(hex) %>% base::t()), collapse = ",")
  return(rgb)
}


df <- read.csv("~/R/projects/30DayChartChallenge/day25/us_pop.csv",
               stringsAsFactors=FALSE) %>%
  setNames(c("year", "pop")) %>%
  mutate(lag1 = lag(pop, n = 1),
         yoy = pop / lag1 -1) %>%
  as_tsibble(index = year)

head(df)
str(df)
df


plot_ly(data = df) %>%
  add_lines(x = ~ year,
            y = ~ pop)




md <- df %>%
  model(ets1 = ETS(pop ~ season("N") + trend("A")),
        ets2 = ETS(pop ~ season("N") + trend("Ad")),
        ets3 = ETS(pop ~ season("N") + trend(method = "Ad", phi = 0.9)))

md %>% report()

md %>% select(ets1) %>% report()
md %>% select(ets2) %>% report()
md %>% select(ets3) %>% report()
md %>% augment()

fc <- md %>% forecast(h = 5) %>%
  hilo(level = c(80, 95))

fc1 <- fc %>% filter(.model == "ets1")
fc2 <- fc %>% filter(.model == "ets2")
fc3 <- fc %>% filter(.model == "ets3")
#Plot parameters----
colors_list <- c("#9467bd", "#2ca02c", "#e377c2")
opacity <- 0.7
background <- "#edf2fb"

# Plot ----

# Forecast
p1 <- plot_ly(data = df) %>%
  add_lines(x = ~ year,
            y = ~ pop,
            line = list(color = paste("rgb(", hex_to_rgb("#1f77b4"),")", sep = "")),
            showlegend = FALSE) %>%
  plotly::add_ribbons(x = fc1$year,
                      ymin = fc2$`95%`$lower,
                      ymax = fc2$`95%`$upper,
                      line = list(color = paste("rgba(", hex_to_rgb(colors_list[2]),",", opacity ,")", sep = "")),
                      fillcolor = paste("rgba(", hex_to_rgb(colors_list[2]),",", 0.5 ,")", sep = ""),
                      showlegend = FALSE,
                      name = "md2",
                      hovertemplate = paste('%{y}'),
                      legendgroup = "md2") %>%
  plotly::add_lines(x = fc2$year,
                    y = fc2$.mean,
                    line = list(color =  colors_list[2], dash = "dash"),
                    hovertemplate = paste('%{y}'),
                    name = "md2",
                    legendgroup = "md2",
                    showlegend = FALSE)  %>%
  layout(yaxis = list(title = "Population (Millions)")) %>%
  add_annotations(text = "U.S. Population Forecast",
                  x = 0.5,
                  y = 1.02,
                  xref = "paper",
                  yref = "paper",
                  font = list(family = "Ariel",
                              size = 20),
                  showarrow = FALSE)

df1 <- df %>%
  dplyr::select(year, pop) %>%
  dplyr::mutate(type = "actual") %>%
  dplyr::bind_rows(fc2 %>% dplyr::select(year, pop = .mean) %>%
                     dplyr::mutate(type = "forecast")) %>%
  dplyr::arrange(year) %>%
  dplyr::mutate(lag1 = dplyr::lag(pop, n = 1),
                yoy = 100 * (pop / lag1 - 1)) %>%
  dplyr::mutate(yoy_a = ifelse(type == "actual", yoy, NA),
                yoy_f = ifelse(type == "forecast", yoy, NA))

df1

# YoY growth
p2 <- plotly::plot_ly(data = df1,
                      x = ~ year,
                      y = ~ yoy_a,
                      type = "bar",
                      text = ~ paste(round(yoy_a,2), "%", sep = ""),
                      textposition = "auto",
                      marker = list(color = "#1f77b4",
                                    line = list(color = 'rgb(8,48,107)',
                                                width = 1.5)),
                      name = "Actual") %>%
  plotly::add_trace(x = ~ year,
                    y = ~ yoy_f,
                    type = "bar",
                    text = ~ paste(round(yoy_f,2), "%", sep = ""),
                    textposition = "auto",
                    marker = list(color = paste("rgba(", hex_to_rgb(colors_list[2]),",", opacity ,")", sep = ""),
                                  line = list(color = 'rgb(8,48,107)',
                                              width = 1.5)),
                    name = "Forecast") %>%
  layout(yaxis = list(title = "YoY Growth (%)",
                      ticksuffix = '%')) %>%
  add_annotations(text = "US Population YoY Growth (%)",
                  x = 0.5,
                  y = 1.02,
                  xref = "paper",
                  yref = "paper",
                  font = list(family = "Ariel",
                              size = 20),
                  showarrow = FALSE)

# Plot
subplot(p1, p2, nrows = 2,
        shareX = TRUE,
        heights = c(0.65, 0.35),
        titleY = TRUE,
        margin = 0.04) %>%
  layout(title = "",
         legend = list(x = 0.05, y = 0.95),
         font = list(family = "Ariel",
                     size = 16),
         xaxis = list(title = ""),
         margin = list(t = 40, b = 90, r = 40, l = 40)) %>%
  add_annotations(text = paste(paste("Using Exponential Smoothing state-space model with an additive trend with damped variants"),
                               paste("#30DayChartChallenge Day 25",
                                     "Viz: Rami Krispin",
                                     "Data: U.S. Census Bureau",
                                     sep = " | "),
                               sep = "<br>"),
                  font = list(size = 16,
                              family = "Ariel"),
                  align = "left",
                  x = -0.02,
                  y = -0.13,
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE)

