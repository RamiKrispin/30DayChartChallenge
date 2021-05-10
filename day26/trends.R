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
        ets3 = ETS(pop ~ season("N") + trend(method = "Ad", phi = 0.95)),
        ets4 = ETS(pop ~ season("N") + trend(method = "Ad", phi = 0.9)))

md %>% report()

md %>% select(ets1) %>% report()
md %>% select(ets2) %>% report()
md %>% select(ets3) %>% report()
md %>% select(ets4) %>% report()
md %>% augment()

fc <- md %>% forecast(h = 5) %>%
  hilo(level = c(80, 95))

fc1 <- fc %>% filter(.model == "ets1")
fc2 <- fc %>% filter(.model == "ets2")
fc3 <- fc %>% filter(.model == "ets3")
fc4 <- fc %>% filter(.model == "ets4")
#Plot parameters----
colors_list <- c("#9467bd", "#2ca02c", "#e377c2", "#f4a261")
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
  layout(yaxis = list(title = "Population (Millions)"))

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
                  y = 1,
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
  layout(title = "U.S. Population Forecast",
         legend = list(x = 0.1, y = 0.9),
         font = list(family = "Ariel",
                     size = 15),
         xaxis = list(title = ""),
         margin = list(t = 60))


plot_ly(data = df) %>%
  add_lines(x = ~ year,
            y = ~ pop) %>%
  plotly::add_ribbons(x = fc1$year,
                      ymin = fc1$`95%`$lower,
                      ymax = fc1$`95%`$upper,
                      line = list(color = paste("rgba(", hex_to_rgb(colors_list[1]),",", opacity ,")", sep = "")),
                      fillcolor = paste("rgba(", hex_to_rgb(colors_list[1]),",", opacity ,")", sep = ""),
                      showlegend = FALSE,
                      name = "md1",
                      hovertemplate = paste('%{y}'),
                      legendgroup = "md1") %>%
  plotly::add_lines(x = fc1$year,
                    y = fc1$.mean,
                    line = list(color =  colors_list[1], dash = "dash"),
                    hovertemplate = paste('%{y}'),
                    name = "md1",
                    legendgroup = "md1") %>%
  plotly::add_ribbons(x = fc2$year,
                      ymin = fc2$`95%`$lower,
                      ymax = fc2$`95%`$upper,
                      line = list(color = paste("rgba(", hex_to_rgb(colors_list[2]),",", opacity ,")", sep = "")),
                      fillcolor = paste("rgba(", hex_to_rgb(colors_list[2]),",", opacity ,")", sep = ""),
                      showlegend = FALSE,
                      name = "md2",
                      hovertemplate = paste('%{y}'),
                      legendgroup = "md2") %>%
  plotly::add_lines(x = fc2$year,
                    y = fc2$.mean,
                    line = list(color =  colors_list[2], dash = "dash"),
                    hovertemplate = paste('%{y}'),
                    name = "md2",
                    legendgroup = "md2") %>%
  plotly::add_ribbons(x = fc3$year,
                      ymin = fc3$`95%`$lower,
                      ymax = fc3$`95%`$upper,
                      line = list(color = paste("rgba(", hex_to_rgb(colors_list[3]),",", opacity ,")", sep = "")),
                      fillcolor = paste("rgba(", hex_to_rgb(colors_list[3]),",", opacity ,")", sep = ""),
                      showlegend = FALSE,
                      name = "md3",
                      hovertemplate = paste('%{y}'),
                      legendgroup = "md3") %>%
  plotly::add_lines(x = fc3$year,
                    y = fc3$.mean,
                    line = list(color =  colors_list[3], dash = "dash"),
                    hovertemplate = paste('%{y}'),
                    name = "md3",
                    legendgroup = "md3") %>%
  plotly::add_ribbons(x = fc4$year,
                      ymin = fc4$`95%`$lower,
                      ymax = fc4$`95%`$upper,
                      line = list(color = paste("rgba(", hex_to_rgb(colors_list[4]),",", opacity ,")", sep = "")),
                      fillcolor = paste("rgba(", hex_to_rgb(colors_list[4]),",", opacity ,")", sep = ""),
                      showlegend = FALSE,
                      name = "md4",
                      hovertemplate = paste('%{y}'),
                      legendgroup = "md4") %>%
  plotly::add_lines(x = fc4$year,
                    y = fc4$.mean,
                    line = list(color =  colors_list[4], dash = "dash"),
                    hovertemplate = paste('%{y}'),
                    name = "md4",
                    legendgroup = "md4") %>%
  layout(plot_bgcolor = background,
         paper_bgcolor = background)



# Plot setting
color_line <- "#1f77b4"
color_fc <- "#a8dadc"
color_fc <- "#90e0ef"
opacity <- 0.3
background <- "#edf2fb"

p1 <- plot_ly(data = df) %>%
  add_lines(x = ~ year,
            y = ~ pop,
            showlegend = FALSE) %>%
  plotly::add_ribbons(x = fc1$year,
                      ymin = fc1$`95%`$lower,
                      ymax = fc1$`95%`$upper,
                      line = list(color = paste("rgba(", hex_to_rgb(color_fc),",", opacity ,")", sep = "")),
                      fillcolor = paste("rgba(", hex_to_rgb(color_fc),",", opacity ,")", sep = ""),
                      showlegend = FALSE,
                      name = "md1",
                      hovertemplate = paste('%{y}'),
                      legendgroup = "md1") %>%
  plotly::add_lines(x = fc1$year,
                    y = fc1$.mean,
                    showlegend = FALSE,
                    line = list(color =  color_line, dash = "dash"),
                    hovertemplate = paste('%{y}'),
                    name = "md1",
                    legendgroup = "md1") %>%
  layout(yaxis = list(title = ""),
         xaxis = list(title = ""))


p2 <- plot_ly(data = df) %>%
  add_lines(x = ~ year,
            y = ~ pop,
            line = list(color =  color_line),
            showlegend = FALSE) %>%
  plotly::add_ribbons(x = fc2$year,
                      ymin = fc2$`95%`$lower,
                      ymax = fc2$`95%`$upper,
                      line = list(color = paste("rgba(", hex_to_rgb(color_fc),",", opacity ,")", sep = "")),
                      fillcolor = paste("rgba(", hex_to_rgb(color_fc),",", opacity ,")", sep = ""),
                      showlegend = FALSE,
                      name = "md2",
                      hovertemplate = paste('%{y}'),
                      legendgroup = "md2") %>%
  plotly::add_lines(x = fc2$year,
                    y = fc2$.mean,
                    showlegend = FALSE,
                    line = list(color =  color_line, dash = "dash"),
                    hovertemplate = paste('%{y}'),
                    name = "md2",
                    legendgroup = "md2") %>%
  layout(yaxis = list(title = ""),
         xaxis = list(title = ""))


p3 <- plot_ly(data = df) %>%
  add_lines(x = ~ year,
            y = ~ pop,
            line = list(color =  color_line),
            showlegend = FALSE) %>%
  plotly::add_ribbons(x = fc3$year,
                      ymin = fc3$`95%`$lower,
                      ymax = fc3$`95%`$upper,
                      line = list(color = paste("rgba(", hex_to_rgb(color_fc),",", opacity ,")", sep = "")),
                      fillcolor = paste("rgba(", hex_to_rgb(color_fc),",", opacity ,")", sep = ""),
                      showlegend = FALSE,
                      name = "md3",
                      hovertemplate = paste('%{y}'),
                      legendgroup = "md3") %>%
  plotly::add_lines(x = fc3$year,
                    y = fc3$.mean,
                    showlegend = FALSE,
                    line = list(color =  color_line, dash = "dash"),
                    hovertemplate = paste('%{y}'),
                    name = "md3",
                    legendgroup = "md3") %>%
  layout(yaxis = list(title = ""),
         xaxis = list(title = ""))


p4 <- plot_ly(data = df) %>%
  add_lines(x = ~ year,
            y = ~ pop,
            line = list(color =  color_line),
            showlegend = FALSE) %>%
  plotly::add_ribbons(x = fc4$year,
                      ymin = fc4$`95%`$lower,
                      ymax = fc4$`95%`$upper,
                      line = list(color = paste("rgba(", hex_to_rgb(color_fc),",", opacity ,")", sep = "")),
                      fillcolor = paste("rgba(", hex_to_rgb(color_fc),",", opacity ,")", sep = ""),
                      showlegend = FALSE,
                      name = "md4",
                      hovertemplate = paste('%{y}'),
                      legendgroup = "md4") %>%
  plotly::add_lines(x = fc4$year,
                    y = fc4$.mean,
                    line = list(color =  color_line, dash = "dash"),
                    showlegend = FALSE,
                    hovertemplate = paste('%{y}'),
                    name = "md4",
                    legendgroup = "md4") %>%
  layout(yaxis = list(title = ""),
         xaxis = list(title = ""))

plotly::subplot(p1,p2,
                p3, p4,
                nrows = 2,
                shareY = TRUE,
                shareX = TRUE) %>%
  layout(
         margin = list(t = 180, r = 60, l = 60, b = 80)) %>%
  add_annotations(text = "Scenario-based Forecast",
                  xref = "paper",
                  yref = "paper",
                  x = - 0.02,
                  y = 1.32,
                  align = "left",
                  font = list(family = "Ariel",
                              size = 36),
                  showarrow = FALSE) %>%
  add_annotations(text = "In some cases, forecasting future growth or decay of a series could be super challenging, mainly when the rates are",
                  xref = "paper",
                  yref = "paper",
                  x = 0.28,
                  y = 1.29,
                  align = "left",
                  font = list(family = "Ariel",
                              size = 19),
                  showarrow = FALSE) %>%
  add_annotations(text = paste("derived from indeterministic external factors. Therefore, it would be hard to infer those rates solely by using the historical observations of the series. One simple approach",
                               'for this type of problem is to leverage some business assumptions and create a "what-if" forecast by modeling different growth/decay scenarios. The following example',
                               'demonstrated the use of "what-if" forecasting approach to model four different trend scenarios in the US population five years forecast by using Exponential Smoothing',
                               "state-space model (ETS) with an additive trend and different tuning of the dumpeing parameter.",
                               sep = "<br>"),
                  align = "left",
                  xref = "paper",
                  yref = "paper",
                  x =- 0.02,
                  y = 1.247,
                  font = list(family = "Ariel",
                              size = 19),
                  showarrow = FALSE) %>%
  add_annotations(text = "Scenario 1: Linear Trend",
                  xref = "paper",
                  yref = "paper",
                  x = 0.01,
                  y = 0.98,
                  align = "left",
                  xanchor = "left",
                  font = list(family = "Ariel",
                              size = 16),
                  showarrow = FALSE) %>%
  add_annotations(text = TeX("\\phi = 1"),
                  xref = "paper",
                  yref = "paper",
                  x = 0.01,
                  y = 0.94,
                  align = "left",
                  xanchor = "left",
                  font = list(family = "Ariel",
                              size = 16),
                  showarrow = FALSE) %>%
  add_annotations(text = "Scenario 2:  Trend Dumpeing - Auto Tuning",
                  xref = "paper",
                  yref = "paper",
                  x = 0.52,
                  y = 0.98,
                  align = "left",
                  xanchor = "left",
                  font = list(family = "Ariel",
                              size = 16),
                  showarrow = FALSE) %>%
  add_annotations(text = TeX("\\phi = 0.975"),
                  xref = "paper",
                  yref = "paper",
                  x = 0.52,
                  y = 0.94,
                  align = "left",
                  xanchor = "left",
                  font = list(family = "Ariel",
                              size = 16),
                  showarrow = FALSE) %>%
  add_annotations(text = "Scenario 3: Moderate Trend Dumping",
                  xref = "paper",
                  yref = "paper",
                  x = 0.01,
                  y = 0.44,
                  align = "left",
                  xanchor = "left",
                  font = list(family = "Ariel",
                              size = 16),
                  showarrow = FALSE) %>%
  add_annotations(text = TeX("\\phi = 0.95"),
                  xref = "paper",
                  yref = "paper",
                  x = 0.01,
                  y = 0.40,
                  align = "left",
                  xanchor = "left",
                  font = list(family = "Ariel",
                              size = 16),
                  showarrow = FALSE) %>%
  add_annotations(text = "Scenario 4: Aggressive Trend Dumping ",
                  xref = "paper",
                  yref = "paper",
                  x = 0.52,
                  y = 0.44,
                  align = "left",
                  xanchor = "left",
                  font = list(family = "Ariel",
                              size = 16),
                  showarrow = FALSE) %>%
  add_annotations(text = TeX("\\phi = 0.9"),
                  xref = "paper",
                  yref = "paper",
                  x = 0.52,
                  y = 0.40,
                  align = "left",
                  xanchor = "left",
                  font = list(family = "Ariel",
                              size = 16),
                  showarrow = FALSE) %>%

  add_annotations(text = "US Population Forecast",
                  xref = "paper",
                  yref = "paper",
                  x = 0.5,
                  y = 1.05,
                  align = "left",
                  xanchor = "center",
                  font = list(family = "Ariel",
                              size = 24),
                  showarrow = FALSE) %>%
  add_annotations(text = paste(paste("Using Exponential Smoothing state-space model (ETS) with an additive trend and damped variants"),
                               paste("#30DayChartChallenge Day 26",
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
                  showarrow = FALSE) %>%
  config(mathjax = 'cdn')

