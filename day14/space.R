library(readr)
library(dplyr)
library(plotly)

# Data
atens <- read_csv("day14/Atens.csv", trim_ws = FALSE) %>%
  mutate(type = "atens")
head(atens)
table(atens$site)


amors <- read_csv("day14/Amors.csv", trim_ws = FALSE) %>%
  mutate(type = "amors")


apollos <- read_csv("day14/Apollos.csv", trim_ws = FALSE) %>%
  mutate(type = "apollos")

head(apollos)


df <- bind_rows(atens, amors, apollos) %>%
  filter(a < 5,
         H < 40,
         EMoid < 0.6)


# general plot setting
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



# Sampling
d <- df[sample(1:nrow(df), size = 500),] %>%
  as.data.frame()

d1 <- d %>% filter(H < 22 & EMoid < 0.05)
# exploring the data
plot_ly(data = apollos,
        y = ~ a,
        x = ~ q)



plot_ly()  %>%
  add_trace(y = d$H,
            x = d$EMoid,
            color = d$q,
            size = d$a,
            type = 'scatter', mode = 'markers',
            marker = list(opacity = 0.4)) %>%
  add_trace(x = d1$EMoid,
            y = d1$H,
            color = d1$type,
            xaxis = 'x2', yaxis= 'y2',
            type = "scatter") %>%
  add_segments(x = 0.05, xend = 0.05,
               y = 0, yend = 22,
               showlegend = FALSE,
               marker = list(color = "blue", size = 0, opacity = 0),
               line = list(color = "white", dash = "dash")) %>%
  add_segments(x = 0, xend = 0.05,
               y = 22, yend = 22,
               showlegend = FALSE,
               marker = list(color = "blue", size = 0, opacity = 0),
               line = list(color = "white", dash = "dash")) %>%
  layout(plot_bgcolor = background,
         paper_bgcolor = background,
         font = list(color = "white"),
         xaxis = list(zerolinecolor = "white"),
         xaxis2 = list(domain = c(0.6, 0.95), anchor='y2'),
         yaxis2 = list(domain = c(0.6, 0.95), anchor='x2'),
         yaxis = list(zerolinecolor = "white", range = c(10, 35)),
         margin = list(t = 20, b = 20, l = 20, r = 20, pad = 4))

plot_ly(data = df[sample(1:nrow(df), size = 4000),],
        y = ~ a,
        x = ~ q,
        color = ~type,
        size = ~ Q,
        type = 'scatter', mode = 'markers',
        marker = list(opacity = 0.4)) %>%
  layout(plot_bgcolor = background,
         paper_bgcolor = background,
         font = list(color = "white"),
         xaxis = list(zerolinecolor = "white"),
         margin = list(t = 20, b = 20, l = 20, r = 20, pad = 4))







plot_ly(data = df[sample(1:nrow(df), size = 3000),],
        y = ~ a,
        x = ~ q,
        color = ~type,
        size = ~ Q,
        # type = 'scatter', mode = 'markers',
        marker = list(opacity = 0.4)) %>%
  layout(plot_bgcolor = background,
         paper_bgcolor = background,
         font = list(color = "white"),
         xaxis = list(zerolinecolor = "white"),
         margin = list(t = 20, b = 20, l = 25, r = 20, pad = 4))




# For testing, sample data
d <- df[sample(1:nrow(df), size = 1000),] %>%
  as.data.frame()

# Full data
d <- df %>%
  as.data.frame()


# Final plot

plot_ly()  %>%
  add_trace(y = d$H,
            x = d$EMoid,
            color = d$q,
            size = d$a,
            type = 'scatter', mode = 'markers',
            marker = list(opacity = 0.35)) %>%
  add_trace(y = c(10, 10, 10),
            x = c(0.52, 0.545, 0.57),
            color = 1.3,
            size = c(0.5, 2.5, 5),
            showlegend = FALSE,
            type = 'scatter', mode = 'markers',
            marker = list(opacity = 1)) %>%
  add_segments(x = 0.05, xend = 0.05,
               y = 0, yend = 22,
               showlegend = FALSE,
               marker = list(color = "blue", size = 0, opacity = 0),
               line = list(color = "white", dash = "dash")) %>%
  add_segments(x = 0, xend = 0.05,
               y = 22, yend = 22,
               showlegend = FALSE,
               marker = list(color = "blue", size = 0, opacity = 0),
               line = list(color = "white", dash = "dash")) %>%
  layout(plot_bgcolor = background,
         paper_bgcolor = background,
         font = list(color = "white"),
         xaxis = list(zerolinecolor = "white",
                      title = "Earth Minimum Orbit Intersection Distance (MOID)",
                      range = c(0, 0.6)),
         yaxis = list(zerolinecolor = "white",
                      title = "Absolute Magnitude (H)",
                      range = c(8, 32.5)),
         margin = list(t = 50, b = 110, l = 25, r = 20, pad = 4)) %>%
  add_annotations(text = "Near Earth Objects (NEOs)",
                  font = list(color = "white",
                              size = 36,
                              family = "Ariel"),
                  align = "center",
                  x = 0.5,
                  y = 1.05,
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE) %>%
  add_annotations(text = paste("NEOs are asteroids and comets with perihelion distance q less than 1.3 au.",
                               "Near-Earth Comets (NECs) are further restricted to include only short-period",
                               "comets (i.e., orbital period P less than 200 years). The vast majority of",
                               "NEOs are asteroids, referred to as Near-Earth Asteroids (NEAs). NEAs are",
                               "divided into groups (Atira, Aten, Apollo, and Amor) according to their perihelion",
                               "distance (q), aphelion distance (Q), and their semi-major axes (a).<sup>1</sup>",
                               sep = "<br>"),
                  font = list(color = "white",
                              size = 18,
                              family = "Ariel"),
                  align = "left",
                  x = 0.43,
                  y = 0.65,
                  xref = "paper",
                  yref = "paper",
                  showarrow = TRUE,
                  arrowhead = arrowhead,
                  arrowsize = arrowsize,
                  arrowwidth = arrowwidth,
                  arrowcolor = "white",
                  xanchor = "left",
                  yanchor = "bottom",
                  ax = 90,
                  ay = -50) %>%
  add_annotations(text = paste("Potentially Hazardous Asteroids (PHAs) are currently defined based on parameters",
                               "that measure the asteroid's potential to make threatening close approaches to the",
                               "Earth. Specifically, all asteroids with an Earth Minimum Orbit Intersection Distance",
                               "(MOID) of 0.05 au or less and an absolute magnitude (H) of 22.0 or less are considered",
                               "PHAs. In other words, asteroids that can't get any closer to the Earth (i.e., MOID)",
                               "than 0.05 au (roughly 7,480,000 km or 4,650,000 mi) or are smaller than about 140 m",
                               " (~500 ft) in diameter (i.e., H = 22.0 with an assumed albedo of 14%) are not considered PHAs.<sup>1</sup>",
                               sep = "<br>"),
                  font = list(color = "white",
                              size = 18,
                              family = "Ariel"),
                  align = "left",
                  x = 0.06,
                  y = 0.38,
                  xref = "paper",
                  yref = "paper",
                  showarrow = TRUE,
                  arrowhead = arrowhead,
                  arrowsize = arrowsize,
                  arrowwidth = arrowwidth,
                  arrowcolor = "white",
                  xanchor = 'left',
                  yanchor = 'top',
                  ax = 70,
                  ay = 50) %>%
  add_annotations(text = paste("Perihelion",
                               "Distance (q)",
                               sep = "<br>"),
                  font = list(color = "white",
                              size = 14,
                              family = "Ariel"),
                  align = "center",
                  x = 1.07,
                  y = 1.04,
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE) %>%
  add_annotations(text = "Semi-major axes (a) scale",
                  font = list(color = "white",
                              size = 14,
                              family = "Ariel"),
                  align = "center",
                  x = 0.97,
                  y = 0.11,
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE) %>%
  add_annotations(text = "0.5",
                  font = list(color = "white",
                              size = 14,
                              family = "Ariel"),
                  align = "center",
                  x = 0.875,
                  y = 0.035,
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE) %>%
  add_annotations(text = "2.5",
                  font = list(color = "white",
                              size = 14,
                              family = "Ariel"),
                  align = "center",
                  x = 0.916,
                  y = 0.035,
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE) %>%
  add_annotations(text = "5",
                  font = list(color = "white",
                              size = 14,
                              family = "Ariel"),
                  align = "center",
                  x = 0.955,
                  y = 0.035,
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE) %>%
  add_annotations(text = paste("<sup>1</sup> NEO Basics - NASA Center for Near-Earth Object Studies (CNEOS)",
                               paste("#30DayChartChallenge Day 14",
                                     "Viz: Rami Krispin",
                                     "Data: NASA Center for Near-Earth Object Studies (CNEOS)",
                                     sep = " | "),
                               sep = "<br>"),
                  font = list(color = "white",
                              size = 13,
                              family = "Ariel"),
                  align = "left",
                  x = 0.01,
                  y = -0.15,
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE)
