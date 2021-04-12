library(USgrid)
library(plotly)
library(lubridate)
library(dplyr)

# Function for converting hex to rgb
hex_to_rgb <- function(hex){
  rgb <- paste0(as.numeric(grDevices::col2rgb(hex) %>%
                             base::t()), collapse = ",")
  return(rgb)
}

# Loading the data
data("Cal_elec")
head(Cal_elec)
max(Cal_elec$date_time)
unique(Cal_elec$operator)

# Reformatting the data
df <- Cal_elec %>%
  filter(operator == "Total") %>%
  mutate(date = as.Date(date_time),
         year = year(date_time)) %>%
  filter(year == 2020) %>%
  as.data.frame() %>%
  group_by(date) %>%
  summarise(min_day = round(min(series, na.rm = TRUE), 2),
            max_day = round(max(series, na.rm = TRUE), 2),
            mean_day = round(mean(series, na.rm = TRUE), 2)) %>%
  mutate(year = year(date),
         yday = yday(date),
         theta = (yday - 1) * 360 / 365)

head(df)
tail(df)
nrow(df)

# Plot setting
fcolor <- hex_to_rgb("#48cae4")
opacity <- 0.5
background <- "#fefae0"
arrowhead <- 4
arrowsize <- 1
arrowwidth <- 1

# Plot
plot_ly(
  type = 'scatterpolargl',
  mode = 'lines'
) %>%  add_trace(
  r = df$max_day,
  theta = df$theta,
  name = "Demand for Electricity",
  fill = 'tozeroy',
  fillcolor = paste("rgba(", fcolor, ",", opacity, ")", sep = ""),
  line = list(color = "black",
              width = 1)

) %>%
  layout(
    title = "California Demand for Electricity During 2020 - Daily Peak (MWh)",
    showlegend = F,
    margin = list(t = 60, b = 60,
                  l = 150, r = 85),
    paper_bgcolor = background,
    polar = list(
      bgcolor = background,
      angularaxis = list(
        rotation = 90,
        thetaunit = "degrees",
        direction = "clockwise",
        categoryorder = "array",
        tickmode = "array",
        tickwidth = 2,
        linewidth = 3,
        tickvals = c(0, 90, 180, 270),
        ticktext = c("Jan", "Apr", "Jul", "Oct"),
        layer = 'below traces'
      ),
      radialaxis = list(
        range = c(10000, 50000),
        side = 'counterclockwise',
        angle = 90,
        showline = T,
        linewidth = 1,
        tickwidth = 2,
        nticks = 4,
        gridcolor = 'gray',
        gridwidth = 1
      )
    )
  ) %>%
  add_annotations(text = paste("#30DayChartChallenge Day 11",
                               "Viz: Rami Krispin",
                               "Data: US Energy Information Administration",
                               sep = " | "),
                  font = list(color = "#1d3557",
                              size = 10,
                              family = family),
                  align = "left",
                  y = -0.13,
                  x = -0.32,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper") %>%
  add_annotations(text = paste("California hot Summer -",
                               "high demand for electricity",
                               "during the summer months of",
                               "the year, most likely derived from",
                               "high usage of air condition",
                               sep = "<br>"),
                  font = list(color = "#1d3557",
                              size = 12,
                              family = family),
                  align = "left",
                  y = 0.32,
                  x = 0.15,
                  showarrow = TRUE,
                  arrowhead = arrowhead,
                  arrowsize = arrowsize,
                  arrowwidth = arrowwidth,
                  arrowcolor = "#1d3557",
                  xanchor = 'center',
                  yanchor = 'middle',
                  ax = -90,
                  ay = 55,
                  yref = "paper",
                  xref = "paper") %>%
  add_annotations(text = paste("The demand during the",
                               "Fall/Winter months is",
                               "fairly stable and",
                               "consistent. You can",
                               "observe a nice day",
                               "of the week",
                               "seasonal effect",
                               sep = "<br>"),
                  font = list(color = "#1d3557",
                              size = 12,
                              family = family),
                  align = "right",
                  y = 0.70,
                  x = 0.67,
                  showarrow = TRUE,
                  arrowhead = arrowhead,
                  arrowsize = arrowsize,
                  arrowwidth = arrowwidth,
                  arrowcolor = "#1d3557",
                  xanchor = 'left',
                  yanchor = 'middle',
                  ax = 60,
                  ay = -40,
                  yref = "paper",
                  xref = "paper")

