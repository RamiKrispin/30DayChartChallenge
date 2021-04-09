# Plotting the dist of landing at SFO airport
# Data sfo package sourced from SF Data portal
library(sfo)
library(dplyr)
library(plotly)


sfo_stats %>%
  filter(activity_period >= 201901 & activity_period < 202001,
         aircraft_manufacturer != "") %>%
  group_by(geo_region, landing_aircraft_type,
           aircraft_manufacturer, aircraft_model,
           aircraft_body_type) %>%
  summarise(total_landing = sum(landing_count),
            .groups = "drop") %>%
  sankey_ly(cat_cols = c("geo_region",
                         "landing_aircraft_type",
                         "aircraft_manufacturer",
                         "aircraft_model",
                         "aircraft_body_type"),
            num_col = "total_landing",
            title = "SFO Landing Summary by Geo Region and Aircraft Type During 2019") %>%
  add_annotations(text = "#30DayChartChallenge | Day 1",
                  font = list(color = "black",
                              size = 12),
                  align = "left",
                  x = 0,
                  y = 0,
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE) %>%
  add_annotations(text = paste("Rami Krispin"),
                  font = list(color = "gray",
                              size = 12),
                  align = "left",
                  x = 0,
                  y = -0.04,
                  xref = "paper",
                  yref = "paper",
                  showarrow = FALSE) %>%
  layout(margin = list(
    l = 20,
    r = 20,
    b = 20,
    t = 40,
    pad = 20
  ))
