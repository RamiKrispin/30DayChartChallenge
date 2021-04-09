# Based on this example - https://jkunst.com/highcharter/articles/fontawesome.html
# Data from the USgrid package - https://github.com/RamiKrispin/USgrid
# Icons from - https://fontawesome.com/icons?d=gallery&p=2


library(fontawesome)
library(highcharter)
library(stringr)
library(dplyr) # to wokr with list columns
library(purrr) # to wokr with list columns
library(USgrid)

# Data
data("US_source")

head(US_source)


fa_to_png_to_datauri <- function(name, ...) {

  tmpfl <- tempfile(fileext = ".png")

  fontawesome::fa_png(name, file = tmpfl, ...)

  knitr::image_uri(tmpfl)

}

# Setting icon to data mapping

icon_map <- data.frame(source = c("coal", "hydro", "natural gas", "nuclear",
                                  "other", "petroleum", "solar", "wind"),
                       label = c("Coal", "Hydro", "Natural Gas", "Nuclear",
                                 "Other", "Petroleum", "Solar", "Wind"),
                       faico = c("fire-alt",  "water","burn","radiation-alt", "question",
                                 "oil-can", "sun", "fan"),
                       col = c("#d62828", "#1d3557", "#2a9d8f", "#2b2d42",
                               "#9bf6ff", "black", "orange", "#a8dadc"),
                       stringsAsFactors = FALSE)


# Aggregate the data by energy source and add the icons data
df <- US_source %>%
  dplyr::mutate(year = lubridate::year(date_time)) %>%
  dplyr::filter(year == 2020) %>%
  as.data.frame() %>%
  dplyr::group_by(source) %>%
  dplyr::summarise(total = sum(series, na.rm = TRUE)) %>%
  dplyr::mutate(perc =  100 * total / sum(total)) %>%
  dplyr::left_join(icon_map, by = "source") %>%
  dplyr::mutate(
    uri = map2_chr(faico, col, ~fa_to_png_to_datauri(.x, fill = .y)),
    marker = map(uri, ~ list(symbol = str_glue("url({data_uri})", data_uri = .x)))
  ) %>%
  dplyr::mutate(perc_floor = floor(perc),
                reminder = perc - perc_floor,
                perc_round = perc) %>%
  dplyr::arrange(-reminder)
df



# Round the percentage
df[, c("perc", "perc_floor", "reminder", "perc_round")]

for(i in 1:nrow(df)){
  if(df$reminder[i] > 0){
    if(df$perc_round[i] %% 1 != 0){
      df$perc_round[i] <- df$perc_floor[i] + 1
      df$reminder[i] <- 0
      total_reminder <- 100 - sum(df$perc_round)
      for(l in nrow(df):1){
        if(df$perc_round[l] %% 1 != 0 && total_reminder != 0){
          if(df$reminder[l] > abs(total_reminder)){
            df$perc_round[l] <- df$perc_round[l] + total_reminder
            total_reminder <- 0
          } else if(df$reminder[l] < abs(total_reminder)){
            df$perc_round[l] <- df$perc_round[l] - df$reminder[l]
            total_reminder <- total_reminder + df$reminder[l]
            df$reminder[l] <- 0
          }
        }
      }
    }
  }
}


df$perc_round[5] <- 38
df$perc_round[6] <- 7
df$perc_round <- round(df$perc_round)
sum(df$perc_round)

df <- df %>% dplyr::arrange(-perc_round)

# Plot

hchart(
  df,
  "item",
  hcaes(name = source, y = perc_round),
  name = "Energy Source",
  showInLegend = TRUE
) %>%
  hc_plotOptions(
    # avoid hide series due bug
    series = list(point = list(events = list(legendItemClick = JS("function(e) {e.preventDefault() }"))))
  ) %>%
  hc_legend(
    labelFormat =  '{name} <span style="opacity: 0.5">{y}%</span>'
  ) %>%
  hc_colors(pull(df, col)) %>%
  hc_title(text = "The US Electrcity Generation Dist by Energy Source During 2020")

