library(coronavirus)
library(plotly)
library(dplyr)
df <- coronavirus::refresh_coronavirus_jhu()
head(df)


label_size <- 9

df %>%

  dplyr::group_by(location, data_type) %>%
  dplyr::summarise(total_cases = sum(value),
                   .groups = "drop") %>%
  tidyr::pivot_wider(names_from = data_type, values_from = total_cases) %>%
  dplyr::arrange(- cases_new) %>%
  dplyr::filter(recovered_new > 0) %>%
  dplyr::slice_head(n = 100) %>%
  dplyr::mutate(recover_rate = recovered_new / cases_new,
                death_rate = deaths_new / cases_new) %>%
  dplyr::filter(recover_rate > 0.6) %>%
  dplyr::mutate(recover_rate = dplyr::if_else(is.na(recover_rate), 0, recover_rate),
                death_rate = dplyr::if_else(is.na(death_rate), 0, death_rate)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(confirmed_normal = as.numeric(cases_new) / max(as.numeric(cases_new))) %>%
  plotly::plot_ly(y = ~ round(100 * recover_rate, 1),
                  x = ~ round(100 * death_rate, 1),
                  size = ~  log(cases_new),
                  sizes = c(5, 70),
                  type = 'scatter', mode = 'markers',
                  marker = list(color = "#90e0ef",
                                sizemode = 'diameter' ,
                                opacity = 0.45),
                  hoverinfo = 'text',
                  text = ~paste("</br>", location,
                                "</br> Confirmed Cases: ", cases_new,
                                "</br> Recovery Rate: ", paste(round(100 * recover_rate, 1), "%", sep = ""),
                                "</br> Death Rate: ",  paste(round(100 * death_rate, 1), "%", sep = ""))
  ) %>%
  plotly::layout(title = "Recovery / Death Ratio (Countries with More than 20,000 Cases)",
                 yaxis = list(title = "Recovery Rate", ticksuffix = "%"),
                 xaxis = list(title = "Death Rate", ticksuffix = "%",
                              dtick = 1,
                              tick0 = 0),
                 hovermode = "compare")



df %>%
  dplyr::group_by(location, data_type) %>%
  dplyr::summarise(total_cases = sum(value),
                   .groups = "drop") %>%
  tidyr::pivot_wider(names_from = data_type, values_from = total_cases) %>%
  dplyr::arrange(- cases_new) %>%
  dplyr::filter(recovered_new > 0) %>%
  dplyr::slice_head(n = 100) %>%
  dplyr::mutate(recover_rate = recovered_new / cases_new,
                death_rate = deaths_new / cases_new) %>%
  dplyr::filter(recover_rate > 0.6) %>%
  dplyr::mutate(recover_rate = dplyr::if_else(is.na(recover_rate), 0, recover_rate),
                death_rate = dplyr::if_else(is.na(death_rate), 0, death_rate)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(confirmed_normal = as.numeric(cases_new) / max(as.numeric(cases_new))) %>%
  dplyr::bind_rows(data.frame(location = c("s", "m", "l"), recover_rate = c(1, 1, 1), death_rate = c(0.08, 0.09, 0.1),
                              cases_new = c(1 * 10 ^ 5, 5 * 10 ^ 5, 1 * 10 ^ 6))) %>%
  plotly::plot_ly(y = ~ round(100 * recover_rate, 1),
                  x = ~ round(100 * death_rate, 1),
                  size = ~  log(cases_new),
                  sizes = c(5, 70),
                  type = 'scatter', mode = 'markers',
                  marker = list(color = "#90e0ef",
                                sizemode = 'diameter' ,
                                opacity = 0.45),
                  hoverinfo = 'text',
                  text = ~paste("</br>", location,
                                "</br> Confirmed Cases: ", cases_new,
                                "</br> Recovery Rate: ", paste(round(100 * recover_rate, 1), "%", sep = ""),
                                "</br> Death Rate: ",  paste(round(100 * death_rate, 1), "%", sep = ""))
  ) %>%
  plotly::layout(title = "Recovery / Death Ratio (Countries with More than 20,000 Cases)",
                 yaxis = list(title = "Recovery Rate", ticksuffix = "%",
                              zeroline = FALSE,
                              showline = FALSE),
                 xaxis = list(title = "Death Rate", ticksuffix = "%",
                              dtick = 1,
                              tick0 = 0,
                              zeroline = FALSE,
                              showline = FALSE),
                 margin = list(t = 60, b = 90),
                 hovermode = "compare") %>%
  add_annotations(text = paste("#30DayChartChallenge Day 10",
                               "Viz: Rami Krispin",
                               "Data: Johns Hopkins University Center for Systems Science and Engineering",
                               sep = " | "),
                  font = list(color = "#1d3557",
                              size = label_size,
                              family = family),
                  align = "left",
                  y = -0.13,
                  x = -0.03,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper") %>%
  add_annotations(text = paste("100,000",  "Cases",sep = "<br>"),
                  font = list(color = "#1d3557",
                              size = label_size,
                              family = family),
                  align = "center",
                  y = 0.87,
                  x = 0.77,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper") %>%
  add_annotations(text = paste("500,000",  "Cases",sep = "<br>"),
                  font = list(color = "#1d3557",
                              size = label_size,
                              family = family),
                  align = "center",
                  y = 0.87,
                  x = 0.86,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper") %>%
  add_annotations(text = paste("1,000,000",  "Cases",sep = "<br>"),
                  font = list(color = "#1d3557",
                              size = label_size,
                              family = family),
                  align = "center",
                  y = 0.87,
                  x = 0.95,
                  showarrow = FALSE,
                  yref = "paper",
                  xref = "paper") %>%
  add_annotations(text = "Singapore",
                  font = list(color = "#1d3557",
                              size = label_size,
                              family = family),
                  align = "center",
                  y = 100.5,
                  x = 0,
                  showarrow = FALSE,
                  yref = "y",
                  xref = "x") %>%
  add_annotations(text = "Mexico",
                  font = list(color = "#1d3557",
                              size = label_size,
                              family = family),
                  align = "center",
                  y = 100.5,
                  x = 0,
                  showarrow = FALSE,
                  yref = "y",
                  xref = "x")



