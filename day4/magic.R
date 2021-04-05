# Plotting the the demographics of
# Harry Potter characters across all books with Sankey diagram
# Data source: https://www.kaggle.com/gulsahdemiryurek/harry-potter-dataset

library(readr)
library(dplyr)
library(sfo)
library(plotly)

# Loading the data
raw <- readr::read_delim("day4/Characters.csv",
                         ";", escape_double = FALSE, col_types = cols(Job = col_character()),
                         trim_ws = TRUE)

# Reformatting and cleaning
raw$student <- ifelse(raw$Job == "Student", "Student", "Not Student")
raw$student <- ifelse(is.na(raw$Job), "Not Student", raw$student)
raw$eye_color <- gsub("\xa0", "", raw$`Eye colour`)
raw$eye_color <- gsub("\\[", "", raw$eye_color)
raw$eye_color <- ifelse(is.na(raw$eye_color), "Unknown Eye Color", raw$eye_color)
raw$blood <- gsub("\xa0", "", raw$`Blood status`)
raw$blood <- gsub("\\[", "", raw$blood)
raw$blood <- ifelse(is.na(raw$blood) | raw$blood == "Unknown", "Unknown Blood Status", raw$blood)


# Grouping
df <- raw %>%
  dplyr::filter(!is.na(Gender)) %>%
  dplyr::mutate(house = ifelse(is.na(House), "Unknown", House),
                patronus = ifelse(is.na(Patronus), "Unknown Patronus", Patronus),
                hair = ifelse(is.na(`Hair colour`), "Unknown Colour", `Hair colour`),
                dead = ifelse(is.na(Death), "Live", "Dead")) %>%
  dplyr::group_by(Gender, student, house, patronus, eye_color, dead,blood) %>%
  dplyr::summarise(count = dplyr::n(),
                   .groups = "drop") %>%
  as.data.frame()

names(df) <- tolower(names(df))

# Plotting
backgroud_color <- "#f0efeb"
sankey_ly(df, cat_cols = c("student","gender","house", "eye_color", "blood", "dead"),
          num_col = "count",
          title = "Harry Potter Books Characters Demographics (student, gender, house, eye color, blood type, and life status)") %>%
  layout(plot_bgcolor = backgroud_color,
         paper_bgcolor = backgroud_color,
         xaxis = list(showgrid = F, zeroline = F, showticklabels = F),
         yaxis = list(showgrid = F, zeroline = F, showticklabels = F),
         font = list(
           size = 14,
           color = 'black'
         ),
         annotations = list(
           x = 0.5,
           y = -0.05,
           text = "Data Source: https://www.kaggle.com/gulsahdemiryurek/harry-potter-dataset",
           xref = "paper",
           yref = "paper",
           font = list(size = 16),
           showarrow = FALSE),
         margin = list(t = 50, b = 70))

