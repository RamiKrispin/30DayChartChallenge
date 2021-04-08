# Ploting the dist of endangered species in CA
# Data source: US Fish and Wildlife Service
# https://www.fws.gov/endangered/
library(plotly)
library(dplyr)

# Loading the data
df <- read.csv("day8/species-listings-by-state-report.csv", stringsAsFactors = FALSE)

str(df)
head(df)
nrow(df)
table(df$Group)
table(df$ESA.Listing.Status)
table(df$Region)
table(df$Where.Listed)


# Ploting treemap
plot_ly(
  data = df %>% group_by(Group) %>%
    summarize(total = n()) %>%
    mutate(parent = ""),
  type = 'treemap',
  labels = ~ Group,
  parents = ~ parent,
  values = ~ total,
  textinfo = "label+value+percent parent+percent",
  domain = list(column=0)) %>%
  layout(title = "Distribution of Endangered Species in California by Type",
         annotations = list(text = "Source: US Fish and Wildlife Service",
                            x = 0.01,
                            y = -0.02,
                            font = list(size = 12),
                            xref = "paper",
                            yref = "paper",
                            showarrow = FALSE),
         margin = list(t = 30, b = 20, l = 10, r = 10, pad = 2))
