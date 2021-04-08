# Mapping meteorite landings in the US
# Data source - NASA database
# https://data.nasa.gov/Space-Science/Meteorite-Landings/gh4g-9sfh

library(data.table)
library(dplyr)
library(geojsonio)
library(sp)
library(leaflet)
# Pulling the data from the API
cmd <- "curl 'https://data.nasa.gov/resource/gh4g-9sfh.json?$limit=50000' | jq  -r '.[] | [.name, .nametype, .recclass, .mass, .fall, .year, .reclat, .reclong] | @csv'"

raw <- fread(cmd = cmd,
             col.names = c("name", "nametype", "recclass",
                           "mass", "fall", "year_temp", "lat", "long"))

# Reformatting the year
df <- raw
df[, year := as.numeric(substr(year_temp, start = 1, stop = 4))]
df$year_temp <- NULL
df <- df %>% dplyr::filter(!is.na(mass),
                           !is.na(year))

str(df)
table(is.na(df$year))
hist(df$mass)
table(df$recclass)
df


# Filtering for US data (lower 48)
# Source - https://stackoverflow.com/questions/51266193/how-do-i-automatically-determine-a-state-given-the-latitude-and-longitude-coor
# get usa polygon data
# http://eric.clst.org/tech/usgeojson/
usa <- geojson_read(
  "http://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_500k.json",
  what = "sp"
)

df$state <- NA

# taking few mins to run this loop (probably would be faster with lapply)
for (i in 1:nrow(df)) {
  coords <- c(df$long[i], df$lat[i])
  if(any(is.na(coords))) next
  point <- sp::SpatialPoints(
    matrix(
      coords,
      nrow = 1
    )
  )
  sp::proj4string(point) <- sp::proj4string(usa)
  polygon_check <- sp::over(point, usa)
  df$state[i] <- as.character(polygon_check$NAME)
}

df

# Filter for US (lower 48)
df1 <- df %>% dplyr::filter(!is.na(state),
                            !is.na(mass),
                            !state %in% c("Hawaii","Alaska"))


head(df1)
sort(table(df1$state))

# Set the color bins
bpal <- colorBin("Reds", log(df1$mass), bins = 10)

# Plot the data
leaflet(data = df1) %>% addTiles() %>%
  addCircleMarkers(lng = ~ long,
                   lat = ~ lat,
                   opacity = 1,
                   color = ~ bpal(log(mass)),
                   fillColor = ~ bpal(log(mass)),
                   radius = ~ log(mass) / 1.4 ) %>%
  addLegend(position = "bottomright",
            pal = bpal,
            values = ~ log(mass),
            title = "Meteorite Mass <br> Log Scale",
            opacity = 1)

