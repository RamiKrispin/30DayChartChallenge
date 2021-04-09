library(USgas)
data("us_total")
# Set labels
ne <- c("Connecticut", "Maine", "Massachusetts",
        "New Hampshire", "Rhode Island", "Vermont")

# Filter the data
ne_gas <-  us_total[which(us_total$state %in% ne),]

# Transform to wide format
ne_wide <- reshape(ne_gas, v.names = "y", idvar = "year",
                   timevar = "state", direction = "wide")
ne_wide <- ne_wide[order(ne_wide$year), ]

names(ne_wide) <- c("year",ne)

# Set the y and x axis ticks:
at_x <- seq(from = 2000, to = 2020, by = 5)

at_y <- pretty(ne_gas$y)[c(2, 4, 6)]

# plot the first series
plot(ne_wide$year, ne_wide$Connecticut,
     type = "l",
     col = "#073b4c",
     frame.plot = FALSE,
     axes = FALSE,
     panel.first = abline(h = c(at_y), col = "grey80"),
     main = "New England Annual Natural Gas Consumption by State",
     cex.main = 1.2, font.main = 1, col.main = "black",
     xlab = "Source: https://www.eia.gov/",
     font.axis = 1, cex.lab= 1,
     ylab = "Million Cubic Feet",
     ylim = c(min(ne_gas$y, na.rm = TRUE), max(ne_gas$y, na.rm = TRUE)),
     xlim = c(min(ne_gas$year), max(ne_gas$year) + 3))

# Add the 5 other series
lines(ne_wide$year, ne_wide$Maine, col = "#1f77b4")
lines(ne_wide$year, ne_wide$Massachusetts, col = "#118ab2")
lines(ne_wide$year, ne_wide$`New Hampshire`, col = "#06d6a0")
lines(ne_wide$year, ne_wide$`Rhode Island`, col = "#ffd166")
lines(ne_wide$year, ne_wide$Vermont, col = "#ef476f")

# Add the y and x axis ticks

mtext(side =1, text = format(at_x, digits=0, nsmall=0), at = at_x,
      col = "grey20", line = 1, cex = 0.8)

mtext(side =2, text = format(at_y, scientific = FALSE), at = at_y,
      col = "grey20", line = 1, cex = 0.8)

# Add text
text(max(ne_wide$year) + 2,
     tail(ne_wide$Connecticut,1),
     "Connecticut",
     col = "#073b4c",
     cex = 0.7)

text(max(ne_wide$year) + 2,
     tail(ne_wide$Maine,1) * 0.95,
     "Maine",
     col = "#1f77b4",
     cex = 0.7)

text(max(ne_wide$year) + 2,
     tail(ne_wide$Massachusetts,1),
     "Massachusetts",
     col = "#118ab2",
     cex = 0.7)

text(max(ne_wide$year) + 2,
     tail(ne_wide$`New Hampshire`,1) * 1.1,
     "New Hampshire",
     col = "#06d6a0",
     cex = 0.7)

text(max(ne_wide$year) + 2,
     tail(ne_wide$`Rhode Island`,1) * 1.1,
     "Rhode Island",
     col = "#ffd166",
     cex = 0.7)

text(max(ne_wide$year) + 2,
     tail(ne_wide$Vermont,1),
     "Vermont",
     col = "#ef476f",
     cex = 0.7)
