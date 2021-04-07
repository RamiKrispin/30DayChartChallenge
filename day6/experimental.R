# Calculating the residual distribution for the US electricity forecast
# The residuals are used to create prediction interval for the forecast
# The US demand electricity forecast available here:
# https://ramikrispin.github.io/USelectricity/

set.seed(123456)
library(dplyr)
library(plotly)

# Loading functions
source("https://raw.githubusercontent.com/RamiKrispin/USelectricity/main/functions/forecast.R")

# Model parameters
alpha <- 0.66
lambda <- 0.0005
cores <- parallel::detectCores()
lags <- c(1:24, 48, 72, 168)
# Set the number of simulations
# 100 may take some time to run... 30-40 will also provides nice dist
# Saved simulation available on ./day6/res_df.rda
n <- 100
# Loading rda file from github
url <- "https://github.com/RamiKrispin/USelectricity/blob/main/data/elec_df.rda?raw=true"
temp <- tempfile()
download.file(url, temp)
load(temp)


# Data prep
df <- elec_df %>%
  dplyr::filter(type == "demand") %>%
  dplyr::select(date_time, y = series) %>%
  as.data.frame() %>%
  dplyr::mutate(time = lubridate::with_tz(time = date_time, tzone = "US/Eastern")) %>%
  dplyr::arrange(time) %>%
  dplyr::select(time, y)



start <- seq.POSIXt(from = as.POSIXct("2020-12-15 01:00:00", tz = "US/Eastern"),
                    by = "24 hours",
                    length.out = n)


fc_sim <- parallel::mclapply(seq_along(start),
                             mc.cores = cores,
                             mc.preschedule = FALSE,
                             mc.cleanup = TRUE,
                             mc.silent = FALSE,
                             function(i){
                               port <- 9000 + i * 3
                               x <- system(command = paste("netstat -taln | grep", port, sep = " "), intern = TRUE)
                               if(length(x) != 0){
                                 cat("\033[0;93mport", port, "is not available\033[0m\n")

                                 port <- 9000 + i * 3 + 100
                               }
                               fc <- NULL
                               c <- 3
                               while(c > 0){
                                 tryCatch(
                                   fc <- glm_fc(data = df %>%
                                                  dplyr::filter(time < start[i]),
                                                y = "y",
                                                date_time = "time",
                                                alpha = alpha,
                                                lambda = lambda,
                                                lags = lags,
                                                trend = TRUE,
                                                seasonal = list(hour = TRUE,
                                                                yday = TRUE,
                                                                wday = TRUE,
                                                                month = TRUE,
                                                                year = TRUE),
                                                port = port,
                                                max_mem_size = "1G",
                                                h = 72),
                                   error = function(c){
                                     base::message(paste("Error,", c, sep = " "))
                                   }
                                 )
                                 if(class(fc) != "try-error"){
                                   c <- 0
                                 } else {
                                   c <- c - 1
                                 }
                               }
                               if(is.null(fc) || class(fc) != "list"){
                                 fc <- start[i]
                               }
                               return(fc)
                             })


# Recalculate models that failed
for(i in 1:length(fc_sim)){
  if(!is.list(fc_sim[[i]])){
    fc <- NULL
    fc <- glm_fc(data = df %>%
                   dplyr::filter(time < start[i]),
                 y = "y",
                 date_time = "time",
                 alpha = alpha,
                 lambda = lambda,
                 lags = lags,
                 trend = TRUE,
                 seasonal = list(hour = TRUE,
                                 yday = TRUE,
                                 wday = TRUE,
                                 month = TRUE,
                                 year = TRUE),
                 port = 9000 +i*4,
                 max_mem_size = "1G",
                 h = 72)
    if(is.list(fc)){
      fc_sim[[i]] <- fc
    }


  }
}



# Polling the residuals
res_df <- lapply(1:length(fc_sim), function(i){
  fc <- fc_sim[[i]]$forecast %>% dplyr::select(time, yhat, index) %>%
    dplyr::mutate(index = index - min(index) + 1) %>%
    dplyr::left_join(df, by = "time") %>%
    dplyr::mutate(res = y - yhat,
                  label = as.Date(min(time)))


  return(fc)
}) %>% dplyr::bind_rows()

head(res_df)

# Saving the results
save(res_df, file = "./day6/res_df.rda")

# Plotting the residuals dist
plot_ly(data = res_df) %>%
  add_trace(x = ~ index,
            y = ~ res,
            marker = list(
              opacity = 0.2,
              color = "#90e0ef",
              size = 6)) %>%
  layout(title = "US Electricity Demand Forecast - Residuals Distribution (Simulated)",
         plot_bgcolor = "black",
         paper_bgcolor = "black",
         annotations = list(text = paste("Model: GLM",
                                         "Horizon: 72 Hr",
                                         "Num. of Sims: 100",
                                         sep = "\n"),
                            align = "left",
                            x = 0.05,
                            y = 0.99,
                            font = list(size = 10),
                            xref = "paper",
                            yref = "paper",
                            showarrow = FALSE),
         font = list(color = "white"),
         yaxis = list(title = "Residuals (Megawatt)",font = list(size = 10)),
         xaxis = list(title = "Forecast Horizon (Hours)", font = list(size = 20)),
         margin = list(t = 50, b = 80),
         legend = list(x = 0.05, y = 0.95))
