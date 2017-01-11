#loads Shiny package
library(shiny)
#loads library for change points
library(cpm)
#loads library for technical indicators
library(TTR)
#loads package for forecasts
library(forecast)
#load package for data tables
library(DT)
#loads library for different Shiny themes
library(shinythemes)






shinyServer(function(input, output){
  
  ###create reactive dataset for stock symbol
  stock_data = reactive({
  stock_symbol = as.character(input$stock_symbol)
  ###get stock data directly from Yahoo
  ###http://stackoverflow.com/questions/3507744/downloading-yahoo-stock-prices-in-r
  stock_URL = paste(URL <- "http://ichart.finance.yahoo.com/table.csv?s=", stock_symbol, sep = "")
  stock_data = read.csv(stock_URL)
  ###convert factor to Date, reverse levels
  stock_data$DATE = as.Date(stock_data$Date)
  ###subset for date range input
  stock_data  = subset(stock_data, stock_data$DATE >= input$date_range[1]
                       & stock_data$DATE <= input$date_range[2]
                       )
  ###order by date ascending
  stock_data = stock_data[order(stock_data$DATE), ]
  ###create index for subsetted data set
  stock_data$t = 1:nrow(stock_data)
  ###detect change points
  #cpm = processStream(stock_data$Close, cpmType = input$cpm_type, startup = input$startup)
  cpm = processStream(stock_data$Close, cpmType = input$cpm_type, startup = input$startup)
  change_points = data.frame("t" = cpm$changePoints,
                        "Change.Point" = cpm$changePoints)
  detection_times = data.frame("t" = cpm$detectionTimes,
                         "Detection.Time" = cpm$detectionTimes)
  ###left join change points and detection times to original data set
  stock_data2 = merge(stock_data, change_points, by = "t", all.x = TRUE)
  stock_data3 = merge(stock_data2, detection_times, by = "t", all.x = TRUE)
  ###add dates of change points and detection times
  stock_data3$Change.Point.Date = ifelse(is.na(stock_data3$Change.Point), NA, stock_data3$DATE) 
  stock_data3$Detection.Date = ifelse(is.na(stock_data3$Detection.Time), NA, stock_data3$DATE)
  ###add Bollinger Bands
  Bollinger.Bands = BBands(stock_data3[,"Close"], n = input$periods, sd = 2)
  Bollinger.Bands = data.frame(Bollinger.Bands)
  stock_data3$Bollinger.Band.Upper = Bollinger.Bands$up
  stock_data3$Bollinger.Band.Lower = Bollinger.Bands$dn
  ###add exponentially-weighted moving average and volume-weighted moving average
  WW.EWMA = data.frame("Welles.Wilder.EWMA" = EMA(stock_data3$Close, n = input$periods, wilder = TRUE))
  Volume.WMA = data.frame("Volume.Weighted.Moving.Avg" = VWMA(stock_data3$Close, stock_data3$Volume, n = input$periods))
  stock_data3$Welles.Wilder.EWMA = WW.EWMA$Welles.Wilder.EWMA
  stock_data3$Volume.Weighted.Moving.Avg = Volume.WMA$Volume.Weighted.Moving.Avg
  ###add tslm forecast, 7 day forecast
  ###transformations of t
  stock_data3$t2 = stock_data3$t ^ 2
  stock_data3$t3 = stock_data3$t ^ 3
  stock_data3$sin.t = sin(stock_data3$t)
  stock_data3$cos.t = cos(stock_data3$t)
  stock_data3$exp.t = exp(stock_data3$t)
  stock_data3$log.t = log(stock_data3$t)
  #progress bar
  withProgress(message = 'Remixing data...',
               detail = 'Please wait...', value = 0, {
                 for (i in 1:15) {
                   incProgress(1/15)
                   Sys.sleep(0.25)
                 }
               })
  ###optimal Box Cox transformation of y
  lambda = BoxCox.lambda(stock_data3$Close)
  stock_data3$trans.Close = BoxCox(stock_data3$Close, lambda)
  ###begin tslm model
  attach(stock_data3)
  TSLM = lm(trans.Close ~ t + t2 + t3 + sin.t + cos.t + exp.t + log.t)
  ###stepwise regression
  TSLM2 = step(TSLM)  
  ###append 7 dates and create data set
  stock_data3$Forecast = NA
  forecast_data = data.frame(matrix(nrow = 0, ncol = ncol(stock_data3)))
  names(forecast_data) = names(stock_data3)
  max_DATE = max(stock_data3$DATE)
  max_t = max(stock_data3$t)
  forecast_data2 = data.frame(
                        cbind(
                        "t" = seq(from = max_t + 1, by = 1, length.out=7),
                        "DATE" = seq(from = max_DATE + 1, by = 1, length.out = 7)
                         )
                            )
  forecast_data2$DATE = as.Date(forecast_data2$DATE)
  forecast_data3 = merge(forecast_data, forecast_data2, all.y=TRUE)
  ###transformations of t
  forecast_data3$t2 = forecast_data3$t ^ 2
  forecast_data3$t3 = forecast_data3$t ^ 3
  forecast_data3$sin.t = sin(forecast_data3$t)
  forecast_data3$cos.t = cos(forecast_data3$t)
  forecast_data3$exp.t = exp(forecast_data3$t)
  forecast_data3$log.t = log(forecast_data3$t)
  ###begin forecast
  attach(forecast_data3)
  Forecast = forecast.lm(TSLM2, newdata = forecast_data3, h = 7, lambda = lambda)
  ###add forecast
  forecast_data3$Forecast = Forecast$mean
  ###append to original data set
  stock_data4 = rbind(stock_data3, forecast_data3)
  stock_data4
  })
  
  ###generate stock price chart
  output$stock_chart = renderPlot({
    ###read in reactive dataset
    stock_data = stock_data()
    ###begin line chart
    ###define upper and lower limits of Bollinger Bands with slack
    lim_data = subset(stock_data, Bollinger.Band.Upper != "NA")
    lowerlim = min(lim_data$Bollinger.Band.Lower) * .95
    upperlim = max(lim_data$Bollinger.Band.Upper) * 1.05
    ###background color change and text color
    par(bg = "#2A343F", col = "white", col.lab = "white", col.axis = "white", family = "sans")
    ###line chart with line width = 1, horizonal y-axis
    p = plot(x = stock_data$DATE, y = stock_data$Close, type = "l", col = "#FB8E1E", lwd = 1, las = 1, 
         ylim = c(lowerlim, upperlim), xlab = "Date", ylab = "Close Price $")
    ###add points
    p = p + points(x = stock_data$DATE, y = stock_data$Close, col = "#FB8E1E", pch = 16)
    ###add grid
    p = p + grid(col = "lightgrey", lty = "dotted")
    ###add legend, define line types for legend and line width, number of columns for legend, and legend box type(bty)
    legend(x = "topleft", legend = c("Close Price", "Change Points"), col = c("#FB8E1E","#DA291C"), lty = c(1,2), 
                                                                              lwd = c(2,2), bty = "n")
    ###add change points
    p = p + abline(v = stock_data$Change.Point.Date, lty = "dotted", col = "#DA291C", lwd = 3)
    ###add bollinger bands, Welles-Wilder Exponential Moving Average, Volume Weighted Moving Average, and Forecast
    ###based on checkboxInput
    p = p + ifelse(input$technical_indicators, p + lines(x = stock_data$DATE, y = stock_data$Bollinger.Band.Upper, col = "#2DCB93", lty = "dotted", lwd = 2) +
                     lines(x = stock_data$DATE, y = stock_data$Bollinger.Band.Lower, col = "#2DCB93", lty = "dotted", lwd = 2) + 
                     lines(x = stock_data$DATE, y = stock_data$Welles.Wilder.EWMA, col = "#668BFC", lty = "dotted", lwd = 2) + 
                     lines(x = stock_data$DATE, y = stock_data$Volume.Weighted.Moving.Avg, col = "#FC566A", lty = "dotted", lwd = 2) +
                     lines(x = stock_data$DATE, y = stock_data$Forecast, col = "#FB8E1E", lty = "dotted", lwd = 2),
                     p)
    p = ifelse(input$technical_indicators,
                     legend(x = "topleft", legend = c(
                       "Close Price", 
                       "Change Points",
                       "Bollinger Bands", 
                       "Wilder Exponential Moving Avg", 
                       "Volume-Weighted Moving Avg", 
                       "Forecast"
                     ),
                     col = c(
                       "#FB8E1E",
                       "#DA291C",
                       "#2DCB93",
                       "#668BFC",
                       "#FC566A",
                       "#FB8E1E"
                     ),
                     lty = c(1,2,2,2,2,2), lwd = 2, ncol = 2, bty = "n"),
                   p)
    p
  })
  
  
  ###generate data table of Change Points and Detection Times
  output$change_point_data = DT::renderDataTable({
    #progress bar
    withProgress(message = 'Remixing data...',
                 detail = 'Please wait...', value = 0, {
                   for (i in 1:15) {
                     incProgress(1/15)
                     Sys.sleep(0.25)
                   }
                 })
    stock_data = stock_data()
    stock_data = subset(stock_data, Change.Point.Date != "NA", select = c(Change.Point.Date, Close))
    stock_data$Change.Point.Date = as.Date(stock_data$Change.Point.Date)
    stock_data$Research = paste("<a target='_blank' href = 'http://www.bloomberg.com/quote/",
                                      input$stock_symbol,
                                     ":US",
                                       "'>What happened?</a>",
                                       sep = "")
    ###format Date and Close for DT
    stock_data$Close = round(stock_data$Close, 2)
    DT::datatable(stock_data, selection = c("single"),
                  ###disable searching and pagination, restore table state on page reload
                  options = list(searching = FALSE, paging = FALSE, stateSave = TRUE),
                  ###add caption
                  caption = "Dates that Close Price Statistically Changed based on Selected Model",
                  ###remove row names
                  rownames = FALSE,
                  ###make table columns responsive when page is too narrow
                  extensions = 'Responsive',
                  ###allow for HTML links for Google Map link
                  escape = FALSE) %>% formatCurrency('Close')
    
  })
  
  
  
  
  output$stockdata = DT::renderDataTable({
    stock_data = stock_data()
    stock_data = subset(stock_data, Close != "NA", select = c(DATE, Open, High, Low, Close, Volume, Adj.Close,
                                                              Bollinger.Band.Lower, Bollinger.Band.Upper,
                                                              Welles.Wilder.EWMA, Volume.Weighted.Moving.Avg))
    DT::datatable(stock_data, 
                  ###disable searching and pagination, restore table state on page reload
                  options = list(searching = FALSE, paging = FALSE, stateSave = TRUE),
                  ###add caption
                  caption = "Stock Price Data",
                  ###remove row names
                  rownames = FALSE,
                  ###make table columns responsive when page is too narrow
                  extensions = 'Responsive')
  })
  
  downloadable_stock_data = reactive({
    stock_data = stock_data()
    stock_data = subset(stock_data, Close != "NA", select = c(DATE, Open, High, Low, Close, Volume, Adj.Close,
                                                              Bollinger.Band.Lower, Bollinger.Band.Upper,
                                                              Welles.Wilder.EWMA, Volume.Weighted.Moving.Avg))
    stock_data
  })
  
  ###DOWNLOAD OPTION FOR STOCK DATA
  output$download_stock_data = downloadHandler(
    filename = function() { 
      paste(input$stock_symbol, "_stock_data", '.csv', sep='') 
    },
    content = function(file) {
      write.csv(downloadable_stock_data(), file, row.names = FALSE)
    }
  )
  
  
})