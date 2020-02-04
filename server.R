###########################################################
# Wellington Silva                                        #
# https://www.kaggle.com/gatunnopvp                       #
# https://github.com/WellingtonSilv                       #
# https://www.linkedin.com/in/wellington-silva-b430a1195/ #
###########################################################

# loading required libraries
library(shiny)
library(quantmod)
library(tidyverse)
library(hrbrthemes)
library(viridis)

# getting bitcoin historical data from yahoo finance
tickers <- c("BTCUSD=X")
startdate <- "2010-01-01"
endtdate <- Sys.Date()
getSymbols(tickers,src = "yahoo",from=startdate,to=endtdate)

# deating with dates
asset <- as.data.frame(`BTCUSD=X`[,1:4])
names(asset) <- c("open","low","high","close")
asset$data <- as.POSIXct(row.names(asset),tz="GMT",format="%Y-%m-%d")

# calculating bitcoin returns
asset$returns <- asset$close/asset$open-1

# removing extreme outliers that exist almost in only this data source
asset$returns[asset$returns>=1.5 | asset$returns<= -1.5] <- 0

# creating the server logic
server <- function(input, output){
  
  # rendering prices plot
  output$prices <- renderPlot({
    
    # selecting the start date and end date
    start <- as.POSIXct(input$dates[1],tz="GMT",format="%Y-%m-%d")
    end <- as.POSIXct(input$dates[2],tz="GMT",format="%Y-%m-%d")
    
    # selecting log prices based on start and end dates
    prices <- log(asset$close[asset$data >= start & asset$data <= end])
    
    # selecting dates based on start and end dates
    dates <- asset$data[asset$data >= start & asset$data <= end]
    
    # # creating a dataframe with dates and log prices
    data <- data.frame(dates, prices, check.names = T)
    
    # creating a line plot with prices over the time
    ggplot(data, aes(x = dates, y = prices)) +
      geom_line(color = input$colors_prices, lwd = 0.8) +
      labs(x = "Time", y = "Close prices") +
      ggtitle('Bitcoin Prices Over The Time')+
      theme(axis.text.x = element_text(angle = 45, hjust = 1)
            , axis.title = element_text(size = rel(1.5), angle = 1)
            , plot.title = element_text(size = rel(2))
            , axis.text =  element_text(size = rel(1.5))
            , axis.ticks = element_line(size = 1))
    
  })
  
  # rendering returns plot
  output$returns <- renderPlot({
    
    # selecting the start date and end date
    start <- as.POSIXct(input$dates[1],tz="GMT",format="%Y-%m-%d")
    end <- as.POSIXct(input$dates[2],tz="GMT",format="%Y-%m-%d")
    
    # selecting dates based on start and end dates
    dates <- asset$data[asset$data >= start & asset$data <= end]
    
    # selecting returns based on start and end date
    returns <- asset$returns[asset$data >= start & asset$data <= end]
    
    # creating a dataframe with dates and returns
    data <- data.frame(dates, returns, check.names = T)
    
    # creating a line plot with returns over the time
    ggplot(data, aes(x = dates, y = returns)) +
      geom_line(color = input$colors_returns, lwd = 0.8) +
      labs(x = "Time", y = "Returns") +
      ggtitle('Bitcoin Returns Over The Time')+
      theme(axis.text.x = element_text(angle = 45, hjust = 1)
            , axis.title = element_text(size = rel(1.5), angle = 1)
            , plot.title = element_text(size = rel(2))
            , axis.text =  element_text(size = rel(1.5))
            , axis.ticks = element_line(size = 1))
  })
  
  # rendering boxplots
  output$boxplot <- renderPlot({
    
    # selecting the start date and end date
    start <- as.POSIXct(input$dates[1],tz="GMT",format="%Y-%m-%d")
    end <- as.POSIXct(input$dates[2],tz="GMT",format="%Y-%m-%d")
    
    # selecting returns based on start and end date
    returns <- asset$returns[asset$data >= start & asset$data <= end]
    
    # seeing what days we have a Bull or a Bear market
    side <- ifelse(returns>0,"Bull","Bear")
    
    # creating a dataframe with "Bull, Bear" information and returns
    data <- data.frame(side, returns, check.names = T)
    
    # creating a boxplot with returns by side
    ggplot(data, aes(x=side, y=returns, fill=side)) +
      geom_boxplot() +
      scale_fill_viridis(discrete = TRUE, alpha=0.6)+
      ggtitle("Returns 'Bull and Bear' distributions and outliers") +
      xlab("")+
      theme_ipsum() +
      theme(
        legend.position="none"
        , axis.title = element_text(size = rel(1.5), angle = 1)
        , plot.title = element_text(size = rel(2))
        , axis.text =  element_text(size = rel(1.5))
        , axis.ticks = element_line(size = 1)
      )
  })
  
  # rendering histogram plot
  output$hist <- renderPlot({
    
    # selecting the start and end date
    start <- as.POSIXct(input$dates[1],tz="GMT",format="%Y-%m-%d")
    end <- as.POSIXct(input$dates[2],tz="GMT",format="%Y-%m-%d")
    
    # selecting returns based on start and end dates
    returns <- asset$returns[asset$data >= start & asset$data <= end]
    
    # creating a histogram with returns distribution
    hist(returns
         , breaks = input$breaks
         , main = "Returns Distribution"
         , col = input$colors_hist
         , xlab = "Returns"
         , ylab = "Frequency"
         , cex.lab=1.2
         , cex.axis=1.1
         , cex.main=1.7
         , cex.sub=1.1)
  })
  
}
# end of server