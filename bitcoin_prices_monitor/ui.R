###########################################################
# Wellington Silva                                        #
# https://www.kaggle.com/gatunnopvp                       #
# https://github.com/WellingtonSilv                       #
# https://www.linkedin.com/in/wellington-silva-b430a1195/ #
###########################################################

# loading required libraris
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

# dealing with dates
asset <- as.data.frame(`BTCUSD=X`[,1:4])
names(asset) <- c("open","low","high","close")
asset$data <- as.POSIXct(row.names(asset),tz="GMT",format="%Y-%m-%d")

# creating the user interface
ui <- fluidPage(
  
  # defyning a title to our fluid page
  titlePanel("Bitcoin 'Real Time' Monitor"),
  
  # creating a sidebarLayout to put our input paramaters and plots
  sidebarLayout(
    
    # creating a sidebarPanel to our input parameters
    sidebarPanel(
      
      # creating dates input parameter
      dateRangeInput(inputId = "dates"
                     , label = "Select the period"
                     , start = head(asset$data,1)
                     , end = tail(asset$data,1)),
      
      # creating bins input parameter
      sliderInput(inputId = "breaks"
                  , label = "Choose number of bins to histogram"
                  , value = 25, min = 10, max = 100),
      
      # creating colors input parameters
      selectInput(inputId = "colors_prices"
                  , label = "Select prices color"
                  , choices = c("red","blue","green","darkblue"
                                ,"darkgreen","orange","black","darkred")
                  , selected = "darkgreen"
      ),
      
      # colors
      selectInput(inputId = "colors_returns"
                  , label = "Select returns color"
                  , choices = c("red","blue","green","darkblue"
                                ,"darkgreen","orange","black","darkred")
                  , selected = "blue"
      ),
      
      # colors
      selectInput(inputId = "colors_hist"
                  , label = "Select histogram color"
                  , choices = c("red","blue","green","darkblue"
                                ,"darkgreen","orange","black","darkred")
                  , selected = "red"
      ),
      
      # defyning panel width
      width = 3
    ),
    
    # creating the main panel to our plots
    mainPanel(
      
      # creating a space to our prices and returns
      fluidRow(
        
        splitLayout(cellWidths = c("50%", "50%")
                    , plotOutput("prices")
                    , plotOutput("returns")
        )
        
      ),
      
      # craeting a space to our boxplot and histogram plots
      fluidRow(
        
        splitLayout(cellWidths = c("50%", "50%")
                    , plotOutput("boxplot")
                    , plotOutput("hist")
        )
      )
    )
    # end of mainPanel
  )
  # end of sidebarLayout
)
# end of user interface(UI)