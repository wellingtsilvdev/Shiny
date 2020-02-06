###########################################################
# Wellington Silva                                        #
# https://www.kaggle.com/gatunnopvp                       #
# https://github.com/WellingtonSilv                       #
# https://www.linkedin.com/in/wellington-silva-b430a1195/ #
###########################################################

# loading required libraries
library(roll)
library(shiny)

# creating the server logic
function(input, output){
  
  # creating a Reactive object to react to submit prediction buttom
  pred_final <- eventReactive(input$submit
                              , {
                                
                                # ------ loading dataset, models and make prediction ----- #
                                
                                # Importing and pre-processing data
                                asset <- read.csv('bitcoin_finex.csv',header=T,sep = ';')
                        
                                asset$data <- as.character(asset$data)
                                
                                # creating a new row to add new prices to prediction
                                new_row <- dim(asset)[1]+1
                                asset[new_row,] <- 0
                                
                                # taking new data from UI
                                dates <- input$date
                                open <- input$open
                                high <- input$high
                                low <- input$low
                                close <- input$close
                                
                                # adding new data to our dataset
                                asset$data[new_row] <- dates
                                asset$open[new_row] <- open
                                asset$high[new_row] <- high
                                asset$low[new_row] <- low
                                asset$close[new_row] <- close
                                
                                # removing possible missing value rows
                                asset <- na.omit(asset)
                                
                                # exporting the dataset with new prices and day
                                write.table(asset,'bitcoin_finex.csv',sep = ';',dec = '.',row.names = F)
                                
                                # calculating returns close to close
                                for(i in 1:dim(asset)[1]){
                                  asset$returns_closes[i] <- asset$close[i+1]/asset$close[i]-1
                                }
                                
                                # calculating returns close+ to open+
                                for(i in 1:dim(asset)[1]){
                                  asset$returns_cl_op[i] <- asset$close[i+1]/asset$open[i+1]-1
                                }
                                
                                # saving returns and removing returns from asset
                                returns_closes <- asset$returns_closes
                                returns_cl_op <- asset$returns_cl_op
                                asset$returns_closes <- NULL
                                asset$returns_cl_op <- NULL
                                
                                # Logging our prices
                                dates <- asset$data
                                asset <- log(asset[,2:5])
                                asset <- cbind(dates,asset)
                                
                                # creating variables
                                asset$var <- asset$close/asset$open-1
                                
                                asset$var1 <- roll_sd(asset$var,5)
                                asset$var2 <- roll_sd(asset$var,10)
                                asset$var3 <- roll_sd(asset$var,15)
                                asset$var4 <- roll_sd(asset$var,20)
                                asset$var5 <- roll_sd(asset$var,25)
                                asset$var6 <- roll_sd(asset$var,30)
                                
                                # setting directory and sourcing travel.R function to replace Na's
                                source('travel.R')
                                
                                asset <- travel(asset,element = 0,last.na = T,first_row.na = T) # replacing missing values
                                
                                #--------------- Creating model1 --------------
                                
                                # Creating the target to model1
                                asset$target <- ifelse(returns_closes>=roll_mean(returns_closes,15) & roll_mean(returns_closes,15)>0,1,NA)
                                asset$target <- ifelse(returns_closes<=roll_mean(returns_closes,15) & roll_mean(returns_closes,15)<0,0,asset$target)
                                asset$target[is.na(asset$target)==T] <- 2
                                
                                asset$returns_closes <- returns_closes
                                
                                # separating data to prediction
                                test <- asset[1101:dim(asset)[1],]
                                
                                # loading model1
                                tree <- readRDS("model1.rds")
                                
                                testing <- test # data to prediction
                                
                                pred <- predict(tree,testing)
                                pred <- as.data.frame(pred)
                                names(pred) <- c('sell1','buy1','out1')
                                
                                testing$pred <- ifelse(pred$sell>pred$buy & pred$sell>pred$out,'0',NA)
                                testing$pred <- ifelse(pred$buy>pred$sell & pred$buy>pred$out,'1',testing$pred)
                                testing$pred[is.na(testing$pred)==T] <- '2'
                                
                                testing$pred <- as.factor(testing$pred)
                                testing$target <- as.factor(testing$target)
                                
                                #--------------- Creating model2 --------------
                                
                                # Creating the target to model2
                                asset$target <- ifelse(returns_cl_op>=roll_mean(returns_cl_op,20) & roll_mean(returns_cl_op,20)>0,1,NA)
                                asset$target <- ifelse(returns_cl_op<=roll_mean(returns_cl_op,20) & roll_mean(returns_cl_op,20)<0,0,asset$target)
                                asset$target[is.na(asset$target)==T] <- 2
                                
                                asset$returns_cl_op <- returns_cl_op
                                asset$returns_closes <- NULL
                                
                                # separating data to model2 prediction
                                test2 <- asset[1101:dim(asset)[1],]
                                
                                # creating the model
                                tree2 <- readRDS("model2.rds")
                                
                                testing2 <- test2 # data to prediction
                                
                                pred2 <- predict(tree2,testing2)
                                pred2 <- as.data.frame(pred2)
                                names(pred2) <- c('sell2','buy2','out2')
                                
                                testing2$pred2 <- ifelse(pred2$sell>pred2$buy & pred2$sell>pred2$out,'0',NA)
                                testing2$pred2 <- ifelse(pred2$buy>pred2$sell & pred2$buy>pred2$out,'1',testing2$pred2)
                                testing2$pred2[is.na(testing2$pred2)==T] <- '2'
                                
                                testing2$pred2 <- as.factor(testing2$pred2)
                                testing2$target <- as.factor(testing2$target)
                                
                                #------------------------------ creating the voting system ------------------------
                                
                                final_pred <- ifelse(testing$pred==1,1,NA)
                                final_pred <- as.data.frame(final_pred)
                                
                                final_pred$dates <- testing2$dates
                                
                                final_pred$final_pred <- ifelse(testing2$pred2==0,0,final_pred$final_pred)
                                final_pred$final_pred[is.na(final_pred$final_pred)==T] <- 2
                                
                                final_pred$final_pred <- ifelse(final_pred$final_pred==1,"COMPRA!",final_pred$final_pred)
                                final_pred$final_pred <- ifelse(final_pred$final_pred==0,"VENDA!",final_pred$final_pred)
                                final_pred$final_pred <- ifelse(final_pred$final_pred==2,"FORA!",final_pred$final_pred)
                                
                                final_pred$final_pred <- as.factor(final_pred$final_pred)
                                
                                pred_final <- tail(final_pred$final_pred,1)
                              })
  
  # creating a Reactive object to react to delete last row buttom
  delete_message <- eventReactive(input$del_last_row
                                , {
                                  
                                  # Importing and pre-processing data
                                  asset <- read.csv('bitcoin_finex.csv',header=T,sep = ';')
                                  
                                  asset[dim(asset)[1],] <- NA
                                  
                                  # removing possible missing value rows
                                  asset <- na.omit(asset)
                                  
                                  # exporting the dataset with new prices and day
                                  write.table(asset,'bitcoin_finex.csv',sep = ';',dec = '.',row.names = F)
                                  
                                  delete_message <- "last row deleted, click again to delete another"
                                  
                                })
  
  # creating a Reactive object to react to submit prediction or delete last row buttons
  tabble <- eventReactive(list(input$submit,input$del_last_row)
                          , {
                            
                            # Importing dataset to table check plot
                            tablee <- read.csv("bitcoin_finex.csv", header = T, sep = ";")
                            tail(tablee,10)
                          })
  
  # rendering prediction output
  output$prediction <- renderText({
    
    # pasting prediction on mainpanel
    paste(pred_final())
  })
  
  # rendering prediction output
  output$del_message <- renderText({
    
    # pasting prediction on mainpanel
    paste(delete_message())
  })
  
  # rendering prediction output
  output$check_table <- renderTable({

    # showing the bitcoin prices table with the last 10 days
    tabble()
    
  })
  
} # end of server