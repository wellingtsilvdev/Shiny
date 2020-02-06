###########################################################
# Wellington Silva                                        #
# https://www.kaggle.com/gatunnopvp                       #
# https://github.com/WellingtonSilv                       #
# https://www.linkedin.com/in/wellington-silva-b430a1195/ #
###########################################################

# loading required libraries
library(shiny)

# creating a fluidpage
fluidPage(
  
  # inserting a title to our app
  titlePanel("Decision Tree Emsemble for Bitcoin"),
  
  # creating a sidebarLayout to show input and output objects
  sidebarLayout(
    
    # creating a Panel to show inputs
    sidebarPanel(
      
      titlePanel("Insert the new day parameters"),
      
      # creating input objects
      textInput(inputId = "date"
                , label = "Insert the new date here"
                , value = "01/01/2000"),
      
      numericInput(inputId = "open"
                   , label = "Insert the new open here"
                   , value = 0.0),
      
      numericInput(inputId = "high"
                   , label = "Insert the new high here"
                   , value = 0.0),
      
      numericInput(inputId = "low"
                   , label = "Insert the new low here"
                   , value = 0.0),
      
      numericInput(inputId = "close"
                   , label = "Insert the new close here"
                   , value = 0.0)
      
    ),
    
    # creatin a panel to show outputs
    mainPanel(
      
      # inserting instructions
      
      h3("Predicting Bitcoin movement side with decision trees"),
      
      h4("App instructions:"),
      
      h5("1 - To make a new prediction insert the new day parameters 
         and click on submit prediction, after that a new row with the 
         parameters inserted will appear in a table with bitcoin prices
         of last 10 days"),
      
      h5("2 - If you insert wrong parameters and already clicked on 
         submit predicion button, you can deleted the last row of the dataset,
         insert the parameters again and make a new prediction following
         the intruction number 1"),
      
      h5("3 - You can see the inserted parameters and deleted parameters
         upadate in 'Bitcoin prices of last 10 days' table"),
      
      # creating a panel to show submit button and output prediction
      sidebarPanel(
        
        titlePanel("Click on 'submit prediction' to make a new prediction:"),
        
        actionButton(inputId = "submit"
                     , label = "submit prediction"),
        
        # output object to show the prediction
        textOutput(outputId = "prediction")
        
      ),
      
      # creating a panel to show delete button and delete message
      sidebarPanel(
        
        titlePanel("Click on 'delete button' to delete the last row:"),
        
        actionButton(inputId = "del_last_row"
                     , label = "delete button"),
        
        textOutput(outputId = "del_message")
      ),
      
      # creating a panel to show check table
      sidebarPanel(
        
        titlePanel("Bitcoin Prices of last 10 days"),
        
        tableOutput(outputId = "check_table"),
        
        width = 5
      )
    )
  )
) # end of User Interface(UI)