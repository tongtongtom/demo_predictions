#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Static Problem Identification"),
  
 
  
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "Dataset", multiple = FALSE, label ='dataset', 
                  choices = list('7days normalized','7 days','30 days normalized','30 days',
                                 '90 days normalized','90 days'),
                  selected = '7 days'),
      selectInput(inputId = "Contrast",multiple =FALSE,label = "website:",
                  choices = list("Bets increase, balance decreases" = 'ContrastA', 
                                 "fanshui or reductions increase, stickyness decreases" = 'ContrastB',
                                 "Stickyness increasing, fanshui flat or decreasing" = 'ContrastC',
                                 "Deposit increasing, profit decreasing" = 'ContrastD',
                                 "average saving increased/decreased, total saving increased/decreased" ='ContrastE',
                                  "average member contribution decreases"='ContrastF','any vs any (two characteristics)'='ContrastZZ')),
      conditionalPanel(condition="input.Contrast.match('Contrast')",
        sliderInput('variable1', 'VariableSlider1', min=-2 , max=2, value=0.1, step=0.01)),
      conditionalPanel(condition="input.Contrast != 'ContrastF'",
        sliderInput('variable2', 'VariableSlider2', min=-2 , max=2, value=0.1, step=0.01)),
      conditionalPanel(condition="input.Contrast == 'ContrastZZ'",
                       selectInput(inputId = 'CompareVariable1', multiple = FALSE, label = 'Compare Variable 1',choices = list()),
                       selectInput(inputId = 'CompareVariable2', multiple = FALSE, label = 'Compare Variable 2',choices = list())),
      selectInput(inputId = "Characteristics", multiple=TRUE, label="Characteristics:",
                  choices = list()),
      conditionalPanel(condition="input.overlays == 1|2",
        selectInput(inputId = "detailsWebsite", multiple = FALSE, label ="Details Website", choices = list())
      )
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id='overlays',
                  tabPanel('One Site:',
                           DT::dataTableOutput('summaryTable'),
                           plotOutput('Contrasts1'),
                           DT::dataTableOutput('selectedDatasets'),
                           value = 1),
                  
                  tabPanel('Slopes:',
                           plotOutput("Empty"),
                           value = 2)
    )
  )
  )
))

#[1] "AddictiveRate"          "balance"                "BetAmountSums"          "Bite"                  
#[5] "BitePerBet"             "BitePerUser"            "ChurnRate1"             "ChurnRate30"           
#[9] "ChurnRate7"             "CommissionableBet"      "CommissionableSums"     "CommissionableUser"    
#[13] "CreditDepositBet"       "CreditDepositPTime"     "CreditDepositSums"      "CreditDepositTimes"    
#[17] "CreditDepositUser"      "CreditWithdrawBet"      "CreditWithdrawPTime"    "CreditWithdrawSums"    
#[21] "CreditWithdrawTimes"    "CreditWithdrawUser"     "DepositAmount"          "DepositSums"           
#[25] "DepositTimes"           "DiscountBets"           "DiscountSums"           "DiscountUser"          
#[29] "FavorableBet"           "FavorableSums"          "FavorableUser"          "FirstDepositAmount"    
#[33] "FirstDepositBet"        "FirstDepositNumber"     "FirstDepositpTime"      "FirstDepositUser"      
#[37] "MemberCounts"           "MemberDiscountBet"      "MemberDiscountSums"     "MemberDiscountUser"    
#[41] "OthersBet"              "OthersSums"             "OthersUser"             "PayoffBet"             
#[45] "PayoffSums"             "PayoffUser"             "RetentionRate1"         "RetentionRate30"       
#[49] "RetentionRate7"         "ThirdPartyPaymentBet"   "ThirdPartyPaymentPTime" "ThirdPartyPaymentSums" 
#[53] "ThirdPartyPaymentTimes" "ThirdPartyPaymentUser"  "WagersCounts"           "WithdrawAmount"        
#[57] "WithdrawBet"            "WithdrawPTime"          "WithdrawSums"           "WithdrawTimes"         
#[61] "WithdrawUser"          
