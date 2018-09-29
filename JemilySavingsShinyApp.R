rm(list=ls())
library(shiny)
library(dplyr)
library(ggplot2)

#++++++++++++++
# Parameters - these show up as current values in app
#++++++++++++++

mortgageCurrent <- 1291+1359+909 # current mortgage payments per month. See WestpacOne app for details
mortgageInterestOnly <- round(239247*(0.0464/12)+228217*(0.0509/12)+175039*(0.043/12), 0) # if mortgage payments were interest only. Based on current loan sizes and interest rates. See WestpacOne app for details. Divide annual interest rate by 12 for monthly rate, as described here https://www.mtgprofessor.com/a%20-%20interest%20rates/interest_rate_fundamentals.htm (this is v. close to rates I can see on WestpacOne app, which says how much goes to interest each month)
jayCurrentSalary <- 130000
emilyCurrentSalary <- 70000
monthlyOut <- mean(8442, 9078, 8226, 8200)-mortgageCurrent # numbers come from CashNav app.

#++++++++++++++
# Function to calc take home pay after KS, PAYE and ACC
#++++++++++++++

takeHomeSalaryFun <- function(salary) {
  
  # Calc KS (assuming 3%)
  KS <- salary*0.03
  
  # Calc ACC (assuming 1.39%) [in reality it's only this amount up to ~125K]
  ACC <- salary*0.0139
  
  # Calc PAYE
  PAYE <- ifelse(salary<=14000, (salary*.105),
                             ifelse(salary<=48000, (salary-14000)*.175+1470,
                                    ifelse(salary<=70000, (salary-48000)*.30+7420,
                                           (salary-70000)*.33+14020)))
  # Calc take-home pay
  out <- salary-KS-ACC-PAYE
  return(out)
}

#__________________________________________________________________________________________________________________________________

# Define inputs ----
#__________________________________________________________________________________________________________________________________

ui <- fluidPage(
  # titlePanel("How much will Jemily save?"),
  
  sidebarLayout(
    
    #===============
    # Define sidebar elements
    #===============
    
    sidebarPanel(
      numericInput(inputId="numEm", 
                   label=h4("E salary:"), 
                   value=emilyCurrentSalary), 
      numericInput(inputId="numJay", 
                   label=h4("J fulltime salary:"), 
                   value=jayCurrentSalary),
      numericInput(inputId="numOut", 
                   label=h4("Monthly outgoings ex. mortgage repayments:"), 
                   value=monthlyOut),
      helpText(paste0("Currently $", monthlyOut)),
      
      # Select jay salary type
      radioButtons(inputId="radio_jaySal", 
                   label=h4("Jay salary type"), 
                   choiceNames=list("Fulltime", "4d per wk"),
                   choiceValues=list(1,2),
                   selected=1),
      
      # Select mortgate repayment type
      radioButtons(inputId="radio", 
                   label=h4("Monthly mortgage repayments"), 
                   choiceNames=list(paste0("Current ($", mortgageCurrent, ")"), 
                                    paste0("Interest-only ($", mortgageInterestOnly, ")"),
                                    "Other (specify)"),
                   choiceValues=list(1,2,3),
                   selected=1),
      
      # Only show this panel if Custom mortage repayment is selected
      conditionalPanel(
        condition = "input.radio==3",
        numericInput(inputId="mortgageOther", label=NULL, value=0))),
    
    #===============
    # Define main panel elements
    #===============
    
    mainPanel(
      br(),
      h4(textOutput("savings")),
      h4(textOutput("earnings")),
      h4(textOutput("outgoings")),
      br(),
      br(),
      plotOutput("plot1")
      # )
    )
  )
)

#__________________________________________________________________________________________________________________________________

# Define outputs ----
#__________________________________________________________________________________________________________________________________

server <- function(input, output) {
  
  # Convert salaries from annual to monthly, and calc Jay's 4d week salary. Doing as a reactive expression cos needed in multiple outputs below.
  jaySal <- reactive({
    if(input$radio_jaySal==1){
      round(takeHomeSalaryFun(input$numJay)/12, 0)
    } else {
      round(takeHomeSalaryFun((input$numJay*0.8))/12, 0)
    }
  })
  emSal <- reactive({round(takeHomeSalaryFun(input$numEm)/12, 0)})
  
  # Report savings based on current params
  output$savings <- renderText({ 
    mortgagePayments <- ifelse(input$radio==1, mortgageCurrent, 
                               ifelse(input$radio==2, mortgageInterestOnly, 
                                      input$mortgageOther))
    paste0("Savings per month under current inputs: $", emSal() + jaySal() - input$numOut - mortgagePayments)
  })
  
  # Report Emily earnings required to break even
  output$earnings <- renderText({ 
    mortgagePayments <- ifelse(input$radio==1, mortgageCurrent, 
                               ifelse(input$radio==2, mortgageInterestOnly, 
                                      input$mortgageOther))
    paste0("Emily salary required to break even: $", 
           emSal() + -1*(emSal() + jaySal() - input$numOut - mortgagePayments))
  })
  
  # Report outgoings required to break even
  output$outgoings <- renderText({ 
    mortgagePayments <- ifelse(input$radio==1, mortgageCurrent, 
                               ifelse(input$radio==2, mortgageInterestOnly, 
                                      input$mortgageOther))
    paste0("Max outgoings (ex. mortgage) required to break even: $", 
           emSal() + jaySal() - mortgagePayments)
  })
  
  # Graph of savings against wide range of param values
  output$plot1 <- renderPlot({ 
    mortgagePayments <- ifelse(input$radio==1, mortgageCurrent, 
                               ifelse(input$radio==2, mortgageInterestOnly, 
                                      input$mortgageOther))
    
    # Plot changing e salary and j salary for constant mortgage repayment
    currentSavings <- emSal() + jaySal() - mortgagePayments - input$numOut # to plot current inputs on plot
    jaySalVec <- jaySal() 
    if(emSal()==0) emSalVec <- seq(from=0, to=1.5, by=0.05) # need ifelse to avoid error in seq() when numEm==0
    if(emSal()!=0) emSalVec <- c(seq(from=0, to=1.5*emSal(), by=50), emSal()) # adding emSal() allows me to highlight current parameter values
    
    plotdf <- expand.grid(jaySalVec=jaySalVec, emSalVec=emSalVec)
    plotdf <- plotdf %>%
      mutate(mortgagePayments=mortgagePayments) %>%
      mutate(savings=jaySalVec + emSalVec - input$numOut - mortgagePayments) 
    
    ggplot(plotdf, aes(emSalVec, savings)) + 
      # show breaking even as a horiz line
      geom_abline(slope=0, intercept=0, colour='grey') +
      geom_line(colour='red') + 
      xlab("Emily's salary") + ylab("Savings") + ggtitle("Savings per month under changes to Emily's salary") +
      # add a point giving currwent param values
      geom_point(x=emSal(), y=currentSavings, size=10, alpha=0.1, shape=1, colour='black') +
      annotate("text", x=emSal(), y=currentSavings, label="Current inputs", vjust=3, hjust=-0.25) +
      theme(text=element_text(size=15))
  })
}

#__________________________________________________________________________________________________________________________________

# Create app ----
#__________________________________________________________________________________________________________________________________

shinyApp(ui, server)
# runApp('JemilySavingsShinyApp', display.mode='showcase') # can't have this as an 'active' line in script (if detected it will throw an error). So just save any changes to the script then copy this line into console to run. showcase model helps to debug.
