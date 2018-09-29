rm(list=ls())
library(shiny)
library(dplyr)
library(ggplot2)

#===============
# Parameters - these show up as current values in app
#===============

mortgageCurrent <- 1291+1359+909 # current mortgage payments per month. See WestpacOne app for details
mortgageInterestOnly <- round(239247*(0.0464/12)+228217*(0.0509/12)+175039*(0.043/12), 0) # if mortgage payments were interest only. Based on current loan sizes and interest rates. See WestpacOne app for details. Divide annual interest rate by 12 for monthly rate, as described here https://www.mtgprofessor.com/a%20-%20interest%20rates/interest_rate_fundamentals.htm (this is v. close to rates I can see on WestpacOne app, which says how much goes to interest each month)
jayCurrentSalary <- 7544
jayCurrentSalary_4d <- 1486*4 # https://www.paye.net.nz/calculator.html. May not be that accurate, it underestimated current pay by ~$300.
emilyCurrentSalary <- 2653*2
monthlyOut <- mean(11485, 11883, 11338, 12235)-mortgageCurrent # numbers come from CashNav app.

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
                   label=h4("E monthly salary after tax:"), 
                   value=emilyCurrentSalary), 
      helpText(paste0("Currently $", emilyCurrentSalary), 
               "| 2d p/w @ current pay (no extra units): $2107", 
               "| 2d p/w @ $300 p/d: $1994"),
      numericInput(inputId="numJay", 
                   label=h4("J monthly salary after tax:"), 
                   value=jayCurrentSalary),
      helpText(paste0("Currently $", jayCurrentSalary)),
      numericInput(inputId="numJay_4d", 
                   label=h4("J monthly salary, 4d pw:"), 
                   value=jayCurrentSalary_4d),
      helpText(paste0("Currently $", jayCurrentSalary_4d)),
      numericInput(inputId="numOut", 
                   label=h4("Monthly outgoings ex. mortgage repayments:"), 
                   value=monthlyOut),
      helpText(paste0("Currently $", monthlyOut), 'xxx'),
      
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
      # fluidRow(column(2,
      br(),
      h3(textOutput("savings")),
      br(),
      h4(textOutput("earnings")),
      h4(textOutput("outgoings")),
      br(),
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
  
  # Report savings based on current params
  output$savings <- renderText({ 
    mortgagePayments <- ifelse(input$radio==1, mortgageCurrent, 
                               ifelse(input$radio==2, mortgageInterestOnly, 
                                      input$mortgageOther))
    paste0("Savings per month under current inputs: $", input$numEm + input$numJay - input$numOut - mortgagePayments)
  })
  
  # Report Emily earnings required to break even
  output$earnings <- renderText({ 
    mortgagePayments <- ifelse(input$radio==1, mortgageCurrent, 
                               ifelse(input$radio==2, mortgageInterestOnly, 
                                      input$mortgageOther))
    paste0("Emily salary required to break even: $", 
           input$numEm + -1*(input$numEm + input$numJay - input$numOut - mortgagePayments))
  })
  
  # Report outgoings required to break even
  output$outgoings <- renderText({ 
    mortgagePayments <- ifelse(input$radio==1, mortgageCurrent, 
                               ifelse(input$radio==2, mortgageInterestOnly, 
                                      input$mortgageOther))
    paste0("Maximum outgoings required to break even: $", 
           input$numOut + input$numEm + input$numJay - input$numOut - mortgagePayments)
  })
  
  # Graph of savings against wide range of param values
  output$plot1 <- renderPlot({ 
    mortgagePayments <- ifelse(input$radio==1, mortgageCurrent, 
                               ifelse(input$radio==2, mortgageInterestOnly, 
                                      input$mortgageOther))
    
    # Plot changing e salary and j salary for constant mortgage repayment
    currentSavings <- input$numEm + input$numJay - mortgagePayments - input$numOut # to plot current inputs on plot
    jaySalVec <- c(input$numJay_4d, input$numJay) 
    if(input$numEm==0) emSalVec <- seq(from=0, to=1.5, by=0.05) # need ifelse to avoid error in seq() when numEm==0
    if(input$numEm!=0) emSalVec <- c(seq(from=0, to=1.5*input$numEm, by=50), input$numEm) # adding input$numEm allows me to highlight current parameter values
    
    plotdf <- expand.grid(jaySalVec=jaySalVec, emSalVec=emSalVec)
    plotdf <- plotdf %>%
      mutate(mortgagePayments=mortgagePayments) %>%
      mutate(savings=jaySalVec + emSalVec - input$numOut - mortgagePayments) %>%
      mutate(`Jay's Salary`=ifelse(jaySalVec==input$numJay_4d, 'Jay salary 4d pw', 'Jay full salary'))
    
    ggplot(plotdf, aes(emSalVec, savings, colour=`Jay's Salary`)) + 
      # show breaking even as a horiz line
      geom_abline(slope=0, intercept=0, colour='grey') +
      geom_line() + 
      xlab("Emily's salary") + ylab("Savings") + ggtitle("Savings per month under changes to Emily's salary") +
      # add a point giving currwent param values
      geom_point(x=input$numEm, y=currentSavings, size=10, alpha=0.1, shape=1, colour='black') +
      annotate("text", x=input$numEm, y=currentSavings, label="Current inputs", vjust=3, hjust=-0.25) +
      theme(text=element_text(size=15))
  })
}

#__________________________________________________________________________________________________________________________________

# Create app ----
#__________________________________________________________________________________________________________________________________

shinyApp(ui, server)
# runApp('JemilySavingsShinyApp', display.mode='showcase') # can't have this as an 'active' line in script (if detected it will throw an error). So just save any changes to the script then copy this line into console to run. showcase model helps to debug.
