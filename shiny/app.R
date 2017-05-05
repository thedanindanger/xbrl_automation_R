#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# rstudioapi::getActiveDocumentContext()
# this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# setwd(this.dir)

library(shiny)
library(XBRL)
library(xbrlus)
library(finstr)
library(dplyr)
library(tidyr)
library(pander)
library(ggplot2)
library(reshape2)

# url = "https://www.sec.gov/Archives/edgar/data/320193/000119312515356351/aapl-20150926.xml"
# 
# fullStatementXbrl <- xbrlDoAll(url, verbose = TRUE, cache.dir = "xbrl.cache", delete.cached.inst = TRUE)

# api_key = "3aa4a7a1-3cb0-4dbb-b346-63219773bfe1"
# Sys.setenv(XBRLUS_API_KEY=api_key)

balanceSheetCompare <- function(c = "aapl, goog, fb", year =2016){
  
  
  #heavily lifted from example on https://github.com/bergant/xbrlus
  
  c_vect <- trim.spaces(unlist(strsplit(c,",", fixed = TRUE)))
  
  cik <- xbrlCIKLookup(c_vect)
  
  elements <- xbrlBaseElement(c(
    "AssetsCurrent",
    "AssetsNoncurrent",
    "Assets",
    "LiabilitiesCurrent",
    "LiabilitiesNoncurrent",
    "Liabilities",
    "StockholdersEquity",
    "MinorityInterest",
    "StockholdersEquityIncludingPortionAttributableToNoncontrollingInterest",
    "LiabilitiesAndStockholdersEquity"
  ))
  
  values <- xbrlValues( 
    CIK = cik$cik, 
    Element = elements$elementName, 
    DimReqd = FALSE, 
    Period = "Y",
    Year = year,
    NoYears = 1,
    Ultimus = TRUE,
    Small = TRUE,
    as_data_frame = TRUE
  )
  
  balance_sheet <- 
    elements %>% 
    left_join(values, by = "elementName") %>% 
    select(entity, standard.text, amount) %>% 
    mutate(amount = round(amount / 10e6,0)) %>%  
    spread(entity, amount)
  
  bal_cols = ncol(balance_sheet)
  
  extra_col_count = length(c_vect) + 2
  
  if(bal_cols == extra_col_count) {balance_sheet[,bal_cols] <- NULL}
  
  balance_sheet <- balance_sheet[
    order(order(elements$elementName)),   
    !is.na(names(balance_sheet))]
  row.names(balance_sheet) <- NULL
  
  # pandoc.table(
  #   balance_sheet,
  #   caption = "Balance Sheet Comparison",
  #   big.mark = ",",
  #   split.table = 200,
  #   style = "rmarkdown",
  #   justify = c("left", rep("right", 3)))
  # 
  # 
  # #print(pandoc.table)
  return(balance_sheet)
  
}

getXbrlStatement <- function(url) {
  old_o <- options(stringsAsFactors = FALSE)
  fullStatementXbrl <- xbrlDoAll(url, verbose = TRUE, cache.dir = "xbrl.Cache", delete.cached.inst = TRUE)
  options(old_o)
  
  fullStatementDF <- xbrl_get_statements(fullStatementXbrl)
  fullStatementDF
}

getBalanceSheet <- function(parsedXBRL){
  if(!is.null(parsedXBRL$StatementOfFinancialPositionClassified)){
    balanceSheet <- parsedXBRL$StatementOfFinancialPositionClassified
  }
  if(!is.null(parsedXBRL$ConsolidatedBalanceSheets)){
    balanceSheet <- parsedXBRL$ConsolidatedBalanceSheets
  }
  return(balanceSheet)
}

getIncomeStatement <- function(parsedXBRL){
  if(!is.null( parsedXBRL$StatementOfIncome)){
    incomeStatement <- parsedXBRL$StatementOfIncome
  }
  if(!is.null( parsedXBRL$ConsolidatedStatementsOfComprehensiveIncome)){
    incomeStatement <- parsedXBRL$ConsolidatedStatementsOfComprehensiveIncome
  }
  return(incomeStatement)
}

checkBalanceSheet <- function(url = "https://www.sec.gov/Archives/edgar/data/320193/000162828016020309/aapl-20160924.xml") {
  xbrlBalanceSheet <- getXbrlStatement(url)
  balance_sheet <- getBalanceSheet(xbrlBalanceSheet)
  return(check_statement(balance_sheet))
}



balance_sheet_ratios <- function(XBRLYear1URL = "https://www.sec.gov/Archives/edgar/data/320193/000119312515356351/aapl-20150926.xml", 
                                 XBRLYear2URL = "https://www.sec.gov/Archives/edgar/data/320193/000162828016020309/aapl-20160924.xml"){
  
  
  
  XBRLYear2 <- invisible(getXbrlStatement(XBRLYear2URL))
  XBRLYear1 <- invisible(getXbrlStatement(XBRLYear1URL))
  
  BalanceYear2 <- getBalanceSheet(XBRLYear2)
  BalanceYear1 <- getBalanceSheet(XBRLYear1)
  
  balance_sheet <- merge( BalanceYear1,BalanceYear2 )
  
  ratios <- balance_sheet %>% calculate( digits = 2,
                                         
                                         current_ratio = AssetsCurrent / LiabilitiesCurrent,
                                         
                                         quick_ratio =  
                                           ( CashAndCashEquivalentsAtCarryingValue + 
                                               AvailableForSaleSecuritiesCurrent +
                                               AccountsReceivableNetCurrent
                                           ) / LiabilitiesCurrent
                                         
  )
  
  long_df <- invisible(melt(ratios)) #invisible to hide log output
  
  ratio_plot <-   ggplot(data = long_df, aes(x=date, y=value, fill = variable)) 
  
  ratio_plot <- ratio_plot + geom_bar(stat="identity", position=position_dodge())
  
  return(list(ratios,ratio_plot))
  
}



ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Automatic XBRL Parsing"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         "Compare Balance Sheets by Year",
         br(),
         br(),
         textInput("c",
                     "Comma Seperated Lists of Stocks:",
                     value = "goog, fb, aapl"),
         textInput("year", 
                   "Year to compare",
                   value = 2016),
         
         br(),
         br(),
         "Check for Errors in XBRL Balance Sheet",
         br(),
         textInput("url_check",
                   "URL from EDGAR",
                   value = "https://www.sec.gov/Archives/edgar/data/320193/000119312515356351/aapl-20150926.xml" ),
         
         br(),
         br(),
         "Compare Financial Ratios Over Time",
         br(),
         textInput("url_year1",
                   "First Period Financial Statement",
                   value = "https://www.sec.gov/Archives/edgar/data/320193/000119312515356351/aapl-20150926.xml"),
         textInput("url_year2",
                   "Second Period Financial Statement",
                   value = "https://www.sec.gov/Archives/edgar/data/320193/000162828016020309/aapl-20160924.xml")
         
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         
         tabsetPanel(
           tabPanel("Ticker Compare",
                    dataTableOutput("balance_sheet")
                    )
           ,
            tabPanel("Balance Sheet Check",
                     textOutput("check")
            )
          ,
          tabPanel("Financial Ratios",
                   fluidPage(
                   dataTableOutput("ratio_table"),
                   plotOutput("ratio_plot"))
          )
                    
           )
         )
         
      )
   )
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {

   balance_sheet <- reactive({
     bs <- balanceSheetCompare(input$c,input$year)
     bs
   })
   
  output$balance_sheet <- renderDataTable({
    data.frame(balance_sheet())
    })
    
  output$check <- renderText({
    check <- checkBalanceSheet(input$url_check)
    check
  })


  #ratios <- reactive({balance_sheet_ratios(input$url_year1, input$url_year2)})
  
  ratios <- reactive({balance_sheet_ratios(input$url_year1, input$url_year2)})

  output$ratio_table <- renderDataTable({
    ratios <- ratios()
    ratios[1]
    })

  output$ratio_plot <- renderPlot({
    ratios <- ratios()
    ratios[2]
    })

})

# Run the application 
shinyApp(ui = ui, server = server)

