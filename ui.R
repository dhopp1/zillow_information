library(shinyapps)
library(shiny)
library(RCurl)
library(XML)
county <- read.csv("FIPS.csv")

shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Zillow Data"),
  sidebarPanel( width = 2,
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    h5("Data Source"),  
    checkboxInput("type", label = "CSV",value = F),
    checkboxInput("apikey", label = "I have my own API Key",value = F),
    conditionalPanel(
      condition = "input.apikey==true",
      textInput("ownapi", "Paste your API key below:")
    ),
    br(),
    fileInput('file1', h5('Upload Addresses')),
    
    downloadButton('downloadTemplate', 'Download Template'),
    br(),
    br(),
    downloadButton('downloadReport', 'Download Results')
  ),
  
  mainPanel(
    helpText("-Paste a list of unique identifiers, addresses (just street number and name, no city, state, etc.), and zip codes, in that order, to see their Zillow information.  If there are many properties this may take a while (a progress bar is in development)"),
    helpText("-If you have your own Zillow API key, check \"I have my own API key\" and paste it in the text box below, otherwise it will use the default and is limited to 1000 calls a day"),
    helpText("-To get Zillow data for properties in an Excel file, upload a CSV file with the columns \"unique\", \"address\", and \"zip\", where \"unique\" is a unique identifier for each property, or download the template.  Once uploaded a table of Zillow information will appear. To download to CSV, just press \"Download\""),
    conditionalPanel(
      condition="input.type==false",
      HTML('<textarea id="uniques" rows="10" cols="1" placeholder="Unique IDs"></textarea>'),
      HTML('<textarea id="addresses" rows="10" cols="1" placeholder="Addresses"></textarea>'),
      HTML('<textarea id="zips" rows="10" cols="1" placeholder="Zips"></textarea>')
    ),
    tableOutput("table")
  )
))