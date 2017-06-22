#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(sqldf)
library(data.table)
require(RCurl)
library(foreign)

url <- 'https://github.com/tengbateng/WWF_Drilldown_Mockup/raw/master/alertyears.txt'

dat <- getURL(url)                

alerts <- read.csv(textConnection(dat), header=TRUE)
#alerts <- read.csv(temp, header = TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(
      
      # Application title
      titlePanel("Worldwide,Regional, Subregional and Country Discrepancies"),
      
      fluidRow(
            
            
            sidebarPanel(
                  
                  selectInput("n_year", label = "Select Year:",
                              choices=c('Select Year',alerts$Year), 
                              selected = NULL)
                  
            ),
            mainPanel(
                  
                  h4('Regional Trade Discrepancy Totals'),
                  tableOutput("Table")
                  
            )
      ),
      # Sidebar with a slider input for number of bins 
      fluidRow(
            conditionalPanel(
                  
                  condition = "output.Table != null",
                  
                  sidebarPanel(
                        uiOutput("SomeControls") 
                        
                  ) ,
                  
                  mainPanel(
                        h4('Sub-Region Level Trade Discrepancy Totals'),
                        tableOutput("Table2")
                        
                  )
                  
            )
      ),
      
      fluidRow(
            conditionalPanel(
                  
                  condition = "output.Table2 != null",
                  
                  sidebarPanel(
                        
                        uiOutput("moreControls")
                        
                  ) ,
                  
                  mainPanel(
                        h4('Country Level Trade Discrepancy Totals'),
                        tableOutput("Table3")
                  )
                  
            )
      ),
      fluidRow(
            conditionalPanel(
                  
                  condition = "output.Table3 != null",
                  
                  sidebarPanel(
                        
                        uiOutput("EvenMoreControls")
                        
                  ) ,
                  
                  mainPanel(
                        h4('Fish Group Level Trade Discrepancy Totals'),
                        tableOutput("Table4")
                  )
                  
            )
      )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
      
      countrymapping <- read.csv("UN Comtrade Country List.csv", header = TRUE)
      #alerts <- read.csv("alertyears.txt", header = TRUE)
      
      
      output$FirstControls <- renderUI ({
            temp <- as.character(alerts$Year)
            
            temp <- c("Select a Year", temp)
            tagList(
                  selectInput("n_year", label = "Select Year:",
                              choices=temp, 
                              selected = NULL)
            )
      })
      
      
      
      
      output$SomeControls <- renderUI ({
            
            if(!is.null(input$n_year) && input$n_year != "Select Year"){
                  temp <- as.character(unique(countrymapping$UN.Geoscheme.level.1[
                        countrymapping$UN.Geoscheme.level.1 != "World"]))
                  
                  temp <- temp[order(temp)]
                  temp <- c("Select Region", temp)
                  
                  tagList(
                        selectInput("n_region", label = "Select Region:",
                                    choices=temp, 
                                    selected = NULL)
                  )
            }
            
            
      })
      
      output$moreControls <- renderUI ({
            
            if((!is.null(input$n_year) && input$n_year != "Select Year") && 
               (!is.null(input$n_region) || input$n_region == "Select Region")){
                  
                  temp <- as.character(countrymapping$UN.Geoscheme.level.2[countrymapping$UN.Geoscheme.level.1 ==
                                                                                 input$n_region])
                  
                  temp <- c("Select Subregion", temp)
                  tagList(
                        selectInput("n_subregion", label = "Select SubRegion:",
                                    choices=temp, 
                                    selected = NULL)
                  )
            }
            
      })
      
      output$EvenMoreControls <- renderUI ({
            temp <- as.character(
                  countrymapping$Cty.Abbreviation[countrymapping$UN.Geoscheme.level.2 ==
                                                        input$n_subregion])
            temp <- c("Select Country", temp)
            tagList(
                  selectInput("n_country", label = "Select Country:",
                              choices=temp, 
                              selected = NULL)
            )
      })
      
      
      output$Table <- renderTable({
            
            if(!is.null(input$n_year)){
                  source("regional_trade_var.R")
                  dat <- regional_trade_var(input$n_year, "KG")
                  return(dat)  
            }
            else{
                  return(NULL)
            }
            
            
      })
      
      
      
      
      output$Table2 <- renderTable({
            
            
            if((!is.null(input$n_year) && input$n_year != "Select Year") && 
               (!is.null(input$n_region) && input$n_region != "Select Region"  )){
                  print(input$n_region)
                  
                  source("subregional_trade_var.R")
                  dat <- subregional_trade_var(input$n_year, input$n_region, "KG")
                  
                  return(dat)   
            }
            else return(NULL)
            
      })
      
      output$Table3 <- renderTable({
            
            if((!is.null(input$n_year) && input$n_year != "Select Year") && 
               (!is.null(input$n_region) && input$n_region != "Select Region"  ) &&
               (!is.null(input$n_subregion) && input$n_subregion != "Select Subregion")){
                  
                  print(input$n_subregion)
                  
                  source("country_trade_var.R")
                  dat <- country_trade_var(input$n_year, input$n_subregion, "KG")
                  
                  return(dat) 
            }
            else return(NULL)
            
      })
      
      output$Table4 <- renderTable({
            
            if((!is.null(input$n_year) && input$n_year != "Select Year") && 
               (!is.null(input$n_region) && input$n_region != "Select Region"  ) &&
               (!is.null(input$n_subregion) && input$n_subregion != "Select Subregion") &&
               (!is.null(input$n_country) && input$n_country != "Select Country")){
                  
                  source("fg_trade_var.R")
                  dat <- fg_trade_var(input$n_year, input$n_country, "KG")
                  
                  return(dat)
            }
            else return(NULL)
            
            
            
      })
      
      
      output$tabtitle1 <- renderText({
            "Regional Discrepancy Totals"
      })
      output$tabtitle2 <- renderText({
            "Sub- Regional Discrepancy Totals"
      })
      
      output$paneltitle1 <- renderText({
            "Regional Data"
      })
}

# Run the application 
shinyApp(ui = ui, server = server)