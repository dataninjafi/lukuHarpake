## app.R ##
library(shinydashboard)
library(shiny)
library(sorvi)

#KÄLI

ui <- dashboardPage(
    dashboardHeader(title = "Basic Dashboard"),
    dashboardSidebar(
        fileInput('file1',

                  'Choose file to upload',
                  
                  accept = c(
                      'text/csv',
                      'text/comma-separated-values',
                      'text/tab-separated-values',
                      'text/plain',
                      '.csv',
                      '.tsv'
                  )
        ),
        checkboxInput('header', 'Header', TRUE),
        radioButtons('sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        radioButtons('quote', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '"'),
        conditionalPanel(
            condition = "output.fileUploaded",
            selectInput("period", "Period:", 
                        choices = list("Years" = 1, "Months" = 2))
        )
    ),
    dashboardBody(
        conditionalPanel(condition = "!output.fileUploaded",
                         box("Data lataamatta")),
        conditionalPanel(condition = "output.fileUploaded",
                         box(uiOutput('columns'))
                         )
                             
        
    )

)

# SERVERI

server <- function(input, output) {
    
    getData <- reactive({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        
        inFile <- input$file1
        
        if (is.null(inFile))
            return(NULL)
        
        read.csv(inFile$datapath, header = input$header,
                 sep = input$sep, quote = input$quote)
    })
    output$fileUploaded <- reactive({
        return(!is.null(getData()))
    })
    
    
    
    hetuSarake <- reactive({
        data <- getData() %>%
            select_if( function(col) all( valid_hetu(col) ) ) %>% 
            names()
        if(!length(data)==1) return(NULL)
        data
    })
    
    output$columns = renderUI({
        found <- hetuSarake()
        selectInput('columns2', 'Columns', names( getData() ), selected=found  )
    })
    
    dateSarakkeet <- reactive({
        data <- getData() %>%
            head(100) %>% 
            select_if( function(cols) all( is.Date(cols) ) )
        if(!ncol(data)==2) return(NULL) 
        #palautetaan alk ensin
        data %>% 
            summarise_all(sum) %>% 
            gather %>%
            arrange(desc(value)) %>% 
            .$key
            
    })
    
    
    
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
    
    
    
}

shinyApp(ui, server)