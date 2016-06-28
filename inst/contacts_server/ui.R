library(shiny)
library(visNetwork)

shinyUI(fluidPage(

    titlePanel("Contact Tracing"),

    sidebarLayout(
        sidebarPanel(
            radioButtons("datasource", "Data", choices = c("Ebola Simulation","MERS South Korea", "Upload Data")),
            conditionalPanel(
            condition = "input.datasource == 'Upload Data'",
            fileInput('linelist', 'Choose A Line List File To Upload',
                accept = c(
                    'text/csv',
                    'text/comma-separated-values',
                    'text/tab-separated-values',
                    'text/plain',
                    '.csv',
                    '.tsv'
                )
            ),
            fileInput('contacts', 'Choose A Contact List File To Upload',
                accept = c(
                    'text/csv',
                    'text/comma-separated-values',
                    'text/tab-separated-values',
                    'text/plain',
                    '.csv',
                    '.tsv'
                )
            ),
            checkboxInput("directed", "Is This A Directed Network ?", value = TRUE)
                ),
            uiOutput("ui1"),
            uiOutput("ui2"),
            actionButton("subset", "Subset Data Based on Inputs Above")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            visNetworkOutput("netplot"),
            tabsetPanel(
                tabPanel('Line List',
                    DT::dataTableOutput("linelisttab")),
                tabPanel('Contact List',
                    DT::dataTableOutput("contactstab"))
            )
        )
    )
))