library(shiny)
library(contacts)
library(visNetwork)

shinyServer(function(input, output) {

    getData <- reactive({

        req(input$linelist, input$contacts)

        linelist <-
#             read.csv(input$linelist$datapath)
            readr::read_csv(input$linelist$datapath)

        contacts <-
#             read.csv(input$contacts$datapath)
            readr::read_csv(input$contacts$datapath)

        epicontact <- make_epi_contacts(linelist,contacts, directed = TRUE)

        return(epicontact)
    })


    output$ui1 <- renderUI({

        # create list of attributes from linelist
        datcols <- names(getData()$linelist)

        selectInput("interact", "Linelist Attributes", choices = datcols)

    })

    output$ui2 <- renderUI({

        req(input$interact)

        dat <- getData()$linelist

        # create interaction
        numcols <- names(dat[,sapply(dat,is.numeric)])
        datecols <- names(dat[,sapply(dat, inherits, "Date")])
        factorcols <- names(dat[,sapply(dat, is.character)])

        switch(input$interact,
            if(input$interact %in% factorcols) {
                checkboxGroupInput("dynamicfactor", "Dynamic", choices = levels(as.factor(dat[,input$interact])))
                # checkboxGroupInput("dynamicfactor", "Dynamic", choices = unique(dat[,input$interact]))
            } else if (input$interact %in% numcols) {
                numericInput("dynamicnum", input$interact, value = median(dat[,input$interact]))
            } else {
                dateRangeInput("dynamicdates", "Dynamic")
            }
        )
    })

    output$netplot <- renderVisNetwork ({

        dat <- getData()
        x <- get_id(dat, "common")[1:30]
        dat <- dat[x]
        plot(dat, annot = TRUE, group = "gender", editor = TRUE)
    })

    output$linelisttab <- DT::renderDataTable ({

        getData()$linelist

    })

    output$contactstab <- DT::renderDataTable ({

        getData()$contacts

    })
})