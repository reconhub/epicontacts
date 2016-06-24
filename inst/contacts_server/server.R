library(shiny)
library(contacts)
library(visNetwork)

shinyServer(function(input, output) {

    getData <- reactive({

        req(input$linelist, input$contacts)

        linelist <-
            readr::read_csv(input$linelist$datapath)

        contacts <-
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
                checkboxGroupInput("dynamic", "Dynamic", choices = levels(as.factor(dat[,input$interact])))
                # checkboxGroupInput("dynamicfactor", "Dynamic", choices = unique(dat[,input$interact]))
            } else if (input$interact %in% numcols) {
                numericInput("dynamicnum", input$interact, value = median(dat[,input$interact]))
            } else {
                dateRangeInput("dynamicdates", "Dynamic")
            }
        )
    })

    output$netplot <- renderVisNetwork ({

        req(input$interact)
        dat <- getData()
        x <- get_id(dat, "common")[1:30]
        dat <- dat[x]

        # NEED TO CHECK CLASS OF INPUT WIDGET

        interactinput <- as.character(input$interact)

        subsetarglist <- list()
        subsetarglist[[1]] <- input$dynamic
        names(subsetarglist)[1] <- input$interact

        dat <- epi_contacts_subset(dat, node.attribute = subsetarglist)

        plot(dat, annot = TRUE, editor = TRUE)
    })

    output$linelisttab <- DT::renderDataTable ({

        getData()$linelist

    })

    output$contactstab <- DT::renderDataTable ({

        getData()$contacts

    })
})