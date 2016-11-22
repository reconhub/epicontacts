library(shiny)
library(epicontacts)
library(visNetwork)

shinyServer(function(input, output) {

    getData <- reactive({

        if (input$datasource == "Ebola Simulation") {

            epicontact <- make_epi_contacts(ebola.sim$linelist,ebola.sim$contacts, directed = TRUE)

            # subset the first 100 records so the network isn't too big
            x <- get_id(epicontact, "common")[1:100]

            epicontact <- epicontact[x]

            return(epicontact)

        } else if (input$datasource == "MERS South Korea") {

            epicontact <- make_epi_contacts(mers.korea.2015[[1]],mers.korea.2015[[2]], directed = TRUE)

            return(epicontact)

        } else {

            req(input$linelist, input$contacts)

            linelist <-
                readr::read_csv(input$linelist$datapath)

            contacts <-
                readr::read_csv(input$contacts$datapath)

            epicontact <- make_epi_contacts(linelist,contacts, directed = input$directed)

            return(epicontact)
        }
    })

    subsetData <- eventReactive(input$subset, {

        dat <- getData()

        # build arguments for subsetting

        subsetarglist <- list()

        if (inherits(dat$linelist[,input$interact], "Date")){

            subsetarglist[[1]] <- c(as.Date(input$dynamic[1]),as.Date(input$dynamic[2]))

        } else if(inherits(dat$linelist[,input$interact], "numeric")) {

            subsetarglist[[1]] <- input$dynamic

        } else {

            subsetarglist[[1]] <- input$dynamic

        }

        names(subsetarglist)[1] <- input$interact

        # call epi_contacts method for subsetting
        subset(dat, node.attribute = subsetarglist)

    })

    output$ui1 <- renderUI({

        # create list of attributes from linelist minus the id column
        datcols <- names(getData()$linelist)[-1]
        selectInput("interact", "Linelist Attributes", choices = datcols)

    })

    output$ui2 <- renderUI({

        req(input$interact)

        dat <- getData()$linelist

        # define character of factor checking function

        factchar <- function(x) {

            is.character(x) | is.factor(x)

        }

        # create list of input options based on class of columm

        numcols <- names(dat[,sapply(dat,inherits, "numeric")])
        datecols <- names(dat[,sapply(dat, inherits, "Date")])
        factorcols <- names(dat[,sapply(dat, factchar)])

        switch(input$interact,
            if(input$interact %in% factorcols) {
                radioButtons("dynamic", input$interact, choices = levels(as.factor(dat[,input$interact])), selected = NULL)
            } else if (input$interact %in% numcols) {
                numericInput("dynamic", input$interact, value = median(dat[,input$interact]))
            } else if (input$interact %in% datecols) {
                dateRangeInput("dynamic", input$interact)
            } else {
                textInput("dynamic", input$interact)
            }
        )
    })

    output$netplot <- renderVisNetwork ({

        req(input$interact)

        if(input$subset) {
            dat <- subsetData()
        } else {
            dat <- getData()
        }

        plot(dat, annot = TRUE, editor = TRUE, group = input$interact)

    })

    output$linelisttab <- DT::renderDataTable ({

        req(input$interact)

        if(input$subset) {
            subsetData()$linelist
        } else {
            getData()$linelist
        }

    })

    output$contactstab <- DT::renderDataTable ({

        req(input$interact)

        if(input$subset) {
            subsetData()$contacts
          } else {
          getData()$contacts
          }

    })
})
