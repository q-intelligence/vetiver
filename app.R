library(shiny)
library(ggplot2)
library(DBI)
library(RSQLite)
make_reactive_trigger <- function() {
    rv <- reactiveValues(a = 0)
    list(
        depend = function() {
            rv$a
            invisible()
        },
        trigger = function() {
            rv$a <- isolate(rv$a + 1)
        }
    )
}
DBTrigger <- make_reactive_trigger()
con <- dbConnect(RSQLite::SQLite(), "allocations.sqlite")
dbExecute(con, 
          'CREATE TABLE Containers (Container int, Amount float, CHECK(Amount >= 0))')
dbExecute(con, 'INSERT INTO Containers (Container, Amount) VALUES(1, 10)')
dbExecute(con, 'INSERT INTO Containers (Container, Amount) VALUES(2, 20)')
dbExecute(con, 'INSERT INTO Containers (Container, Amount) VALUES(3, 30)')
dbExecute(con, 'INSERT INTO Containers (Container, Amount) VALUES(4, 40)')
initContainers <- dbGetQuery(con, 'SELECT Container, Amount FROM Containers')

ui <- fluidPage(
    
    sidebarLayout(
        sidebarPanel(
            tags$h5('Total Resources: 100'),
            selectInput('From', 'Transfer from:', 
                        choices = initContainers[['Container']]),
            selectInput('To', 'Transfer to:', 
                        choices = initContainers[['Container']][-1]),
            numericInput('Amount', 'Amount', min = 0, value = 0, 
                         max = initContainers[['Amount']][1]),
            actionButton('Submit', 'Submit')
        ),
        mainPanel(
            plotOutput("AllocationPlot")
        )
    )
)

server <- function(input, output, session) {
    Allocations <- reactive({
        DBTrigger$depend()
        dbGetQuery(con, 'SELECT Container, Amount FROM Containers')
    })
    #  Transferring to self is redundant
    observeEvent(input$From, {
        updateSelectInput(
            session, 
            'To', 
            choices = Allocations()[['Container']][
                !Allocations()[['Container']] %in% input$From])
    })
    # Limit tranfer amount
    # this will produce a popup warning but is not currently enforced 
    # server side
    # see shinyvalidate package if using bootstrap3
    observeEvent(Allocations(), {
        i <- Allocations()[['Container']] == input$From
        updateNumericInput(
            session, 'Amount',
            max = Allocations()[['Amount']][i])
    })
    observeEvent(input$Submit, {
        res <- tryCatch({
            dbBegin(con)
            # disallow using negative values
            stopifnot(input$Amount > 0)
            dbExecute(con,
                      sqlInterpolate(con,
                                     'UPDATE Containers 
                                     SET Amount = Amount + ?TransferAmount
                                     WHERE Container = ?ToContainer',
                                     TransferAmount = input$Amount,
                                     ToContainer = input$To)
            )
            dbExecute(con,
                      sqlInterpolate(con,
                                     'UPDATE Containers 
                                     SET Amount = Amount - ?TransferAmount
                                     WHERE Container = ?FromContainer',
                                     TransferAmount = input$Amount,
                                     FromContainer = input$From)
            )
            dbCommit(con)
        }, error = function(e) {
            dbRollback(con)
            e
        })
        if ( inherits(res, 'error') ) {
            showNotification(res$message)
        }
        DBTrigger$trigger()
    })
    # Show Latest Allocations
    output$AllocationPlot <- renderPlot({
        ggplot(Allocations()) +
            geom_col(aes(Container, Amount)) +
            labs(title = sprintf('Total Allocated: %d', 
                                 sum(Allocations()[['Amount']]))) +
            theme_bw()
    })
}
shinyApp(ui = ui, server = server)
