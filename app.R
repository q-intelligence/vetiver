library(shiny)
library(dplyr)
library(purrr)
library(gapminder)
library(highcharter)


ui <- fluidPage(
    sidebarLayout(
        sidebarPanel(
            titlePanel("R Shiny Highcharts"),
            selectInput(
                inputId = "inContinent",
                label = "Continent:",
                choices = unique(gapminder$continent),
                selected = "Europe"
            ),
            selectInput(
                inputId = "inYearMin",
                label = "Start year:",
                choices = unique(gapminder$year)[1:length(unique(gapminder$year)) - 1],
                selected = min(gapminder$year)
            ),
            selectInput(
                inputId = "inYearMax",
                label = "End year:",
                choices = unique(gapminder$year)[2:length(unique(gapminder$year))],
                selected = max(gapminder$year)
            ),
            width = 3
        ),
        mainPanel(
            tags$h3("Latest stats:"),
            tags$div(
                tags$div(
                    tags$p("#Countries"),
                    textOutput(outputId = "outNCountries")
                ),
                tags$div(
                    tags$p("Median life exp."),
                    textOutput(outputId = "outMedLifeExp")
                ),
                tags$div(
                    tags$p("Median population"),
                    textOutput(outputId = "outMedPop")
                ),
                tags$div(
                    tags$p("Median GDP"),
                    textOutput(outputId = "outMedGDP")
                )
            ),
            width = 9
        )
    )
)

server <- function(input, output) {
    data_cards <- reactive({
        gapminder %>%
            filter(
                continent == input$inContinent,
                year == max(year)
            ) %>%
            summarise(
                nCountries = n_distinct(country),
                medianLifeExp = median(lifeExp),
                medianPopM = median(pop / 1e6),
                medianGDP = median(gdpPercap)
            )
    })
    
    output$outNCountries <- renderText({
        data_cards()$nCountries
    })
    output$outMedLifeExp <- renderText({
        paste(round(data_cards()$medianLifeExp, 1), "years")
    })
    output$outMedPop <- renderText({
        paste0(round(data_cards()$medianPopM, 2), "M")
    })
    output$outMedGDP <- renderText({
        paste0("$", round(data_cards()$medianGDP, 2))
    })
}


shinyApp(ui = ui, server = server)