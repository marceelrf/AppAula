library(shiny)
library(lorem)
library(bslib)
library(tidyverse)
library(plotly)
library(DT)

# Dados fixos
dados <- DNase # readr::read_csv... readxl::read_xlsx

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h2("Choices"),
      selectInput(inputId = "runs",
                  label = "Escolha as corridas",
                  choices = unique(dados$Run),
                  selected = 1:11,
                  multiple = TRUE)
    ),
    mainPanel(
      plotlyOutput(outputId = "grafico"),
      dataTableOutput(outputId = "tabela")
    )
  )
)

server <- function(input, output, session) {

  dados_filt <- reactive({

    dados %>%
      filter(Run %in% input$runs)
    })

  output$tabela <- renderDataTable({

    dados_filt() %>%
      group_by(conc) %>%
      summarise(media = mean(density),
                desvp = sd(density))
  })

  output$grafico <- renderPlotly({

    p <- dados_filt() %>%
      ggplot(aes(x = conc ,
                 y = density,
                 color = Run)) +
      geom_line() +
      geom_point()

    ggplotly(p)
  })

}

shinyApp(ui, server)
