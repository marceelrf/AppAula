library(shiny)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      h2("Choices"),
      fileInput(inputId = "file",
                label = "Entre com seus dados.",
                multiple = FALSE,
                accept = ".xlsx"),
      hr(),
      uiOutput(outputId = "selectruns")
      # selectInput(inputId = "runs",
      #             label = "Escolha as corridas",
      #             choices = unique(dados$Run),
      #             selected = 1:11,
      #             multiple = TRUE)
    ),
    mainPanel(
      plotlyOutput(outputId = "grafico"),
      dataTableOutput(outputId = "tabela")
    )
  )
)

server <- function(input, output, session) {

  dados <- reactive({

    meuarquivo <- input$file

    req(meuarquivo)

    readxl::read_xlsx(path = meuarquivo$datapath)
  })

  output$selectruns <- renderUI({

    req(dados())

    selectInput(inputId = "runs",
                label = "Escolha as corridas",
                choices = unique(dados()$Run),
                selected = 1:11,
                multiple = TRUE)
  })


  dados_filt <- reactive({

    dados() %>%
      filter(Run %in% input$runs)
  })

  output$tabela <- renderDataTable({

    req(dados_filt())

    dados_filt() %>%
      group_by(conc) %>%
      summarise(media = mean(density),
                desvp = sd(density))
  })

  output$grafico <- renderPlotly({

    req(dados_filt())

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
