library(shiny)
library(bslib)
library(bsicons)
library(tidyverse)
library(plotly)
library(markdown)

link_shiny <- tags$a(
  shiny::icon("github"), "Shiny",
  href = "https://github.com/rstudio/shiny",
  target = "_blank"
)

link_posit <- tags$a(
  shiny::icon("r-project"), "Posit",
  href = "https://posit.co",
  target = "_blank"
)

# Aqui é a pagina principal
main_page <- div(
  includeCSS("styles.css"),
  h1("Sejam bem vindos!"),
  p("Este aplicativo foi feito com carinho."),
  p(lorem::ipsum(paragraphs = 10))
)

#Pagina da analise

analysis_page <- div(
  tagList(
    layout_sidebar(
      sidebar = tagList(
        h2("Gene information"),
        textInput(inputId = "gene_info",
                  label = "Enter with you gene location:",
                  placeholder = "chr1;1;10000;MRF25"),
        textAreaInput(inputId = "exons_info",
                      label = "Enter with your exons locations:",
                      placeholder = "chr1;10;100;exon1\nchr1;200;777;exon2\nchr1;1111;1234;exon3")
      ),
      plotlyOutput(outputId = "grafico")
    )
  )
)


ui <- page_navbar(
  title = tagList("geneStructure",
          HTML(readLines("www/dna.svg"))),
  id = "painel_principal",
  nav_panel(title = tagList("Main",bs_icon("house-heart")), main_page),
  nav_panel(title = tagList("Analise",bs_icon("bar-chart")), analysis_page),
  nav_panel(tagList("About", bs_icon("search")), includeMarkdown("about.md")),
  nav_spacer(),
  nav_menu(
    title = "Links",
    align = "right",
    nav_item(link_shiny),
    nav_item(link_posit)
  ),
  navbar_options = navbar_options(bg = "orange")

)

server <- function(input, output, session) {


  genes_list_react <- reactive({
    gene <- input$gene_info


    gene_info_list <- str_split(gene,pattern = ";")

    gene_chr <- gene_info_list[[1]][1]

    gene_start <- as.numeric(gene_info_list[[1]][2])
    gene_end <- as.numeric(gene_info_list[[1]][3])
    gene_name <- (gene_info_list[[1]][4])

    return(list(gene_name = gene_name,
                gene_chr = gene_chr,
                gene_start = gene_start,
                gene_end = gene_end))
  })

  exons_df_react <- reactive({

    exons <- input$exons_info

    (exons_list <- str_split(exons,pattern = "\n"))

    exons_list <- unlist(exons_list)

    exons_df <- data.frame(do.call(rbind, str_split(exons_list,pattern = ";")))

    colnames(exons_df) <- c("chr","Start","End","exonName")

    exons_df$Name <- gene_name
    exons_df$Start <- as.numeric(exons_df$Start)
    exons_df$End <- as.numeric(exons_df$End)

    return(exons_df)

  })

  output$grafico <- renderPlotly({

    req(input$gene_info, input$exons_info) # Essa linha corrigiu o erro!


    p <- ggplot(data = tibble(Start = genes_list_react()$gene_start,
                              End = genes_list_react()$gene_end,
                              Name = genes_list_react()$gene_name),
                aes(xmin = Start, xmax = End, y = Name)) +
      geom_linerange(linewidth = 2.5,
                     color = "navy") +
      labs(x = genes_list_react()$gene_chr,
           y = "Gene") +
      geom_rect(data = exons_df_react(),
                aes(ymin = .9, ymax = 1.1),
                fill = "navy", col = "grey")


  })

}

shinyApp(ui, server, options = list(launch.browser = TRUE))
