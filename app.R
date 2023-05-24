source('global.R')

ui <- dashboardPage(skin = 'black',
  dashboardHeader(title = 'scRNAseq Analysis',
                  tags$li(class = "dropdown", actionButton("info", icon('info'), style =  'transform: translateY(20%)'))
                  ),
  dashboardSidebar(
    tags$head(
      tags$style(HTML(".skin-black .main-header .sidebar-toggle {display: none;}"))
    ),
      sidebarMenu(id='tab',
          useShinyjs(),
          menuItem("Home Page", tabName = "home", icon=icon("list")),
          menuItem("scRNAseq Analyzer", tabName = "input", icon=icon("edit")),
          conditionalPanel(condition = "input.tab == 'input'", 
          div(
              fileInput("file", "Upload File", multiple=FALSE, accept=c('.rds')),
              actionButton("reset", "Reset", icon = icon("undo"), style = "color: #fff; background-color: #dc3545; width: 87.25%"),
              actionButton("run", "Run", icon = icon("play"), style = "color: #fff; background-color: #28a745; width: 87.25%")
              )
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "input",
             tabsetPanel(id='main_tabs',
                         tabPanel("Instructions", includeMarkdown('./markdown/instructions.md'))) 
              ),
      tabItem(tabName = "home",
              tabPanel("scRNA-Seq Seurat Analysis Dashboard", includeMarkdown('./markdown/welcome.md'))
              )
    )
  )
)

server <- function(input, output, session){
  options(shiny.maxRequestSize=500*1024^2)
  shinyjs::disable('run')
  observe({
    if (is.null(input$file) != TRUE){
      shinyjs::enable('run')
    } else {
      shinyjs::disable('run')
    }
  })
  
  observeEvent(input$info, {
    shinyalert("scRNAseq Analysis Dashboard", text = HTML("Built by Zain Ziad.<br>Last Updated: May 10th, 2023"), html = TRUE, type = "info")
  })
  
  observeEvent(input$reset, {
    shinyjs::reset("file")
    shinyjs::disable('run')
    removeTab('main_tabs', 'UMAP')
    removeTab('main_tabs', 'Gene Expression')
  })
  
  observeEvent(input$run, {
    shinyjs::disable('run')
    show_modal_spinner(text = "Preparing plots...")
    obj <- load_seurat_obj(input$file$datapath)
    if (is.vector(obj)){
      showModal(modalDialog(
        title = "Error",
        HTML("<h5>There is an error with the file you uploaded. See below for more details.</h5><br>",
             paste(unlist(obj), collapse = "<br><br>"))
      ))
      shinyjs::enable('run')
    } else {
      output$umap <- renderPlot({
        create_metadata_UMAP(obj, input$metadata_col)
      })
      output$featurePlot <- renderPlot({
        create_feature_plot(obj, input$gene)
      })
      
      output$download_umap <- downloadHandler(
        filename = function(){
          paste0(input$metadata_col, '_UMAP.png')
        },
        content = function(file){
          plot <- create_metadata_UMAP(obj, input$metadata_col)
          ggsave(filename = file, width=10, height=5, type='cairo')
        }
      )
      output$downloadFeaturePlot <- downloadHandler(
        filename = function(){
          paste0(input$gene, '_FeaturePlot.png')
        },
        content = function(file){
          plot <- create_feature_plot(obj, input$gene)
          ggsave(filename = file, width=10, height=5, type='cairo')
        }
      )
      
      
      insertTab(
        inputId = 'main_tabs',
        tabPanel("UMAP",
                 fluidRow(column(width = 8, plotOutput(outputId = 'umap'),downloadButton('download_umap', "Download UMAP")),
                          column(width = 4, selectizeInput('metadata_col', "Metadata Column", colnames(obj@meta.data)))))
      )
      insertTab(
        inputId = 'main_tabs',
        tabPanel("Gene Expression",
                 fluidRow(column(width = 8, plotOutput(outputId  = 'featurePlot'),downloadButton('downloadFeaturePlot', "Download Feature Plot")),
                          column(width = 4, selectizeInput('gene', "Genes", rownames(obj)))))
      )
      remove_modal_spinner()
      shinyjs::enable('run')
    }
  })
}

shinyApp(ui, server) 