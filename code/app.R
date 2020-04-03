#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

if (!require(shiny)) {
  utils::install.packages(pkgs = "shiny", dependencies = TRUE)
}
library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Pedotransfer functions for southern Brazil - carbon and organic matter"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      # Dependent variable
      selectInput(
        inputId = "y", 
        label = "Dependent variable (y)", 
        choices = c("Total organic carbon" = "toc", 
                    "Organic carbon" = "oc", 
                    "Organic matter" = "om",
                    "Total organic matter" = "tom"),
        selected = "toc"),
      
      # Predictor analytical method
      selectInput(
        inputId = "x", 
        label = "Predictor analytical method (x)", 
        choices = c("Total organic carbon" = "toc", 
                    "Organic carbon" = "oc", 
                    "Organic matter" = "om",
                    "Total organic matter" = "tom"),
        selected = "oc"),
      
      # Model formulation (additional predictor variables)
      selectInput(
        inputId = "f", 
        label = "Model formulation", 
        choices = c("y ~ 0 + x" = "A", 
                    "y ~ 1 + x" = "B", 
                    "y ~ 1 + x + x^2" = "C",
                    "y ~ 1 + x + x^2 + topsoil" = "D",
                    "y ~ 1 + x + x^2 + topsoil + landuse" = "E",
                    "y ~ 1 + x*clay + x^2*clay + topsoil + landuse + clay" = "F", 
                    "y ~ 1 + x*clay + x^2*clay + topsoil + landuse + clay + taxon" = "G"),
        selected = "A"),
      
      # Level for prediciton interval computation
      sliderInput(
        inputId = "level",
        label = "Prediction interval",
        min = 0, 
        max = 1, 
        value = 0.95,
        step = 0.01),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select a file ----
      fileInput(
        inputId = "file1", 
        label = "Choose CSV File",
        multiple = FALSE,
        accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
        # buttonLabel = "Procurar...",
        # placeholder = "Nenhum arquivo CSV selecionado"
      ),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Checkbox if file has header ----
      # read.csv(header = __)
      checkboxInput(
        inputId = "header", 
        # label = "Colunas possuem nome", 
        label = "Header",
        value = TRUE),
      
      # Input: Select separator ----
      # read.csv(sep = __)
      radioButtons(
        inputId = "sep", 
        # label = "Separador de campos",
        label = "Field separator",
        # choices = c("Vírgula" = ",", "Ponto e vírgula" = ";", "Tabulação" = "\t"),
        choices = c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"),
        selected = ","),
      
      # Input: Select quotes ----
      # read.csv(quote = __)
      radioButtons(
        inputId = "quote", 
        # label = "Delimitador de texto",
        label = "Text delimiter",
        # choices = c(None = "", "Aspas duplas" = '"', "Aspas simples" = "'"),
        choices = c(None = "", "Double quote" = '"', "Single quote" = "'"),
        selected = '"'),
      
      # Horizontal line ----
      tags$hr(),
      
      # Input: Select number of rows to display ----
      radioButtons(
        inputId = "disp", 
        # label = "Mostrar",
        label = "Display",
        # choices = c("Cabeçalho" = "head", "Tudo" = "all"),
        choices = c("Head" = "head", "All" = "all"),
        selected = "head"),
    
      # Horizontal line ----
      tags$hr(),
      
      # Button
      # downloadButton(outputId = "downloadData", label = "Descarregar resultados")
      downloadButton(outputId = "downloadData", label = "Download")
        
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Data file ----
      tableOutput(
        outputId = "contents")
    )
    
  )
)

# Model
getModel <-
  function (y = c("toc", "oc", "om", "tom"), x = c("oc", "toc", "om", "tom"), f = LETTERS[1:7]) {
    
    # Identify model
    y <- match.arg(y)
    x <- match.arg(x)
    soil_vars <- c("toc", "oc", "om", "tom")
    soil_vars <- expand.grid(soil_vars, soil_vars, stringsAsFactors = FALSE)[, 2:1]
    soil_vars <- soil_vars[!soil_vars[, 1] == soil_vars[, 2], ]
    soil_vars$i <- 1:12
    idx1 <- soil_vars[soil_vars$Var2 == y & soil_vars$Var1 == x, "i"]
    idx2 <- which(f == LETTERS[1:7])
    
    # Load calibrated models
    load("../res/r/model_fit.rda")
    model <- model_fit[[idx1]][[idx2]]
    
    # Output
    return (model)
  }

# Prediction
makePrediction <- 
  function (object, newdata, level, weights) {
    
    # Prediction
    pred <- predict.lm(
      object = object, newdata = newdata, interval = "prediction", level = level, se.fit = TRUE,
      weights = 1 / newdata[[weights]])
    out <- as.data.frame(pred$fit)
    colnames(out)[1] <- paste("pred.", names(object$model)[1], sep = "")
    colnames(out)[2:3] <- paste("pred.", colnames(out)[2:3], sep = "")

    # Prediction error variance
    # out$pred.var <- pred$se.fit^2 + pred$residual.scale^2 * newdata[[weights]]
    # out$prop.var <- pred$residual.scale^2 * newdata[[weights]] / out$pred.var
    
    # Output
    out <- cbind(newdata, out)
    return (out)
  }

# Define server logic to read selected file ----
server <- function (input, output) {
  
  # Carrega arquivo de dados do usuário e faz predições
  getData <- reactive({
    
    inFile <- input$file1
    if (is.null(input$file1)) {
      return (NULL)
    }
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch({
      observations <- 
        read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
      },
      error = function (e) {
        
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      })
    
    # Identify model
    model <- getModel(y = input$y, x = input$x, f = input$f)
    
    # Make prediction
    makePrediction(object = model, newdata = observations, weights = input$x, level = input$level)
  })
  
  # Prepara tabela com os dados para apresentação, inclusive predições
  output$contents <- 
    renderTable(
      if(input$disp == "head") {
        head(getData())
      } else {
        getData()
      }
    )
  
  # Descarregar dados processados
  output$downloadData <- downloadHandler(
    filename = function () { 
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    content = function (file) {
      write.csv(getData(), file, row.names = FALSE, sep = input$sep, quote = input$quote)
    }
  )
}

# Create Shiny app ----
shinyApp(ui, server)
