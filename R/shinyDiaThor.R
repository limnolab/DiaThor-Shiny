#### BASIC TEMPLATE ####
install.packages("shiny")
library(shiny)

ui <- fluidPage()

server <- function (input, output) {
}

shinyApp(ui = ui, server = server)

#### END BASIC TEMPLATE ####

#### DIATHOR SHINY TEST ####
# Load packages ----
library(shiny)
library(shinyFiles)
library(stringdist)
library(vegan)
library(diathor)


# User interface ----
ui <- fluidPage(
  titlePanel("Testing Diathor"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Use Diathor"),
      fileInput("file1", "Choose CSV File", accept = ".csv"),
      radioButtons("relAb", "Relative abundance?", choices = c("Yes", "No"), selected = "No"),
      
    selectInput("indice", label = "Select index to calculate", choices = c("All indices","IDP", "TDI"), selected = "All indices"),
    
    shinyDirButton("outputdir", "Output directory", "Select"),
    verbatimTextOutput("outputdirLabel", placeholder = TRUE),

    actionButton("calculatebutton", "Calculate!")
    ),

    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Input", tableOutput("inputTable")),
                  tabPanel("Plots", plotOutput("indicePlot")),
                  tabPanel("Tables", tableOutput("indiceTables"))
      )
      )
  )
)

# test function
loli.plot <- function(result, ylabel, ylow, yhigh, samplenames, color = "#0073C2"){
  x <- rownames(result)
  data <- result
  data[is.na(data)] = 0
  
  for(i in 1:ncol(result)) {
    y <- data[,i]
    
    return(ggplot2::ggplot(data, aes(x=samplenames, y=y)) +
             geom_segment( aes(x=samplenames, xend=samplenames, y=0, yend=y), color="grey") +
             geom_point( color=color, size=4) +
             theme_light() +
             theme(
               panel.grid.major.x = element_blank(),
               panel.border = element_blank(),
               axis.ticks.x = element_blank()
             ) +
             ylim(ylow, yhigh) +
             xlab("") +
             ylab(ylabel)
    )
  }
}


# Server logic ----
server <- function(input, output){
  #MONITOR OUTPUT FOLDER BUTTON
  volumes <- getVolumes()()
  observe({
    shinyDirChoose(input, 'outputdir', roots=volumes)
    outputDirectory <- parseDirPath(volumes, input$outputdir)
    if (length(outputDirectory)!=0){
        filepathsObject <- renderPrint(outputDirectory)
        output$outputdirLabel <- filepathsObject
    } else {
      output$outputdirLabel <- renderText("Choose output folder")
    }
  })
  
  
  #CALCULATE BUTTON
  observeEvent(input$calculatebutton, {
    #GET SELECTED OUTPUT FOLDER
    shinyDirChoose(input, 'outputdir', roots=volumes)
    outputDirectory <- parseDirPath(volumes, input$outputdir)
    if (length(outputDirectory)==0){
      output$outputdirLabel <- renderText("Choose output folder")
    } else {
      #GET INPUT FILE
      file <- input$file1
      filecsv <- read.csv(file$datapath)
      loadedData <- diat_loadData(species_df = filecsv, resultsPath = outputDirectory)
      
      #INPUT TABLE
      output$inputTable <- renderTable({
        file <- input$file1
        ext <- tools::file_ext(file$datapath)
        req(file)
        validate(need(ext == "csv", "Please upload a csv file"))
        read.csv(file$datapath)
      })
      
      #PLOTS & TABLES
      if (input$indice == "IDP"){
        idp.results <- diat_idp(loadedData)
        output$indicePlot <- renderPlot(loli.plot(as.data.frame(idp.results[,1]), "IDP", 0, 4, samplenames=rownames(idp.results)))
        output$indiceTables <- renderTable(idp.results)
        
      } else if (input$indice == "TDI"){
        tdi.results <- diat_tdi(loadedData)
        output$indicePlot <- renderPlot(loli.plot(as.data.frame(tdi.results[,1]), "TDI - Standardized", 0, 20, samplenames=rownames(tdi.results)) + geom_hline(yintercept=1, linetype="dashed", color = "darkgray", size=1))
        output$indiceTables <- renderTable(tdi.results)
        
      } else if (input$indice == "All indices"){
        all.results <- diaThorAll(species_df = filecsv, resultsPath = outputDirectory)
        output$indiceTables <- renderTable(all.results)
      }      
    } #close if output folder is selected
  })
}
  


# Run app ----
shinyApp(ui, server)


