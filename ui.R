#### DIATHOR SHINY INTERFACE ####
# Load packages ----
library(shiny)
library(shinyFiles)
library(DT)
library(shinybusy)
library(stringdist)
library(vegan)
library(diathor)

#Add additional scripts
source("plots.R")


# User interface ----
ui <- fluidPage(

  titlePanel("DiaThor - Calculate biotic indices using diatoms"),
  "This app will help you calculate biotic indices using the R package 'DiaThor'. For more information, go to",
  tags$a(href="https://github.com/limnolab/DiaThor/","https://github.com/limnolab/DiaThor/"),
  tags$div(class = "header", checked = NA,
    "To test the tool with sample data, you can download and use the data ",
    tags$a(href="https://raw.githubusercontent.com/limnolab/DiaThor/master/sampleData.csv","provided here"),
    tags$br(""),
  ),
  span(textOutput("message"), style="color:red"),
  span(textOutput("consoleOutput"), style="color:blue"),
  span(textOutput("csvsizeOutput"), style="color:darkgreen"),

  sidebarLayout(
    sidebarPanel(

      tabsetPanel(type = "tabs",
                  #Basic tab
                  tabPanel("Basic",
                           #input file
                           h4("1. Choose input file"),
                           fileInput("file1", "Choose a CSV File", accept = ".csv"),

                           #Indices to calculate
                           h4("2. Calculate!"),
                           #Calculate button
                           actionButton("calculatebutton", "Calculate!"),
                           #Select input index


                           #Download all results button
                           # h4("4. Or download all the results (CSV)"),
                           # downloadButton("downloadAll", label = "Download (CSV)"),
                           # actionButton("downloadAll", label = "Download (CSV)", icon = icon("download"))
                  ),
                  #Advanced tab
                  tabPanel("Options",
                           #Advanced options
                           h4("Advanced options:"),
                           radioButtons("relAb", "Is data in relative abundance?", choiceNames = c("Yes", "No"), choiceValues = c(TRUE, FALSE), selected = FALSE),
                           sliderInput("maxDistTaxa", "Maximum distance (in characters) for taxa recognition", value = 2, min = 0, max = 10)
                           # radioButtons("vandamReports", "Export Van Dam reports?", choiceNames = c("Yes", "No"), choiceValues = c(TRUE, FALSE), selected = FALSE),
                           # radioButtons("plotAll", "Export plots in PDF file?", choiceNames = c("Yes", "No"), choiceValues = c(TRUE, FALSE), selected = TRUE),
                           )
                  ),
    ),
    #Main panel with results
    mainPanel(
      tabsetPanel(id = "outputTabs2", type = "tabs",
                  tabPanel("Select result to display", selectInput("indice", label = "Select result to plot", choices = c("IPS", "ILM", "DES", "EPID", "IDAP", "IDCH", "LOBO", "SLA", "SPEAR", "IDP", "TDI", "PBIDW", "DISP", "# Chloroplasts", "Shape Chloroplasts", "Biovolume", "Size classes", "Guilds", "Salinity (Van Dam)", "N-heterotrophy (Van Dam)", "Oxygen (Van Dam)", "Saprobity (Van Dam)", "Moisture (Van Dam)", "Trophic state (Van Dam)"), selected = "IPS"),)
      ),
      tabsetPanel(id = "outputTabs", type = "pills",
                  tabPanel("Input data", div(DT::dataTableOutput('inputTable'), style = "font-size:60%"), width = 300),
                  tabPanel("Species recognized", div(DT::dataTableOutput('recognizedTables'), style = "font-size:60%")),
                  tabPanel("Plots", verbatimTextOutput("plotHelp"), verbatimTextOutput("plotText"),
                           plotOutput("indicePlot")),
                  tabPanel("Indices", downloadButton("downloadData", label = "Download table (CSV)"), DT::dataTableOutput('indiceTables')),
                  tabPanel("All results", downloadButton("downloadAll", label = "Download all results (CSV)"), div(DT::dataTableOutput('AllResultsTables'), style = "font-size:60%"))
                )
             )
    ),
  tags$i("Sample data from: Nicolosi Gelis et al. (2020)- Exploring the use of nuclear alterations, motility and ecological guilds in epipelic diatoms as biomonitoring tools for water quality improvement in urban impacted lowland streams”. Ecological Indicators, 110"),
  textOutput("Version")
)



