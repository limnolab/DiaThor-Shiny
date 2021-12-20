#### DIATHOR SHINY INTERFACE ####
# Load packages ----
library(shiny)
library(shinyFiles)
library(DT)
library(shinybusy)
library(stringdist)
library(vegan)
library(diathor)
library(BH)

#Add additional scripts
source("plots.R")


# Server logic ----
server <- function(input, output, session){

  #Tab control
  hideTab(inputId = "outputTabs2", target = "Select result to display")
  hideTab(inputId = "outputTabs", target = "Input data")
  hideTab(inputId = "outputTabs", target = "Plots")
  hideTab(inputId = "outputTabs", target = "Indices")
  hideTab(inputId = "outputTabs", target = "Species recognized")
  hideTab(inputId = "outputTabs", target = "All results")

  output$Version <- renderText(paste("Package version: ",packageVersion("diathor")))

  #MONITOR INPUT FILE CHANGE
  observeEvent(input$file1, {
    file <- input$file1
    filecsv <- read.csv(file$datapath)
    #show the input file in the "input" tab
    output$inputTable <- DT::renderDataTable(
      DT::datatable(filecsv, options = list(pageLength = 25))
    )
    #column validation
    if("species" %in% colnames(filecsv)) {
      output$consoleOutput <- renderText ({"Species column found"})
      } else {
      output$consoleOutput <- renderText ({"Species column not found. The first column of the file has to contain the species' name, and be labeled 'species'"})
      return()
      }
    if("acronym" %in% colnames(filecsv)) {
      output$consoleOutput <- renderText ({"Species column found. Acronym column found"})
    } else {
      output$consoleOutput <- renderText ({"Species column found. Acronym column not found. The acronyms for each species will be searched for, although there is a chance they are not found."})
    }
    #Summary of the DB

    output$csvsizeOutput <- renderText ({paste("File loaded, with", nrow(filecsv), "species and", ncol(filecsv) - 2, "samples", sep =" " )})

    # #Show tab
    showTab(inputId = "outputTabs", target = "Input data")
 })


  #CALCULATE BUTTON
  observeEvent(input$calculatebutton, {

    #GET INPUT FILE
    file <- input$file1
    #Check if input file exists
    if (is.null(file)){
      output$message <- renderText({"No input file"})
      return(NULL)
    }
    filecsv <- read.csv(file$datapath)
    print(file$datapath)
    #SET OUTPUT FOLDER
    wd <- getwd()
    outputDirectory <- tempdir()

    #reset validation text
    output$message <- renderText({""})
    output$plotHelp <- renderText({"Hint: download or copy the plots by right-clicking on them!"})




    #PLOTS & TABLES FOR ALL INDICES
      show_modal_spinner(spin = "flower",text = "Calculating all indices, this might take a while, please wait!") # show the modal window

      #load data and calculate diathorAll
      loadedData <- diathor::diat_loadData(species_df = filecsv, resultsPath = outputDirectory, isRelAb = input$relAb, maxDistTaxa = input$maxDistTaxa)
      all.results <- diathor::diaThorAll(species_df = filecsv, resultsPath = outputDirectory, isRelAb = input$relAb, maxDistTaxa = input$maxDistTaxa, vandamReports = FALSE, plotAll = FALSE, exportFormat = 3)
      sampleNames <- rownames(all.results)

      #Show all results
      output$AllResultsTables <- DT::renderDataTable(
        DT::datatable(all.results, options = list(pageLength = 25))
      )
      outputMainTable <- reactive(all.results)

      #calculate numcloroplastos.result, biovol.val.result, size.results, guilds.results, vandam.results
      morphoresults <- diat_morpho(loadedData)
      numcloroplastos.result <- morphoresults[[1]]
      shpcloroplastos.result <- morphoresults[[2]]
      biovol.val.result <- morphoresults[[3]]
      size.results <- diat_size(loadedData)
      guilds.results <- diat_guilds(loadedData)

      #Van Dam results
      vandam.results <- diat_vandam(loadedData)
      vdamSalinity <- vandam.results[,startsWith(colnames(vandam.results),"VD.Salinity")]
      vdamNHeterotrophy <- vandam.results[,startsWith(colnames(vandam.results),"VD.N.Het")]
      vdamOxygen <- vandam.results[,startsWith(colnames(vandam.results),"VD.Oxygen")]
      vdamSaprobity <- vandam.results[,startsWith(colnames(vandam.results),"VD.Saprobity")]
      vdamAero <- vandam.results[,startsWith(colnames(vandam.results),"VD.Aero")]
      vdamTrophic <- vandam.results[,startsWith(colnames(vandam.results),"VD.Trophic")]
      #remove the Taxa Used column
      vdamSalinity <- vdamSalinity[1:(ncol(vdamSalinity)-1)]
      vdamNHeterotrophy <- vdamNHeterotrophy[1:(ncol(vdamNHeterotrophy)-1)]
      vdamOxygen <- vdamOxygen[1:(ncol(vdamOxygen)-1)]
      vdamSaprobity <- vdamSaprobity[1:(ncol(vdamSaprobity)-1)]
      vdamAero <- vdamAero[1:(ncol(vdamAero)-1)]
      vdamTrophic <- vdamTrophic[1:(ncol(vdamTrophic)-1)]



      # Downloadable csv of selected dataset ----
      output$downloadData <- downloadHandler(
        filename = function(){"All results.csv"},
        content = function(file) {
          write.csv(outputMainTable(), file, row.names = TRUE)
        }
      )

      # Add recognized_acronyms
      #recognizedData <- as.data.frame(cbind(rownames(loadedData[["taxaIn"]]), loadedData[["taxaIn"]]$acronym,loadedData[["taxaIn"]]$new_species ))
      recognizedData <- as.data.frame(cbind(rownames(loadedData[["taxaIn"]]),loadedData[["taxaIn"]]$new_species ))
      #colnames(recognizedData) <- c("Input species", "Recognized acronym", "Updated species")
      colnames(recognizedData) <- c("Input species","Updated species")
      output$recognizedTables <- DT::renderDataTable(
        DT::datatable(recognizedData, options = list(pageLength = 25))
      )
      # output$plotHelp <- renderText("Select index to plot from the combo box")
      remove_modal_spinner()

  #PLOTS INDIVIDUAL INDICES (inside Calculate button)
  observeEvent(input$indice,

               if (input$indice == "IPS"){
                 output$indicePlot <- renderPlot(loli.plot(as.data.frame(all.results[,"IPS"]), "IPS", 0, 5, samplenames=rownames(all.results)) + geom_hline(yintercept=1, linetype="dashed", color = "darkgray", size=1))
                 indexdata <- cbind(all.results[,"IPS"], all.results[,"IPS20"])
                 rownames(indexdata) <- rownames(all.results)
                 colnames(indexdata) <- c("IPS", "IPS20")

                 output$indiceTables <- DT::renderDataTable(
                   DT::datatable(indexdata, options = list(pageLength = 25))
                 )
                 outputMainTable <- reactive(indexdata)
                 # Downloadable csv of selected dataset ----
                 output$downloadData <- downloadHandler(
                   filename = function(){"ips.csv"},
                   content = function(file) {
                     write.csv(outputMainTable(), file, row.names = TRUE)
                   }
                 )
               }
               else if (input$indice == "ILM"){
                 output$indicePlot <- renderPlot(loli.plot(as.data.frame(all.results[,"ILM"]), "ILM", 0, 5, samplenames=rownames(all.results)) + geom_hline(yintercept=1, linetype="dashed", color = "darkgray", size=1))
                 indexdata <- cbind(all.results[,"ILM"], all.results[,"ILM20"])
                 rownames(indexdata) <- rownames(all.results)
                 colnames(indexdata) <- c("ILM", "ILM20")


                 output$indiceTables <- DT::renderDataTable(
                   DT::datatable(indexdata, options = list(pageLength = 25))
                 )
                 outputMainTable <- reactive(indexdata)
                 # Downloadable csv of selected dataset ----
                 output$downloadData <- downloadHandler(
                   filename = function(){"ilm.csv"},
                   content = function(file) {
                     write.csv(outputMainTable(), file, row.names = TRUE)
                   }
                 )
               } else if (input$indice == "DES"){
                 output$indicePlot <- renderPlot(loli.plot(as.data.frame(all.results[,"DES"]), "DES", 0, 5, samplenames=rownames(all.results)) + geom_hline(yintercept=1, linetype="dashed", color = "darkgray", size=1))
                 indexdata <- cbind(all.results[,"DES"], all.results[,"DES20"])
                 rownames(indexdata) <- rownames(all.results)
                 colnames(indexdata) <- c("DES", "DES20")

                 output$indiceTables <- DT::renderDataTable(
                   DT::datatable(indexdata, options = list(pageLength = 25))
                 )
                 outputMainTable <- reactive(indexdata)
                 # Downloadable csv of selected dataset ----
                 output$downloadData <- downloadHandler(
                   filename = function(){"des.csv"},
                   content = function(file) {
                     write.csv(outputMainTable(), file, row.names = TRUE)
                   }
                 )
               } else if (input$indice == "EPID"){
                 output$indicePlot <- renderPlot(loli.plot(as.data.frame(all.results[,"EPID"]), "EPID", 0, 4, samplenames=rownames(all.results)))
                 indexdata <- cbind(all.results[,"EPID"], all.results[,"EPID20"])
                 rownames(indexdata) <- rownames(all.results)
                 colnames(indexdata) <- c("EPID", "EPID20")

                 output$indiceTables <- DT::renderDataTable(
                   DT::datatable(indexdata, options = list(pageLength = 25))
                 )
                 outputMainTable <- reactive(indexdata)
                 # Downloadable csv of selected dataset ----
                 output$downloadData <- downloadHandler(
                   filename = function(){"epid.csv"},
                   content = function(file) {
                     write.csv(outputMainTable(), file, row.names = TRUE)
                   }
                 )
               } else if (input$indice == "IDAP"){
                 output$indicePlot <- renderPlot(loli.plot(as.data.frame(all.results[,"IDAP"]), "IDAP", 0, 5, samplenames=rownames(all.results)) + geom_hline(yintercept=1, linetype="dashed", color = "darkgray", size=1))
                 indexdata <- cbind(all.results[,"IDAP"], all.results[,"IDAP20"])
                 rownames(indexdata) <- rownames(all.results)
                 colnames(indexdata) <- c("IDAP", "IDAP20")

                 output$indiceTables <- DT::renderDataTable(
                   DT::datatable(indexdata, options = list(pageLength = 25))
                 )
                 outputMainTable <- reactive(indexdata)
                 # Downloadable csv of selected dataset ----
                 output$downloadData <- downloadHandler(
                   filename = function(){"idap.csv"},
                   content = function(file) {
                     write.csv(outputMainTable(), file, row.names = TRUE)
                   }
                 )

               } else if (input$indice == "IDCH"){
                 output$indicePlot <- renderPlot(loli.plot(as.data.frame(all.results[,"IDCH"]), "IDCH", 0, 8, samplenames=rownames(all.results)) + geom_hline(yintercept=1, linetype="dashed", color = "darkgray", size=1))
                 indexdata <- cbind(all.results[,"IDCH"], all.results[,"IDCH20"])
                 rownames(indexdata) <- rownames(all.results)
                 colnames(indexdata) <- c("IDCH", "IDCH20")

                 output$indiceTables <- DT::renderDataTable(
                   DT::datatable(indexdata, options = list(pageLength = 25))
                 )
                 outputMainTable <- reactive(indexdata)
                 # Downloadable csv of selected dataset ----
                 output$downloadData <- downloadHandler(
                   filename = function(){"idch.csv"},
                   content = function(file) {
                     write.csv(outputMainTable(), file, row.names = TRUE)
                   }
                 )
               } else if (input$indice == "LOBO"){
                 output$indicePlot <- renderPlot(loli.plot(as.data.frame(all.results[,"LOBO"]), "LOBO", 0, 4, samplenames=rownames(all.results))+ geom_hline(yintercept=1, linetype="dashed", color = "darkgray", size=1))
                 indexdata <- cbind(all.results[,"LOBO"], all.results[,"LOBO20"])
                 rownames(indexdata) <- rownames(all.results)
                 colnames(indexdata) <- c("LOBO", "LOBO20")

                 output$indiceTables <- DT::renderDataTable(
                   DT::datatable(indexdata, options = list(pageLength = 25))
                 )
                 outputMainTable <- reactive(indexdata)
                 # Downloadable csv of selected dataset ----
                 output$downloadData <- downloadHandler(
                   filename = function(){"lobo.csv"},
                   content = function(file) {
                     write.csv(outputMainTable(), file, row.names = TRUE)
                   }
                 )
               } else if (input$indice == "SLA"){
                 output$indicePlot <- renderPlot(loli.plot(as.data.frame(all.results[,"SLA"]), "SLA", 0, 4, samplenames=rownames(all.results)))
                 indexdata <- cbind(all.results[,"SLA"], all.results[,"SLA20"])
                 rownames(indexdata) <- rownames(all.results)
                 colnames(indexdata) <- c("SLA", "SLA20")

                 output$indiceTables <- DT::renderDataTable(
                   DT::datatable(indexdata, options = list(pageLength = 25))
                 )
                 outputMainTable <- reactive(indexdata)
                 # Downloadable csv of selected dataset ----
                 output$downloadData <- downloadHandler(
                   filename = function(){"sla.csv"},
                   content = function(file) {
                     write.csv(outputMainTable(), file, row.names = TRUE)
                   }
                 )
               } else if (input$indice == "SPEAR"){
                 output$indicePlot <- renderPlot(loli.plot(as.data.frame(all.results[,"SPEAR"]), "SPEAR", 0, 100, samplenames=rownames(all.results)))
                 indexdata <- as.data.frame(all.results[,"SPEAR"])
                 rownames(indexdata) <- rownames(all.results)
                 colnames(indexdata) <- "SPEAR"

                 output$indiceTables <- DT::renderDataTable(
                   DT::datatable(indexdata, options = list(pageLength = 25))
                 )
                 outputMainTable <- reactive(indexdata)
                 # Downloadable csv of selected dataset ----
                 output$downloadData <- downloadHandler(
                   filename = function(){"spear.csv"},
                   content = function(file) {
                     write.csv(outputMainTable(), file, row.names = TRUE)
                   }
                 )
               } else if (input$indice == "IDP"){
                 output$indicePlot <- renderPlot(loli.plot(as.data.frame(all.results[,"IDP"]), "IDP", 0, 4, samplenames=rownames(all.results)))
                 indexdata <- cbind(all.results[,"IDP"], all.results[,"IDP20"])
                 rownames(indexdata) <- rownames(all.results)
                 colnames(indexdata) <- c("IDP", "IDP20")

                 output$indiceTables <- DT::renderDataTable(
                   DT::datatable(indexdata, options = list(pageLength = 25))
                 )
                 outputMainTable <- reactive(indexdata)
                 # Downloadable csv of selected dataset ----
                 output$downloadData <- downloadHandler(
                   filename = function(){"idp.csv"},
                   content = function(file) {
                     write.csv(outputMainTable(), file, row.names = TRUE)
                   }
                 )
               } else if (input$indice == "TDI"){
                output$indicePlot <- renderPlot(loli.plot(as.data.frame(all.results[,"TDI20"]), "TDI - Standardized", 0, 20, samplenames=rownames(all.results)) + geom_hline(yintercept=1, linetype="dashed", color = "darkgray", size=1))
                indexdata <- cbind(all.results[,"TDI20"], all.results[,"TDI100"])
                rownames(indexdata) <- rownames(all.results)
                colnames(indexdata) <- c("TDI20", "TDI100")

                 output$indiceTables <- DT::renderDataTable(
                   DT::datatable(indexdata, options = list(pageLength = 25))
                 )
                 outputMainTable <- reactive(indexdata)
                 # Downloadable csv of selected dataset ----
                 output$downloadData <- downloadHandler(
                   filename = function(){"tdi.csv"},
                   content = function(file) {
                     write.csv(outputMainTable(), file, row.names = TRUE)
                   }
                 )
               } else if (input$indice == "PBIDW"){
                 output$indicePlot <- renderPlot(loli.plot(as.data.frame(all.results[,"PBIDW"]), "PBIDW", 0, 100, samplenames=rownames(all.results)))
                 indexdata <- as.data.frame(all.results[,"PBIDW"])
                 rownames(indexdata) <- rownames(all.results)
                 colnames(indexdata) <- "PBIDW"

                 output$indiceTables <- DT::renderDataTable(
                   DT::datatable(indexdata, options = list(pageLength = 25))
                 )
                 outputMainTable <- reactive(indexdata)
                 # Downloadable csv of selected dataset ----
                 output$downloadData <- downloadHandler(
                   filename = function(){"pbidw.csv"},
                   content = function(file) {
                     write.csv(outputMainTable(), file, row.names = TRUE)
                   }
                 )
               } else if (input$indice == "DISP"){
                 output$indicePlot <- renderPlot(loli.plot(as.data.frame(all.results[,"DISP"]), "DISP", 0, 6, samplenames=rownames(all.results)))
                 indexdata <- as.data.frame(all.results[,"DISP"])
                 rownames(indexdata) <- rownames(all.results)
                 colnames(indexdata) <- "DISP"

                 output$indiceTables <- DT::renderDataTable(
                   DT::datatable(indexdata, options = list(pageLength = 25))
                 )
                 outputMainTable <- reactive(indexdata)
                 # Downloadable csv of selected dataset ----
                 output$downloadData <- downloadHandler(
                   filename = function(){"disp.csv"},
                   content = function(file) {
                     write.csv(outputMainTable(), file, row.names = TRUE)
                   }
                 )
               }
               #MORPHOS
               else if (input$indice == "# Chloroplasts"){
                 output$indicePlot <- renderPlot(percentbarchart.plot(numcloroplastos.result, "Number of chloroplasts", sampleNames))
                 output$indiceTables <- DT::renderDataTable(
                   DT::datatable(numcloroplastos.result, options = list(pageLength = 25))
                 )
                 outputMainTable <- reactive(numcloroplastos.result)
                 # Downloadable csv of selected dataset ----
                 output$downloadData <- downloadHandler(
                   filename = function(){"# Chloroplasts.csv"},
                   content = function(file) {
                     write.csv(outputMainTable(), file, row.names = TRUE)
                   }
                 )
               } else if (input$indice == "Shape Chloroplasts"){
                 output$indicePlot <- renderPlot(percentbarchart.plot(shpcloroplastos.result, "Shape of chloroplasts", sampleNames))
                 output$indiceTables <- DT::renderDataTable(
                   DT::datatable(shpcloroplastos.result, options = list(pageLength = 25))
                 )
                 outputMainTable <- reactive(shpcloroplastos.result)
                 # Downloadable csv of selected dataset ----
                 output$downloadData <- downloadHandler(
                   filename = function(){"Shape Chloroplasts.csv"},
                   content = function(file) {
                     write.csv(outputMainTable(), file, row.names = TRUE)
                   }
                 )
               } else if (input$indice == "Biovolume"){
                 output$indicePlot <- renderPlot(loli.plot(as.data.frame(biovol.val.result[,1]), "Biovolume", 0, max(as.data.frame(biovol.val.result[,1])), samplenames=sampleNames))
                 output$indiceTables <- DT::renderDataTable(
                   DT::datatable(biovol.val.result, options = list(pageLength = 25))
                 )
                 outputMainTable <- reactive(biovol.val.result)
                 # Downloadable csv of selected dataset ----
                 output$downloadData <- downloadHandler(
                   filename = function(){"Biovolume.csv"},
                   content = function(file) {
                     write.csv(outputMainTable(), file, row.names = TRUE)
                   }
                 )
               } else if (input$indice == "Size classes"){
                 output$indicePlot <- renderPlot(percentbarchart.plot(size.results, "Size classes", sampleNames))
                 output$indiceTables <- DT::renderDataTable(
                   DT::datatable(size.results, options = list(pageLength = 25))
                 )
                 outputMainTable <- reactive(size.results)
                 # Downloadable csv of selected dataset ----
                 output$downloadData <- downloadHandler(
                   filename = function(){"Size classes.csv"},
                   content = function(file) {
                     write.csv(outputMainTable(), file, row.names = TRUE)
                   }
                 )
               } else if (input$indice == "Guilds"){
                 output$indicePlot <- renderPlot(percentbarchart.plot(guilds.results, "Guilds", sampleNames))
                 output$indiceTables <- DT::renderDataTable(
                   DT::datatable(guilds.results, options = list(pageLength = 25))
                 )
                 outputMainTable <- reactive(guilds.results)
                 # Downloadable csv of selected dataset ----
                 output$downloadData <- downloadHandler(
                   filename = function(){"Guilds.csv"},
                   content = function(file) {
                     write.csv(outputMainTable(), file, row.names = TRUE)
                   }
                 )
               } else if (input$indice == "Salinity (Van Dam)"){
                 output$indicePlot <- renderPlot(percentbarchart.plot(vdamSalinity, "Salinity (Van Dam)", sampleNames))
                 output$indiceTables <- DT::renderDataTable(
                   DT::datatable(vdamSalinity, options = list(pageLength = 25))
                 )
                 outputMainTable <- reactive(vdamSalinity)
                 # Downloadable csv of selected dataset ----
                 output$downloadData <- downloadHandler(
                   filename = function(){"Salinity.csv"},
                   content = function(file) {
                     write.csv(outputMainTable(), file, row.names = TRUE)
                   }
                 )
               } else if (input$indice == "N-heterotrophy (Van Dam)"){
                 output$indicePlot <- renderPlot(percentbarchart.plot(vdamNHeterotrophy, "N-heterotrophy (Van Dam)", sampleNames))
                 output$indiceTables <- DT::renderDataTable(
                   DT::datatable(vdamNHeterotrophy, options = list(pageLength = 25))
                 )
                 outputMainTable <- reactive(vdamNHeterotrophy)
                 # Downloadable csv of selected dataset ----
                 output$downloadData <- downloadHandler(
                   filename = function(){"N-heterotrophy.csv"},
                   content = function(file) {
                     write.csv(outputMainTable(), file, row.names = TRUE)
                   }
                 )
               } else if (input$indice == "Oxygen (Van Dam)"){
                 output$indicePlot <- renderPlot(percentbarchart.plot(vdamOxygen, "Oxygen (Van Dam)", sampleNames))
                 output$indiceTables <- DT::renderDataTable(
                   DT::datatable(vdamOxygen, options = list(pageLength = 25))
                 )
                 outputMainTable <- reactive(vdamOxygen)
                 # Downloadable csv of selected dataset ----
                 output$downloadData <- downloadHandler(
                   filename = function(){"Oxygen.csv"},
                   content = function(file) {
                     write.csv(outputMainTable(), file, row.names = TRUE)
                   }
                 )
               } else if (input$indice == "Saprobity (Van Dam)"){
                 output$indicePlot <- renderPlot(percentbarchart.plot(vdamSaprobity, "Saprobity (Van Dam)", sampleNames))
                 output$indiceTables <- DT::renderDataTable(
                   DT::datatable(vdamSaprobity, options = list(pageLength = 25))
                 )
                 outputMainTable <- reactive(vdamSaprobity)
                 # Downloadable csv of selected dataset ----
                 output$downloadData <- downloadHandler(
                   filename = function(){"Saprobity.csv"},
                   content = function(file) {
                     write.csv(outputMainTable(), file, row.names = TRUE)
                   }
                 )
               } else if (input$indice == "Moisture (Van Dam)"){
                 output$indicePlot <- renderPlot(percentbarchart.plot(vdamAero, "Moisture (Van Dam)", sampleNames))
                 output$indiceTables <- DT::renderDataTable(
                   DT::datatable(vdamAero, options = list(pageLength = 25))
                 )
                 outputMainTable <- reactive(vdamAero)
                 # Downloadable csv of selected dataset ----
                 output$downloadData <- downloadHandler(
                   filename = function(){"Moisture.csv"},
                   content = function(file) {
                     write.csv(outputMainTable(), file, row.names = TRUE)
                   }
                 )
               } else if (input$indice == "Trophic state (Van Dam)"){
                 output$indicePlot <- renderPlot(percentbarchart.plot(vdamTrophic, "Trophic state (Van Dam)", sampleNames))
                 output$indiceTables <- DT::renderDataTable(
                   DT::datatable(vdamTrophic, options = list(pageLength = 25))
                 )
                 outputMainTable <- reactive(vdamTrophic)
                 # Downloadable csv of selected dataset ----
                 output$downloadData <- downloadHandler(
                   filename = function(){"Trophic state.csv"},
                   content = function(file) {
                     write.csv(outputMainTable(), file, row.names = TRUE)
                   }
                 )
               }


  ) #end observeEvent for the spinner

  #Show all tabs
  showTab(inputId = "outputTabs2", target = "Select result to display")
  showTab(inputId = "outputTabs", target = "Plots")
  showTab(inputId = "outputTabs", target = "Indices")
  showTab(inputId = "outputTabs", target = "Species recognized")
  showTab(inputId = "outputTabs", target = "All results")


  ### DOWNLOAD ALL DATA BUTTON
    # Downloadable csv of selected dataset ----
    outputMainTable <- reactive(all.results)
    output$downloadAll <- downloadHandler(
      filename = function(){"All results.csv"},
      content = function(file) {
        print("all.results")
        write.csv(outputMainTable(), file, row.names = TRUE)
        }
    )

  })#end observeEvent for Calculate Button
} #end server

