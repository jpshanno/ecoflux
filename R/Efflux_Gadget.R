# Efflux ---------------
#' Easily fit multilple linear regressions over selected data points
#' 
#' This shiny gadget provides an easy interactive method to trim sample data and
#' fit linear regressions to each sample's data. It is specifically designed for
#' soil and stem efflux. It will work with any type of data that has a unique
#' sample ID and dependent/independent variables.
#' Select points to remove from the data by clicking on them or remove multiple
#' points by clicking and dragging a box around the points you wish to remove.
#' Please keep in mind that this step is not for removing data you view as
#' 'outliers'. Points should only be removed due to equipment/sampling errors or
#' to remove efflux 'ramp up' at the beginning of sampling. 
#' After you are finished with a sample press 'Save & Next' to advance to the
#' next sample. This will also save information about the model fit and update
#' the output data, removing the data you selected. You can view any sample
#' using the dropdown mean or return to the previous plot with the 'Previous'
#' button (navigating this way will not save any of your selections). Resetting
#' the probe will delete the saved regression information and add all of the
#' sample points back to your plot and the output data.
#' See the \href{https://joeshannon.shinyapps.io/efflux}{efflux web app}
#' for example datasets
#' 
#' @return A list consiting of a dataframe of the fitted regressions
#'   ("regressions"), a dataframe of the points removed ("removedPoints"), a
#'   dataframe of samples that were editted (processedData), a dataframe of the
#'   output data (input data merged with any changes) ("finalData"), a list of
#'   the final plots by sample ("plots")
#' 
#' @param input.data A dataset containing a response variable and one or more
#'   columns that can be used to create a unique ID
#' @param xvar The time step variable or any independent variable (quoted)
#' @param yvar Gas concentrations or any dependent variable (quoted)
#' @param idvar A single ID column (quoted) or a character vector containing the
#'   names of the ID columns
#' @export
#' @examples
#' efflux()

efflux <- function(input.data){
  
  time <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  temporaryDirectory <- 
    paste("./Temp", 
          format(Sys.time(), "%Y%m%d_%H%M%S"), 
          sep = "_")
  
  dir.create(temporaryDirectory)
  dir.create(paste(temporaryDirectory,
                   "/Plots",
                   sep = ""))

  
#___________________________________________
  
  server  <-   function(input, output, session) {
    
    
    # Reactives ---------------------------------------------------------------
    
    # Load selected file
    Data <- shiny::reactive({
      
      interface$type <- "web"
      
      if(exists("input.data")){
        interface$type <- "local"
        return(input.data)}
      
      if(input$csvExample > 0) {
        return(readr::read_csv("./www/examples/Multi-Column_ID.csv"))}
      if(input$datExample > 0) {
        return(readr::read_tsv("./www/examples/EGM4_Output.dat", skip = 2) %>% 
                 dplyr::filter(row_number() != nrow(.)))}
      
    })
    
    # This reactive is the workhorse of the data processing. It selects data from 
    # only the selected sample, fits a model, and creates a plot. The data, model,
    # and plot are all returned.
    Plot <- shiny::reactive({
      Data <- 
        data$plotting
      
      xCoord <- min(Data[[input$X]], na.rm = T)
      yDiff <- max(Data[[input$Y]], na.rm = T) - min(Data[[input$Y]], na.rm = T)
      yCoord <- max(Data[[input$Y]], na.rm = T) - 0.05*yDiff
      
      
      # Fit basic linear model with only unselected rows
      if(nrow(data$plotting) == 0){
        Model <- NULL
      } else {
        Model <- lm(get(input$Y) ~ get(input$X), data = Data)}
      
      # Build plots with points and model
      if(nrow(data$plotting) != 0){
        Plot <- 
          ggplot2::ggplot(data = Data,
                          ggplot2::aes(x = get(input$X),
                                       y = get(input$Y))) +
          ggplot2::xlab(input$X) +
          ggplot2::ylab(input$Y) +
          ggplot2::geom_point(color = "darkgray") +
          ggplot2::geom_abline(slope = coef(Model)[2],
                               intercept = coef(Model)[1],
                               color = "black") +
          ggplot2::annotate("text", 
                            label = paste("Slope =", 
                                          round(coef(Model)[2],2), 
                                          sep = " "), 
                            x = xCoord, 
                            y = yCoord, 
                            hjust = "inward") +
          ggplot2::theme_bw()
        if(input$zeroLimit == T){
          upperY <- ggplot2::ggplot_build(Plot)$panel$ranges[[1]]$y.range[2]
          Plot <- Plot + ggplot2::coord_cartesian(ylim = c(0, upperY))
        }
      } else {
        Plot <- 
          ggplot2::ggplot(ggplot2::aes(x = get(input$X),
                                       y = get(input$Y)),
                          data =  Data) +
          ggplot2::xlab(input$X) +
          ggplot2::ylab(input$Y) +
          ggplot2::geom_blank()
      }
      
      # Return all function values
      info <- list(Model = Model, 
                   Data = Data, 
                   Plot = Plot)
      return(info)
    })
    
    # Reactive Values ---------------------------------------------------------
    # Create value that can be updated and used on the server
    
    displayConditions <-
      shiny::reactiveValues(
        idNotSelected = TRUE,
        variablesNotSelected = TRUE)
    
    data <- shiny::reactiveValues(
      input = NULL,
      
      editted = NULL,
      
      unprocessed = NULL,
      
      removed = NULL,
      
      plotting = 
        NULL,
      regressionInfo = 
        tbl_df(data.frame(
          workingSampleID = character(), 
          nPointsUsed = numeric(),
          intercept = numeric(), 
          slope = numeric(), 
          adjustedRSquared = numeric(), 
          residualStandardError = numeric(), 
          degreesFreedom = numeric(), 
          fStatistic = numeric())),
      plots =
        list()
    )
    
    
    IDs <- shiny::reactiveValues(
      all = character(),
      processed = list()
    ) 
    
    sample <- shiny::reactiveValues(
      index = 1,
      name = character()
    )
    
    interface <- shiny::reactiveValues(
      type = character()
    )
    
    # General Observers ------
    
    # Populate the drop down menus with the column names from the uploaded data
    shiny::observe({
      shiny::updateSelectizeInput(session,
                                  inputId = "X",
                                  choices = c(Choose = "", 
                                              names(Data())[sapply(Data(),is.numeric)]))
      
      shiny::updateSelectizeInput(session,
                                  inputId = "Y",
                                  choices = c(Choose = "", 
                                              names(Data())[sapply(Data(),is.numeric)]))
      
      shiny::updateSelectizeInput(session,
                                  inputId = "UniqueID",
                                  choices = c(Choose = "", 
                                              names(Data())))
    })
    
    # Flag whether or not variables have been selected
    shiny::observe({
      if(!is.null(input$X) && 
         !is.null(input$Y) && 
         input$X != "" && 
         input$Y != ""){
        displayConditions$variablesNotSelected <- FALSE
      } else {
        displayConditions$variablesNotSelected <- TRUE
      }
      
      if(!is.null(input$UniqueID) && 
         input$UniqueID != ""){
        displayConditions$idNotSelected <- FALSE
      } else {
        displayConditions$idNotSelected <- TRUE
      }
    })
    
    
    # Set working values when ID is selcted
    shiny::observe({
      if(displayConditions$idNotSelected){return(NULL)}
      # Create a unique ID using the columns selected for ID
      data$input <-
        Data() %>%
        tidyr::unite_("workingSampleID", unlist(input$UniqueID), sep = "_") %>% 
        arrange_("workingSampleID")
      
      data$editted <-
        Data() %>%
        tidyr::unite_("workingSampleID", unlist(input$UniqueID), sep = "_") %>%
        slice(0)
      
      # Generate a string of all the sample IDs
      IDs$all <- 
        data$input  %>%
        distinct(workingSampleID) %>%
        arrange(workingSampleID) %>% 
        .$workingSampleID
    })
    
    shiny::observe({
      if(displayConditions$idNotSelected){return(NULL)}
      sample$name <- IDs$all[sample$index]})    
    
    shiny::observe({
      if(displayConditions$idNotSelected){return(NULL)}
      if(sample$name %in% IDs$processed){
        data$plotting <- 
          data$editted %>% 
          filter(workingSampleID == sample$name)
      } else {
        data$plotting <- 
          data$input %>% 
          filter(workingSampleID == sample$name)
      }
    })
    
    
    
    # Event Observers -----
    
    # End app and return data
    shiny::observeEvent(input$done,{
      shiny::stopApp(
        returnValue = list(
          processedSamples = data$editted,
          unproccessedSamples = data$input %>%
            filter(!(workingSampleID %in% IDs$processed)),
          removedPoints = anti_join(data$input, data$editted) %>% 
            filter(workingSampleID %in% IDs$processed),
          modelFits = data$regressionInfo
        )
      )
    })
    
    output$download <-
      shiny::downloadHandler(
        filename = "Processed_Data.zip",
        content = function(file){
          
          # Create CSVs
          readr::write_csv(data$editted,
                           "Processed_Samples.csv")
          readr::write_csv(data$regressionInfo, 
                           "Model_Fits.csv")
          readr::write_csv(anti_join(data$input, data$editted) %>% 
                             filter(workingSampleID %in% IDs$processed),
                           "Removed_Points.csv")
          readr::write_csv(data$input %>%
                             filter(!(workingSampleID %in% IDs$processed)),
                           "Unprocessed_Samples.csv")
          
          # Create a list of plots to save
          plots <- list()
          dir.create("Plots")
          for(i in names(data$plots)){
            plots[[i]] <- paste("Plots/", i, ".jpeg", sep = "")
            ggplot2::ggsave(data$plots[[i]], filename = plots[[i]])
          }
          
          # Place everything in a zip file
          zip(file, files = c("Processed_Samples.csv", 
                              "Unprocessed_Samples.csv",
                              "Removed_Points.csv",
                              "Model_Fits.csv",
                              unlist(plots)
          ))
        }
      )
    
    # Run on click on the plot
    shiny::observeEvent(input$plot_click, {
      data$plotting <- anti_join(data$plotting, 
                                 shiny::nearPoints(Plot()$Data, 
                                                   input$plot_click,
                                                   xvar = input$X,
                                                   yvar = input$Y,
                                                   maxpoints = 1))
      # if(nrow(data$plotting) == 0){sample$index <- sample$index + 1}
    })
    
    # Run on dragging a box on the plot Return a data frame of points with a
    # column (selected_) indicating which point was selected. Max points ensures
    # that only the closest point is returned
    shiny::observeEvent(input$plot_brush, {
      data$plotting <- anti_join(data$plotting, 
                                 shiny::brushedPoints(Plot()$Data,
                                                      input$plot_brush, 
                                                      xvar = input$X,
                                                      yvar = input$Y))
      # if(nrow(data$plotting) == 0){sample$index <- sample$index + 1}
    })
    
    shiny::observeEvent(input$plot_dblclick, {
      data$plotting <- 
        filter(data$plotting, is.na(workingSampleID))
    })
    
    # Jump to selected sample
    shiny::observeEvent(input$ID,{
      sample$name <- input$ID
      sample$index <- grep(sample$name, IDs$all)
    })
    
    # Runs when you press Save & Next
    shiny::observeEvent(input$nextID,{
      
      # Add the value in the ID dropdown menu to list of processed samples
      IDs$processed[[sample$name]] <- sample$name
      
      # Add the sample plot to a list of generated plots - overwrites the plot as
      # data is selected and removed
      data$plots[[sample$name]] <- Plot()$Plot
      
      # Extracted values from the model and merge them into a tbl
      if(!is.null(Plot()$Model)){
        data$regressionInfo <-
          bind_rows(data$regressionInfo %>%
                      filter(workingSampleID != sample$name),
                    data.frame(
                      workingSampleID =
                        sample$name,
                      nPointsUsed =
                        nrow(Plot()$Data),
                      intercept =
                        coef(Plot()$Model)[1],
                      slope =
                        coef(Plot()$Model)[2],
                      adjustedRSquared =
                        summary(Plot()$Model)$adj.r.squared,
                      residualStandardError =
                        summary(Plot()$Model)$sigma,
                      degreesFreedom =
                        summary(Plot()$Model)$fstatistic[3],
                      fStatistic =
                        summary(Plot()$Model)$fstatistic[1]))}
      
      data$editted <- 
        bind_rows(
          filter(data$editted, workingSampleID != sample$name),
          data$plotting) %>% 
        arrange(workingSampleID)
      
      data$unprocessed <- 
        data$input %>%
        filter(!(workingSampleID %in% IDs$processed))
      
      data$removed <- 
        anti_join(data$input, data$editted) %>%
        filter(workingSampleID %in% IDs$processed)
      
      
      if(interface$type == "local"){
        readr::write_csv(data$editted,
                         paste(temporaryDirectory,
                               "/processedSamples",
                               time,
                               ".csv",
                               sep = ""))
        readr::write_csv(data$unprocessed,
                         paste(temporaryDirectory,
                               "/unprocessedSamples",
                               time,
                               ".csv",
                               sep = ""))
        readr::write_csv(data$removed,
                         paste(temporaryDirectory,
                               "/removedPoints",
                               time,
                               ".csv",
                               sep = ""))
        readr::write_csv(data$regressionInfo,
                         paste(temporaryDirectory,
                               "/unprocessedSamples",
                               time,
                               ".csv",
                               sep = ""))
        ggplot2::ggsave(paste(temporaryDirectory,
                              "/Plots/Plots_",
                              sample$name,
                              ".jpeg",
                              sep = ""),
                        Plot()$Plot)
      }
      
      
      
      if(sample$index != length(IDs$all)) {sample$index <- sample$index + 1}
      sample$name <- IDs$all[sample$index]
      
      # Update the ID dropdown menu to the next sample
      shiny::updateSelectizeInput(session,
                                  inputId = "ID",
                                  selected = sample$name)
    })
    
    # Runs when you press Reset Probe
    shiny::observeEvent(input$undoEdit,{
      
      if(sample$name %in% IDs$processed){
        
        # Remove the current sample from the list of processed samples
        IDs$processed[[sample$name]] <- NULL
        
        # Display all points again. keepRows is a logical that indicates with sample
        # points to keep in the data & display
        data$plotting <- 
          data$input %>% 
          filter(workingSampleID == sample$name)
        
        # Remove the processed plot for the current sample from the list of plots
        data$plots[[sample$name]] <- NULL
        
        # Remove the model information for the current sample
        data$regressionInfo <- 
          data$regressionInfo %>% 
          filter(workingSampleID != sample$name)
        
        # Remove the data ponits from the current probe from the editted dataset
        
        data$editted <-
          filter(data$editted, workingSampleID != sample$name)
        
        data$processed <- 
          filter(data$processed, workingSampleID != sample$name)
        
        data$removed <- 
          filter(data$removed, workingSampleID != sample$name)
      }
      
    })  
    
    # Runs when you press previous
    shiny::observeEvent(input$previousID,{
      if(sample$index != 1) {sample$index <- sample$index - 1}
      sample$name <- IDs$all[sample$index]
      # Update the ID dropdown menu to the previous sample
      shiny::updateSelectizeInput(session,
                                  inputId = "ID",
                                  selected = sample$name)
    })
    
    
    
    # Render Output -----
    
    # Display the name of the selected
    output$filename <- shiny::renderUI({
      if(!is.null(input$File)){
        return(HTML(
          paste("<strong>Uploaded File:</strong>",
                basename(input$File$name))
        ))}
      if(input$datExample > 0){
        return(HTML("<strong>Uploaded File:</strong> EGM-4 Example"))}
      if(input$csvExample > 0){
        return(HTML("<strong>Uploaded File:</strong> CSV Example"))}
    })
    
    # Create the plot, do not attempt to run if variables are not set
    output$plotConc <- shiny::renderPlot({
      
      if(displayConditions$variablesNotSelected) {return(NULL)}
      
      Plot()$Plot
      
    }, height = 600)
    
    # Generate the plot controls
    output$plotControls <- shiny::renderUI({
      if(length(input$UniqueID) == 0) {return(NULL)}
      shiny::column(width = 12,
                    shiny::fluidRow(
                      shiny::actionButton("previousID",
                                          label = "Previous",
                                          width = "30%",
                                          class = "btn-info"),
                      shiny::actionButton("undoEdit",
                                          label = "Reset Probe",
                                          width = "30%",
                                          class = "btn-info"),
                      shiny::actionButton("nextID",
                                          label = "Save & Next",
                                          width = "30%",
                                          class = "btn-info")
                    )
      )
    })
    
    # Generate the ID selection box
    output$idSelection <- shiny::renderUI({
      if(length(input$UniqueID) == 0) {return(NULL)}
      shiny::fluidRow(
        shiny::selectizeInput("ID",
                              label = NULL,
                              choices = IDs$all,
                              multiple = F)
      )
    })
    
    
    # Generate the table of only editted table
    output$edittedData <- DT::renderDataTable({
      if(length(IDs$processed) ==0) {return(NULL)}
      DT::datatable(
        data$editted,
        options = list(paging = FALSE, searching = FALSE),
        rownames = F)
    })
    
    # Generate the table of data that will be downloaded
    output$outputData <- DT::renderDataTable({
      if(length(IDs$processed) ==0) {return(NULL)}
      DT::datatable(
        bind_rows(filter(data$input, !(workingSampleID %in% IDs$processed)),
                  filter(data$editted, workingSampleID %in% IDs$processed)),
        options = list(paging = FALSE, searching = FALSE),
        rownames = F)
    })
    
    #Generate the table of model information
    output$regressionData <- DT::renderDataTable({
      if(length(IDs$processed) ==0) {return(NULL)}
      DT::datatable(
        data$regressionInfo,
        options = list(paging = FALSE, searching = FALSE),
        rownames = F)
    })
    
    output$buttons <- shiny::renderUI({
      if(length(IDs$processed)== 0) {return(NULL)}
      
      if(interface$type == "local") {
        shiny::actionButton("done",
                            label = "Finish and return to RStudio",
                            class = "btn-info")
      } else {
        shiny::downloadButton("download",
                              label = "Download all data and plots",
                              class = "btn-info")
      }
    })
    
    # Instructions that vary based on where the user is in the process
    output$instructions <- renderUI({
      
      if (is.null(Data())) {
        fluidRow(
          h4("Select an Example Dataset"),
          p("Two example datasets are available. The first is raw output from a PP Systems EGM-4. The second respresents a full season of sampling where each unique sample is represented by its treatment, collar number, month of sample, and day of sample.")
        )
      } else {
        if (!displayConditions$idNotSelected & !displayConditions$variablesNotSelected) {
          fluidRow(
            h4("Step 2: Evaluate Data and Fit Models"),
            p("Select points to remove from the data by clicking on them or remove multiple points by clicking and dragging a box around the points you wish to remove. Please keep in mind that this step is not for removing data you view as 'outliers'. Points should only be removed due to equipment/sampling errors or to remove efflux 'ramp up' at the beginning of sampling."),
            p("After you are finished with a sample press 'Save & Next' to advance to the next sample. This will also save information about the model fit and update the output data, removing the data you selected. You can view any sample using the dropdown mean or return to the previous plot with the 'Previous' button (navigating this way will not save any of your selections). Resetting the probe will delete the saved regression information and add all of the sample points back to your plot and the output data."),
            p("Once you have saved information from at least one plot the updated datset, regression information, and the final plot will all be available for download. I recommend downloading data often to avoid losing work if the app or your browser has a problem."),
            p("Downloaded data will be a zipped file containing the final plots for each sample and five CSVs. 'Processed_Samples.csv' and 'Unprocessed_Samples.csv' contain the samples you viewed and saved and the samples that were not saved, respecitvely. 'Removed_Points.csv' contains all of the data points that you removed. 'Efflux_Summary.csv' contains the sample ID and the slope (assumed to be efflux in ppm), futher information about the models can be found in 'Model_Fits.csv'.")
          )
        } else{
          fluidRow(
            h4("Step 1: Set Variable & ID Columns"),
            p("If you are working with efflux data select the columns that contains sample time steps and sample concentrations. If you are working with any other data the time variable corresponds to the x-axis and the concentration variable corresponds to the y-axis."),
            p("Choose one or more columns that separates your data into unique samples. Changing the ID variables will delete all processed data")
          )
        }
      }
    })
    
    
    
    # Conditional Panel Controls --------------------------------------------------------
    
    output$fileUploaded <- shiny::reactive({
      return(is.null(Data()))
    })
    
    output$showPlot <- shiny::reactive({
      return(!displayConditions$variablesNotSelected & !displayConditions$idNotSelected)
    })
    
    # Set output options to keep them running in the background
    shiny::outputOptions(output, 'fileUploaded', suspendWhenHidden = F)
    shiny::outputOptions(output, "showPlot", suspendWhenHidden = F)
    
    
    # Testing Tools -----
    
    # output$test <- renderPrint({
    #  input$csvExample
    # })
    # # # 
    # output$test2 <- renderPrint({
    #   extension
    # })
  }
  
  # UI -------
  ui  <-  
    shiny::fluidPage(
      # theme = "http://forest.mtu.edu/blackashwetlands/libs/css/efflux.css",
      shiny::includeCSS("http://forest.mtu.edu/blackashwetlands/libs/css/efflux.css"),
      # shiny::tags$head(
      #   shiny::tags$link(rel="stylesheet", 
      #                    type="text/css", 
      #                    href="http://forest.mtu.edu/blackashwetlands/libs/css/efflux.css")),
      title = "Interactive Sample Cleaning and Linear Modeling",
      style = "margin:2em",
      
      
      # Header -------
      
      shiny::fluidRow(
        style = "margin-bottom:2em",
        shiny::column(8,
                      shiny::h1("Efflux"),
                      shiny::h3("Interactive Trimming and Linear Modeling"),
                      shiny::br(),
                      shiny::h4("Purpose", style = "margin-left:0.25em"),
                      shiny::HTML(
                        "<p style = 'margin-left:0.25em'>This shiny application provides an easy interactive method to trim sample data and fit linear regressions to each sample's data. It is specifically designed for CO<sub>2</sub> and CH<sub>4</sub> efflux.  It will work with any type of data that has a unique sample ID and dependent/independent variables. The code for this project is available on <a href = 'https://github.com/jpshanno/efflux'>GitHub</a> and is available as a function in <a href = 'https://github.com/jpshanno/ecoFlux'>the ecoFlux package</a> (devtools::install_github('jpshanno/ecoFlux')). Please contact <a href = 'mailto:josephshannon@outlook.com'>Joe Shannon</a> with any questions or problems.</p>")
        )
      ),    
      
      
      # Sidebar ------
      
      shiny::sidebarPanel(
        width = 3,
        style = "margin-top:3em",
        
        # Hide the file upload panel once a file has been chosen
        shiny::conditionalPanel("output.fileUploaded == true",
                                
                                shiny::p("Example output from PP Systems EGM-4"),
                                shiny::actionButton("datExample", 
                                                    label = "Load Example",
                                                    class = "btn-info"),
                                shiny::p("Example CSV with multiple ID columns"),
                                shiny::actionButton("csvExample", 
                                                    label = "Load Example",
                                                    class = "btn-info")
        ),
        
        # Once a file has been chosen show the file name and the variable/id selections
        shiny::conditionalPanel(
          "output.fileUploaded != true",
          
          shiny::h4("Time"),
          shiny::p("Select the column that contains your time steps"),
          shiny::selectizeInput(inputId = "X",
                                label = NULL,
                                choices = NULL,
                                multiple = F),
          
          shiny::h4("Concentrations"),
          shiny::p("Select the column that contains gas concentration"),
          shiny::selectizeInput(inputId = "Y",
                                label = NULL,
                                choices = NULL,
                                multiple = F),
          shiny::checkboxInput("zeroLimit",
                               label = "Set lower y-axis limit to zero?",
                               value = FALSE),
          shiny::h4("Unique ID"),
          shiny::p("Select one or more columns that represent a unique ID for each sample location and event"),
          shiny::selectizeInput(inputId = "UniqueID",
                                label = NULL,
                                choices = NULL,
                                multiple = T)
        )     
      ),
      
      
      # Main Panel Set-up -----
      
      shiny::mainPanel(
        align = "center",
        width = 9,
        shiny::tabsetPanel(
          
          # Data Processing Tab -----        
          
          
          shiny::tabPanel("Data Selection", 
                          style = "padding-top:2em; margin-left:2em",
                          shiny::fluidRow(
                            
                            # Add the interactive plot & navigation tools
                            shiny::conditionalPanel(
                              "output.showPlot == true",
                              shiny::column(8,
                                            shiny::uiOutput("idSelection"),
                                            shiny::plotOutput("plotConc",
                                                              click = "plot_click",
                                                              dblclick = "plot_dblclick",
                                                              hover = "plot_hover",
                                                              brush = shiny::brushOpts(id = "plot_brush",
                                                                                       delay = 4000,
                                                                                       delayType = "debounce",
                                                                                       resetOnNew = TRUE
                                                              ),
                                                              height = "100%"
                                            ),
                                            
                                            shiny::uiOutput("plotControls")
                              )
                            ),
                            
                            # Display the appropriate set up instructions
                            shiny::column(4,
                                          # verbatimTextOutput("test"),
                                          # verbatimTextOutput("test2"),
                                          # shiny::uiOutput("fileLabel"),
                                          shiny::uiOutput("buttons"),
                                          shiny::uiOutput("instructions", align = "left")
                                          
                                          
                                          
                                          
                            )
                            
                          )
          ),
          
          
          # Processed Samples Tab -----     
          
          shiny::tabPanel("Processed Samples Only",
                          DT::dataTableOutput("edittedData")
          ),
          
          
          # Output Dataset Tab -----     
          
          shiny::tabPanel("Output Dataset",
                          DT::dataTableOutput("outputData")
          ),
          
          
          # Regression Information Tab -----     
          
          shiny::tabPanel("Regression Output",
                          DT::dataTableOutput("regressionData")
          )
        )
      )
    )
  
  effluxApp <- shiny::shinyApp(ui = ui, server = server)
  
  #____________________________________________
  
  shiny::runApp(effluxApp)
}

