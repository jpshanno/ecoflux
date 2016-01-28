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

efflux <- function(input.data, xvar, yvar, idvar){
  
  server  <-  function(input, output, session) {
    
    
    # Reactive Values ---------------------------------------------------------
    
    data <- shiny::reactiveValues(
      input = 
        input.data  %>%
        tidyr::unite_("outputSampleID", idvar, sep = "_") %>% 
        dplyr::arrange_("outputSampleID"),
      editted = 
        input.data  %>%
        tidyr::unite_("outputSampleID", idvar, sep = "_") %>% 
        dplyr::arrange_("outputSampleID"),
      plotting = 
        NULL,
      regressionInfo = 
        dplyr::tbl_df(data.frame(
          outputSampleID = character(), 
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
    
    
    # Reactives ---------------------------------------------------------------
    
    # This reactive is the workhorse of the data processing. It selects data from 
    # only the selected sample, fits a model, and creates a plot. The data, model,
    # and plot are all returned.
    
    PlotData <- shiny::reactive({
      # If no ID variable has been selected display the plot with all the data
      Data <- 
        data$plotting
      
      # Fit basic linear model with only unselected rows
      if(nrow(data$plotting) == 0){
        Model <- NULL
      } else {
        Model <- lm(get(yvar) ~ get(xvar), data = Data)}
      
      # Build plots with points and model
      if(nrow(data$plotting) != 0){
        Plot <- 
          ggplot2::ggplot(data =  Data) +
          ggplot2::xlab(xvar) +
          ggplot2::ylab(yvar) +
          ggplot2::ggtitle(sample$name) +
          ggplot2::geom_point(ggplot2::aes(x = get(xvar),
                                           y = get(yvar)),
                              color = "darkgray") +
          ggplot2::geom_abline(slope = coef(Model)[2],
                               intercept = coef(Model)[1],
                               color = "black") +
          ggplot2::theme_bw()
      } else {
        Plot <- 
          ggplot2::ggplot(ggplot2::aes(x = get(xvar),
                                       y = get(yvar)),
                          data =  Data) +
          ggplot2::xlab(xvar) +
          ggplot2::ylab(yvar) +
          ggplot2::ggtitle(sample$name) +
          ggplot2::geom_blank()
      }
      
      # Return all function values
      info <- list(Model = Model, 
                   Data = Data, 
                   Plot = Plot)
      return(info)
    })
    
    
    
    
    # General Observers ----
    
    shiny::observe({
      IDs$all <- 
        data$input  %>%
        dplyr::distinct(outputSampleID) %>%
        dplyr::arrange(outputSampleID) %>% 
        .$outputSampleID
      sample$name <- IDs$all[sample$index]
    })
    
    shiny::observe({
      if(sample$name %in% IDs$processed){
        data$plotting <- 
          data$editted %>% 
          dplyr::filter(outputSampleID == sample$name)
      } else {
        data$plotting <- 
          data$input %>% 
          dplyr::filter(outputSampleID == sample$name)
      }
    })
    
    # Event Observers -----
    
    
    shiny::observeEvent(input$done, {
      returnValue <- list(
        # efflux = data$regressionInfo %>% 
        #   select(outputSampleID, slope) %>% 
        #   rename(`Efflux (ppm)` = slope),
        regressions = data$regressionInfo,
        removedPoints = dplyr::anti_join(data$input, data$editted),
        processedData = data$editted %>% 
          dplyr::filter(outputSampleID %in% IDs$processed),
        finalData = data$editted,
        plots = data$plots
      )
      shiny::stopApp(returnValue)
    })
    
    # Run on click on the plot Return a data frame of points with a column
    # (selected_) indicating which point was selected. Max points ensures that
    # only the closest point is returned
    shiny::observeEvent(input$plot_click, {
      data$plotting <- dplyr::anti_join(data$plotting, 
                                        shiny::nearPoints(PlotData()$Data, 
                                                          input$plot_click,
                                                          xvar = xvar,
                                                          yvar = yvar,
                                                          maxpoints = 1))
      # if(nrow(data$plotting) == 0){sample$index <- sample$index + 1}
    })
    
    # Run on dragging a box on the plot Return a data frame of points with a
    # column (selected_) indicating which point was selected. Max points ensures
    # that only the closest point is returned
    shiny::observeEvent(input$plot_brush, {
      data$plotting <- dplyr::anti_join(data$plotting, 
                                        shiny::brushedPoints(PlotData()$Data,
                                                             input$plot_brush, 
                                                             xvar = xvar,
                                                             yvar = yvar))
      # if(nrow(data$plotting) == 0){sample$index <- sample$index + 1}
    })
    
    shiny::observeEvent(input$plot_dblclick, 
                        {data$plotting <- 
                          dplyr::filter(data$plotting, is.na(outputSampleID))})
    
    # Runs when you press Save & Next
    shiny::observeEvent(input$nextID,{
      
      # Add the value in the ID dropdown menu to list of processed samples
      IDs$processed[[sample$name]] <- sample$name
      
      # Add the sample plot to a list of generated plots - overwrites the plot as
      # data is selected and removed
      data$plots[[sample$name]] <- PlotData()$Plot
      
      # Extracted values from the model and merge them into a tbl
      if(!is.null(PlotData()$Model)){
        data$regressionInfo <-
          dplyr::bind_rows(data$regressionInfo %>%
                             dplyr::filter(outputSampleID != sample$name),
                           data.frame(
                             outputSampleID =
                               sample$name,
                             nPointsUsed =
                               nrow(PlotData()$Data),
                             intercept =
                               coef(PlotData()$Model)[1],
                             slope =
                               coef(PlotData()$Model)[2],
                             adjustedRSquared =
                               summary(PlotData()$Model)$adj.r.squared,
                             residualStandardError =
                               summary(PlotData()$Model)$sigma,
                             degreesFreedom =
                               summary(PlotData()$Model)$fstatistic[3],
                             fStatistic =
                               summary(PlotData()$Model)$fstatistic[1]))}
      
      data$editted <- 
        dplyr::bind_rows(
          dplyr::filter(data$input, outputSampleID != sample$name),
          data$plotting) %>% 
        dplyr::arrange(outputSampleID)
      
      if(sample$index != length(IDs$all)) {sample$index <- sample$index + 1}
    })
    
    # Runs when you press Reset Probe
    shiny::observeEvent(input$resetProbe,{
      
      # Remove the current sample from the list of processed samples
      IDs$processed[[sample$name]] <- NULL
      
      # Display all points again
      data$plotting <- 
        data$input %>% 
        dplyr::filter(outputSampleID == sample$name)
      
      # Remove the processed plot for the current sample from the list of plots
      data$plots[[sample$name]] <- NULL
      
      # Remove the model information for the current sample
      if(nrow(data$regressionInfo)) {
        data$regressionInfo <- 
          data$regressionInfo %>% 
          dplyr::filter(outputSampleID != sample$name)}
      
      # Remove the data ponits from the current probe from the editted dataset
      data$editted <-
        dplyr::filter(data$editted, outputSampleID != sample$name)
    })
    
    # Runs when you press previous
    shiny::observeEvent(input$previousID,{
      if(sample$index != 1) {sample$index <- sample$index - 1}
    })
    
    
    
    # Render Output -----
    
    # Create the plot, do not attempt to run if variables are not set
    output$plotConc <- shiny::renderPlot({
      PlotData()$Plot
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
      DT::datatable(
        dplyr::bind_rows(filter(data$input, !(outputSampleID %in% IDs$processed)),
                         filter(data$editted, outputSampleID %in% IDs$processed)),
        options = list(paging = FALSE, searching = FALSE),
        rownames = F)
    })
    
    #Generate the table of model information
    output$regressionData <- DT::renderDataTable({
      if(length(IDs$processed) ==0) {return(NULL)}
      # DT::datatable(
      data$regressionInfo#,
      # options = list(paging = FALSE, searching = FALSE),
      # rownames = F)
    })
  }
  
  # UI -------
  ui <- 
    miniUI::miniPage(
      # theme = "efflux.css",
      miniUI::gadgetTitleBar("Interactive Sample Cleaning and Linear Modeling"),
      
      miniUI::miniTabstripPanel(
        
        # Data Processing Tab -----        
        
        miniUI::miniTabPanel("Data Selection",
                             miniUI::miniButtonBlock(
                               border = "top",
                               shiny::actionButton("previousID",
                                                   label = "Previous"),
                               shiny::actionButton("resetProbe",
                                                   label = "Reset Probe"),
                               shiny::actionButton("nextID",
                                                   label = "Save & Next")
                             ),
                             # verbatimTextOutput("test"),
                             miniUI::miniContentPanel(
                               shiny::plotOutput("plotConc",
                                                 # click = "plot_click",
                                                 dblclick = "plot_dblclick",
                                                 hover = "plot_hover",
                                                 # brush = "plot_brush",
                                                 brush = shiny::brushOpts(id = "plot_brush",
                                                                          delay = 4000,
                                                                          delayType = "debounce",
                                                                          resetOnNew = TRUE
                                                 ),
                                                 height = "100%")
                             )
        ),
        
        # Processed Samples Tab -----     
        miniUI::miniTabPanel(
          "Processed Samples",
          miniUI::miniContentPanel(
            shiny::dataTableOutput("edittedData")
          )
        ),
        
        
        # Output Dataset Tab -----     
        
        miniUI::miniTabPanel(
          "Output Dataset",
          miniUI::miniContentPanel(
            shiny::dataTableOutput("outputData")
          )
        ),        
        
        # Regression Information Tab -----     
        
        miniUI::miniTabPanel(
          "Regression Dataset",
          miniUI::miniContentPanel(
            shiny::dataTableOutput("regressionData")
          )
        )
      )
    )
  shiny::runGadget(ui, server, viewer = shiny::paneViewer("efflux"))
}

