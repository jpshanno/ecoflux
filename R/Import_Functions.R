# Read rdb ----------------------------------------------------------------
#' Read in USGS rdb data
#'
#' This function is a wrapper for read.delim and was taken from the source of
#' \code{\link[dataRetrieval]{importRDB1}}. This function acts upon exisiting 
#' downloads and does not import data from a url
#' @param file A file in the USGS .rdb format
#' @export
#' @examples
#' read_rdb()

read_rdb <- function(file){
  dplyr::slice(
    dplyr::tbl_df(
      read.delim(file,
                 header = TRUE,  
                 quote="\"",  
                 dec=".",  
                 sep='\t', 
                 colClasses=c('character'), 
                 fill = TRUE,  
                 comment.char="#",
                 check.names = FALSE)),
    2:n())
} 

# Read directory ----------------------------------------------------------------
#' Read in a directory of files
#'
#' This function reads in all files from a directory using the choosen import 
#' function.  Use the 'pattern' argument to specificy a set of files, or a 
#' single file type. If collapse = TRUE \code{\link[dplyr]{bind_rows}} is used 
#' to match column names and bind the imported data into a single object.
#' @return  When \code{collapse = T} a single object matching the output class 
#'   of \code{fun} is returned. When \code{collapse = F} a list of objects 
#'   matching the output class of \code{fun} is returned with names corresponded
#'   to the names of the imported files
#' @param dir A directory that contains your data files, defaults to the working
#'   directory
#' @param pattern A pattern to match filenames as in
#'   \code{\link[base]{list.files}}
#' @param collapse A logical argument, when true a single object is returned, 
#'   when false an object is returned for each file, defaults to \code{TRUE}
#' @param fun The function to use to read in the files, defaults to 
#'   \code{\link[base]{read.csv}}
#' @param recursive A logical argument, when true files are read recursively, 
#'   defaults to \code{TRUE}. See \code{\link[base]{list.files}}
#' @param ... Additional arguments to pass to the input method
#' @export
#' @examples
#' read_dir()

read_dir <- function(dir = getwd(), pattern = "*", collapse = TRUE, recursive = FALSE, fun = read.csv, ...){
  
  if(!all(sapply(list(dir, pattern), is.character))){
    stop("dir and pattern must be character vectors.")
  }
  
  if(!is.logical(collapse)){
    stop("collapse must be TRUE or FALSE.")
  }
  
  if(!is.logical(try(is.function(fun), silent = T))){
    stop("The function specified in fun is not found. Check your spelling or ensure that the necessary library is loaded.")
  }
  
  fileslist <- list.files(dir, 
                          pattern = pattern, 
                          full.names = T, 
                          recursive = recursive)
  nFiles <- length(fileslist)
  import_list <- list()
  
  for(i in 1:nFiles){
    file.i <- fileslist[i]
    import_list[[basename(file.i)]] <- try(fun(file.i, ...))
  }
  
  import_list <- import_list[which(sapply(import_list, function(x){class(x) != "try-error"}))]
  
  if(collapse){
    import_list <- dplyr::bind_rows(import_list, .id = "sourceFile")
    return(import_list)
  } else {
    lapply(seq_along(import_list),
           function(x){
             assign(names(import_list)[x], import_list[[x]], envir = .GlobalEnv)
           })
  }
}


# Read XLE ----------------------------------------------------------------

#' Read in Solinst .xle levellogger and barologger files
#' 
#' Use \code{read_xle} to read in Solinst data from levelloggers and barologgers that are in the *.xle format without exporting the file to a csv. Use \code{read_and_convert_xle} to convert the stored values as they are read in.
#'
#' @param file A file in the .xle format
#' @param conversions A list of character vectors of length 2, each consisting of the input unit and the desired unit using abbreviations from \code{\link[measurements]{conv_unit}}
#'
#' @return A dataframe containing project ID, site, coordinates, date of measurement ('yyyymmdd'), time of measurement ('HH:MM:SS'), and the data extracted from each channel in the logger. If a channel includes measurement parameters such as offset or altitude these are included in the dataframe.
#' @export
#' @rdname read_xle
#' @examples
#' read_and_convert_xle(file = "some_file.xle", 
#'                      conversions = list(c("ft", "m"), 
#'                                         c("F", "C")))
read_xle <- 
  function(file = NULL){
    require(XML)
    
    testXML <- xmlParse(file, 
                        encoding = "ISO-8859-1")
    
    # Extract header information from file, not currently used in reporting
    xleFileInfo <- xmlToDataFrame(testXML["//Body_xle/File_info"],
                                  stringsAsFactors = FALSE)
    xleLoggerInfo <- xmlToDataFrame(testXML["//Body_xle/Instrument_info"],
                                    stringsAsFactors = FALSE)
    xleLoggerHeader <- xmlToDataFrame(testXML["//Body_xle/Instrument_info_data_header"],
                                      stringsAsFactors = FALSE)
    
    # Extract information about chanels and format column names
    channelNames <- 
      sapply(
        testXML["//Body_xle/*[starts-with(name(), 'Ch')]"],
        function(x){
          # Extract channel ID and strip trailing whitespace that was found in some files
          id <- gsub("[[:blank:]+$]", "",xmlValue(x["Identification"]$Identification))
          # Extract channel unit and replace '/' with _
          unit <- gsub("\\/", "_", xmlValue(x["Unit"]$Unit))
          # Combine channel ID and unit into a single standardized name
          channelName <- tolower(
            paste0(
              id,
              "_",
              ifelse(id == "TEMPERATURE",
                     substr(unit, 
                            nchar(unit), 
                            nchar(unit)),
                     unit)
            )
          )
          channelName <- gsub("[[:blank:]]", "_", channelName)
          return(channelName)
        }
      )
    
    # Check to see if the file contains any data
    if(length(testXML["//Body_xle/Data/Log"]) == 0){
      stop(paste0("The file ",
                  file,
                  " is apparently empty"))
    }
    
    
    # Combine header data and logged data into a single dataframe.
    xleData <- 
      as.data.frame(
        list(
          xleLoggerHeader$Project_ID,
          xleLoggerHeader$Location,
          paste(xleLoggerHeader$Latitude, xleLoggerHeader$Latitude, sep = ", "),
          sapply(testXML["//Body_xle/Data/Log/Date"],
                 function(x){gsub("/", "", xmlValue(x))}),
          sapply(testXML["//Body_xle/Data/Log/Time"],
                 xmlValue),
          sapply(testXML["//Body_xle/Data/Log/ch1"],
                 function(x){as.numeric(xmlValue(x))}),
          sapply(testXML["//Body_xle/Data/Log/ch2"],
                 function(x){as.numeric(xmlValue(x))})
        ),
        col.names = c("projectID",
                      "site",
                      "coordinates",
                      "date_yyyymmdd",
                      "time_hhmmss",
                      channelNames),
        stringsAsFactors = FALSE
      )
    
    # Extract any parameter information embedded in each channel
    params <- 
      lapply(
        testXML["//Body_xle/*/Parameters"],
        xmlToList
      )
    
    if(!is.null(params)){
      
      # Name each list of parameters with the channel names
      names(params) <- channelNames
      
      # Remove information about channels that do not contain any parameters
      params <- params[!sapply(params, is.null)]
      
      # Extract the values from each parameter
      paramValues <- 
        rapply(
          params,
          function(x){as.numeric(x[["Val"]])}
        )
      
      # Assign each parameter a name of the form channel_parameter_unit
      names(paramValues) <- 
        unlist(
          lapply(
            seq_along(params),
            function(x){
              gsub("[[:blank:]]", "_",
                   tolower(
                     paste0(
                       gsub("_.*", "", names(params)[x]),
                       "_",
                       names(params[[x]]),
                       "_",
                       sapply(params[[x]], function(y){gsub("\\/", "_", y["Unit"])})
                     )
                   )
              )
            })
        )
      
      # Add the parameter information to the return dataframe
      for(i in 1:length(paramValues)){
        xleData[, names(paramValues)[i]] <- paramValues[i]}
    }
    
    return(xleData)
  }

#' @export
#' @rdname read_xle
read_and_convert_xle <- 
  function(x, conversions = NULL){
    
    if(!all(unlist(conversions) %in% unlist(measurements::conv_unit_options))){
      stop("Unit conversions failed. Check that all listed units match the available units in ?measurements::conv_unit") 
    }

    DATA <- read_xle(x)
    
    lapply(conversions, 
           function(X){
             oldUnit <- 
               paste0("_", X[1], "$")
             newUnit <- 
               paste0("_", X[2])
             oldIndex <- 
               grep(oldUnit, names(DATA))
             if(length(oldIndex)==0){message(paste0("No columns with unit '",
                                                    X[1],
                                                    "' were found."))}
             DATA[,oldIndex] <<- 
               measurements::conv_unit(DATA[,oldIndex], X[1], X[2])
             newNames <- 
               gsub(oldUnit, newUnit, names(DATA)[oldIndex])
             names(DATA)[oldIndex] <<- 
               newNames
           })
    
    return(DATA)
  }
