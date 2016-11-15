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
#' This function reads in all files from a directory and stores them as
#' individual objects or collapses them into a single object.  Use the 'pattern'
#' argument to specificy a set of files, or a single file type. 
#' 
#' If collapse = TRUE then all files must have identical layouts. When collapse
#' = F the data will automatically be assigned to objects, so no assignment is
#' necessary.
#' @param dir A directory that contains your data files, defaults to the working directory
#' @param pattern A pattern to match filenames as in \code{\link[base]{list.files}}
#' @param fun The function to use to read in the files, defaults to 
#' \code{\link[base]{read.csv}}
#' @param collapse A logical argument, when true a single object is returned,
#'   when false an object is returned for each file
#' @param ... Additional arguments to pass to the input method
#' @export
#' @examples
#' read_dir()

read_dir <- function(dir = getwd(), pattern = NULL, collapse = TRUE, fun = read.csv, ...){
  
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
                          full.names = T)
  nFiles <- length(fileslist)
  for(i in 1:nFiles){
    file.i <- fileslist[i]
    import.i <- fun(file.i, ...)
    if(collapse){
      if(i != 1) {
        import_list <- rbind(import_list, import.i)
      } else {
        import_list <- import.i}
    } else {
      assign(basename(file.i), 
             import.i,
             envir = .GlobalEnv)
    }
  }
  if(collapse){return(import_list)}
}
