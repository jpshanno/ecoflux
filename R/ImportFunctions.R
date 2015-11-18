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
                 comment.char="#")),
    2:n())
} 

# Read directory ----------------------------------------------------------------
#' Read in a directory of files with identical data formats
#'
#' This function reads in all files from a directory and stores them in a 
#' \code{\link[dplyr]{tbl_dt}}.  All files must have the same format.
#' @param dir A directory with only data files
#' @param pattern A pattern to match filenames as in \code{\link[base]{list.files}}
#' @param fun The function to use to read in the files, defaults to 
#' \code{\link[readr]{read_csv}}
#' @param ... Additaional arguments to pass to the input method
#' @export
#' @examples
#' read_dir()

read_dir <- function(dir = NULL, pattern = NULL, fun = readr::read_csv, ...){
  fileslist <- paste(dir, list.files("./data/raw", pattern = pattern), sep = "/")
  nFiles <- length(fileslist)
  for(i in 1:nFiles){
    file.i <- fileslist[i]
    import.i <- fun(file.i, ...)
    if(i != 1) {import_list <- rbind(import_list, import.i)}
    else {import_list <- import.i}
  }
  return(dplyr::tbl_df(import_list))
}
