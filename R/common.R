
#' get_remote_file
#'
#' @description get the file provided by the URL
#' 
#' @param remote_URL remote URL 
#' @param file_type  file type
#'
#' @return Na if any exception happens
#' @export
#'
#' @examples
#'       a <- get_remote_file("https://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_AT_3zip.xlsx", "xlsx")   
get_remote_file <- function(remote_URL, file_type)
{
  tryCatch(
    {
       temp_file <- tempfile(fileext = paste0(".", file_type))
       u <- curl::curl_fetch_memory(remote_URL)
       writeBin(object = u$content, con = temp_file)
       return (temp_file)
    },
    error=function(e) {
      message(paste("Failed to connect ", remote_URL))
      message(e)
      # Choose a return value in case of error
      return(NULL)
    }
  )
}

#' Check the File is valid 
#'
#' @param filename 
#' @param ext 
#'
#' @return
#' @export
#'
#' @examples
validate_file <- function(filename, ext)
{
   value = FALSE
   if (!is.null(filename)) {
      if (file.exists(filename))
      {
        ex <- strsplit(basename(filename), split="\\.")[[1]]
        if (ex[-1] == ext)
        {
          value =  TRUE
        }
      }
   }
   return (value)
}


