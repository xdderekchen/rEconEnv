

#' get Interest Rate
#'
#' @param in_ir_file a file downloaded from http://www.freddiemac.com/pmms/docs/30yr_pmmsmnth.xls or http://www.freddiemac.com/pmms/docs/15yr_pmmsmnth.xls
#' @param default_file the remote file on http://www.freddiemac.com/pmms/docs
#' @param from_year  Four digits representing the data starting year.
#'
#' @return
#' @export
#'
#' @examples
#'      ir30file <- "http://www.freddiemac.com/pmms/docs/30yr_pmmsmnth.xls"
#'      get_IR_data(in_ir_file= NULL, default_file=ir30file, from_year =2017)
#'
#'      ir15file <- "http://www.freddiemac.com/pmms/docs/15yr_pmmsmnth.xls"
#'      get_IR_data(in_ir_file= NULL, default_file=ir15file, from_year =2010)
get_IR_data <- function(in_ir_file, default_file, from_year)
{
  requireNamespace("data.table")

  fmt = "xls"
  dt <- NULL
  tryCatch(
    {
      isRemote = FALSE
      inputfile = NULL
      if (validate_file(in_ir_file,fmt ))
      {
        inputfile = in_ir_file
      } else
      {
        inputfile = get_remote_file(default_file, fmt)
        isRemote = TRUE
      }
      if (!is.null(inputfile))
      {
        dt <- suppressWarnings(readxl::read_excel(path=inputfile, col_names = FALSE, skip =2 ))
        if (isRemote){
          unlink(inputfile) # delete the temp file
        }
      }
    },
    error=function(e) {
      message(paste("Failed to read file!"))
      message(e)
    }
  )
  
  if (is.null(dt))
  {
    return (NULL)
  }
  remove_all_NA <- function(df, re_names = TRUE)
  {
    df0 <- Filter(function(x)!all(is.na(x)), df)
    df0 <- df0[rowSums(is.na(df0)) != ncol(df0),]
    num <- dim(df0)[2]
    colnames(df0) <- paste0("V", 1:num)
    return (df0)
  }

  rawdata0 <- remove_all_NA(dt)
  from_year =ifelse(is.null(from_year), 1970, from_year)


  MonNames = "January,February,March,April,May,June,July,August,September,October,November,December"
  MonNames = unlist(strsplit(MonNames, ","))

  datalist <- list()
  i = 2
  n = 1

  while (i < dim(rawdata0)[2]) {
    sub_data <- rawdata0[, c(1, i, i+1 )]
    colnames(sub_data) <- c("V1", "V2", "V3")
    sub_data$Year <- as.integer(ifelse(is.na(sub_data$V1) & sub_data$V2 %in% as.character(1970:2030), sub_data$V2, NA))
    data.table::setnafill(sub_data, type = "locf", cols = "Year")


    subindex <- (sub_data$V1 %in% MonNames) & !is.na(sub_data$V2) &
                      !is.na(sub_data$V3) & (sub_data$V3 != 'n.a.') & (sub_data$V2!="NA") &
                      sub_data$Year>=from_year

    sub_data <- sub_data[subindex,]
    if (dim(sub_data)[1] > 0)
    {

       sub_data$Date = format(as.Date(paste(sub_data$Year, sub_data$V1, "01"), format = "%Y %B %d"), "%Y%m")
       sub_data$IR   = as.numeric(sub_data$V2) + as.numeric(sub_data$V3) / 4

       setDT(sub_data)
       sub_data0 <- sub_data[, c("Date", "IR")]

       datalist[[n]] = sub_data0
       n = n + 1
     #  print("*****")
    }
    i = i + 2
  }

  ir_data <- rbindlist(datalist)
  ####ir_data <- ir_data[order(Date,decreasing=FALSE),]
  data.table::setorder(ir_data, "Date")

  return (ir_data)
}

#' get FRM 30yr interest rate
#'
#' @param irfile The filename for the data downloaded from "http://www.freddiemac.com/pmms/docs/30yr_pmmsmnth.xls". If this is null, the function will connect to the site and download data.
#' @param from_year Four digits representing the data starting year.
#'
#' @return
#' @export
#'
#' @examples
#'     irdata <- get_IR_FRM30()
#'
get_IR_FRM30 <- function(irfile=NULL, from_year = NULL)
{
  ir30file <- "http://www.freddiemac.com/pmms/docs/30yr_pmmsmnth.xls"
  return (get_IR_data(in_ir_file=irfile, default_file =ir30file, from_year))
}


#' get FRM 15yr interest rate
#'
#' @param irfile The filename for the data downloaded from "http://www.freddiemac.com/pmms/docs/15yr_pmmsmnth.xls". If this is null, the function will connect to the site and download data.
#' @param from_year Four digits representing the data starting year.
#'
#' @return
#' @export
#'
#' @examples
#'      irdata <- get_IR_FRM15()
#'
get_IR_FRM15 <- function(irfile=NULL, from_year = NULL)
{
  ir15file <- "http://www.freddiemac.com/pmms/docs/15yr_pmmsmnth.xls"
  return (get_IR_data(in_ir_file=irfile, default_file =ir15file, from_year))

}


#' get FRM 30yr and 15yr interest rate
#'
#' @param irfile The filename for the data downloaded from "http://www.freddiemac.com/pmms/docs/15yr_pmmsmnth.xls". If this is null, the function will connect to the site and download data.
#' @param irfile The filename for the data downloaded from "http://www.freddiemac.com/pmms/docs/30yr_pmmsmnth.xls". If this is null, the function will connect to the site and download data.
#' @param from_year Four digits representing the data starting year.
#'
#' @return
#' @export
#'
#' @examples
#'     irdata <- get_IR_FRM_15_30()
get_IR_FRM_15_30 <- function(irfile15=NULL, irfile30=NULL, from_year = NULL)
{
  if (is.null(from_year)) {
    from_year = 2000
  }

  ir15 = get_IR_FRM15(irfile15, from_year)
  setnames(ir15, old = c("IR"), new = c('IR15'))
  ir30 = get_IR_FRM30(irfile30, from_year)
  setnames(ir30, old = c("IR"), new = c('IR30'))
  ir_data <- merge(ir15, ir30, all=FALSE)
  return(ir_data)
}


#' Plotting IR data.
#'
#' @param dt IR data
#' @param title Title appearing the chart
#'
#' @return dygraphs object
#' @export
#'
#' @examples
#'    irdata <- get_IR_FRM_15_30()
#'    plot_IR(irdata, title="IR rate for FRM30yr and FRM15yr")
plot_IR <- function(dt, title="Interest Rate")
{
  #requireNamespace("dygraphs")
  firstDate = dt[[1]][1]
  #year= lubridate::year(firstDate)
  #month=lubridate::month(firstDate)
  #dt = as.data.frame(dt)
  dt$Date = NULL
  data = ts(dt, frequency =12, start=c(as.integer(substr(firstDate, 1, 4)), as.integer(substr(firstDate, 5, 6))))
  return (dygraph(data,  main =title ) %>% dyAxis("x", drawGrid = TRUE) %>% dyAxis("y", label = "Interest Rate")  %>% dyRangeSelector())
}
