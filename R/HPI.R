
#' Interpolate Quarterly HPI to Monthly HPI.
#'
#' @param dt Quarterly HPI
#' @param group_var region or zip3 value
#' @param from_year Four digits representing the data starting year.
#'
#' @return
#' @export
#'
#' @examples Used internally.
convert_quarterly_to_monthly <- function(dt, group_var, from_year)
{

  dt <- as.data.table(dt)
  dt[, Next_Q  := shift(Index, 1L,   type="lead"), by=group_var]
  #interpolate the 2 months index using geometric mean
  dt[, Index_M1  := ifelse( is.na(Next_Q), Index, exp((log(Index)*2 + log(Next_Q)) / 3))]
  dt[, Index_M2 := exp((log(Index) + log(Next_Q)*2) / 3)]
  dt[, YearMM := paste0(Year, sprintf("%02d", Quarter*3-1))]
  dt[, Next_M := paste0(Year, sprintf("%02d",Quarter*3))]
  dt[, Next_M2 := paste0(Year + (Quarter==4), sprintf("%02d", (Quarter*3+1) %% 12 ))]

  dt_curr = dt[Year>=from_year, c(group_var, "YearMM", "Index"),  with=FALSE]
  dt_next = dt[Year>=from_year, c(group_var, "Next_M", "Index_M1"),  with=FALSE] %>% setnames(old = c("Next_M", "Index_M1"), new = c('YearMM','Index'))
  dt_next2 = dt[Year>=from_year, c(group_var, "Next_M2", "Index_M2"),  with=FALSE]  %>% setnames(old = c("Next_M2", "Index_M2"), new = c('YearMM','Index'))

  dt <- rbindlist(list(dt_curr, dt_next, dt_next2))

  setorderv(dt, c(group_var, "YearMM"))
  return (dt[!is.na(Index)])
}

#' get ZIP3 HPI data
#'
#' @description get ZIP3 level Home Price Index data
#'
#' @param zip3file The filename for the data downloaded from "https://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_AT_3zip.xlsx". If this is null, the function will connect to the site and download data.
#' @param from_year Four digits representing the data starting year.
#' @param out.Monthly TRUE means Monthly data, FASLE means Quarterly data.
#'
#' @return data in the data.table format with columns (ZIP3, Date, Index)
#' @export
#'
#' @examples
#'       # get recent ZIP3 Home Price Index (monthly) since 2010
#'       zip3hpi_m    <- get_ZIP3_hpi(from_year=2010, out.Monthly=TRUE)
#'       # get recent ZIP3 (US) Home Price Index (quarterly) since 2000
#'       zip3hpi_Q    <- get_ZIP3_hpi(from_year=2000, out.Monthly=FALSE)
get_ZIP3_hpi <- function(zip3file=NULL, from_year=NULL, out.Monthly=TRUE)
{
  default_file <- "https://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_AT_3zip.xlsx"
  fmt = "xlsx"
  dt <- NULL
  
  tryCatch(
    {
      isRemote = FALSE
      inputfile = NULL
      if (validate_file(zip3file,fmt ))
      {
        inputfile = zip3file
      } else
      {
        inputfile = get_remote_file(default_file, fmt)
        isRemote = TRUE
      }
      if (!is.null(inputfile))
      {
        dt <- readxl::read_excel(path=inputfile, skip=4)
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
  if (!is.null(dt))
  {
    dt %>% setnames(old = "Index (NSA)", new = "Index") %>%
           setnames(old = "Three-Digit ZIP Code", new = "ZIP3")
    dt$ZIP3 = sprintf("%03d", dt$ZIP3)
    dt$YearQ = paste0(dt$Year, "Q", dt$Quarter)
    dt <- data.table(dt)

    from_year =ifelse(is.null(from_year), 1995, from_year)
    
     if (out.Monthly == FALSE)
     {
         return (dt[Year>=from_year, c("ZIP3", "YearQ", "Index")]%>% setnames(old = c("YearQ"), new = c('Date')))
     } else
     {
         return (convert_quarterly_to_monthly(dt, "ZIP3", from_year)%>% setnames(old = c("YearMM"), new = c('Date')))
     }
  } else
  {
     return (NULL)
  }
}

#' get State HPI data
#'
#' @description get State level Home Price Index data
#'
#' @param statefile The filename for the data downloaded from "https://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_AT_state.csv". If this is null, the function will connect to the site and download file.
#' @param from_year Four digits representing the data starting year.
#' @param out.Monthly TRUE means Monthly data, FASLE means Quarterly data.
#'
#' @return data in the data.table format with columns (State, Date, Index)
#' @export
#'
#' @examples
#'       # get recent States Home Price Index (monthly) since 2010
#'       statehpi_m    <- get_State_hpi(from_year=2010, out.Monthly=TRUE)
#'       # get recent States Home Price Index (quarterly) since 2000
#'       statehpi_Q    <- get_State_hpi(from_year=2000, out.Monthly=FALSE)
get_State_hpi <- function(statefile=NULL, from_year=NULL, out.Monthly=TRUE)
{
  default_file <- "https://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_AT_state.csv"
  fmt = "csv"
  dt <- NULL
 
  tryCatch(
    {
      isRemote = FALSE
      inputfile = NULL
      if (validate_file(statefile,fmt ))
      {
        inputfile = statefile
      } else
      {
        inputfile = get_remote_file(default_file, fmt)
        isRemote = TRUE
      }
      if (!is.null(inputfile))
      {
        dt <- data.table::fread(file = inputfile, header=FALSE,sep=",",
                                stringsAsFactors=FALSE,
                                col.name = c("State" , "Year", "Quarter", "Index"),
                                colClasses=c("character","integer","integer","numeric"))
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
 
  dt <- dt[Year>=ifelse(is.null(from_year), 1995, from_year)]
  dt$Index <- nafill(as.numeric(dt$Index),type="locf")
  dt[, YearQ:= paste0(Year, "Q", Quarter)]
 
  if (out.Monthly == FALSE)
  {
    return (dt[Year>=from_year, c("State", "YearQ", "Index")]%>% data.table::setnames(old = c("YearQ"), new = c('Date')))
  } else
  {
    return (convert_quarterly_to_monthly(dt, "State", from_year)%>% data.table::setnames(old = c("YearMM"), new = c('Date')))
  }

}


#' get National HPI data
#'
#' @description get National Home Price Index data
#'
#' @param usfile The filename for the data downloaded from "https://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_AT_us_and_census.csv".
#'               If this is null, the function will connect to the site and then download file.
#' @param from_year Four digits representing the data starting year.
#' @param out.Monthly TRUE means Monthly data, FASLE means Quarterly data.
#'
#' @return data in the data.table format with columns (State, Date, Index)
#' @export
#'
#' @examples
#'       #  get recent National (US) Home Price Index (monthly) since 2010
#'       ushpi_m    <- get_US_hpi(from_year=2010, out.Monthly=TRUE)
#'       #  get recent National (US) Home Price Index (quarterly) since 2000
#'       ushpi_Q    <- get_US_hpi(from_year=2000, out.Monthly=FALSE)
get_US_hpi <- function(usfile=NULL, from_year=NULL,  out.Monthly=TRUE)
{
  default_file <- "https://www.fhfa.gov/DataTools/Downloads/Documents/HPI/HPI_AT_us_and_census.csv"
  fmt = "csv"
  dt <- NULL
  
  tryCatch(
    {
      isRemote = FALSE
      inputfile = NULL
      if (validate_file(usfile,fmt ))
      {
        inputfile = usfile
      } else
      {
        inputfile = get_remote_file(default_file, fmt)
        isRemote = TRUE
      }
      if (!is.null(inputfile))
      {
        dt <- data.table::fread(file = inputfile, header=FALSE,sep=",",
                                stringsAsFactors=FALSE,
                                col.name = c("State" , "Year", "Quarter", "Index"),
                                colClasses=c("character","integer","integer","numeric"))
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
  
  dt = dt[State == "USA"]
  dt = dt[, YearQ:= paste0(Year, "Q", Quarter)]
  from_year =ifelse(is.null(from_year), 1995, from_year)
  
  if (out.Monthly == FALSE)
  {
    return (dt[Year>=from_year, c("State", "YearQ", "Index"), with = FALSE]%>% setnames(old = c("YearQ"), new = c('Date')))
  } else
  {
    return (convert_quarterly_to_monthly(dt, "State", from_year)%>% setnames(old = c("YearMM"), new = c('Date')))
  }
}
#   ushpi_m    <- get_US_hpi(from_year=2010, out.Monthly=TRUE)


#' Plotting HPI data.
#'
#' @param dt HPI data
#' @param regionnames either a list of State Names or a list of ZIP3 codes.
#' @param title Title appearing the chart
#'
#' @return dygraphs object
#' @export
#'
#' @examples
#'      # get recent ZIP3 Home Price Index (quarterly) since 2005
#'      zip3hpi_Q    <- get_ZIP3_hpi(from_year=2005, out.Monthly=FALSE)
#'      plot_HP(dt=zip3hpi_Q,  regionnames = c("010", "011", "012") , title="Quarterly HPI for selected zips")

plot_HP <- function(dt, regionnames=NULL, title="Home Price Index")
{
  requireNamespace("dygraphs")

  colnames = colnames(dt)

  firstDate = dt$Date[1]

  freq = 12
  start = c()
  if (grepl("Q", as.character(firstDate)))
  {
    freq = 4
    start=c(as.integer(substr(firstDate, 1, 4)), as.integer(substr(firstDate, 6, 6)) * 3 -1)

  } else
  {
    freq = 12
    start=c(as.integer(substr(firstDate, 1, 4)), as.integer(substr(firstDate, 5, 6)))
  }
  if ("ZIP3" %in% colnames) {
    if (!is.null(regionnames)) {
      dt = dt[ZIP3 %in% regionnames, ]
    }
    dt = dcast(dt, Date ~ ZIP3,  value.var="Index")[order(Date,decreasing=FALSE), ]
  } else
  {
    if (!is.null(regionnames)) {
      dt = dt[State %in% regionnames, ]
    }
    dt = dcast(dt, Date ~ State,  value.var="Index")[order(Date,decreasing=FALSE), ]
  }
  data = ts(dt[, -("Date") ], frequency =freq, start=start)

  return (dygraph(data, main =title) %>%
            dyAxis("x", drawGrid = TRUE) %>%
            dyAxis("y", label = "Home Price Index")  %>%
            dyRangeSelector())
}


#ushpi_Q1    <- get_US_hpi(from_year=2000, out.Monthly=FALSE)

#ushpi_q    <- get_US_hpi(from_year=2010, out.Monthly=FALSE)
#and plotting the data:
#plot_HP(dt=ushpi_q,  title="Monthly National HPI" )
