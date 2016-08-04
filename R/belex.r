#' Download Historical Data from the Belgrade Stock Exchange
#'
#' \code{belex} function downloads historical financial time series
#' from the Belgrade Stock Exchange. One can specify which ticker
#' or index to download, from and to date.
#'
#' @param ticker a character string giving the name of the ticker or
#' index to download. See the web page of the data provider
#' http://belex.rs for information about the available ticker
#' symbols and indexes. See below.
#'
#' @param from  date of the start of the period to download.
#' This must be in the following format: yyyy-mm-dd.
#' Defaults to the first available data.
#'
#' @param to    date of the end of the period to download.
#' This must be in the following format: yyyy-mm-dd.
#' Defaults to the last available data.
#'
#' @details The name of the Belgrade Stock Exchange
#' index is BELEX15 (from September 2005).
#'
#' @return A list consists of five components: \code{ticker}, \code{from}, \code{to},
#' \code{nrows} and \code{data}.
#' Component \code{data} is a data frame with the following time series:
#' Date, Close, Transactions, Volume, Open, Low, High, Total Bid, Total Ask.
#'
#' @author Milos Vilotic and Zlatko Kovacic
#'
#' @examples
#' \dontrun{
#' Belex15 index from 2009-11-23 to 2012-05-15
#'
#' belex15.data <- belex("belex15", "2009-11-23", "2012-05-15")
#'
#' All available data for ticker AIKB
#'
#' aikb.data <- belex("AIKB")
#'
#' Data for ticker NIIS from 2010-10-01
#'
#' niis.data <- belex("NIIS", from = "2010-10-01")
#'
#' Data for ticker IMLK from beginning to 2010-05-12
#'
#'imlk.data <- belex("IMLK", to = "2010-05-12")
#'}
#'


#' @export
belex <- function(ticker, from = NULL, to = NULL)
{

  w <- getOption("warn")
  options(warn = -1)

  ticker <- tolower(ticker)

  urlHTML <- function(ticker)
  {
    if (ticker == "belex15")
    {
      url.I <- "http://www.belex.rs/trgovanje/indeksi/"
      url.II <-"/istorijski/3y"
      url.ticker <- ticker
      url <- paste(url.I, url.ticker, url.II, sep="")
      return(url)
    }
    else
    {
      url.I <- "http://www.belex.rs/trgovanje/istorijski/"
      url.II <-"/3y"
      url.ticker <- ticker
      url <- paste(url.I, url.ticker, url.II, sep="")
      return(url)
    }
  }

  urlCSV <- function(ticker)
  {
    if (ticker == "belex15")
    {
      url.csv.I <- "http://www.belex.rs/xml/istorijski_indeks_csv.php?simbol="
      url.csv.ticker <- toupper(ticker)
      url.csv.II <- "&lang=srb"
      url.csv <- paste(url.csv.I, url.csv.ticker, url.csv.II, sep="")
      return(url.csv)
    }
    else
    {
      url.csv.I <- "http://www.belex.rs/xml/istorijski_csv.php?simbol="
      url.csv.ticker <- ticker
      url.csv.II <- "&lang=srb"
      url.csv <- paste(url.csv.I, url.csv.ticker, url.csv.II, sep="")
      return(url.csv)
    }
  }

  dataHTML <- function(url.HTML)
  {

    if (ticker=="belex15")
    {
      tables <- XML::readHTMLTable(url.HTML)
      data.fac <- tables[[2]]
      data.mat <- as.matrix(data.fac)
      data.mat[ ,2:8] <- gsub("[.]","", data.mat[ ,2:8])
      data.mat[ ,2:8] <- gsub("[,]",".", data.mat[ ,2:8])
      data.mat <- cbind(data.mat[ ,1:2], data.mat[ ,5:8])
      colnames(data.mat) <- c("Date", "Close", "Open", "Low", "High", "Volume")
      data.mat.num <- apply(data.mat[ ,2:6], 2,as.numeric)
      Date <- as.Date(data.mat[ ,1], format='%d.%m.%Y')
      data.final <- data.frame(Date, data.mat.num)
      bool <- data.final$Date > "2012-12-31"
      data.final <- data.final[bool, 1:6]
      return(data.final)
    }
    else
    {
      tables <- XML::readHTMLTable(url.HTML)

      data.fac <- tables[[2]]
      data.mat <- as.matrix(data.fac)
      data.mat[ ,2:14] <- gsub("[.]","", data.mat[ ,2:14])
      data.mat <- data.mat[,-c(3,4,10,13,14)]
      data.mat.num <- apply(data.mat[ ,2:9], 2,as.numeric)
      Date <- as.Date(data.mat[ ,1], format='%d.%m.%Y')
      data.final <- data.frame(Date, data.mat.num)
      bool <- data.final$Date > "2012-12-31"
      data.final <- data.final[bool, ]
      colnames(data.final) <- c("Date", "Close", "Transactions", "Volume", "Open", "Low", "High","Total Bid","Total Ask")

      return(data.final)
    }
  }

  dataCSV <- function(url.CSV)
  {
    if (ticker=="belex15")
    {
      #' @importFrom utils read.csv
      data.csv <- read.csv(url.CSV, header=TRUE, sep=";")

      data.csv.mat <- as.matrix(data.csv)
      data.csv.mat[ ,2:8] <- gsub("[.]","", data.csv.mat[ ,2:8])
      data.csv.mat[ ,2:8] <- gsub("[,]",".", data.csv.mat[ ,2:8])
      data.csv.mat <- cbind(data.csv.mat[,1:2], data.csv.mat[ ,4:7])
      data.csv.mat.num <- apply(data.csv.mat[ ,2:6], 2,as.numeric)
      Date <- as.Date(data.csv.mat[ ,1], format='%d.%m.%Y')
      data.csv.final <- data.frame(Date, data.csv.mat.num)
      colnames(data.csv.final) <- c("Date", "Close", "Open", "Low", "High", "Volume")
      return(data.csv.final)
    }
    else
    {
      data.csv <- read.csv(url.CSV, header=TRUE, sep=";")
      data.csv.mat <- as.matrix(data.csv)
      data.csv.mat[ ,2:12] <- gsub(",00","", data.csv.mat[ ,2:12])
      data.csv.mat[ ,2:12] <- gsub("[.]00","", data.csv.mat[ ,2:12])
      data.csv.mat[ ,2:12] <- gsub("[.]","", data.csv.mat[ ,2:12])
      data.csv.mat.num <- apply(data.csv.mat[ ,2:12], 2,as.numeric)
      Date <- as.Date(data.csv.mat[,1], format='%d.%m.%Y')
      data.csv.final <- data.frame(Date, data.csv.mat.num[,-c(2,10,11)])
      colnames(data.csv.final) <- c("Date", "Close", "Transactions", "Volume", "Open", "Low", "High", "Total Bid","Total Ask")

      return(data.csv.final)
    }
  }

  dataMerge <- function(data.HTML, data.CSV)
  {
    data.merge <- rbind(data.HTML, data.CSV)
    data.merge <- data.merge[with(data.merge, order(Date)), ]

    return(data.merge)

  }

  removeNA <- function(data.merge)
  {
    close.na <- is.na(data.merge$Close)
    datum.na <- is.na(data.merge[ ,1])
    data.merge <- data.merge[!close.na, 1:ncol(data.merge)]
    data.merge <- data.merge[!datum.na, 1:ncol(data.merge)]

    return(data.merge)
  }

  dataSample <- function(data.merge, from, to)
  {
    if (is.null(from) & is.null(to))
    {
      data.merge <- removeNA(data.merge)
      return(data.merge)
    }
    else
    {
      if (is.null(from) & !is.null(to))
      {
        bool.sample <- data.merge[ ,1] <= to
        if (ticker=="belex15")
        {
          data.sample <- data.merge[bool.sample, 1:ncol(data.merge)]
        }
        else
        {
          data.sample <- data.merge[bool.sample, 1:ncol(data.merge)]
        }
        data.sample <- removeNA(data.sample)
        return(data.sample)
      }
      else
      {
        if (!is.null(from) & is.null(to))
        {
          bool.sample <- data.merge[ ,1] >= from
          if (ticker=="belex15")
          {
            data.sample <- data.merge[bool.sample, 1:ncol(data.merge)]
          }
          else
          {
            data.sample <- data.merge[bool.sample, 1:ncol(data.merge)]
          }
          data.sample <- removeNA(data.sample)
          return(data.sample)
        }
        else
        {
          bool.sample <- (data.merge[ ,1] >= from & data.merge[ ,1] <= to)
          if (ticker=="belex15")
          {
            data.sample <- data.merge[bool.sample, 1:ncol(data.merge)]
          }
          else
          {
            data.sample <- data.merge[bool.sample, 1:ncol(data.merge)]
          }
          data.sample <- removeNA(data.sample)
          return(data.sample)
        }
      }
    }
  }

  url.HTML <- urlHTML(ticker)
  url.CSV <- urlCSV(ticker)
  data.HTML <- dataHTML(url.HTML)
  data.CSV <- dataCSV(url.CSV)
  data.Merge <- dataMerge(data.HTML, data.CSV)
  data.Sample <- dataSample(data.Merge, from, to)

  data.Sample <- data.Sample[!duplicated(data.Sample$Date), ]
  row.names(data.Sample) <- 1:dim(data.Sample)[1]

  cat("Download Complete!\n")
  cat("\nTicker:", toupper(ticker),"\n")
  cat("From:", as.character(data.Sample$Date[1]),"\n")
  cat("To:", as.character(data.Sample$Date[dim(data.Sample)[1]]),"\n")
  cat("No rows:", dim(data.Sample)[1],"\n")

  data <- list(ticker = toupper(ticker),
               from = data.Sample$Date[1],
               to = data.Sample$Date[dim(data.Sample)[1]],
               nrows = dim(data.Sample)[1],
               data = data.Sample)

  options(warn = w)

  return(data)
}
