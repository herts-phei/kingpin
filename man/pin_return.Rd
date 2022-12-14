% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pin_return.R
\name{pin_return}
\alias{pin_return}
\title{Pin return (pin_read equivalent)}
\usage{
pin_return(
board,
name,
...)
}
\arguments{
\item{board}{A pins board object from board_rsconnect()}

\item{name}{Name of pin to be read}

\item{...}{Any additional arguments for pins::pin_read}
}
\value{
A pinned object from the specified pins board
}
\description{
Reads a pin from a specified board while recording metadata to kingpin
}
\examples{
# Basic usage, assuming .Renviron is set up with CONNECT_SERVER and CONNECT_API_SERVER environmental variables:
library(kingpin)
board <- pins::board_rsconnect(server = Sys.getenv("CONNECT_SERVER"), key = Sys.getenv("CONNECT_API_KEY"))

# Pin something temporary first
pins::pin_write(board, x = iris, name = "temp_iris")

# Retrieve pin
pin_return(board, name = "temp_iris")

# To check if kingpin has updated:
kingpin <- pins::pin_read(board, name = "kingpin")$records

}
