# Kingpin (in progress)

## How to use

First, make sure you have environmental variables `CONNECT_SERVER` and `CONNECT_API_KEY` set up in an .Renviron file. Make sure the `CONNECT_SERVER` value is a URL ending in a slash. To generate an API key in RSConnect, go to RStudio Connect > profile icon > API Keys > New API Key. 

Download the current version of the package by running 

`devtools::install_github("herts-phei/kingpin")
library(kingpin)`

Test if you can connect.

`board <- board_rsconnect(server = Sys.getenv("CONNECT_SERVER"),
                         key = Sys.getenv("CONNECT_API_KEY"))
                         
                         kingpin <- pin_return(board, "kingpin")`
