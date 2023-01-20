
#' Connect to RSConnect board
#'
#' @description Equivalent to pins::board_rsconnect()
#' @usage board_rsconnect(
#' server,
#' key,
#' ...)
#'
#' @param server URL of the board server. It's recommended to store these details in .Renviron and use Sys.getenv()
#' @param key API key to access the board. It's recommended to store these details in .Renviron and use Sys.getenv()
#' @param ... Any additional arguments to pass to pins::board_rsconnect()
#' @return Connection to the specified RSConnect board
#' @export
#' @examples
#' #Basic usage, assuming .Renviron is set up with CONNECT_SERVER and CONNECT_API_SERVER environmental variables:
#' library(kingpin)
#' board_rsconnect(server = Sys.getenv("CONNECT_SERVER"), key = Sys.getenv("CONNECT_API_KEY"))
#'
board_rsconnect <- function(server,
                            key,
                            ...) {

  pins::board_rsconnect(server = server,
                        key = key, ...)

}
