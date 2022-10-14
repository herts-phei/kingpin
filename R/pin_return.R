
#' Pin return (pin_read equivalent)
#'
#' @description Reads a pin from a specified board while recording metadata to kingpin
#' @usage pin_return(
#' board,
#' name,
#' ...)
#'
#' @param board A pins board object from board_rsconnect()
#' @param name Name of pin to be read
#' @param ... Any additional arguments for pins::pin_read
#'
#' @return A pinned object from the specified pins board
#' @export pin_return
#' @examples
#' # Basic usage, assuming .Renviron is set up with CONNECT_SERVER and CONNECT_API_SERVER environmental variables:
#' board <- pins::board_rsconnect(server = Sys.getenv("CONNECT_SERVER"), key = Sys.getenv("CONNECT_API_SERVER"))
#'
#' # Pin something temporary first
#' #pins::pin_write(board, data.frame(a = 1:10, b = 1:10), "temp")
#'
#' # Retrieve pin
#' #pin_return(board, name = paste0(Sys.info()["user"], "/temp"))
#'
#' # To check if kingpin has updated:
#'  #purrr::quietly(pins::pin_read)(board, name = "kingpin")$result$records
#'
pin_return <- function(board, name, ...) {

  # Update kingpin
  kingpin <- purrr::quietly(pins::pin_read)(board, "kingpin")$result
  kingpin$records <- kingpin$records |>
    dplyr::bind_rows(data.frame(pin_name = name, # pin name
                                project_name = sub('.*/', '', rstudioapi::getActiveProject()), # name of project associated with pin, if applicable
                                writer = NA, # username of pin_write instance
                                write_date = NA, # date of pin_write instance
                                reader = Sys.info()["user"], # username of pin_read instance
                                read_date = as.character(Sys.time()) # date of pin_read instance
    ))

  purrr::quietly(pins::pin_write)(board, kingpin, "kingpin")

  # Return pin in question
  purrr::quietly(pins::pin_read)(board, name, ...)$result

}
