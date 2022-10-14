
#' Pin throw (pin_write equivalent)
#'
#' @description Writes a pin to to specified board while recording metadata to kingpin
#' @usage pin_throw(
#' board,
#' file,
#' name,
#' versioned = FALSE,
#' ...)
#'
#' @param board A pins board object from board_rsconnect()
#' @param file Object to be pinned
#' @param name Name of object to be pinned
#' @param versioned Whether to pin with version control on
#' @param ... Any additional arguments for pins::pin_write
#'
#' @return A pinned object to the specified pins board
#' @export pin_throw
#' @examples
#' # Basic usage, assuming .Renviron is set up with CONNECT_SERVER and CONNECT_API_SERVER environmental variables:
#' board <- pins::board_rsconnect(server = Sys.getenv("CONNECT_SERVER"), key = Sys.getenv("CONNECT_API_SERVER"))
#'
#' #pins::pin_throw(board, data.frame(a = 1:10, b = 1:10), "temp")
#'
#' # To check if kingpin has updated:
#' #kingpin <- purrr::quietly(pins::pin_read)(board, "kingpin")$result$records
#'
pin_throw <- function(board, file, name,
                      versioned = FALSE, ...) {

  # Update kingpin
  kingpin <- purrr::quietly(pins::pin_read)(board, "kingpin")$result
  kingpin$records <- kingpin$records |>
    dplyr::bind_rows(data.frame(pin_name = name, # pin name
                                project_name = sub('.*/', '', rstudioapi::getActiveProject()), # name of project associated with pin, if applicable
                                writer = Sys.info()["user"], # username of pin_write instance
                                write_date = as.character(Sys.time()), # date of pin_write instance
                                reader = NA, # username of pin_read instance
                                read_date = NA # date of pin_read instance
    ))

  purrr::quietly(pins::pin_write)(board, kingpin, "kingpin")

  # Write pin in question
  purrr::quietly(pins::pin_write)(board, file, name, versioned = versioned, ...)

}
