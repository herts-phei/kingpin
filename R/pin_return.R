
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
# library(kingpin)
# board <- kingpin::board_rsconnect(server = Sys.getenv("CONNECT_SERVER"), key = Sys.getenv("CONNECT_API_KEY"))
#
# # Pin something temporary first
# pins::pin_write(board, x = iris, name = "temp_iris")
#
# # Retrieve pin
# pin_return(board, name = "temp_iris")
#
# # To check if kingpin has updated:
# kingpin <- pins::pin_read(board, name = "kingpin")$records
#'
pin_return <- function(board, name, ...) {

  # Clean pin name
  name <- sub('.*/', '', name)

  # Get active project
  call <- purrr::safely(rstudioapi::getActiveProject)()
  if(is.null(call$result) | !is.null(call$error)) { # If not in a project or not in a session
    project <- "none"
  } else {
    project <- sub('.*/', '', rstudioapi::getActiveProject())
  }

  # Check if user has access to the pin
  content <- suppressMessages(purrr::safely(pins::pin_read)(board, name, ...))
  if (is.null(content$result)) { stop("Error occured during fetching of pin. Please check if you have access or have provided the correct pin name.") }

  # Check if there's a comment
  comment <- ifelse(is.null(comment(content$result)), NA, comment(content$result))

  # Update kingpin
  kingpin <- purrr::quietly(pins::pin_read)(board, "kingpin")$result
  kingpin$records <- kingpin$records |>
    dplyr::bind_rows(data.frame(pin_name = name, # pin name
                                project_name = project, # name of project associated with pin, if applicable
                                writer = NA, # username of pin_write instance
                                write_date = NA, # date of pin_write instance
                                reader = Sys.info()["user"], # username of pin_read instance
                                read_date = as.character(Sys.time()), # date of pin_read instance
                                last_modified = NA,
                                comment = comment
    ))

  out <- purrr::quietly(pins::pin_write)(board, kingpin, "kingpin")

  # Return pin in question
  cat(comment(content$result))
  content$result

}
