
#' Pin deactivate
#'
#' @description "Softly" deactivates a pin by transferring its data to a temporary pin scheduled for deletion.
#' @usage pin_deactivate(
#' board,
#' name,
#' force,
#' ...)
#'
#' @param board A pins board object from board_rsconnect()
#' @param name Name of pin to be read
#' @param force Whether to bypass confirmation messages on console
#'
#' @return Specified pin will be deleted but backed up in pin_pit
#' @export
#' @examples
#' # Basic usage, assuming .Renviron is set up with CONNECT_SERVER and CONNECT_API_SERVER environmental variables:
#' board <- pins::board_rsconnect(server = Sys.getenv("CONNECT_SERVER"), key = Sys.getenv("CONNECT_API_SERVER"))
#'
#' # Pin something temporary first
#' #pins::pin_write(board, data.frame(a = 1:10, b = 1:10), "temp")
#'
#' # Retrieve pin
#' #pin_deactivate(board, name = "temp")
#'
#' # To check if the pin has been backed up in pin_pit:
#'  #purrr::quietly(pins::pin_read)(board, name = "pin_pit")$result
#'
pin_deactivate <- function(board,
                           name,
                           force = FALSE,
                           group = "Epi") {
# TODO NOTE DONT SPECIFY USER NAME WITH PIN

  text <- "Deleted and backed up: \n"

  for (i in 1:length(name)) {

    # BACKUP TO PIN PIT
    backup_first <- purrr::quietly(pins::pin_read)(board, name[i])
    backup <- purrr::quietly(pins::pin_read)(board, "pin_pit")$result
    backup[[name[i]]] <- list(content = backup_first,
                              countdown = "7 days to deletion")

    res <- purrr::quietly(pins::pin_write)(board, backup, "pin_pit")

    call_pins <- httr::GET(paste0(server, "__api__/v1/content"),
                           httr::add_headers(Authorization = paste("Key", key)))

    id <- dplyr::bind_rows(httr::content(call_pins))
    id <- id$guid[id$name == name[i]]

    # DELETION
    if(!force) {
        user_input <- readline(paste0("Are you sure you want to delete '", name[i], "'? (y/n)"))

        if(!grepl("n", user_input)) { stop("Operation halted.") } else {
          result <- httr::DELETE(paste0(server, "__api__/v1/content/", id),
                                 httr::add_headers(Authorization = paste("Key", key)))

          text <- cat(text, paste0(name[i], " \U0002705", "\n"))
        }

    } else {
      result <- httr::DELETE(paste0(server, "__api__/v1/content/", id),
                             httr::add_headers(Authorization = paste("Key", key)))

      text <- cat(text, paste0(name[i], " \U0002705", "\n"))
    }
  }

}
