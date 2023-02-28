
#' Pin throw (pin_write equivalent)
#'
#' @description Writes a pin to to specified board while recording metadata to kingpin
#' @usage pin_throw(
#' board,
#' file,
#' name,
#' comment = NULL,
#' ...)
#'
#' @param board A pins board object from board_rsconnect()
#' @param file Object to be pinned
#' @param name Name of object to be pinned
#' @param comment A description of the pin to be pinned
#' @param ... Any additional arguments for pins::pin_write
#'
#' @return A pinned object to the specified pins board
#' @export pin_throw
#' @examples
#' # Basic usage, assuming .Renviron is set up with CONNECT_SERVER and CONNECT_API_SERVER environmental variables:
#' library(kingpin)
#' board <- pins::board_rsconnect(server = Sys.getenv("CONNECT_SERVER"), key = Sys.getenv("CONNECT_API_KEY"))
#'
#' kingpin::pin_throw(board, data.frame(a = 1:10, b = 1:10), "tempiris")
#'
#' # To check if kingpin has updated:
#' kingpin <- pins::pin_read(board, "kingpin")$records
#'
pin_throw <- function(board,
                      file,
                      name,
                      comment = NULL,
                      ...) {

  # Clean pin name
  name <- sub('.*/', '', name)

  # Adding comments
  if (is.null(comment)) { # if not specified

    # Add comment if it already exists
    if(!is.null(comment(file))) comment <- comment(file)

    # OR add comment if it already exists from the previous version of the pin (high priority)
    pin_prev <- suppressMessages(purrr::safely(pins::pin_read)(board, name))
    if(is.null(pin_prev$error)) { comment <- comment(pin_prev$result) }

  } else { # if specified
    # Add comment
    comment(file) <- comment
  }

  # If comment is still NULL:
  if (is.null(comment)) {
    message("Pin will be pinned with no description. Please consider adding a description using the `comment` argument.")
    comment <- NA } else { # If comment is not NULL
      cat("Pinned", name, "with the description '", comment(file), "'.")
    }

  # Get active project
  call <- purrr::safely(rstudioapi::getActiveProject)()
  if(is.null(call$result) | !is.null(call$error)) { # If not in a project or not in a session
    project <- "none"
  } else {
    project <- sub('.*/', '', rstudioapi::getActiveProject())
  }

  # Get the old pin's metadata
  old_info <- suppressMessages(purrr::safely(pins::pin_meta)(board, name))

  # Write pin and check if user has access to the pin
  access <- suppressMessages(purrr::safely(pins::pin_write)(board, file, name, ...))
  if (is.null(access$result)) { stop("Error occured during pinning.") }

  if(!is.null(old_info$error)){ # If old pin doesnt exist yet/error in collecting pin, it has no size

    modified <- NA

  } else { # If it exists, compare sizes

    # Get new metadata
    new_info <- suppressMessages(purrr::safely(pins::pin_meta)(board, name))

    if(new_info$result$file_size == old_info$result$file_size){
      modified = NA
    } else {
      modified = as.character(Sys.time())
    }

  }

  # # Get pin ID for
  # call_pins <- httr::GET(paste0(server, "__api__/v1/content"),
  #                        httr::add_headers(Authorization = paste("Key", key)))
  #
  # id <- dplyr::bind_rows(httr::content(call_pins))
  # id <- id$guid[id$name == name] # ID of the pin to delete

  # Update kingpin
  kingpin <- purrr::quietly(pins::pin_read)(board, "kingpin")$result
  kingpin$records <- kingpin$records |>
    dplyr::bind_rows(data.frame(pin_name = name, # pin name
                                project_name = project, # name of project associated with pin, if applicable
                                writer = Sys.info()["user"], # username of pin_write instance
                                write_date = as.character(Sys.time()), # date of pin_write instance
                                reader = NA, # username of pin_read instance
                                read_date = NA, # date of pin_read instance
                                last_modified = modified,
                                comment = comment

    ))

  out <- purrr::quietly(pins::pin_write)(board, kingpin, "kingpin")

}
