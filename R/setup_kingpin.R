
#' Setup kingpin file to board
#'
#' @description Sets up an empty "kingpin" pin to a board to be populated by activity data
#' @usage setup_kingpin(
#' server,
#' key,
#' force = FALSE)
#'
#' @param server URL of the board server. It's recommended to store these details in .Renviron and use Sys.getenv()
#' @param key API key to access the board. It's recommended to store these details in .Renviron and use Sys.getenv()
#' @param force Whether to force an override of an existing kingpin.
#'
#' @return A new pin named "kingpin" will be pinned on the board supplied to the function. NOTE: Access permissions are not automatically granted to users other than the active user.
#' @export
#' @examples
#' # Basic usage, assuming .Renviron is set up with CONNECT_SERVER and CONNECT_API_SERVER environmental variables:
#' setup_kingpin(server = Sys.getenv("CONNECT_SERVER"), key = Sys.getenv("CONNECT_API_KEY"))
#'
setup_kingpin <- function(server,
                          key,
                          force = FALSE,
                          group = "Epi") {

  board <- pins::board_rsconnect(server = server,
                                 key = key)

  kingpin <- list(
    records = data.frame(pin_name = "kingpin", # pin name
                         project_name = NA, # name of project associated with pin, if applicable
                         writer = Sys.info()["user"], # username of pin_write instance
                         write_date = Sys.time(), # date of pin_write instance
                         reader = NA, # username of pin_read instance
                         read_date = NA # date of pin_read instance
    ),
    summary = data.frame(pin_name = "kingpin", # pin name
                         project_names = NA, # concatenated names of project associated with pin, if applicable
                         project_count = NA, # number of associated projects
                         writers = Sys.info()["user"], #  concatenated usernames of pin_write instances
                         write_date_range = NA, # date range of pin_write instances
                         write_instances = NA, # count of write instances
                         readers = NA, # concatenated usernames of pin_read instances
                         read_date_range = NA, # date range of pin_read instances
                         read_instances = NA # count of read instances
    )
  )

  # ERROR HANDLING
  prev_data <- purrr::quietly(try)( # catch error if it happens. class will be try-error if it does.
    pins::pin_read(board, "kingpin"), silent = T)$result

  if(class(prev_data) == "try-error") {

    if (!force) {
      message("Kingpin already exists in RSConnect Board. Either manually remove, or use force = TRUE.")
    } else {
      user_input <- readline("Overriding previous kingpin using force = TRUE. \nAre you sure you want to proceed? (y/n)")
      if(!grepl("y", user_input)) message('Kingpin already exists in RSConnect Board.')
    }

  } else {

    # PIN KINGPIN
    pins::pin_write(board, kingpin, "kingpin")
    message("Kingpin on board.")

    # ADJUST PERMISSIONS
    call_group <- httr::GET(paste0(server, "__api__/v1/groups"),
                        httr::add_headers(Authorization = paste("Key", key)))

    guid <- dplyr::bind_rows(httr::content(call_group)$results) |>
      dplyr::filter(name == group) |>
      dplyr::pull(guid)

    call_pins <- httr::GET(paste0(server, "__api__/v1/content"),
                           httr::add_headers(Authorization = paste("Key", key)))

    id <- dplyr::bind_rows(httr::content(call_pins)) |>
      dplyr::filter(name == "kingpin") |>
      dplyr::pull(guid)

    body <- paste0('{
    "principal_guid": "', guid, '",
    "principal_type": "group",
    "role": "owner"
    }')

    result <- httr::POST(paste0(server, "__api__/v1/content/", id, "/permissions"),
                   body = body, encode = "raw",
                   httr::add_headers(Authorization = paste("Key", key)))

    message(paste0("Owner permissions given to group ", group))

  }

}
