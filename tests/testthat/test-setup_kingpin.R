test_that("execution halts if kingpin exists (no force)", {
  messages <- purrr::quietly(setup_kingpin)(Sys.getenv("CONNECT_SERVER"), Sys.getenv("CONNECT_API_KEY"))$messages

  expect_true(any(messages %in% c(
    "Kingpin already exists in RSConnect Board. Either manually remove, or use force = TRUE.\n",
    "Kingpin on board.\n")))
})

test_that("kingpin is the correct format", {
  connect <- purrr::quietly(setup_kingpin)(Sys.getenv("CONNECT_SERVER"), Sys.getenv("CONNECT_API_KEY"))
  board <- suppressMessages(pins::board_rsconnect(server = Sys.getenv("CONNECT_SERVER"),
                                 key = Sys.getenv("CONNECT_API_KEY")))

  kingpin <- suppressMessages(pins::pin_read(board, "kingpin"))

  expect_true(
    all(
      is.list(kingpin), # list with two elements
      ncol(kingpin[[1]] >= 5), # with at least 5 columns
      length(kingpin[[1]] >= 1) # with at least 1 row
      )
    )
})

test_that("pin_pit is made", {
  connect <- purrr::quietly(setup_kingpin)(Sys.getenv("CONNECT_SERVER"), Sys.getenv("CONNECT_API_KEY"))
  board <- suppressMessages(pins::board_rsconnect(server = Sys.getenv("CONNECT_SERVER"),
                                 key = Sys.getenv("CONNECT_API_KEY")))

  pin_pit <- purrr::quietly(pins::pin_read)(board, "pin_pit")
  expect_true(!is.null(pin_pit$result))
})

test_that("kingpin and pin_pit permissions are correctly given", {

   call_group <- httr::GET(paste0(Sys.getenv("CONNECT_SERVER"), "__api__/v1/groups"),
                           httr::add_headers(Authorization = paste("Key", Sys.getenv("CONNECT_API_KEY"))))

   guid <- dplyr::bind_rows(httr::content(call_group)$results) |>
     dplyr::filter(name == "Epi") |> #TODO
     dplyr::pull(guid)

   call_pins <- httr::GET(paste0(Sys.getenv("CONNECT_SERVER"), "__api__/v1/content"),
                          httr::add_headers(Authorization = paste("Key", Sys.getenv("CONNECT_API_KEY"))))

   ids <- dplyr::bind_rows(httr::content(call_pins)) |>
     dplyr::filter(name %in% c("kingpin", "pin_pit")) |>
     dplyr::pull(guid)

   result1 <- httr::content(
     httr::GET(paste0(Sys.getenv("CONNECT_SERVER"), "__api__/v1/content/", ids[1], "/permissions"),
                        body = body, encode = "raw",
                        httr::add_headers(Authorization = paste("Key", Sys.getenv("CONNECT_API_KEY"))))
   )

   result2 <- httr::content(
     httr::GET(paste0(Sys.getenv("CONNECT_SERVER"), "__api__/v1/content/", ids[2], "/permissions"),
               body = body, encode = "raw",
               httr::add_headers(Authorization = paste("Key", Sys.getenv("CONNECT_API_KEY"))))
   )

   expect_true(all(
     guid %in% unlist(result1),
     guid %in% unlist(result2)
   ))

 })
