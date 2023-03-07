test_that("reading works and action is recorded in kingpin", {

  board <- suppressMessages(board_rsconnect(Sys.getenv("CONNECT_SERVER"), Sys.getenv("CONNECT_API_KEY")))
  test_name <- paste0(gsub(" ", "", Sys.info()["user"]), "_unittest1-1_", Sys.Date())

  suppressMessages(pins::pin_write(board, data.frame(col = test_name),
            name = test_name))

  res1 <- pin_return(board, test_name)$col
  res2 <- suppressMessages(pins::pin_read(board, "kingpin")$records) # kingpin record

  # Delete temporary pin
  call_pins <- httr::GET(paste0(Sys.getenv("CONNECT_SERVER"), "__api__/v1/content"),
                         httr::add_headers(Authorization = paste("Key", Sys.getenv("CONNECT_API_KEY"))))

  id <- dplyr::bind_rows(httr::content(call_pins))
  id <- id$guid[id$name == test_name] # ID of the pin to delete

  result <- httr::DELETE(paste0(Sys.getenv("CONNECT_SERVER"), "__api__/v1/content/", id),
                         httr::add_headers(Authorization = paste("Key", Sys.getenv("CONNECT_API_KEY"))))

  # Tests
  expect_true(
    all(
      res1 == test_name, # check that pin is correct
      as.character(Sys.Date()) %in% # check that kingpin record exists
        sub("\\ .*","", res2$read_date[res2$pin_name == test_name])
    )
  )
})

test_that("pin information is correct in kingpin", {

  board <- suppressMessages(board_rsconnect(Sys.getenv("CONNECT_SERVER"), Sys.getenv("CONNECT_API_KEY")))

  test_name <- paste0(gsub(" ", "", Sys.info()["user"]), "_unittest1-2_", Sys.Date())

  suppressMessages(pins::pin_write(board, data.frame(col = test_name), name = test_name))

  return <- pin_return(board, test_name)
  res1 <- pin_return(board, "kingpin")$records # kingpin record
  res1 <- res1[res1$pin_name == test_name, ]

  # Delete temporary pin
  call_pins <- httr::GET(paste0(Sys.getenv("CONNECT_SERVER"), "__api__/v1/content"),
                         httr::add_headers(Authorization = paste("Key", Sys.getenv("CONNECT_API_KEY"))))

  id <- dplyr::bind_rows(httr::content(call_pins))
  id <- id$guid[id$name == test_name] # ID of the pin to delete

  result <- httr::DELETE(paste0(Sys.getenv("CONNECT_SERVER"), "__api__/v1/content/", id),
                         httr::add_headers(Authorization = paste("Key", Sys.getenv("CONNECT_API_KEY"))))

  # Get active project
  call <- purrr::safely(rstudioapi::getActiveProject)()
  if(is.null(call$result) | !is.null(call$error)) { # If not in a project or not in a session
    project <- "none"
  } else {
    project <- sub('.*/', '', rstudioapi::getActiveProject())
  }

  # Tests
  expect_true(
    all(
      all(test_name == res1$pin_name), # check that pin name is correct
      all(as.character(Sys.Date()) == sub("\\ .*","", res1$read_date)), # check that pin date is correct
      all(Sys.info()["user"] == res1$reader), # check that pin user is correct
      all(project %in% c(res1$project_name, "none")), # check that pin project is correct
      all(is.na(res1$writer), is.na(res1$write_date)) # check that writer cols are NA
    )
  )
})
