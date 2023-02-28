test_that("pinning works and action is recorded in kingpin", {

  board <- suppressMessages(board_rsconnect(Sys.getenv("CONNECT_SERVER"), Sys.getenv("CONNECT_API_KEY")))

  test_name <- paste0(Sys.info()["user"], "_unittest2-1_", Sys.Date())
  suppressMessages(pin_throw(board, data.frame(col = test_name),
            name = test_name))

  res1 <- suppressMessages(pins::pin_read(board, test_name)$col) # pin
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
        sub("\\ .*","", res2$write_date[res2$pin_name == test_name])
    )
  )
})

test_that("pin information is correct in kingpin", {

  board <- suppressMessages(board_rsconnect(Sys.getenv("CONNECT_SERVER"), Sys.getenv("CONNECT_API_KEY")))
  test_name <- paste0(Sys.info()["user"], "_unittest2-2_", Sys.Date())

  suppressMessages(pin_throw(board, data.frame(col = test_name),
            name = test_name))

  res1 <- suppressMessages(pins::pin_read(board, "kingpin")$records) # kingpin record
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
      all(as.character(Sys.Date()) == sub("\\ .*","", res1$write_date)), # check that pin date is correct
      all(Sys.info()["user"] == res1$writer), # check that pin user is correct
      all(project %in% c(res1$project_name, "none")), # check that pin project is correct
      all(is.na(res1$reader), is.na(res1$read_date)) # check that reader cols are NA
    )
  )
})

test_that("comment is kept if provided", {

  board <- suppressMessages(board_rsconnect(Sys.getenv("CONNECT_SERVER"), Sys.getenv("CONNECT_API_KEY")))
  test_name <- paste0(Sys.info()["user"], "_unittest2-3_", Sys.Date())

  suppressMessages(pin_throw(board, data.frame(col = test_name),
            name = test_name,
            comment = test_name))

  res1 <- suppressMessages(pins::pin_read(board, "kingpin")$records) # kingpin record
  res1 <- res1[res1$pin_name == test_name, ]
  res2 <- purrr::quietly(pin_return)(board, test_name)$result
  res3 <- purrr::quietly(pin_return)(board, test_name)$output

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
      all(res1$comment == test_name), # check that comment is correct in kingpin
      comment(res2) == test_name, # check that comment on pin is correct
      res3 == test_name # check if printed message is correct
    )
  )
})

test_that("comment is kept if previous version of pin has a comment", {

  board <- suppressMessages(board_rsconnect(Sys.getenv("CONNECT_SERVER"), Sys.getenv("CONNECT_API_KEY")))
  test_name <- paste0(Sys.info()["user"], "_unittest2-3_", Sys.Date())

  suppressMessages(pin_throw(board, data.frame(col = test_name),
                             name = test_name,
                             comment = test_name))

  suppressMessages(pin_throw(board, data.frame(col = test_name),
                             name = test_name))

  retrieve_pin <- suppressMessages(pins::pin_read(board, name = test_name))
  res1 <- comment(retrieve_pin)

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
      all(test_name == res1) # check that comment is correct even after pinning a second time without comment specified
    )
  )
})

test_that("last_modified column in kingpin is stored when dataset is changed", {

  board <- suppressMessages(board_rsconnect(Sys.getenv("CONNECT_SERVER"), Sys.getenv("CONNECT_API_KEY")))
  test_name <- paste0(gsub(" ", "", Sys.info()["user"]), "_unittest2-4_", Sys.Date())

  # If pin doesn't exist, last modified should be NA
  suppressMessages(pin_throw(board, data.frame(col = test_name),
                             name = test_name))

  res1 <- suppressMessages(pins::pin_read(board, "kingpin")$records) %>%
    dplyr::filter(pin_name == test_name) %>%
    dplyr::slice(1)

  # If pin exists, but there's been no change, last modified should be NA
  suppressMessages(pin_throw(board,
                             file = data.frame(col = test_name),
                             name = test_name))

  res2 <- suppressMessages(pins::pin_read(board, "kingpin")$records) %>%
    dplyr::filter(pin_name == test_name) %>%
    dplyr::slice(2)

  # If pin exists, and there's been a change, last modified should be current time
  suppressMessages(pin_throw(board,
                             file = data.frame(col = test_name, col2 = test_name),
                             name = test_name))

  res3 <- suppressMessages(pins::pin_read(board, "kingpin")$records) %>%
    dplyr::filter(pin_name == test_name) %>%
    dplyr::slice(3)

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
      is.na(res1$last_modified), # If pin doesn't exist, last modified should be NA
      is.na(res2$last_modified), # If pin exists, but there's been no change, last modified should be NA
      !is.na(res3$last_modified) # If pin exists, and there's been a change, last modified shouldn't be NA
    )
  )
})
