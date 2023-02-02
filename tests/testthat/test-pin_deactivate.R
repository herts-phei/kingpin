
test_that("function deletes pin and backs up to pin_pit", {

  board <- suppressMessages(board_rsconnect(Sys.getenv("CONNECT_SERVER"), Sys.getenv("CONNECT_API_KEY")))

  test_name <- paste0(Sys.info()["user"], "_unittest3-1_", Sys.Date())

  suppressMessages(pins::pin_write(board, data.frame(col = test_name), name = test_name))

  suppressMessages(pin_deactivate(board, server = Sys.getenv("CONNECT_SERVER"),
                 key = Sys.getenv("CONNECT_API_KEY"), name = test_name))

  res1 <- purrr::safely(pins::pin_read)(board, test_name) # kingpin record
  res2 <- suppressMessages(pins::pin_read(board, "pin_pit"))

  # Tests
  expect_true(
    all(
      !is.null(res1$error), # error message suggests the pin doesn't exist or deleted
      test_name %in% names(res2), # temp pin has been backed up
      test_name %in% res2[[test_name]]$content$col, # content of backup is correct
      "7 days to deletion" %in% res2[[test_name]]$countdown # maximum duration is given before deletion
    )
  )

})
