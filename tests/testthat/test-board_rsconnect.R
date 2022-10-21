test_that("board connects", {
  board <- kingpin::board_rsconnect(Sys.getenv("CONNECT_SERVER"), Sys.getenv("CONNECT_API_KEY"))
  expect_true(
    exists("board")
  )
})
