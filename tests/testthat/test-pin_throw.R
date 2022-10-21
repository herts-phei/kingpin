# test_that("pin name correction works", {
#   board <- board_rsconnect(Sys.getenv("CONNECT_SERVER"), Sys.getenv("CONNECT_API_KEY"))
#
#   pin_throw(board, iris, name = paste0(Sys.info()["user"], "/iris_", Sys.time()))
#   expect_true(
#     exists("board")
#   )
# })
