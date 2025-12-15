# devtools::load_all()

test_that("stations id repair works", {
  expect_equal(
    stations_id_repair(c("5785.10", "5785.1", "5779.1", "5721.01_Pillar")),
    c("5785.10", "5785.10", "5779.10", "5721.01")
  )
})
