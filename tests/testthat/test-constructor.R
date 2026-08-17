test_that("BGF construction works", {
  expect_type(new_BGF(name = "myBGF",
                      ProcessTemp = 52,
                      InocToSubRatio = 2,
                      ReactorLayout = LETTERS[1:3],
                      BlankLabel = "A",
                      MeasurementType = "test"), "list")
})
