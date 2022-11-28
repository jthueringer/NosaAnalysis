test_that("empty yaml, no parameter", {
 yc = YamlClass$new(list())
 expect_equal(length(createYaml(yc)$sheets), 6)
 expect_equal(length(createYaml(yc)$prep), 6)
 expect_equal(length(createYaml(yc)$outputs), 7)
})

test_that("valid and invalid yaml parameter input", {

  yaml = list()
  yaml$Prep = list()
  yaml$Prep$NeedsTimeCorrection = FALSE
  yaml$Prep$nonsense = FALSE
  yaml$Output = list()
  yaml$Output$SEM$nonsense = c(1,2)
  yaml$Output$SEM$CropTrace = 2
  yaml$Sheets = list()
  yaml$Sheets$metadata = list()
  yaml$Sheets$nonsense = list()

  yc = YamlClass$new(yaml)

  expect_warning(preps <- createYaml(yc, prep = yaml$Prep)$prep, "Invalid parameter removed")
  expect_equal(preps$NeedsTimeCorrection, FALSE)
  expect_null(preps$nonsense)

  expect_warning(outs <- createYaml(yc, outputs = yaml$Output)$outputs, "Invalid parameter removed")
  expect_null(outs$SEM$nonsense1)
  expect_equal(outs$SEM$CropTrace, 2)
  expect_equal(length(outs), 7)

  expect_warning(sheets <- createYaml(yc, sheets = yaml$Sheets)$sheets, "Invalid parameter removed")
  expect_null(sheets$Nonsense)
  expect_equal(sheets$metadata, list())


  yaml2 = list()
  yaml2$Output = list()
  yaml2$Output$Nonsense = list()
  yc2 = YamlClass$new(yaml2)
  expect_warning(createYaml(yc2, outputs = yaml2$Output)$outputs, "Invalid parameter removed")
})

test_that("yaml default values", {

  yc = YamlClass$new(list())
  yml = createYaml(yc)
  expect_equal(yml$sheets$metadata, list())
  expect_equal(yml$prep$NeedsTimeCorrection, TRUE)
  expect_equal(yml$outputs$TimeSlots$Sheet, "Processed")
})


