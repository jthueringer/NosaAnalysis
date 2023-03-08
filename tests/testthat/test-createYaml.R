
test_that("valid and invalid yaml parameter input", {

  yaml = list()
  yaml$Prep = list()
  yaml$Prep$NeedsTimeCorrection = FALSE
  yaml$Prep$nonsense = "hallo"
  yaml$Output = list()
  yaml$Output$SEM$nonsense = c(1,2)
  yaml$Output$SEM$before = 3.0
  yaml$Sheets = list()
  yaml$Sheets$metadata = list()
  yaml$Sheets$nonsense = list()

  yc = YamlClass$new(yaml)

  expect_message(preps <- createYaml(yc, prep = yaml$Prep)$prep, "Invalid 'Prep' parameter removed")
  expect_equal(preps$NeedsTimeCorrection, FALSE)
  expect_null(preps$nonsense)

  expect_message(outs <- createYaml(yc, outputs = yaml$Output)$outputs, "Invalid 'Output' parameter removed")
  expect_null(outs$SEM$nonsense1)
  expect_equal(outs$SEM$before, 3.0)
  expect_equal(length(outs), 1)

  expect_message(sheets <- createYaml(yc, sheets = yaml$Sheets)$sheets, "Invalid 'Sheets' parameter removed")
  expect_null(sheets$Nonsense)
  expect_equal(sheets$metadata, list())
})

test_that("invalid analyser name", {
  yaml = list()
  yaml$Output = list()
  yaml$Output$Nonsense = list()
  yc = YamlClass$new(yaml)
  expect_message(createYaml(yc, outputs = yaml$Output)$outputs, "Invalid 'Output' parameter removed")
})

test_that("get default values, if empty yaml given", {

  yc = YamlClass$new(list())
  yml = createYaml(yc)
  expect_equal(length(yml$sheets), 6)
  expect_equal(length(yml$prep), 5)
  expect_equal(length(yml$outputs), 8)
  expect_equal(yml$sheets$metadata, list())
  expect_equal(yml$prep$NeedsTimeCorrection, TRUE)
  expect_equal(yml$outputs$TimeSlots$Sheet, "Processed")
})


