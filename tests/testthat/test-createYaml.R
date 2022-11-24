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

  yc = YamlClass$new(yaml)

  expect_warning(preps <- createYaml(yc, prep = yaml$Prep)$prep, "Invalid parameter removed")
  expect_equal(preps$NeedsTimeCorrection, FALSE)
  expect_null(preps$nonsense)

  expect_warning(outs <- createYaml(yc, outputs = yaml$Output)$outputs, "Invalid parameter removed")
  expect_null(outs$SEM$nonsense1)
  expect_equal(outs$SEM$CropTrace, 2)
  expect_equal(length(outs), 7)
})

test_that("yaml default values", {

  yc = YamlClass$new(list())
  expect_equal(createYaml(yc)$sheets$metadata, list())
  expect_equal(createYaml(yc)$prep$NeedsTimeCorrection, TRUE)
  expect_equal(createYaml(yc)$outputs$TimeSlots$Sheet, "Processed")
})


