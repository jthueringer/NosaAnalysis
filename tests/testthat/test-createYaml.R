
test_that("valid and invalid yaml parameter input", {

  yaml = list()
  yaml$Directories = list()
  yaml$Directories$nonsense = "hallo"
  yaml$Output = list()
  yaml$Output$SEM$nonsense = c(1,2)
  yaml$Output$SEM$PeakAverage = TRUE
  yaml$DataManipulation = list()
  yaml$DataManipulation$Normalization = list()
  yaml$DataManipulation$Normalization$nonsense = list()

  yc = YamlClass$new(yaml)
  expect_message(dirs <- createYaml(yc, dirs = yaml$Directories)$dirs, "Invalid parameter removed: nonsense")
  expect_null(dirs$nonsense)

  expect_message(outs <- createYaml(yc, outputs = yaml$Output)$outputs, "Invalid parameter removed : SEM : nonsense")
  expect_null(outs$SEM$nonsense1)
  expect_true(outs$SEM$PeakAverage)
  expect_equal(length(outs), 1)

  expect_message(manipulations <- createYaml(yc, manipulate = yaml$DataManipulation)$manipulation,
                 "Invalid parameter removed : Normalization : nonsense")
  expect_null(manipulations$Normalization$Nonsense)
  expect_null(manipulations$Normalization)
})

test_that("invalid analyser name", {
  yaml = list()
  yaml$Output = list()
  yaml$Output$Nonsense = list()
  yc = YamlClass$new(yaml)
  expect_message(createYaml(yc, outputs = yaml$Output)$outputs, "Invalid parameter removed: Nonsense")
})

test_that("get default values, if empty yaml given", {

  yc = YamlClass$new(list())
  yml = createYaml(yc)
  expect_equal(length(yml$manipulate), 6)
  expect_equal(length(yml$dirs), 3)
  expect_equal(length(yml$outputs), 8)
  expect_false(yml$manipulate$Normalization$Execute)
  expect_equal(yml$outputs$TimeSlots$Sheet, "Processed")
})


