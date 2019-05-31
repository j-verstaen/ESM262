test_that("surge_to_damages gives correct values for simple test", {
  expect_that(surge_to_damage(surge=0,surge.min=900,base=40,K=20), equals(0))
  expect_that(surge_to_damage(surge=1000, surge.min=900, base=0,K=20), equals(2000))
})
test_that("surge_to_damage can handle 0s", {
  expect_that(surge_to_damage(surge=0,surge.min=0,base=0,K=0), equals(0))
})
