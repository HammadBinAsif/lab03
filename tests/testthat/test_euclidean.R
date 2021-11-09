
test_that("test reponse for correct euclidean algorithem", {
  expect_equal(euclidean(123612,13892347912), 4)
  expect_equal(euclidean(100,1000), 100)
})



test_that("test reponse for euclidean algorithem", {
  expect_error(euclidean("123612",13892347912))
  expect_error(euclidean("100",1000))
})
