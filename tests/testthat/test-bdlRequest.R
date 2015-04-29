context("bdl request")

test_that("BdlRequest generics", {

  req = BdlRequestGetData()
  
  reqs = print(req)
  
  expect_equal(class(reqs), 'character')
 
})

