vcr::use_cassette("organizations", {
test_that("gh_organizations works", {

  foo <- gh_organizations()

  testthat::expect_type(foo, "character")
})
})
read
