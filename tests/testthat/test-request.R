context("build_request")

test_that("all requests are equivalent", {
  # When
  req <- build_request("GET /api/v2/branding", token = "token", api_url = "https://auth0-org-domain.aut0.com")

  # Then
  expect_equal(req$method, "GET")
  expect_equal(req$url, "https://auth0-org-domain.aut0.com/api/v2/branding")

  expect_equal(build_request("/api/v2/branding", token = "token", api_url = "https://auth0-org-domain.aut0.com"), req)
  expect_equal(build_request("GET https://auth0-org-domain.aut0.com/api/v2/branding", token = "token"), req)
  expect_equal(build_request("https://auth0-org-domain.aut0.com/api/v2/branding", token = "token"), req)
})

test_that("arg sets method", {
  # When
  req <- build_request("/test", method = "POST", token = "token")
  # Then
  expect_equal(req$method, "POST")
})

test_that("wrong method throwing error", {
  expect_error(build_request("BEE /test", method = "BEE", token = "token"))
  expect_error(build_request("BEE /test", token = "token"))
})
