context("test-backend-oracle.R")

test_that("custom scalar functions translated correctly", {

  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_oracle())
  }

  expect_equal(trans(as.character(x)), sql("CAST(`x` AS VARCHAR2(255))"))
  expect_equal(trans(as.integer64(x)), sql("CAST(`x` AS NUMBER(19))"))
  expect_equal(trans(as.double(x)),    sql("CAST(`x` AS NUMBER)"))

})

test_that("queries translate correctly", {
  mf <- lazy_frame(x = 1, con = simulate_oracle())

  expect_match(
    mf %>% head() %>% sql_render(simulate_oracle()),
    sql("^SELECT [*] FROM [(]SELECT [*]\nFROM [(]`df`[)] [)] `[^`]*` WHERE ROWNUM [<][=] 6")
  )
})

test_that("paste and paste0 translate correctly", {
  trans <- function(x) {
    translate_sql(!!enquo(x), con = simulate_oracle(), window = FALSE)
  }

  expect_equal(trans(paste(x, y)), sql("`x` || ' ' || `y`"))
  expect_equal(trans(paste0(x, y)), sql("`x` || `y`"))
})

test_that("custom %in% is working correctly",{
  library(stringr)
  trans <- function(x, con = simulate_oracle()) {
    translate_sql(!!enquo(x), con = con)
  }
  long_or <- trans(x %in% 1:1001)
  normal_or <- trans(x %in% 1:1000)
  normal_gen <- trans(x %in% 1:1000, con = simulate_odbc())

  expect_equal(str_count(long_or, "OR"), 1)
  expect_equal(str_count(long_or, "`x` IN"), 2)

  expect_equal(normal_or, normal_gen)



})
