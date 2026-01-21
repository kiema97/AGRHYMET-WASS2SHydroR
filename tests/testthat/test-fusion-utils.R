test_that(".ensure_year_bounds handles YYYY", {
  expect_equal(.ensure_year_bounds(c(2001, 2005)), c(20010101, 20051231))
})

test_that(".ensure_year_bounds handles YYYYMMDD", {
  expect_equal(.ensure_year_bounds(c(20010701, 20050701)), c(20010701, 20050701))
})

test_that(".ensure_year_bounds handles mixed formats", {
  expect_equal(.ensure_year_bounds(c(2001, 20050701)), c(20010101, 20050701))
})

test_that(".ensure_year_bounds rejects invalid input", {
  expect_error(.ensure_year_bounds(c("2001", "2005")))
  expect_error(.ensure_year_bounds(c(1800, 2005)))
  test_that(".ensure_year_bounds sorts bounds when input order is reversed", {
    expect_equal(.ensure_year_bounds(c(2005, 2001)), c(20010101, 20051231))
  })

})
