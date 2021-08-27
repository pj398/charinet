test_that("helper for checking suggested packages are installed works", {
  expect_error(check_sugs("fake_package_name"))
})

test_that("quick read function works", {
  test_film <- charinet::qread_charinet(events_file = system.file("testdata", "test_eventlist.csv", package = "charinet"),
                                        nodes_file = system.file("testdata", "test_nodelist.csv", package = "charinet"),
                                        from = 3, check_errors = FALSE)
  expect_equal(length(test_film), 3)
})

test_that("reshape functions return original data", {
  test_film <- charinet::qread_charinet(events_file = system.file("testdata", "test_eventlist.csv", package = "charinet"),
                                        nodes_file = system.file("testdata", "test_nodelist.csv", package = "charinet"),
                                        from = 3, check_errors = FALSE)
  test_dyads <- charinet::multicast_to_dyadic(test_film$event_list, from = 3)
  test_multicast <- charinet::dyadic_to_multicast(test_dyads,
                                                  from = 4,
                                                  to = 5,
                                                  time_col = 2,
                                                  char_names = test_film$node_list$char_name)
  expect_identical(rowSums(test_multicast[ , 2:ncol(test_multicast)]),
                   rowSums(test_film$event_list[ , 3:ncol(test_film$event_list)]))
})
