test_that("heatmap functions work", {
  tfa <- movienetdata::starwars_01
  tfa_hm <- activity_heatmap(event_list = tfa$event_list,
                             char_names = tfa$node_list$char_name,
                             n_chunks = 10, from = 3)
  testthat::expect_true(is.matrix(tfa_hm))
})
