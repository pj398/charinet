test_that("narrative centrality measure works", {
  tfa <- movienetdata::starwars_tfa
  tfa_scores <- narrative_centrality(tfa$event_list,
                                     char_names = tfa$node_list$char_name,
                                     from = 3,
                                     wp = 0.01,
                                     normalised = TRUE,
                                     mode = "both")
  expect_type(tfa_scores, "list")
  testthat::expect_true(is.numeric(tfa_scores$out_scores))
})
