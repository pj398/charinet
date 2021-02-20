test_that("plotting functions throw errors due to dependencies", {
  tfa <- movienetdata::starwars_01
  expect_error(charinet::plot_charinet(tfa$adjacency,
                                       char_names = tfa$node_list$char_name,
                                       degree = tfa$node_list$size,
                                       cutoff = 3,
                                       title = "The Force Awakens"))
})
