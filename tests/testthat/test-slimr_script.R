slim_script(
  slim_block(initialize(),
             {
               ## set the overall mutation rate
               initializeMutationRate(1e-7);
               ## m1 mutation type: neutral
               initializeMutationType("m1", 0.5, "f", 0.0);
               ## g1 genomic element type: uses m1 for all mutations
               initializeGenomicElementType("g1", m1, 1.0);
               ## uniform chromosome of length 100 kb
               initializeGenomicElement(g1, 0, 99999);
               ## uniform recombination along the chromosome
               initializeRecombinationRate(1e-8);
             }),
  slim_block(1,
             {
               sim.addSubpop("p1", 500);
             }),
  slim_block(10000,
             {
               sim.simulationFinished();
             })
) -> script_1

test_that("slimr_script output is correct", {
  verify_output(test_path("slimr_script_output_test.txt"),
                script_1)

  skip_if_not_installed("crayon")
  verify_output(test_path("slimr_script_output_test_ansi.txt"),
                script_1,
                crayon = TRUE)

})

test_that("slimr_script objects concatenate correctly", {
  suppressWarnings(test_script_1 <- slim_script(slim_block(1,
                             {
                               sim.addSubpop("p1", 500);
                             })))

  suppressWarnings(test_script_2 <-  slim_script(slim_block(10000,
                              {
                                sim.simulationFinished();
                              })))

  test_script_1_2 <- vctrs::vec_c(test_script_1, test_script_2)
  test_script_2_1 <- vctrs::vec_c(test_script_2, test_script_1)

  expect_s3_class(test_script_1_2, "slimr_script")
  expect_s3_class(test_script_2_1, "slimr_script")
})

test_that("incompatable types throw error", {
  suppressWarnings(test_script_1 <- slim_script(slim_block(1,
                                                           {
                                                             sim.addSubpop("p1", 500);
                                                           })))

  expect_error(vec_c(test_script_1, "a"), class = "vctrs_error_incompatible_type")
  expect_error(vec_c(test_script_1, 1), class = "vctrs_error_incompatible_type")
})
