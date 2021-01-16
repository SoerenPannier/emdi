# The data that is used for testing is the data from the sae package. 
load("EBP/ebp_summary.RData")
load("FH/fh_summary.RData")
load("Direct/direct_summary.RData")

# Test if return is a data.frame
test_that("Test that the summary output works as expected", {
  # check ebp summary
  expect_equal(summary_ebp,
               capture_output_lines(summary(model_ebp), 
                                    print = TRUE, width = 120))
  # check fh summary
  # print(summary_fh)
  # print(capture_output_lines(summary(model_fh), print = TRUE))
  expect_equal(summary_fh,
               capture_output_lines(summary(model_fh),
                                    print = TRUE, width = 120))
  
  # check direct summary
  expect_equal(summary_direct,
               capture_output_lines(summary(model_direct),
                                    print = TRUE, width = 120))
})