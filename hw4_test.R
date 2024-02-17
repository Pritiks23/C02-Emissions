# This file contains part of my auto-grader. You can use this test file to check 
# to make sure that your code is running as expected. Feel free to modify 
# this file! 

test_that("Data Wrangling and Cleaning", {
  expect_equal( exists("df"), TRUE)
  expect_equal( is.data.frame(get("df")), TRUE)
  expect_equal( ncol(df) >= 15, TRUE)
  expect_equal( nrow(df) > 196 & nrow(df) < 23640, TRUE)
  expect_equal( "pop_fix" %in% colnames(df), TRUE)
  expect_equal( df[df$name == "Nicaragua" & df$Year == 1932, "pop_fix"], 850000)
  expect_equal( df[df$name == "Norway" & df$Year == 1940, "pop_fix"], 3000000)
  expect_equal(df[df$name == "Palau" & df$Year == 1909, "pop_fix"], 6040)
  expect_equal( df[df$name == "China" & df$Year == "1980", "pop_fix"], 1010000000)
})

test_that("Examining Trends in Carbon Footprint", {
  expect_equal( exists("yr_region_df"), TRUE)
  expect_equal( nrow(yr_region_df), 328)
  expect_equal( 6.4 %in% round(yr_region_df[yr_region_df$eight_regions == "africa_north" & yr_region_df$Year == 1979, "avg_carbon"], 1), TRUE)
  
})

test_that("Digging into Carbon Footprint Inequalities", {
  expect_equal( exists("high_inc"), TRUE)
  expect_equal( nrow(high_inc), 55)
  expect_equal( exists("per_country_df"), TRUE)
  expect_equal( nrow(per_country_df), 196)
})