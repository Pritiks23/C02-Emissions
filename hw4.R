#Pritika Vipin
#pvipin23@uw.edu
# asked about warning messages that came up for a error
# that kept coming up creating high_inc dataframe
# with Scott Mcdaniel

#all library statements must be at the top of the page!
library(dplyr)
library(stringr)
library(ggplot2)
library(testthat)



# Overview ---------------------------------------------------------------------

# Homework 4: Climate Change Analysis
# Before you begin, make sure you read and understand the assignment description
# on canvas first! This assignment will not make sense otherwise. 
# For each question/prompt, write the necessary code to calculate the answer.
# For grading, it's important that you store your answers in the variable names
# listed with each question in `backtics`. Make sure you DO NOT hardcode values
# unless specified to do so in the instructions! 
# BEFORE YOU SUBMIT YOUR ASSIGNMENT, MAKE SURE YOU COMMENT OUT ANY PLOT() STATEMENTS

# Loads in your datasets
country_df <- read.csv("country_data.csv") #DO NOT CHANGE!
geo_df <- read.csv("geographies.csv") #DO NOT CHANGE!

# Data Wrangling and Cleaning  -------------------------------------------------
#
# Like with the last homework, this assignment asks you to work with two related
# datasets. Before you can visualize any patterns in the dataset, you'll first 
# need to properly join the two datasets into one.
#
# ------------------------------------------------------------------------------

# First, merge your two dataframes using a left join on the geo_df with the 
# country_df and store the combined dataframe into a variable `df`. You need to 
# join based on the names of the countries. 
#1
df <- left_join(geo_df, country_df, by = c("name" = "country"))

# Let's clean-up some potential data problems in your merged `df`. One problem 
# you may have noticed in this dataset is that the datatype for the Population 
# column is "character" (also known as a "String datatype"). Before we can 
# visualize the data, you'll need to convert the Population column to a numeric 
# type and store this new value into a column in your `df` called `pop_fix`.
# HINT 1 - First figure out how to remove the "M", "k", or "B" character from population 
# values, then figure out how to transform the values into numbers, and then 
# multiply each value by the correct amount. You need to keep track of the units! 
# Populations with the B character need to be multiplied by 1000000000; the ones
# with the M character need to be multiplied by 1000000 and k need to be 
# multiplied by 1000. 
# HINT 2 - The as.numeric function in R can help you transform strings into numbers! 
#2 
clean_population <- function(population){
  if (is.na(population)) {
    return (NA)
  } else if (str_detect(population, "B")){
    return(as.numeric(str_replace(population, "B", "")) * 1000000000)
  }  else if (str_detect(population, "M")){
    return(as.numeric(str_replace(population, "M", "")) * 1000000)
  } else if (str_detect(population, "k")){
    return(as.numeric(str_replace(population, "k", "")) * 1000)
  } else{
    return(as.numeric(population))
  }
}
df$pop_fix <- sapply(df$Population, clean_population)
# Examining Trends in Carbon Footprint  ----------------------------------------
#
# In this section, you'll be examining trends in cabon footprint across different
# parts of the world. Before you begin this section, make sure you understand 
# how to use the "group_by" and "summarize" methods!
#
# ------------------------------------------------------------------------------

# Create a new dataframe `yr_region_df` that stores the MEAN carbon footperint 
# per capita PER EACH YEAR across each of the eight regions in the world. Note, 
# this question is asking you to create an aggregated view of your dataset based 
# on year and the eight regions. The aggregated dataframe  (i.e. region_df) should have three coloumns: 
# Year, eight_regions, and avg_carbon
yr_region_df <- summarise(group_by(df, Year, eight_regions),
                          avg_carbon = mean(Carbon_Footprint_percapita, na.rm = TRUE))

# Once you have created your `yr_region_df`, remove any rows from this dataframe
# that contain data from any year before 1979 and/or have an NA value for one of
# the eight regions. Your `yr_region_df` after this step should now have only 328 rows.
yr_region_df <- filter(yr_region_df, Year >= 1979, !is.na(eight_regions))
#print(nrow(yr_region_df))

# Create a variable called `carbon_em_lineplot` that stores a ggplot lineplot 
# showing the average carbon emissions overtime per each of the eight regions. 
# You should use the `region_df` you created in the two steps earlier. 
# The lineplot should have the following characteristics: 
#   x axis & Label: Year  
#   y axis & Label: Average Carbon Footprint per Capita  
#   line color should be determined based on which of the eight regions the 
#     country belongs to.
#   Legend: Eight Regions
# Your plot must have proper axes labels and must have proper legend labels! 
# Do not use default labels! 
carbon_em_lineplot <- ggplot(yr_region_df, aes(x = Year, y = avg_carbon, color = eight_regions)) +
  geom_line() +
  labs(x = "Year", y = "Average Carbon Footprint per Capita")
plot(carbon_em_lineplot)


# Now let's examine the distribution of Carbon Footprint Per Capita across each 
# of the eight world regions. Create a variable called `carb_em_region` that
# stores a ggplot violinplot showing the distribution of carbon emissions per each
# of the eight regions (NOTE: You should NOT use the `yr_region_df` to answer 
# this question).  
# The violinplot should have the following characteristics: 
#   x axis & label: Eight Regions 
#   y axis & label: Carbon Footprint per Capita  
#   fill color should be determined based on which of the eight regions the 
#   country belongs to 
# Your plot must have proper axes labels and must have proper legend labels! 
# Do not use default labels! 
carb_em_region <- ggplot(df, aes(x = eight_regions, y = Carbon_Footprint_percapita, fill = eight_regions)) +
  geom_violin() +
  labs(x = "Eight Regions ", y = "Carbon Footprint per Capita ")
plot(carb_em_region)


# Digging into Carbon Footprint Inequalities -----------------------------------
#
# In this section, you now be digging further into the inequalities in carbon 
# footprint across different parts of the world. 
#
# ------------------------------------------------------------------------------

# let's examine the distribution of Carbon Footprint Per Capita across each 
# of the four income groups. Create a variable called `carb_em_inc` that
# stores a ggplot violinplot that has the following characteristics: 
#   x axis & label: World Bank Income Group 
#   y axis & label: Carbon Footprint per Capita  
#   fill color should be determined based on which of the four income groups 
#     the country belongs to 
# Your plot must have proper axes labels and must have proper legend labels! 
# Do not use default labels! 
carb_em_inc <- ggplot(df, aes(x= income.groups, y = Carbon_Footprint_percapita, fill = income.groups)) +
  geom_violin() +
  labs(x = " World Bank Income Group", y = "Carbon Footprint per Capita")  
  
 
  ggtitle("Distribution of Carbon Footprint Per Capita across each 
 of the four income groups") 
  plot(carb_em_inc)


  
# Create a new dataframe `high_inc` that stores the MEAN carbon footprint 
# per capita for each of the "High Income" countries. Note, this question is asking you to 
# create an aggregated view of your dataset per each country. 
# The aggregated dataframe  (i.e. high_inc) should have atleast two columns:
# name, avg_carb


  high_inc <- summarize(group_by(df, name,  income.groups), avg_carb  = mean(Carbon_Footprint_percapita, na.rm = TRUE))
  high_inc <- filter(high_inc, income.groups == "High income")

 
  
# Finally, remove any rows from your `high_inc` where the avg_carb value is NaN or NA.
# your `high_inc` dataframe should now only have 55 countries. 
    high_inc <-filter(high_inc, !is.na(avg_carb))
  print(nrow(high_inc))

# Create a variable called `high_inc_barplot` that stores a ggplot barplot 
# showing the average carbon emissions per each country for only the high income
# countries. You should use the `high_inc` dataframe you created earlier. 
# The barplot should have the following characteristics: 
#   x axis & Label: Mean Carbon footprint per Capita 
#   y axis & Label: Country Name
#   Title: "Avg. Carbon footprint per Capita for High Income Countries"
#   Caption: "Avg. Carbon footprint per capita between the years 1979 and 2019 for 
#     countries categorized as High Income by the World Bank" 
#   Flipped Coordinates (i.e make a horizontal barchart not a vertical bar chart)
#   hline: Add a blue line to the chart indicating the mean value. 
#   Bars should be ordered from largest mean carbon footprint to smallest 
#     (i.e. Qatar should be on top and Chile last) 
# Your plot must have proper axes labels and must have proper legend labels! 
# Do not use default labels! 

high_inc_barplot <- ggplot(high_inc, aes(x= avg_carb, y = reorder(name, avg_carb))) +
  geom_col() +
  labs(x = "Mean Carbon footprint per Capita ", y = "Country Name") +
  ggtitle("Avg. Carbon footprint per Capita for High Income Countries") +
  geom_vline(xintercept = mean(high_inc$avg_carb, na.rm = TRUE), color = "blue", linetype = "solid") +
  labs(caption = "Avg. Carbon footprint per capita between the years 1979 and 2019 for 
    countries categorized as High Income by the World Bank")
plot(high_inc_barplot)

# Create a dataframe called `per_country_df` that stores the MEAN carbon footprint 
# per capita and the MEAN of the median household income for each country in the dataset. 
# Note, this question is asking you to create an aggregated view of your dataset 
# per each country (and yes, you will be taking the mean of a column that stores
# a median value). Your aggregated dataframe (i.e. per_country_df) should 
# have atleast 3 columns: name, avg_carb, avg_household_inc


per_country_df <- aggregate(df[, c("Carbon_Footprint_percapita", "median_income_perhousehold")], by = list(df$name), FUN = mean, na.rm = TRUE)
colnames(per_country_df) <- c("name", "avg_carb", "avg_household_inc")

# We will also eventually need information on the income group for each country. 
# To add this information to your `per_country_df` dataframe, you'll want to 
# merge/join your `per_country_df` with the earlier `geo_df`. Store your merged 
# results into the variable `per_country_df`. 

per_country_df <- left_join(per_country_df, geo_df, by = "name")
head(per_country_df)
# Now using your `per_country_df`, lets examine the relationship between 
# MEAN carbon foootprint per capita and the MEAN median household income for 
# each country in the dataset. We will also want to examine how this relationship
# is impacted by the income groups of each country. To do this, create a variable 
# called `carb_inc_scatter` that stores a ggplot scatterplot showing the average 
# carbon footprint vs the average household per each country. 
# The scatterplot should have the following characteristics: 
#   x axis & Label: Mean Carbon footprint per Capita 
#   y axis & Label: Avg. Median Household Income (USD $)
#   circle color should be determined based on which of the four income groups the 
#     country belongs to (i.e. "High income", "Upper middle income", 
#     "Lower middle income", or "Low income")
#   Title: "Avg. Carbon footprint per Capita vs Avg. Median Household Income"
#   Caption: "Avg. Carbon footprint per capita versus avg. median household income 
#     from the years 1979 and 2019 for each country. Countries are categorized 
#     using the World Bank's income groups." 
# Your plot must have proper axes labels and must have proper legend labels! 
# Do not use default labels! 

carb_inc_scatter <- ggplot(per_country_df, aes(x=avg_carb, y = avg_household_inc, color = income.groups)) + 
  geom_point() +
  ggtitle("Avg. Carbon footprint per Capita vs Avg. Median Household Income") +
  labs(x = "Mean Carbon footprint per Capita", y = "Avg. Median Household Income (USD $)" , 
  caption = "Avg. Carbon footprint per capita versus avg. median household income 
     from the years 1979 and 2019 for each country. Countries are categorized 
     using the World Bank's income groups.")
  plot(carb_inc_scatter)


# Examining Median Household Income --------------------------------------------
#
# In this section, you now create a function to help provide context for some of
# your earlier plots. This function will help you Examine median household 
# income versus life expectancy overtime. 
#
# ------------------------------------------------------------------------------

# Fill out the following function called `le_vs_mhinc_plot` that given a numeric 
# year, returns a scatterplot. The scatterplot your function creates should have 
# the following characteristics: 
#   x axis & Label: Life Expectancy in years 
#   y axis: Median Income per Household 
#   circle size should be proportional to each country's population
#   circle color should be determined based on which of the four regions the 
#     country belongs to 
#   Title of the plot should read "Life Expectancy VS Median Household Income 
#     in <YR>" where <YR> is the year specified by the user. 
# your plot must have proper axes labels and must have proper legend labels! 
# Do not use default labels! 

le_vs_mhinc_plot <- function(yr){
  year_data <- filter(df, Year == yr)
  plot <- ggplot(data = year_data, aes(x = life_expectancy_years, y = median_income_perhousehold, color = four_regions, size = pop_fix)) +
  geom_point() +
  labs(title = paste("Life Expectancy VS Median Household Income  in " , yr), x = "Life Expectancy in years", y = "Median Income per Household", color = "Region", size = "Population") 
  return(plot)
}
plot(le_vs_mhinc_plot(1989)) 

# When you are complete with you code, add your variables to the report.Rmd file
# and follow the instructions to generate your report. 

#To run the test file, uncomment the line below before running source. 
test_file("hw4_test.R")