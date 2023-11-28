library(dplyr)

poverty_level <- read.csv("poverty_level_wages.csv")
wages <- read.csv("wages_by_education.csv")

df <- left_join(wages, poverty_level, by = "year")

#summarise the average wage of different education level
wages_edu <- summarise(df, 
                       less_than_hs = mean(less_than_hs, na.rm = TRUE),
                       high_school = mean(high_school, na.rm = TRUE),
                       some_college = mean(some_college, na.rm = TRUE),
                       bachelors_degree = mean(bachelors_degree, na.rm = TRUE),
                       advanced_degree = mean(advanced_degree, na.rm = TRUE)
                       )

# add a catogrical value indicating that what poverty level are college students in
df <- mutate(df, college_poverty_level = "")

for (i in 1:50) {
  if(df[[i, "some_college"]] <= df[[i, "X75.100._of_poverty_wages"]]){
    df[[i, "college_poverty_level"]] <- "poor"
  }
  else if(df[[i, "some_college"]] >= df[[i, "X75.100._of_poverty_wages"]] &
     df[[i, "some_college"]] <= df[[i, "X100.125._of_poverty_wages"]]){
    df[[i, "college_poverty_level"]] <- "low"
  }
  else if(df[[i, "some_college"]] >= df[[i, "X100.125._of_poverty_wages"]] &
          df[[i, "some_college"]] <= df[[i, "X125.200._of_poverty_wages"]]){
    df[[i, "college_poverty_level"]] <- "medium"
  }
  else if(df[[i, "some_college"]] >= df[[i, "X200.300._of_poverty_wages"]] &
          df[[i, "some_college"]] <= df[[i, "X300.._of_poverty_wages"]]){
    df[[i, "college_poverty_level"]] <- "high"
  }
  else if(df[[i, "some_college"]] >= df[[i, "X300.._of_poverty_wages"]]){
    df[[i, "college_poverty_level"]] <- "billionaire"
  }
}

# add a column indicating the difference between the wage of under high school poeple and college students
df <- mutate(df, diff = some_college - high_school)
