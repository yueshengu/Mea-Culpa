setwd("/Users/Josh/Documents/Spring2016/DataScience/Project5/finalproject-p5-team1/lib/")

#################### Load Libraries ##################

library("RJSONIO")

#################### Prepare Data ####################

professors <- fromJSON("../data/professors.json")
professors <- matrix(unlist(professors), ncol = 5, byrow = TRUE)
colnames(professors) <- c("first_name", "id", "last_name", "middle_name", "nugget")

prof_reviews <- fromJSON("../data/CS_profs.json")
prof_reviews <- matrix(unlist(prof_reviews), ncol = 6, byrow = TRUE)
colnames(prof_reviews) <- c("course_ids", "created", "id", "professor_ids", "review_text", "workload_text")