#Used to make the dummy dataset for the HIA 225 Intro to Python final group project
library(tidyverse)

prim_dx_types <- c("trauma", "PTSD", "sepsis", "pneumonia", "cesarean")
discharge_loc_types <- c("home", "snf", "rehab", "shelter", "hhc", "rf")

ptid <- 1:5000
dob <- sample(seq(as.Date('1934/01/01'), as.Date('2001/01/01'), by="day"), 5000)
prim_dx <- sample(prim_dx_types, 5000, replace=TRUE, prob=c(0.25, 0.075, 0.05, 0.325, 0.3))
discharge_date <- sample(seq(as.Date('2019/01/01'), as.Date('2019/12/31'), by="day"), replace = TRUE, 5000)
discharge_location <- sample(discharge_loc_types, 5000, replace=TRUE, 
                             prob=c(0.5, 0.15, 0.05, 0.05, 0.15, 0.1))

dd <- data.frame(ptid, dob, prim_dx, discharge_date, discharge_location)

dd2 <- dd %>%
  mutate(age_discharge = round(difftime(discharge_date, dob)/365, 0),
         #Changing prim_dx
         prim_dx = case_when(
           prim_dx == "cesarean" & age_discharge > "45" ~ 
             sample(prim_dx_types[1:4], 5000, replace=TRUE, prob=c(0.3, 0.1, 0.25, 0.35)),
           TRUE ~ as.character(prim_dx)),
         #Changing discharge_location
         discharge_location = case_when(
           prim_dx == "cesarean" & (discharge_location == "snf" | discharge_location == "hhc") ~ "home",
           age_discharge < "55" & (discharge_location == "snf" | discharge_location == "hhc") ~ "home",
           TRUE ~ as.character(discharge_location)),
         #Adding readmiss_dx
         readmis_dx = case_when(
           prim_dx == "cesarean" ~ 
             sample(c("sepsis", "infection", "sharm", "dvt", "pe", "overdose", "chestpain", "dehydration", "ams", "other"),
                    5000, replace=TRUE, prob=c(0.2, 0.4, 0.01, 0.02, 0.02, 0.05, 0.05, 0.05, 0.1, 0.1)),
           prim_dx == "trauma" ~ 
             sample(c("sepsis", "infection", "sharm", "dvt", "pe", "fall", "overdose", "chestpain", "dehydration", "ams", "other"),
                    5000, replace=TRUE, prob=c(0.1, 0.3, 0.025, 0.025, 0.125, 0.025, 0.025, 0.25, 0.05, 0.025, 0.05)),
           prim_dx == "PTSD" ~ 
             sample(c("sharm", "hallucinations","overdose", "chestpain", "dehydration", "ams", "other"),
                    5000, replace=TRUE, prob=c(0.2, 0.25, 0.25, 0.1, 0.05, 0.1, 0.05)),
           prim_dx == "sepsis" ~ 
             sample(c("sepsis", "dvt","pe", "fall", "chestpain", "dehydration", "ams", "other"),
                    5000, replace=TRUE, prob=c(0.6, 0.1, 0.05, 0.025, 0.1, 0.05, 0.025, 0.05)),
           prim_dx == "pneumonia" ~ 
             sample(c("sepsis", "fall", "chestpain", "dehydration", "ams", "dvt", "overdose", "other"),
                    5000, replace=TRUE, prob=c(0.32, 0.13, 0.17, 0.19, 0.07, 0.05, 0.04, 0.03))),
         #Adding readmis_date
         readmis_date = discharge_date + round(sample(rbeta(5000,1.5,5) * 100), 0))

write.csv(dd2, "~/Documents/gproj_dummydata.csv", row.names = FALSE)


