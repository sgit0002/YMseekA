setwd('/Users/uditarora/Documents/GitHub/YMseekA/Iteration-1/Visualization/MentalHealthServices')

library(xlsx)
library(dplyr)
mental_health_provider_types = read.xlsx("Medicare-subsidised-mental-health-specific-services-tables.xlsx", sheetName = "Table MBS.27", startRow = 5, endRow = 603, header = TRUE ,as.data.frame = TRUE)

mental_health_provider_sel_col = mental_health_provider_types[, c('Provider.type', 'Sex', 'Age.group', 'X2015.16', 'X2016.17', 'X2017.18', 'X2018.19', 'X2019.20')]
mental_health_age_provider_data = mental_health_provider_sel_col[which(grepl("18", mental_health_provider_sel_col$Age.group)), ] 

mental_health_grouped = aggregate(cbind(as.numeric(mental_health_age_provider_data$X2015.16), as.numeric(mental_health_age_provider_data$X2016.17), as.numeric(mental_health_age_provider_data$X2017.18), as.numeric(mental_health_age_provider_data$X2018.19), as.numeric(mental_health_age_provider_data$X2019.20)), by=list(mental_health_age_provider_data$Provider.type, mental_health_age_provider_data$Sex), sum)

names(mental_health_grouped) <- c('Provider_type', 'Sex', '2015-2016','2016-2017','2017-2018','2018-2019','2019-2020')

write.xlsx(mental_health_grouped, "mental_health_sex_provider_type.xlsx")

####2
service_areas = read.xlsx("Medicare-subsidised-mental-health-specific-services-tables.xlsx", sheetName = "Table MBS.2", startRow = 5, endRow = 142, header = TRUE ,as.data.frame = TRUE)

reduce_service_areas = service_areas[which(grepl("Remoteness", service_areas$Patient.demographics)), ] 

reduce_service_providers_areas = reduce_service_areas[, c('Provider.type', 'Patient.demographic.characteristics', 'Number.of.patients')]

reduce_service_providers_areas$Number.of.patients = round(as.numeric(reduce_service_providers_areas$Number.of.patients))

write.xlsx(reduce_service_providers_areas, "reduce_service_providers_areas.xlsx")


####3
community_mental_health = read.xlsx("Community-mental-health-care-tables-2018-19.xlsx", sheetName = "Table CMHC.28", startRow = 5, endRow = 139, header = TRUE ,as.data.frame = TRUE)

write.xlsx(community_mental_health, "community_service_centers.xlsx")


####4 Table CMHC.2: Community mental health care service contacts, patients and treatment days, states and territories, 2005–06 to 2018–19																	
treatment_days = read.xlsx("Community-mental-health-care-tables-2018-19.xlsx", sheetName = "Table CMHC.2", startRow = 5, endRow = 85, header = TRUE ,as.data.frame = TRUE)
avg_treatment_days_state = treatment_days[which(grepl("treatment", treatment_days$Measure)), ] 

colnames(avg_treatment_days_state)

avg_treatment_days_state_sel_years = avg_treatment_days_state[,c('State.Territory', 'X2013.142.4', 'X2014.15', 'X2015.166', 'X2016.176.7', 'X2017.18', 'X2018.19')]

names(avg_treatment_days_state_sel_years) <- c('State', '2013-14', '2014-15', '2015-16', '2016-17', '2017-18', '2018-19')

avg_treatment_days_state_sel_years[2, 'State'] = "New South Wales"
avg_treatment_days_state_sel_years[3, 'State'] = "Victoria"
avg_treatment_days_state_sel_years[4, 'State'] = "Queensland"
avg_treatment_days_state_sel_years[6, 'State'] = "South Australia"
avg_treatment_days_state_sel_years[7, 'State'] = "Tasmania"


write.xlsx(avg_treatment_days_state_sel_years, "state_wise_treatment_days.xlsx")

#####5 Table KPI.1: Change in mental health consumer's clinical outcomes, states and territories, 2007–08 to 2018–19																	

performance = read.xlsx("Mental-health-service-key-performance-indicators-tables.xlsx", sheetName = "Table KPI.1", startRow = 5, endRow = 868, header = TRUE ,as.data.frame = TRUE)

performance_agegroup = performance[which(grepl("All", performance$Age.group)), ] 

performance_count = performance_agegroup[which(grepl("Per", performance_agegroup$Count)), ]

names(performance_count) <- c('State', 'Consumer Group', 'Age Group', 'Outcome', 'Count', '2007-08', '2008-09','2009-10','2010-11','2011-12','2012-13','2013-14','2014-15','2015-16','2016-17','2017-18','2018-19', 'Average_annual_change')

write.xlsx(performance_count, "mental_services_outcome.xlsx")
