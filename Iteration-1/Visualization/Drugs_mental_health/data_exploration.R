library(xlsx)

# percentage of each drug source in different years for different drugs
source_to_obtain_drugs = read.xlsx("aihw-phe-221-drug-types_July-2021.xlsx", sheetName = "S2.5", startRow = 3, endRow = 31, header = TRUE)
#Refine dataframe
#Drug column to store name of the drugs
source_to_obtain_drugs$Drug = "N/A"
#Set up the data
source_to_obtain_drugs[2:6, 6] = "Cannabis"
source_to_obtain_drugs[8:11, 6] = "Ecstasy"
source_to_obtain_drugs[13:17, 6] = "Meth/amphetamines(d) "
source_to_obtain_drugs[19:22, 6] = "Cocaine"
source_to_obtain_drugs[24:28, 6] = "Inhalants"
#Remove unnecessary rows
source_to_obtain_drugs <- source_to_obtain_drugs[-c(1, 7, 12, 18, 23), ]
#Remove special characters from data
source_to_obtain_drugs <- as.data.frame(gsub("[*#]", "", as.matrix(source_to_obtain_drugs))) 

names(source_to_obtain_drugs) <- c('Source', '2010', '2013', '2016', '2019', 'Drug')
rownames(source_to_obtain_drugs) <- 1:nrow(source_to_obtain_drugs)
#Export to file
write.csv(source_to_obtain_drugs, "drugs_source.csv")

#######################################################

#Volume of pure alcohol('000 litres) consumed over the years 
connsumption_of_alcohol = read.xlsx("aihw-phe-221-drug-types_July-2021.xlsx", sheetName = "S2.3", startRow = 6, endRow = 63, colIndex = c(1, 10), header = FALSE)
#Define proper names of columns
names(connsumption_of_alcohol) <- c('Year', 'Alcohol_Volume')

#write to file
write.csv(connsumption_of_alcohol, "alcohol_consumption.csv")

#################################

#Perceived availability of drugs among people
perc_avail = read.xlsx("aihw-phe-221-drug-types_July-2021.xlsx", sheetName = "S2.6", startRow = 4, endRow = 8, header = TRUE, colIndex = c(1:13))
#Remove special characters from data
perc_avail <- as.data.frame(gsub("[[:punct:]]", "", as.matrix(perc_avail)))
#rename year column
names(perc_avail)[names(perc_avail) == "NA."] <- "Year"

#Seperate dataframe for different drug types
perc_avail_powder = perc_avail[, c(1:5)]
perc_avail_powder$DrugType = "Powder"

perc_avail_base = perc_avail[, c(1, 6:9)]
perc_avail_base$DrugType = "Base"
names(perc_avail_base) <- c('Year', 'Very.easy', 'Easy', 'Difficult', 'Very.difficult', 'DrugType')

perc_avail_crystal = perc_avail[, c(1, 10:13)]
perc_avail_crystal$DrugType = "Crystal"
names(perc_avail_crystal) <- c('Year', 'Very.easy', 'Easy', 'Difficult', 'Very.difficult', 'DrugType')

#re-create and combine whole data
perc_avail_data = rbind(perc_avail_base, perc_avail_crystal, perc_avail_powder)
write.csv(perc_avail_data, "drug_availability.csv")

############################3

#Opioid and Benzodiazepines prescriptions dispensed by genric name
combo_drugs_presc = read.xlsx("aihw-phe-221-drug-types_July-2021.xlsx", sheetName = "S2.7a", startRow = 2, endRow = 23, header = TRUE, colIndex = c(1:9))
#Rename columns
names(combo_drugs_presc) <- c('Drug', '2012.13', '2013.14','2014.15','2015.16','2016.17', '2017.18','2018.19','2019.20')

#opioids related data
opioids_data <- combo_drugs_presc[2:11,]
#Benzodiazepines related data
benzodiazepines_data <- combo_drugs_presc[13:21,]

names(benzodiazepines_data) <- c("Drug", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-18", "2018-19", "2019-20")
names(opioids_data) <- c("Drug", "2012-13", "2013-14", "2014-15", "2015-16", "2016-17", "2017-18", "2018-19", "2019-20")
opioids_data[8,"2012-13"] <- 0
#write to file
write.csv(opioids_data, "opioids_prescritpion.csv")
write.csv(benzodiazepines_data, "benzodiazepines_prescription.csv")

#####################################

#Drug use by years
drug_use_combined = read.xlsx("aihw-phe-221-data-by-region_July-2021_2.xlsx", sheetName = "Table 6", startRow = 4, endRow = 9, header = TRUE)
#Remove special characters from data
drug_use_combined <- as.data.frame(gsub("[*#]", "", as.matrix(drug_use_combined)))

#define seperate df for each state
nsw_drug_use = drug_use_combined[, c(1:8)]
vic_drug_use = drug_use_combined[, c(1, 9:15)]
qld_drug_use = drug_use_combined[, c(1, 16:22)]
wa_drug_use = drug_use_combined[, c(1, 23: 29)]
sa_drug_use = drug_use_combined[, c(1, 30: 36)]
tas_drug_use = drug_use_combined[, c(1, 37: 43)]
act_drug_use= drug_use_combined[, c(1, 44: 50)]
nt_drug_use= drug_use_combined[, c(1, 51: 57)]
aus_drug_use = drug_use_combined[, c(1, 57: 63)]

#Rename columns
names(nsw_drug_use) <- c('Drug', '2001', '2004', '2007', '2010', '2013', '2016', '2019')
names(vic_drug_use) <- c('Drug', '2001', '2004', '2007', '2010', '2013', '2016', '2019')
names(qld_drug_use) <- c('Drug', '2001', '2004', '2007', '2010', '2013', '2016', '2019')
names(wa_drug_use) <- c('Drug', '2001', '2004', '2007', '2010', '2013', '2016', '2019')
names(sa_drug_use) <- c('Drug', '2001', '2004', '2007', '2010', '2013', '2016', '2019')
names(tas_drug_use) <- c('Drug', '2001', '2004', '2007', '2010', '2013', '2016', '2019')
names(act_drug_use) <- c('Drug', '2001', '2004', '2007', '2010', '2013', '2016', '2019')
names(nt_drug_use) <- c('Drug', '2001', '2004', '2007', '2010', '2013', '2016', '2019')
names(aus_drug_use) <- c('Drug', '2001', '2004', '2007', '2010', '2013', '2016', '2019')

#Add state name to data
nsw_drug_use$State = 'NSW' 
vic_drug_use$State = 'VIC'
qld_drug_use$State = 'QLD'
wa_drug_use$State = 'WA'
sa_drug_use$State = 'SA'
tas_drug_use$State = 'TAS'
act_drug_use$State = 'ACT'
nt_drug_use$State = 'NT'
aus_drug_use$State = 'ALL AUSTRALIA'

#combine the reframed data
drug_use_combined <- rbind(nsw_drug_use, vic_drug_use, qld_drug_use, 
                           wa_drug_use, sa_drug_use, tas_drug_use,
                           act_drug_use, nt_drug_use, aus_drug_use)

#write to file
write.csv(drug_use_combined, "drug_use_state_data.csv")

#################################

#Missing work due to drug use
#Missed at least one day of work due to illness or injury			
missing_work_drugs = read.xlsx("aihw-phe-221-impacts_July-2021_3.xlsx", sheetName = "S1.27", startRow = 4, endRow = 13,colIndex = c(1, 2:5), header = TRUE)
#Remove special characters from data
missing_work_drugs <- as.data.frame(gsub("#", "", as.matrix(missing_work_drugs)))

#write to file
write.csv(missing_work_drugs, "missing_work_drugs.csv")
