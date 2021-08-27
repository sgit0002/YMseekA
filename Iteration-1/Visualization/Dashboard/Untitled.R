my_data = read.csv('Overnight-admitted-mental-health-related-care-tables-1819.csv', header = T, sep = ",", skip = 4)

# Perform the encoding for all the columns

for (i in c(1:18)){
  Encoding(my_data[,i]) <- "UTF-8"
}
# Replace '-' with 0 
my_data[my_data == '-'] <- "0"

# Replace '-' with 0
my_data[my_data == '-'] <- "0"

# Replace "n.p." with 0
my_data[my_data == 'n.p.'] <- "0"

# # Changing the column names
names(my_data)[4] <- "Public Hospitals"
names(my_data)[5] <- "Public Psychiatric Hospitals"
names(my_data)[6] <- "All Public Hospitals (Australia)"
names(my_data)[7] <- "Private Hospitals"
names(my_data)[8] <- "All Hospitals (Public + Private)"


# Cleaning up the names of mental health
my_data$Principal.diagnosis <- gsub("Other organic mental disorders", "Physiological", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Mental and behavioural disorders due to use of alcohol", "Alcohol influenced", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Mental and behavioural disorders due to other psychoactive substance use", "Substance use", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Schizotypal and other delusional disorders", "Schizotypal", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Persistent delusional disorders", "Persistent delusion", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Acute and transient psychotic disorders", "Acute/Trasient Psychosis", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Bipolar affective disorders", "Bipolar disorder", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Recurrent depressive disorders", "Recurrent depression", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Schizoaffective disorders", "Schizoaffective", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Persistent mood (affective) disorders", "Mood disorders", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Other and unspecified mood (affective) disorders", "Unspecified mood dis.", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Phobic anxiety disorders", "Phobic anxiety", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Other anxiety disorders", "Other anxieties", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Obsessive-compulsive disorders", "OCD", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Reaction to severe stress and adjustment disorders", "Stress", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Dissociative (conversion) disorders", "Dissociative disorder", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Somatoform and other neurotic disorders", "Somatoform/other neurotic disorder", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Other behavioural syndromes associated with physiological disturbances and physical factors", "Behavioural syndromes", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Disorders of adult personality and behaviour", "Personality and behaviour disorder", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Disorders of psychological development", "Psychological", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Hyperkinetic disorders", "Hyperkinesis", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Other and unspecified disorders with onset in childhood or adolescence", "Unspecified disorders", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Other factors related to mental and behavioural disorders and substance use", "Other substance use", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Alzheimer's disease", "Alzheimer's", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Other specified mental health-related principal diagnosis", "Other specific", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Alzheimer's disease", "Alzheimer's", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Other and unspecified mood (affective) disorders", "Other mood dis.", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Mental disorder not otherwise specified", "Unspecified Mental dis.", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Depressive episode", "Depression", my_data$Principal.diagnosis)
my_data$Principal.diagnosis <- gsub("Specific personality disorders", "Personality dis.", my_data$Principal.diagnosis)


# Filter the necessary portion of the dataset
my_data <- my_data[1:(nrow(my_data)-7),]

# Check the structure of each column  
# str(my_data)


# Filter out the specialized care
filtered_data <- my_data[my_data$Separation.type=='With specialised psychiatric care',]

# Convert the char to numeric data
filtered_data[,4:18] <- lapply(filtered_data[, 4:18],function(x){as.numeric(gsub(",", "", x))})

# Check for column data type
# str(filtered_data)

# List of diagnosis
separations <- colnames(filtered_data[,4:8])
print(filtered_data)
write.csv(filtered_data, "filtered_data.csv")
write.csv(separations, "separations.csv")

print(filtered_data[, c('Principal.diagnosis', 'Public Hospitals')])

x= read.csv("filtered_data.csv", header = T)
read.csv("separations.csv")$x
