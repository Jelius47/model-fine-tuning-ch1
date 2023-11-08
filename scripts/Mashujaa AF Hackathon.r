#                             Cleaning the working environment

rm(list = ls())
setwd('E:/FAO')#Setting the working directory 
dir(".")

#                             Data importation


data3 <- read.table('train.tsv',sep = '\t',header = T)






##                      Exploring the columns and the whole data within data3

names(data3)
View(data3)
dim(data3)#the dimensions of our data set includes 15837 rows and 11 columns


###_________________________Data cleaning_____________________________________

summary(data3)

#___________ 1. Finding the missing values within client_id column

missing_val_rows <- data3[data3$client_id == "", ]
print(missing_val_rows)
##  NO MISSING VALUE FOUND



#___________ 2. Finding the missing values within path column

missing_val_rows <- data3[data3$path == "", ]
print(missing_val_rows)
##  NO MISSING VALUE FOUND


#___________ 3. Finding the missing values within sentence column

missing_val_rows <- data3[data3$path == "", ]
print(missing_val_rows)
##  NO MISSING VALUE FOUND


#___________ 4. Finding the missing values within up_votes column


missing_val_rows <- data3[is.na(data3$up_votes), ]
print(missing_val_rows)# ONE WHOLE ROW WAS DISCOVERD HAVING MISSING VALUE

#REMOVING THAT WHOLE ROW 

data3 <- subset(data3, !(down_votes == ""))
View(data3)


#___________ 5. Finding the missing values within down_votes column


missing_val_rows <- data3[is.na(data3$down_votes), ]
print(missing_val_rows)
#NO MISSING VALUE WAS FOUND IN THE COLUNMN



#___________ 6. Finding the missing values within age column

missing_val_rows <- data3[data3$age == "", ]
print(missing_val_rows)

#SO MANY ROWS WERE FOUND HAVING MISSING VALUES
#DEALING WITH THE MISSING VALUE

#Finding the unique values within the age column 

unique_val <- unique(data3$age)
print(unique_val)


# Count the occurrences of unique values in the "ager" column
age_counts <- table(data3$age)

# Print the  age counts
print(age_counts)
#          fifties fourties  sixties    teens thirties twenties 
# 7353      461      498       14       84     1736     5690 


#Assigning variables so as to optimize the missing variables


fifties <- 461
sixties <- 14
teens <- 84
thirties <- 1736
twenties <- 5690
fourties <- 498

missing_age <- 7353



# Calculate proportions
total_count <- fifties + sixties + teens + thirties + twenties + fourties

proportion_fifties <- fifties / total_count

proportion_sixties <- sixties / total_count

proportion_thirties <- thirties / total_count

proportion_teens <- teens / total_count

proportion_twenties <- twenties / total_count

proportion_fouties <- fourties / total_count



# Distribute cells based on proportions

distributed_cells_fifties <- round(proportion_fifties * missing_age)

distributed_cells_sixsties <- round(proportion_sixties * missing_age)

distributed_cells_thirties <- round(proportion_thirties * missing_age)

distributed_cells_teens <- round(proportion_teens * missing_age)

distributed_cells_twenties <- round(proportion_twenties * missing_age)

distributed_cells_fourties <- round(proportion_fouties * missing_age)



# Print the distribution

print(paste("distributed_cells_fifties:", distributed_cells_fifties))

print(paste("distributed_cells_sixsties:", distributed_cells_sixsties))

print(paste("distributed_cells_thirties:", distributed_cells_thirties))

print(paste("distributed_cells_teens:", distributed_cells_teens))

print(paste("distributed_cells_twenties:", distributed_cells_twenties))

print(paste("distributed_cells_fourties:", distributed_cells_fourties))

#Creating a loop to fill the missing values in the column age

fif <- 0
six <- 0
thir <-0
twe <- 0
tee <- 0
four <- 0

for (i in 1:nrow(data3)) {
  if (data3$age[i] == "") {
    if (fif < 400) {
      data3$age[i] <- "fifties"
      fif <- fif + 1
    } else if (six < 12) {
      data3$age[i] <- "sixties"
      six <- six + 1
    } else if (thir< 1505) {
      data3$age[i] <- "thirties"
      thir <- thir + 1
    }
    if (tee < 73) {
      data3$age[i] <- "teens"
      tee <- tee + 1
    } else if (four < 432) {
      data3$age[i] <- "fourties"
      four <- four + 1
    } else if (twe < 4932) {
      data3$age[i] <- "twenties"
      twe<- twe + 1
    }
  }
}



#View(data3)

#___________ 7. Finding the missing values within gender column

missing_val_rows <- data3[data3$gender == "", ]
print(missing_val_rows)

#SO MANY ROWS WERE FOUND HAVING MISSING VALUES
#DEALING WITH THE MISSING VALUE


#Finding the unique values within the gender column 

unique_val <- unique(data3$gender)
print(unique_val)


# Count the occurrences of unique values in the "gender" column
gender_counts <- table(data3$gender)

# Print the  age counts
print(gender_counts)
# " "    female   male  other 
# 7201   5465   3167      3 

# Given counts
count_female <- 5465
count_male <- 3167
count_other <- 3

# Total number of cells to distribute
total_cells_to_distribute <- 7201

# Calculate proportions
total_count <- count_female + count_male + count_other
proportion_female <- count_female / total_count
proportion_male <- count_male / total_count
proportion_other <- count_other / total_count

# Distribute cells based on proportions
distributed_cells_female <- round(proportion_female * total_cells_to_distribute)
distributed_cells_male <- round(proportion_male * total_cells_to_distribute)
distributed_cells_other <- round(proportion_other * total_cells_to_distribute)

# Print the distribution
print(paste("Distributed Cells Female:", distributed_cells_female))
print(paste("Distributed Cells Male:", distributed_cells_male))
print(paste("Distributed Cells Other:", distributed_cells_other))


# Distributed Cells Female: 4557
# Distributed Cells Male: 2641
# Distributed Cells Other: 3

#Creating for loop for filling missing values within the columns 

f <- 0
m <- 0
o <- 0

for (i in 1:nrow(data3)) {
  if (data3$gender[i] == "") {
    if (f < 4557) {
      data3$gender[i] <- "female"
      f <- f + 1
    } else if (m < 2641) {
      data3$gender[i] <- "male"
      m <- m + 1
    } else if (o < 3) {
      data3$gender[i] <- "others"
      o <- o + 1
    }
  }
}

#View(data3)


#___________ 8. Finding the missing values within accents   column


# Find the rows where "up_votes" is equal to the maximum value


missing_val_rows <- data2[data3$accents == "", ]

# Print the rows with the missing values

print(missing_val_rows)


#SO MANY ROWS WERE FOUND HAVING MISSING VALUES
#DEALING WITH THE MISSING VALUE

#Finding the unique values within the age column 

unique_val <- unique(data3$accents)
print(unique_val)


# Count the occurrences of unique values in the "accents" column
accents_counts <- table(data3$accents)

# Print the  age counts
print(accents_counts)


#filling the other 15668 missing rows by depending on mode provided 
#that fluent swahili appeared mostly than other variables

data3$accents[data3$accents==""]<-"Fluent Kiswahili"

#View(data3)



#___________ 9. Finding the missing values within variant   column


# Find the rows where "up_votes" is equal to the maximum value


missing_val_rows <- data2[data3$variant == "", ]

# Print the rows with the missing values

print(missing_val_rows)


#SO MANY ROWS WERE FOUND HAVING MISSING VALUES
#DEALING WITH THE MISSING VALUE

#Finding the unique values within the age column 

unique_val <- unique(data3$variant)
print(unique_val)


# Count the occurrences of unique values in the "variant" column
variant_counts <- table(data3$variant)

# Print the  variant_counts
print(variant_counts)
View(variant_counts)


# Given counts
Kiswahili_cha_Bara_ya_Kenya <- 73
kingwana_DRC <- 30
Kiswahili_cha_Bara_ya_Tanzania <- 262
kiswhil_sanifu_EA <- 152

# Total number of cells to distribute
total_cells_to_distribute <- 15319

# Calculate proportions
total_count <- Kiswahili_cha_Bara_ya_Kenya + kingwana_DRC + Kiswahili_cha_Bara_ya_Tanzania + kiswhil_sanifu_EA


proportion_kenya <- Kiswahili_cha_Bara_ya_Kenya / total_count
proportion_DRC <- kingwana_DRC / total_count
proportion_TZ <- Kiswahili_cha_Bara_ya_Tanzania / total_count
proportion_EA <- kiswhil_sanifu_EA / total_count


# Distribute cells based on proportions
distributed_cells_kenya <- round(proportion_kenya * total_cells_to_distribute)
distributed_cells_DRC <- round(proportion_DRC * total_cells_to_distribute)
distributed_cells_TZ <- round(proportion_TZ * total_cells_to_distribute)
distributed_cells_EA <- round(proportion_EA * total_cells_to_distribute)

# Print the distribution
print(paste("distributed_cells_kenya:", distributed_cells_kenya))
print(paste("distributed_cells_DRC:", distributed_cells_DRC))
print(paste("distributed_cells_TZ:", distributed_cells_TZ))
print(paste("distributed_cells_EA:", distributed_cells_EA))


# distributed_cells_kenya:2163 
# distributed_cells_DRC:889
# distributed_cells_TZ:7763
# distributed_cells_EA:4505


#Creating for loop for filling missing values within the columns 

KNY <- 0
DRC <- 0
TZ <- 0
EA <- 0


for (i in 1:nrow(data3)) {
  if (data3$variant[i] == "") {
    if (KNY < 2163) {
      data3$variant[i] <- "Kiswahili cha Bara ya Kenya"
      KNY <- KNY + 1
    } else if (DRC < 889) {
      data3$variant[i] <- "Kingwana (DRC)"
      DRC <- DRC + 1
    } else if (TZ < 7763) {
      data3$gender[i] <- "Kiswahili cha Bara ya Tanzania"
      TZ <- TZ + 1
    }
    else if (EA < 4504) {
      data3$variant[i] <- "Kiswahili Sanifu (EA)"
      EA <- EA + 1
    }
  }
}




#View(data3)

#___________ 10. Finding the missing values within locale   column


missing_val_rows <- data3[data3$locale == "", ]

print(missing_val_rows)

#finding the unique values within the locale column
unique_classes <- unique(data3$locale)


print(unique_classes)

####   There was two unique classes values sw and " "
count_segment <- sum(!is.na(data3$locale))
print(count_segment)
##THERE WAS NO MISSING VALUES IN THIS COLUMN IT IS ONLY FILLED WITH "sw"



#___________ 11. Finding the missing values within segment   column


# Find the rows where "segment" is equal to the maximum value


missing_val_rows <- data3[data3$segment == "", ]


# Print the rows with the missing values

print(missing_val_rows)


##Finding unique classes
unique_classes <- unique(data3$segment)
print(unique_classes)####   There was only one value in this column 


count_segment <- sum(!is.na(data3$segment))
print(count_segment)
#The column segment contains Na as is it full column



# Export 'data1' to a tab-delimited text file
#write.table(data3, "train_new.tsv", sep = "\t", row.names = FALSE)


#read.table("train_new.tsv",sep = "\t",header = T)

















