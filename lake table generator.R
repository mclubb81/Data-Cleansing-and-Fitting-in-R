#This program creates new clean csv tables for the four lakes
setwd("C:/Users/clubb/OneDrive/Desktop/Yahara Lakes New Data 2020")
library(svDialogs)
library(measurements)

#Asks user to enter name of file to alter
myLake <- dlgInput("Enter name of lake file you wish to alter.", Sys.info()["user"])$res # Example:lake kegonsa.csv
myLakeFile <- paste("lake ", myLake, ".csv", sep = "")
#Creating dataframe from entire csv file
lakeDataFrame <- read.csv(myLakeFile)

#Creating dataframe with only the phosphorus data and necessary columns
newLakeDF <- subset(lakeDataFrame, CharacteristicName == "Phosphorus" | CharacteristicName == "Phosphate-phosphorus", c(ActivityStartDate, CharacteristicName, ResultMeasureValue, ResultMeasure.MeasureUnitCode))

#Removing all records with missing values
newLakeDF <- na.omit(newLakeDF)

#Replace records with *123 in front of them in the data with 123
newLakeDF$ResultMeasureValue <- gsub("\\*", "", newLakeDF$ResultMeasureValue)

#Unifying unit measures 
newLakeDF$ResultMeasureValue[newLakeDF$ResultMeasure.MeasureUnitCode == "mg/kg"] <- 0.001 * as.numeric(newLakeDF$ResultMeasureValue[newLakeDF$ResultMeasure.MeasureUnitCode == "mg/kg"])
newLakeDF$ResultMeasure.MeasureUnitCode <- gsub("mg/kg", "mg/l", newLakeDF$ResultMeasure.MeasureUnitCode)

#Removing outliers from the data
outliers <- boxplot(as.numeric(newLakeDF$ResultMeasureValue), main = paste("Lake", myLake, "with outliers") , plot=TRUE)$out
newLakeDF<- newLakeDF[-which(newLakeDF$ResultMeasureValue %in% outliers),]
boxplot(as.numeric(newLakeDF$ResultMeasureValue), main = paste("Lake", myLake, "outliers removed") , plot=TRUE)$out

#Getting only year from date
newLakeDF$ActivityStartDate <- as.Date(newLakeDF$ActivityStartDate)
newLakeDF$ActivityStartDate <- as.numeric(format(newLakeDF$ActivityStartDate,'%Y'))

#Creating new csv with just cleaned phosphorus data shown and asking user to name new file
cleanedLakeFileName <- paste("cleaned ", myLake, ".csv", sep = "")
write.csv(newLakeDF,cleanedLakeFileName)

#Getting Average from each year and placing into new data frame
numYears <- length(1988:2020)
Year <- c(numYears)
Phosphorus_kg <- c(numYears)
for(i in 1:numYears){
  Year[i] <- i+1987
  Phosphorus_kg[i] <- mean(as.numeric(newLakeDF$ResultMeasureValue[newLakeDF$ActivityStartDate == i+1987]))
}

#Replace all NA values in Phosphorus_kg 
for(i in 1:numYears){
  if(is.na(Phosphorus_kg[i]) == TRUE){
    #Handling case first value
    if(i == 1){
      j <- i+1
      #Keep going right until we have a value that is not NA
      while(is.na(Phosphorus_kg[j]) == TRUE){
        j <- j+1
      }
      Phosphorus_kg[i] <- Phosphorus_kg[j]
    }
    
    #Handling case between 
    if(i > 1 && i < numYears){
      #This just replaces NA with mean which is no good for capturing the actual pattern. We need something better.
      #Phosphorus_kg[i] <- mean(Phosphorus_kg, na.rm = TRUE)
      
      #This method will replace the NA with a random number between the years
      m <- i-1
      n <- i+1
      #Move to the left until we have a value that is not NA
      while(is.na(Phosphorus_kg[m]) == TRUE && m > 0){
        m <- m-1
      }
      #Move to the right until we have a value that is not NA
      while(is.na(Phosphorus_kg[n]) == TRUE && n < numYears){
        n <- n+1
      }
      myRange <- c(Phosphorus_kg[m], Phosphorus_kg[n])
      Phosphorus_kg[i] <- runif(1, min(myRange), max(myRange))
    }
    
    #Handling case last value
    if(i == numYears){
      k <- numYears-1
      #Keep going left until we have a value that is not NA
      while(is.na(Phosphorus_kg[k]) == TRUE){
        k <- k-1
      }
      Phosphorus_kg[i] <- Phosphorus_kg[k]
    }
  }
}

#Converting from mg/l to kg
for(i in 1:numYears){
  if(myLake == "mendota"){
    lakeVolume <- conv_unit(5.4E+8, "m3", "l")
    Phosphorus_kg[i] <- conv_unit(Phosphorus_kg[i]*lakeVolume, "mg", "kg")
    
  }else if(myLake == "monona"){
    lakeVolume <- conv_unit(1.3E+8, "m3", "l")
    Phosphorus_kg[i] <- conv_unit(Phosphorus_kg[i]*lakeVolume, "mg", "kg")
    
  }else if(myLake == "waubesa"){
    lakeVolume <- conv_unit(3.95E+7, "m3", "l")
    Phosphorus_kg[i] <- conv_unit(Phosphorus_kg[i]*lakeVolume, "mg", "kg")
    
  }else{ #Lake is kegonsa
    lakeVolume <- conv_unit(6.2E+7, "m3", "l")
    Phosphorus_kg[i] <- conv_unit(Phosphorus_kg[i]*lakeVolume, "mg", "kg")
  }
}

Year_name <- "Year"
Phosphorus_kg_name <- "Phosphorus_kg"
preparedLakeDF <- data.frame(Year, Phosphorus_kg)
names(preparedLakeDF) <- c(Year_name, Phosphorus_kg_name)

#Creating new csv with prepared phosphorus data and asking user to name new file
preparedLakeFileName <- paste("prepared ", myLake, ".csv", sep = "")
write.csv(preparedLakeDF,preparedLakeFileName)


  


