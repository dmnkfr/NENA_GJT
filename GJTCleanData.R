###############################################################################
#####              This script reads, tidies, and organizes               #####
#####       the output of the online GRAMMATICALITY JUDGMENT TASK         #####
#####                 and writes the result into the WD                   #####
###############################################################################
if(!require(tidyverse)){install.packages('tidyverse')}
library(tidyverse)

# count the number of files in folder = number of iterations of loop
NumberPP = sapply("./GJT",function(dir)
{length(list.files(dir,pattern='csv'))})      

# list the files in the relevant folder
files = dir(path = "./GJT", pattern="*.csv")

# create an ampty dataframe for the final data
GJT <- data.frame() 

# For testing only
# i = sample(1:35, 1)
# i = 1
for (i in 1:length(files)) {
        # read file
        data = read.csv(file=(paste("./GJT/", files[i], sep="")),skip=45, header=F,as.is=TRUE,dec=",")
        # select and rename relevant sections
        data = data %>% select(V1, V2, V3, V8, V11, V127, V128, V129) %>% 
                dplyr::rename(Condition = V1,
                              Item = V2,
                              Sentence = V3,
                              Grammaticality = V8,
                              Trigger = V11,
                              Key = V127,
                              Accuracy = V128,
                              ReactionTime = V129)
        # change to comprehensive factor levels based on Trigger numbers
        # begin with Condition (= sub experiments)
        data$Condition[data$Condition == "130" | data$Condition == "140"] <- "Prediction"
        data$Condition[data$Condition == "150"] <- "SimpleWordOrder"
        data$Condition[data$Condition == "160"] <- "ComplexWordOrder"
        data$Condition[data$Condition == "170"] <- "SimpleAgreement"
        data$Condition[data$Condition == "180"] <- "ComplexAgreement"
        # change Type of manipulation (coordination and relative clause in SimpleWordOrder;
        # en and ett words in SimpleAgreement; high, medium high, medium low, and low predictability in Prediction)
        data$Type <- ncol(data)
        data$Type[data$Trigger == "131"] <- "High"
        data$Type[data$Trigger == "133"] <- "MedHigh"
        data$Type[data$Trigger == "135"] <- "MedLow"
        data$Type[data$Trigger == "137"] <- "Low"
        data$Type[data$Trigger == "151"] <- "RelativeClause"
        data$Type[data$Trigger == "153"] <- "RelativeClause"
        data$Type[data$Trigger == "155"] <- "Coordination"
        data$Type[data$Trigger == "157"] <- "Coordination"
        data$Type[data$Trigger == "161"] <- NA
        data$Type[data$Trigger == "163"] <- NA
        data$Type[data$Trigger == "171"] <- "En"
        data$Type[data$Trigger == "172"] <- "En"
        data$Type[data$Trigger == "173"] <- "Ett"
        data$Type[data$Trigger == "174"] <- "Ett"
        data$Type[data$Trigger == "181"] <- NA
        data$Type[data$Trigger == "182"] <- NA
        #Change factor levels to comprehensible names
        data$Grammaticality[data$Grammaticality == "inc"] <- "Incorrect"
        data$Grammaticality[data$Grammaticality == "cor"] <- "Correct"
        data$Grammaticality[data$Grammaticality == "INC"] <- "Incorrect"
        data$Grammaticality[data$Grammaticality == "COR"] <- "Correct"
        # make col for subject ID
        data$Subject <- as.character(i)
        # make col for trial nr
        data$Trial <- 1:nrow(data)
        # reorder cols
        data = data %>% relocate(Subject, Trial, Condition, Type, Grammaticality, Item, Sentence, Key, Accuracy, ReactionTime)
        # delete the Trigger col
        data$Trigger <- NULL
        # in some files there's an additional row at the end (nr 441)
        # If this is the case, delete it
        if (nrow(data) > 440) {data = data[1:440,]}
        # create DF with GJT data
        GJT = rbind(GJT, data)
}

# Test if the final DF has the correct number of rows (= n subjects * n trials)
if (nrow(GJT)/nrow(data)==i){print("SUCCESS!")}

# write as text file into WD
write.table(GJT, "GJT.txt",row.names = F)

# and in parent dir
write.table(GJT, "../GJT.txt",row.names = F)
