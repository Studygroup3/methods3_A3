d$Group <- as.numeric(as.character(d$Group))
Groups <- unique(d$Group[d$Study == 1])
SurrogateList1 <- expand.grid(a = Groups, b = Groups)
SurrogateList1 <- subset(SurrogateList1, a != b)
Groups <- unique(d$Group[d$Study == 2])
SurrogateList2 <- expand.grid(a = Groups, b = Groups)
SurrogateList2 <- subset(SurrogateList2, a != b)
Groups <- unique(d$Group[d$Study == 3])
SurrogateList3 <- expand.grid(a = Groups, b = Groups)
SurrogateList3 <- subset(SurrogateList3, a != b)
SurrogateList <- rbind(SurrogateList1, SurrogateList2, SurrogateList3)

#### this is just an example to help you understand how the row numbers work. You can uncomment to try it out, otherwise move straight to the loop 
# subsetting to create a group that corresponds to the first item in the SurrogateList dataset a and b columns
#z1 <- subset(d, Group == SurrogateList$a[1])
# z2 <- subset(d, Group == SurrogateList$b[1])
# removing the extra rows. z1 has 631 rows and z2 has 639. We need both to have 631 (the length of the shortest one). So we cut z2 in a way that it has the same
# number of rows as z1, it has to start from row 632 up until the number of rows z2 has
# z2 <- z2[-(632:639),]
# now the same thing written has a code. this is what is in the for loop
# z2 <- z2[-((nrow(z1)+1):nrow(z2)),]


## creating a loop that iterates through every row of the SurrogateList
for(i in 1:nrow(SurrogateList)){
  
  # creates a subset of the main dataset that corresponds to the ith group ID of the SurrogateList in a column
  x <- subset(d, Group == SurrogateList$a[i])
  
  # creates a subset of the main dataset that corresponds to the ith group ID of the SurrogateList in b column
  
  y <- subset(d, Group == SurrogateList$b[i])
  # creates a new id for the surrogate group
  group <- 800 + i
  
  # looping through every item of the conditions to make sure the surrogate pairs are per condition
  
  for (co in c("Synchronous", "TurnTaking", "SelfPaced", "Conversation")){
    if (co %in% unique(x$condition) & co %in% unique(y$condition)){
      # creates a subset that corresponds to the co-th item of the conditions, one for x and one for y dataset
      z1 <- subset(x, condition == co)
      z2 <- subset(y, condition == co)
    }
    # compares the number of rows of the new datasets. if one is larger than the other, cuts the difference in the number of rows of the larger dataset
    if (nrow(z1)>nrow(z2)){
      z1 <- z1[-((nrow(z2)+1):nrow(z1)),]
    }
    if (nrow(z2)>nrow(z1)){
      z2 <- z2[-((nrow(z1)+1):nrow(z2)),]
    }
    # creates a new dataframe, where the "other" (or HR2) of the z1 dataset is the "other" of the z2 dataset (i.e. another group)
    w1 <- z1 %>% mutate(
      HR2 = z2$HR2,
      Resp2 = z2$Resp2,
      HR2_lead = z2$HR2_lead,
      Resp2_lead = z2$Resp2_lead,
      HR2_change = z2$HR2_change,
      Resp2_change = z2$Resp2_change
    )
    # replaces the group column with the new group surrogate group number
    w1$Group <- group
    # makes the entire dataframe the surrogate, so that when merged with the real one, this column will be the identifier
    w1$Type <- "Surrogate"
    w <- w1
    # if/else statements that evalueate whether d_surrogate dataframe exists. if it does, binds the w dataframe to it. if it doesn't, creates the d_surrogate
    # dataframe. N.B. because of this line, if you run the loop more than once, your dataframe will be doubled. so make sure to d_surrogate <- NULL first, 
    # then run the loop again
    if(exists("d_surrogate")){d_surrogate <- rbind(d_surrogate, w)}else{(d_surrogate <- w)}
  }
}

# We want our real dataframe to have the "Real" identifier
d$Type = "Real"
# now bind the real and the surrogate dataframes. 
dd <- rbind(d, d_surrogate)
# N.B. the output of the for loop is still in wide format. either make it long, then merge to the long real dataframe
# or merge them in the wide shape and make them long afterwards

