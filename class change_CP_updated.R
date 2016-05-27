
#installing necessary packages
install.packages("lfactors") # for those who do not have the package
library(lfactors)

#reading in dataset for Radu's computer
unrecorded <- read.csv("C:\\Users\\charlotte_probst\\Dropbox\\Radu\\Unrecorded\\Complete_databaseCSV.csv" , header = TRUE, na.strings = "", stringsAsFactors = FALSE) #empty cells are now listed as NA so they can be managed

attach (unrecorded) #reduces typing  (no need to write unrecorded$ anymore)


# changing the classes of the dataet to be appropriate -------------------------------

chclass <-c(rep( "character", 5), rep ("numeric", 5),"character", rep( "numeric", 17)) # class defining vector

# class changing for all the columns

i<-1 #indexing variable for chclass vector

for (name in names (unrecorded))
  {
    suppressWarnings(class(name) <- chclass[i])
    i <- i+1
  }


#creating categorical variables for numerical and character access to values ---------------------------------

region <- lfactor(as.numeric(sapply(region,switch,"AFR"= 1,"AMR"= 2,"EMR"=3,"EUR"= 4,"SEAR" = 5, "WPR"= 6 )), levels = c(1:6), labels = c("AFR", "AMR", "EMR", "EUR", "SEAR","WPR" ) )

income <- lfactor(as.numeric(sapply(income ,switch,"HIC"= 1,"UMIC"= 2,"LMIC"=3,"LIC"= 4)), levels = c(1:4), labels = c("HIC", "UMIC","LMIC", "LIC"))

mistake <- lfactor(as.numeric(mistake), levels = c(0,1), labels = c("no", "yes"))

unrecorded$a_lunrecorded <- round (a_lunrecorded, digits = 1) # a_lunrecorded: round to one decimal  # x.5 rounded up to x+1 (rule is applied to all rounding to 1 digit)
unrecorded$a_ltotal <- round (a_ltotal, digits = 1)           #a_ltotal: continuous, round to one decimal
unrecorded$a_lrecorded <- round (a_lrecorded, digits = 1)     #a_lrecorded:  continuous, round to one decimal
unrecorded$a_punrecorded <- round (a_punrecorded , digits = 3)   # a_punrecorded: continuous between 0 and 1, round to three digits

suppressWarnings(e_agree <- lfactor(as.numeric(e_agree), levels = c(0,1), labels = c("no", "yes")))

suppressWarnings(unrecorded$e_lunrecorded <- round (as.numeric(e_lunrecorded), digits = 1))    #e_lunrecorded: continuous, round to one digit
suppressWarnings(unrecorded$e_punrecorded <- round (as.numeric(e_punrecorded) , digits = 3))   #e_punrecorded: continuous between 0 and 1, round to three digits



# all fo the e_ variables in this section add up to 1 after rounding --------------------------------------

# index of e_p values: 
# e_phome = frac. home produced alcohol
# e_pborder = frac. baught over the border 
# e_pillegal = frac. illegally produced
# e_psurragte = frac. industrial alcohol not for human consumption
# e_pother = frac. other
# all above values should sum to 1

# handling sums that are less than or greater thean 1
# if the sum is less than 1 other will be increased to fill the vacancy
# if the sum is greater than 1 all values are scaled down the same amount to compensate for the overestimation


values_matrix<- matrix(nrow = 119, ncol = 6 , NA) # vacant matrix where we will store  e_p values for each expert

for (i in 1:length(unrecorded [,1])) # indexing through all of the experts
{
  for (j in 15:19) # indexing through the e_p columns
  {
    values_matrix[i,j-14] <- as.numeric(as.character(unrecorded[i,j]))
  }
}

sum_vector <- c(1:119)

for ( i in 1:119) # indexing through the extracted/converted values matrix
{
  sum_vector[i] <-sum (na.omit(values_matrix[i,]))
}


greater_index <- which (sum_vector > 1) # finding index points where the sum is greater than 1
less_index <- which (sum_vector < 1 && sum_vector != 0 ) # finding index points where the sum is less than 1

if (greater_index [1] != 0) # correcting sums greater than 1
  
{
  
  for (i in greater_index)
  {
    diff_g = sum_matrix [i] - 1
    scale_down = diff_g/6
    
    apply(unrecorded[i,15:19], 1, function(x) x-scale_down)
    
  }
  
}

if (less_index[1] !=0) # correcting sums less than 1
{
  for (i in less_index)
  {
    diff_l = 1-sum_matrix [i]
    
    unrecorded[i,19] <- unrecorded[i,19] + diff_l
    
  }  
  
  
}



# ajustting p_ variables 

for (p_var in names(unrecorded)[20:28])
{
 
  unrecorded[[p_var]]<- lfactor(as.numeric(unrecorded[[p_var]]), levels = c(0,1), labels = c("not acceptable", "acceptable"))
  
}

#	Recalculate e_lunrecorded from e_punrecorded and a_lrecorded --------------------------------

# e_lunrecorded = estimate of L of unrecorded alcohol consumtiom (/adult/capita) -- provided by expert but recalculated to verify

# e_punrecorded = fraction of total alcohol consution that is unrecorded  -- provided by expert
# a_ltotal = CAMH estimate of L of alcohol consumed (/adult/capita) 

# e_lunrecorded = (e_punrecorded)(a_ltotal)

# for this I will add a new column to the datframe called e_lunreocrded_recalc which stores these values

unrecorded$e_lunrecorded_recalc <- e_punrecorded * a_ltotal

#now determining if recalculation agrees with submitted value

print (expert[which (unrecorded$e_lunrecorded_recalc != unrecorded$e_lunrecorded)]) # prints out experts that provided estimates where values diagree



# Regenerate e_agree using a_punrecorded  and e_punrecorded -----------------------------------------------
# expert agrees (e_agree = yes) if e_punrecorded == a_punrecorded

unrecorded$e_agree_reeval <- unrecorded$e_agree

agree.index <- which(unrecorded$a_punrecorded == unrecorded$e_punrecorded )
disagree.index <-  which(unrecorded$a_punrecorded != unrecorded$e_punrecorded )
unrecorded$e_agree_reeval[agree.index] <- "yes"
unrecorded$e_agree_reeval[disagree.index] <- "no"

print (expert[which (unrecorded$e_agree_reeval != unrecorded$e_agree)]) # prints out experts that provided estimates where values diagree


# exporting this new dataframe into an excel sheet

write.csv(unrecorded , file = 'unrecorded.csv')

# Write an R code to replicate Table 2 (Round 1 only) from the draft "Unrecorded Manuscript v8" ------------------------

lab <- c("2010 income level","round", " N Answers Experts ","N countries", " Experts", "SD Experts" , "WHO ","SD WHO", "Difference", "SD Difference" )
table2 <- matrix(nrow = 5 , ncol = 10 , NA  )
colnames(table2)<- lab
table2 <- data.frame(table2)


income_list <- c ("LIC", "LMIC", "UMIC", "HIC", "Total")
non_rep_countries <- unique (unrecorded [ ,c(2,5)])
table2 [,1] <- income_list
table2 [,2]<- c("1")

for (i in 1:4)
  {
     table2[i,3] <-  length (which (income == income_list[i]))
     table2[i,4] <-  length (which (non_rep_countries$income == income_list[i]))
     table2[i,5] <-  mean.default(na.omit(e_punrecorded[which (income == income_list[i])]))
     table2[i,6] <-  sd(e_punrecorded[which (income == income_list[i])], na.rm = TRUE)
     table2[i,7] <-  mean.default(na.omit(a_punrecorded[which (income == income_list[i])]))
     table2[i,8] <-  sd(a_lunrecorded[which (income == income_list[i])], na.rm = TRUE)
     table2[i,9] <-  table2[i,5] - table2[i,7]
     table2[i,10] <-  table2[i,6] - table2[i,8]
     
  }

table2$X.N.Answers.Experts.<- as.numeric(table2$X.N.Answers.Experts.)
table2[5,3] <- sum (table2[1:4,3])
table2[5,4] <- sum (table2[1:4,4])
table2[5,5] <- mean.default(na.omit(e_punrecorded))
table2[5,6] <- sd(e_punrecorded, na.rm = TRUE)
table2[5,7] <- mean.default(na.omit(a_punrecorded))
table2[5,8] <- sd(a_lunrecorded , na.rm =TRUE)
table2[5,9] <-  table2[5,5] - table2[5,7]
table2[5,10] <- table2[5,6] - table2[5,8]

  
# Note: unable to figure out how to label the last 6 columns in table 2 with: "Unrecorded consumption estimates in % of all per capita consumption"



