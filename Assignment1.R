# Name: Radu Grovu
# Student number: 260517494


#Question 1: -------------------------------------------------------------------------------------------

#i)-----------------------------------------------------------------------------------------------------
x <- 1
y <- 2

#creating a tmporary object to store the value of x so it will not be lost in the switch
tempx <- x 


# switching x and y
x <- y
y <- tempx

#ii)-------------------------------------------------------------------------------------------------

M <- sample( 1:10000, size = sample(1:10000, size = 1) )
# This sample function takes a random sample of numbers without replacemnt form the range of number 1 to 10,000. 
# It sleects the amount of numbers to sample by picking a random number from 1 to 10,000 via a second sample function
# It is not always the same

#iii) -----------------------------------------------------------------------------------------------

largest_num <- 1

for (index_num in 1:length(M))   # indexes through M and finds the largest number putting it in largest_num
  
  {
   if (M[index_num] > largest_num) 
    {
    largest_num <- M[index_num]
    } 
  
  }
print (largest_num)

#iv)--------------------------------------------------------------------------------------------------

findMax <- function( myData ) 
  
{
  
  largest_num <- 1
  for (index_num in length(M)) 
    {
    if (M[index_num] > largest_num) 
      {
      largest_num <- M[index_num]
      } 
    }
  return (largest_num)
}

findMax (M)

#Question 2 ------------------------------------------------------------------------------------------

rw <- 10 # number of rows in Matrix M
cn <- 7  # number of columns in Matrix M

M <- matrix ( 0, nrow = rw , ncol =cn ) #random matrix


 initializeM <- function( matr )  #function that fills top triangle of the input matrix with -1 and the bottom half with +1 sperating them by 0 on the diagonal of the matrix
   {
 
  
  matr[lower.tri(matr)] <- +1
  matr[upper.tri(matr)] <- -1
 
  return (matr)
  
   }

initializeM (M)


#Question 3 -------------------------------------------------------------------------------------

#i) -----------------------------------------------------------------------

vowels <-c ("a", "e", "i", "o", "u")
consonants<- setdiff(letters, vowels) #i)


#ii)-----------------------------------------------------------------------

odd_numbers  <- seq(1,100,2) 

#iii)----------------------------------------------------------------------

birth_day <- c(27,05,94)

#--------------------------------------------------------------------------

mylist = list(consonants, odd_numbers , birth_day)

mylist [c(1, 3)]  #iv)


#Question 4---------------------------------------------------------------------------------------


setwd ("D:\\COMP 364 rep\\comp364\\src") # sorry for the wierd path name im using windows

source('hucMini.R')                     # loading the hucMini file

dataset.collection <- c("miniTCGA", "nki", "vanvliet")

huc <- huc.load (dataset.collection)


name.subset2 <-c()                      # making a vector to be used to store the lowest set of objects
for (i in 1:3)  # print out names of sections and subsections in each dataset

{
  cat("\n")
  name.set <- dataset.collection[i]
  cat(" main set", ":" , name.set, "\n")
  
  for (u in 1:3 )                     #indexing down one level
  {
    
    name.subset1 <- (names(huc[[i]][u]))
    cat (" subset of main set" ,":", name.subset1, "\n")
  
    if (length(names(huc[[i]][[u]]))==0)
    {
    next()
    cat("\n")
    }
   
    for (o in 1: length(names(huc[[i]][[u]])) )    #indexing down a second level
      {
       name.subset2[o] <- (names(huc[[i]][[u]][o]))
    
      }
    cat (" subset of subset" , ":", name.subset2, "\n", "\n")
  }
}
 



#Question 5 ------------------------------------------------------------------------------------

#a)--------------------number of patients in each dataset---------------------------------(dataset.length)--------------------------
dataset.length <- matrix(0, nrow=1 , ncol =4)
dataset.name <- c()

for (a in 1:length(huc) ) 
  {
  
  dataset.name[a] <- dataset.collection[a]
  
  dataset.length [1,a]<-  length(huc[[a]]$clinical$id)
  
  
  }

dataset.length [1,4]<- apply(dataset.length, 1, sum)
colnames(dataset.length) = c(dataset.name, "all")
row.names(dataset.length) = c("number of patients")

#b/d) ----------------------------faction of ER+ patients------------------------(dataset.erpos)-----------------------

dataset.erpos <- matrix(0, nrow=1 , ncol =4)
dataset.name2 <- c()

er.pos.indiv<- c()
samples.indiv <- c()


for (b in 1:length(huc) ) 
{
  
  dataset.name2[b] <- dataset.collection[b]
  
  er.pos<- sum(huc[[b]]$clinical$er, na.rm=TRUE)
  
  frac<- er.pos/length(huc[[b]]$clinical$er)
  
  er.pos.indiv[b] <- sum(huc[[b]]$clinical$er, na.rm=TRUE)
  samples.indiv [b] <- length(huc[[b]]$clinical$er)
  
  
  dataset.erpos [1,b]<-  frac

  
}
tot.er.pos <- sum(er.pos.indiv)
tot.samples <-sum(samples.indiv)

dataset.erpos [1,4]<- tot.er.pos/tot.samples
colnames(dataset.erpos) = c(dataset.name2, "all")
row.names(dataset.erpos) = c("ER+ fraction")





#c) -------------------------ratio of good to poor outcomes-------------------------(dataset.outcome.ratio)---------------------------------------------

dataset.outcome.ratio <- matrix(0, nrow=1 , ncol =4)
dataset.name3 <- c()

good.stor <- c()
poor.stor<-c()

for ( c in 1:length(huc) )
{
  
  dataset.name3[c] <- dataset.collection[c]
  
  poor<- sum(huc[[c]]$clinical$event.5, na.rm=TRUE)
  poor.stor[c] <- sum(huc[[c]]$clinical$event.5, na.rm=TRUE)
 
  good.data <-huc[[c]]$clinical$event.5
  good.na <- good.data[!is.na(good.data)]
  good <- length(good.na)- sum(good.na)
  good.stor[c] <- length(good.na)- sum(good.na)
 
  
   ratio.g.p <- good/poor
  
  
  good.str<- as.character(good)
  poor.str<- as.character(poor)
  ratio.str<- paste (good, ":", poor, sep = "")
  
  dataset.outcome.ratio [1,c]<- ratio.str

}

tot.good<- sum (good.stor)
tot.poor<- sum (poor.stor)

tot.good.str<- as.character(tot.good)
tot.poor.str<- as.character(tot.poor)
  
tot.ratio <- paste(tot.good.str, ";", tot.poor.str)
  
dataset.outcome.ratio [1,4]<- tot.ratio

colnames(dataset.outcome.ratio) = c(dataset.name3, "all")
row.names(dataset.outcome.ratio) = c("good:poor")



#e)----------------------------faction of patients that are HER2+------------------------(dataset.her)-------------------------------------------

tot.pos.counter.2 =0
tot.neg.counter.2 =0
dataset.her <- matrix(0, nrow=1 , ncol =4)
dataset.name6<- c()

for ( e in 1:length(huc) )
{
  pos.counter.2 = 0
  neg.counter.2 = 0
  dataset.name6[e] <- dataset.collection[e]
  
  for (i in  1:length(huc[[e]]$clinical$id))
  {
    
    if (  is.na (huc[[e]]$clinical$her2[i])==TRUE  )
    {
      neg.counter.2 = neg.counter.2 +1
      tot.neg.counter.2 = tot.neg.counter.2 +1
    }
    else if ( huc[[e]]$clinical$her2[i] == TRUE)
    {
      pos.counter.2= pos.counter.2 +1
      tot.pos.counter.2 = tot.pos.counter +1
    }
    
    else
    {
      neg.counter.2 = neg.counter.2 +1
      tot.neg.counter.2 = tot.neg.counter.2 +1
    }
    
    dataset.her[e]<- pos.counter.2/neg.counter.2
    
  }
}
dataset.her [1,4]<- tot.pos.counter.2/(tot.neg.counter.2+tot.pos.counter.2)

colnames(dataset.her) = c(dataset.name6, "all")
row.names(dataset.her) = c("HER2+ : all")



#f ------------------------------faction of patients that are HER2+, ER+---------------(dataset.er.her)--------------------------------------------------------


tot.pos.counter =0
tot.neg.counter =0
dataset.er.her <- matrix(0, nrow=1 , ncol =4)
dataset.name4<- c()

for ( f in 1:length(huc) )
{
  pos.counter = 0
  neg.counter = 0
  dataset.name4[f] <- dataset.collection[f]
  
  for (j in  1:length(huc[[f]]$clinical$id))
  {
       
  if (  is.na(huc[[f]]$clinical$er[i]) == TRUE | is.na (huc[[f]]$clinical$her2[i])==TRUE  )
    {
    neg.counter = neg.counter +1
    tot.neg.counter = tot.neg.counter +1
    }
    else if (huc[[f]]$clinical$er[i]== TRUE && huc[[f]]$clinical$her2[i] == TRUE)
    {
      pos.counter= pos.counter +1
      tot.pos.counter = tot.pos.counter +1
    }
  
  else
    {
    neg.counter = neg.counter +1
    tot.neg.counter = tot.neg.counter +1
    }
  
  dataset.er.her[f]<- pos.counter/neg.counter
  
  }
}
dataset.er.her [1,4]<- tot.pos.counter/(tot.neg.counter+tot.pos.counter)

colnames(dataset.er.her) = c(dataset.name4, "all")
row.names(dataset.er.her) = c("ER+,HER2+ : all")




#g)------------------faction of patients that are HER2-, ER-, lymph +, <50 years old---------------------------(dataset.er.her.ly.50)------------------------------------------------------------

tot.pos.counter.3 =0
tot.neg.counter.3 =0
dataset.er.her.ly.50 <- matrix(0, nrow=1 , ncol =4)
dataset.name7<- c()

for ( g in 1:length(huc) )
{
  pos.counter.3 = 0
  neg.counter.3 = 0
  dataset.name7[g] <- dataset.collection[g]
  
  for (k in  1:length(huc[[g]]$clinical$id))
  {
    
    if (  is.na(huc[[g]]$clinical$er[i]) == TRUE | is.na (huc[[g]]$clinical$her2[i])==TRUE | is.na (huc[[g]]$clinical$lymph[i])==TRUE)
    {
      neg.counter.3 = neg.counter.3 +1
      tot.neg.counter.3 = tot.neg.counter.3 +1
    }
    else if (huc[[g]]$clinical$er[i]== FALSE && huc[[g]]$clinical$her2[i] == FALSE &&  huc[[g]]$clinical$lymph[i] == TRUE && huc[[g]]$clinical$age[i] < 50 )
    {
      pos.counter.3= pos.counter.3 +1
      tot.pos.counter.3 = tot.pos.counter.3 +1
    }
    
    else
    {
      neg.counter.3 = neg.counter.3 +1
      tot.pos.counter.3 = tot.pos.counter.3 +1
    }
    
    dataset.er.her.ly.50[g]<- pos.counter.3/neg.counter.3
    
  }
}
dataset.er.her.ly.50 [1,4]<- tot.pos.counter.3/(tot.neg.counter.3+tot.pos.counter.3)

colnames(dataset.er.her.ly.50) = c(dataset.name7, "all")
row.names(dataset.er.her.ly.50) = c("ER-,HER2-,ly+,<50 : all")



#h) ----------------------------HER2-, ER-, lymph +, <50 years old, no event.5---------------(dataset.er.her.ly.50.ev5)------------------------------------------------------------

tot.pos.counter.4 =0
tot.neg.counter.4 =0
dataset.er.her.ly.50.ev5 <- matrix(0, nrow=1 , ncol =4)
dataset.name8<- c()

for ( h in 1:length(huc) )
{
  pos.counter.4 = 0
  neg.counter.4 = 0
  dataset.name8[h] <- dataset.collection[h]
  
  for (l in  1:length(huc[[h]]$clinical$id))
  {
    
    if (  is.na(huc[[h]]$clinical$er[i]) == TRUE | is.na (huc[[h]]$clinical$her2[i])==TRUE | is.na (huc[[h]]$clinical$lymph[i])==TRUE | is.na (huc[[h]]$clinical$event.5[i])==TRUE )
    {
      neg.counter.4 = neg.counter.4 +1
      tot.neg.counter.4 = tot.neg.counter.4 +1
    }
    else if (huc[[h]]$clinical$er[i]== FALSE && huc[[h]]$clinical$her2[i] == FALSE &&  huc[[h]]$clinical$lymph[i] == TRUE && huc[[h]]$clinical$age[i] < 50 && huc[[h]]$clinical$event.5[i] == FALSE )
    {
      pos.counter.4= pos.counter.4 +1
      tot.pos.counter.4 = tot.pos.counter.4 +1
    }
    
    else
    {
      neg.counter.4 = neg.counter.4 +1
      tot.neg.counter.4 = tot.neg.counter.4 +1
    }
    
    dataset.er.her.ly.50.ev5[g]<- pos.counter.4/neg.counter.4
    
  }
}
dataset.er.her.ly.50.ev5 [1,4]<- tot.pos.counter.4/(tot.neg.counter.4 + tot.pos.counter.4)

colnames(dataset.er.her.ly.50.ev5) = c(dataset.name8, "all")
row.names(dataset.er.her.ly.50.ev5) = c("ER-,HER2-,ly+,<50, ev5- : all")



#combined table --------------------------print final table summarizing all data gathered in Q5------------------------------------------------------------------------

final.table <- rbind(dataset.length, dataset.erpos,dataset.outcome.ratio,dataset.her, dataset.er.her, dataset.er.her.ly.50 , dataset.er.her.ly.50.ev5 )
final.table 

