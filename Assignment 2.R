
# Question 1

#a--------------------------------------------------------------------------=--------------------

# setting up huc

setwd ("D:\\COMP 364 rep\\comp364\\src") # sorry for the wierd path name im using windows

source('hucMini.R')                     # loading the hucMini file

dataset.collection <- c("miniTCGA", "nki", "vanvliet")

huc <- huc.load (dataset.collection)

#------------------------------------------------------------------------------------------------


# creating a function to run a t.test comparing good and bad outcome probe expressions

stat.test<- function ( st.test = t.test,                       #type of statistical test (t.test or wocoxon.test)
                       p.max = 0.001,                 #p value needed for significance
                       expression.probes = huc$vanvliet$exprs,
                       event.5.outcomes = huc$vanvliet$clinical$event.5 # event 5 dataset 
                       
                     )
  {
            #first sorting the probe dtat into good and bad outcome lists
  
            bad.sort <- sapply(event.5.outcomes, "[[", TRUE) # provides a boolean vector of event.5 where TRUE indicates bad outcome (will be used to sort probes)
            good.sort <- !bad.sort                           # provides a boolean vector of event.5 where TRUE indicated good outcome (will also be used to sort probles)
            
            good.data <- list()                              # empty lists to store probe expression values of good and bad datasets
            bad.data <- list()
            
            for (i in 1:length(expression.probes[,1]))      # putting probe expression data into the appropriate lists
                  
                  {
                    good.data[[i]]<- expression.probes[i,][good.sort]
                    bad.data[[i]]<- expression.probes[i,][bad.sort]
                  }
            
            
            names (good.data) <- huc$vanvliet$probe.info$probe.id   #associating probe names with their expression vectors inside each list
            names (bad.data) <- huc$vanvliet$probe.info$probe.id
            
            #----------------determining which probes are differentially expressed using a statistics test (t.test or wicoxon.test) ----------------------------------------------------------------------------
            
      
            stat.test.out <- sapply(names(good.data),function(x) NULL) # stats test list containing results for each probe between good and bad outcome patients
            
            sig.probes <- c()                                          # vector containgin all significant probles 
            
            
            for (i in 1:length(good.data)) #running t-test or wilcoxon between good and bad outcome probes
              
                  { 
                    stat.test.out[[i]]<- st.test(good.data[[i]],bad.data[[i]], var.eq = FALSE)
                  }  
            
            stat.test.out <- stat.test.out[order(sapply(stat.test.out, function(x) x$p.value) , decreasing =FALSE)] # organizes values form most to least significant
              
            for (i in 1:length(good.data))  # getting probes that are below the 0.001 siginifcance level
               
                 {
              
                  if (stat.test.out[[i]]$p.value < p.max)
                    {
                      sig.probes <- c(sig.probes, names(stat.test.out[i]))
                    }
              
                }
            
              
              return(sig.probes) # returning vectors of all of the significantly different probes in order of significance
  
  }


sig.probes <- stat.test() ; print(sig.probes) # priting all probes in order of significance where there is a significant difference between good and bad outcome patients



#b)-----------------------------------------------------------------------------------------------------
get.gene.names <- function (significant.probes, 
                            number = 10
                           ) {

            gene.names <- c() # vector to store gene neames of differentially expressed genes
            
            for (i in 1:length (sig.probes)) #gets genes neames  for probes 
              
                {
              
                gene.names <- c(gene.names, huc$vanvliet$probe.info$gene.name[     which(huc$vanvliet$probe.info$probe.id == sig.probes[i])  ] )  
                
                }
            
            no.copies <- unique(gene.names) # removes duplicate gene names
            return (no.copies [1:number] ) #prints top 10 genes in orde rof significance between good and bad outcome groups
}

top.ten.genes <- get.gene.names(sig.probes) ; print (top.ten.genes) #printing top ten gene names that are most significantly different between good and bad outcome data sets


#c)-----------------------------------rerunning parts 1a,b with wilcoxon test-------------------------------------------------------------------------------

wil.sig.probes <- stat.test(st.test=wilcox.test) 
wil.top.ten.genes <- get.gene.names(wil.sig.probes) ; print (top.ten.genes)

#d) top gene intersect between b and c-----------------------------------------------------------------------------------------------------------------------------

top.gene <- intersect (top.ten.genes, wil.top.ten.genes) # gets intersect between the two ordered gene lists organized by t-test and wilcoxon
print (top.gene[1]) #prints top gene intersect between the two groups 

# The CX3CR1 gene encodes CX3C chemokine receptor 1 which is a transmembrane protein and chemokine involved in the adhesion and migration of leukocytes. 
# Expression of this receptor is associated with lymphocytes and it also plays a major role in the survival of monocytes
# CX3C is also a coreceptor for HIV-1 and some variants may increase suceptability to HIV-1. 

#e  boxplots for top 100 genes ------------------------------------------------------------------------------------------------------------------------------------

t.top.hun <-get.gene.names(sig.probes,100)  #getting the names of the top 100 genes expressed according to both the t.test and wilcoxon
wil.top.hun<-get.gene.names(sig.probes,100)


box.plotter <- function (sig.genes) #makes comparative boxplots of good and bad outcomes
{
  
        # same as in previous function (getting good and bad data sets)
        
        bad.sort <- sapply(huc$vanvliet$clinical$event.5, "[[", TRUE) # provides a boolean vector of event.5 where TRUE indicates bad outcome (will be used to sort probes)
        good.sort <- !bad.sort                           # provides a boolean vector of event.5 where TRUE indicated good outcome (will also be used to sort probles)
        
        good.data <- list()                              # empty lists to store probe expression values of good and bad datasets
        bad.data <- list()
        
        for (i in 1:length(huc$vanvliet$exprs[,1]))      # putting probe expression data into the appropriate lists
          
            {
              good.data[[i]]<- huc$vanvliet$exprs[i,][good.sort]
              bad.data[[i]]<- huc$vanvliet$exprs[i,][bad.sort]
            }  
        
        
        #renaming probes with the correcponding gene names 
        
        names (good.data) <- huc$vanvliet$probe.info$gene.name
        names (bad.data) <- huc$vanvliet$probe.info$gene.name
        
        # boxplots for the top 100 genes according to t-test   
        
            for (i in 1:100) 
              
            {
              good.vector <- unlist( good.data[sig.genes[i]], use.names=FALSE) #vectorizing so boxplot can read input
              bad.vector <- unlist( bad.data[sig.genes[i]], use.names=FALSE)
              boxplot ( good.vector,bad.vector , main = paste("t-test boxplot of good and bad outcomes for ", sig.genes[i]),  names= c("good outcome", "bad outcome") )
              
            }
  
}


box.plotter(t.top.hun) # creating the boxplots for partb
box.plotter(wil.top.hun) #crating boxplots for part c


# rationale behind t-test or wicoxon test ------------------------------------------------

# while the t-test assumes a normal distribution the The Wilcoxon-Mann-Whitney test is  non-parametric and
# does not assume that the dependent variable is a normally distributed 
# For this data set  many of the genes appear to have normal distributions according to the boxplots created above so the t-test can be used 
# to predict significant differences






# Question 2 --------------------------------------------------------------------------------------------------------------------------------





#a)------------------------------------------------------------------------------------------------------------------------------------------

num.spec <- 30000 # number of spectators 
prob.guess = (0.5)^13 #probability of guessing the correct sequence
prob.win = num.spec*prob.guess #number of spectators expected to win
paste ( " expected number of people to win: ", prob.win)

#flips needed so one person would win half the time 

no.win.fifty =  (log(0.5/30000))/(log(0.5))
paste ( " minimum number of flips needed for someone to win half the time : ", no.win.fifty)



#--------------------------ditribution testing function that makes a random dataset and runs the same tests as in part 1a--------------------------------------------------------------------


test.dist <- function (
                      
                      m.test, #mean of random distribution
                      s.d.test, #standard dev o random distribution
                      p.val = 0.001
                      
                      )  {
  
        #creating the random dataset with any input mean and standard dev that resembles huc$vanvliet$expr
          
        
        huc$vanvliet$rand1.expr <- huc$vanvliet$expr #matrix copy of expression matrix for vanvliet
        
        rand.val <- rnorm(length (huc$vanvliet$rand1.expr), mean = m.test, sd = s.d.test ) #creates the dtribution of the distribution N(m.test,sd.test)
        
        for (ind in 1:length (huc$vanvliet$rand1.expr)) # fills in random number of the distribution N(0,3)
            {
              huc$vanvliet$rand1.expr[ind] = rand.val[ind]
            }
        
       
        r.sig.probes <- stat.test(expression.probes = huc$vanvliet$rand1.expr, p.max = p.val)
        #drawing conclusions form comparing the two probe distributions

return ( list (paste("probes found significant in this N(", m.test, s.d.test, ") distribution:",length (r.sig.probes)),huc$vanvliet$rand1.exp )) # prints how many significant probes were found 

  }


#b--------------------------------------------------
temp.random <- test.dist (0,3)
hist (temp.random[[2]], main = "random probe expression", xlab = "probe value") #histogram for part b of question 2



#c----------------------------------------------------
temp.random[[1]]                                               # probes with significant difference between good and bad outcomes in the random normal distribution N(0,3) of probe expression values
paste("probes found significant in 1a:" ,length (sig.probes))  # probes ith significant difference between good and bad outcomes for 1a


# There is a lot less random significance found in the random distribution (~22 vs 2233)
# This indicates that distribution of probes in part 1a is not random and there are in fact significant correlatons between some of the probes and good as well as bad outcomes



#d)-------------re-running part c with new suggested values -----------------------------------------------------------------------

hist (huc$vanvliet$exprs) #orginal data histogram for probe expression in vanvaliet

# suggesting a better normal distribution based ont he mean and s.d of the actual one

suggested.mean <- mean (huc$vanvliet$exprs) #calculating mean and s.d of the original dataset
suggested.sd <- sd(huc$vanvliet$exprs )

suggested.temp <- test.dist(suggested.mean, suggested.sd); print(suggested.temp[[1]]) #prints out significant probes found in suggested distribution
temp.random <- test.dist (0,3) ; print (temp.random[[1]])                             #prints out significant probes found in c) distribution
paste("probes found significant in 1a:" ,length (sig.probes))                         #prints out significant probes found in the original distribution


# There are more probes that are different with the suggested distribution -- likely indicating a better fit. Even so the random distribution does not seam to come close to the number of significant probes found in the original
# This indicateds that at least some of the probes heavily correlate with good and bad outcomes



#e)-----------random event.5--------------------------------------------------------------------------------------

huc$vanvliet$clinical$event.5.random <- huc$vanvliet$clinical$event.5 # making a copy of event 5 data


b <- sample(1:length(huc$vanvliet$clinical$event.5.random), size = 1)   # random number of TRUE values
g <- length(huc$vanvliet$clinical$event.5.random) - b                   # random number of FAlSE values

random.bool <-  (sample(1:length (huc$vanvliet$clinical$event.5.random), size =length(huc$vanvliet$clinical$event.5.random)) <= b ) #creating a random boolean vector for assigning b TRUE values anfd g FALSE values

for (i in 1: length(huc$vanvliet$clinical$event.5.random)) #assigning random T/F values to the event.5 data copy
  
    {
      huc$vanvliet$clinical$event.5.random[i] <- random.bool[i]
    }

#f------------------------------comparing 2b-e to 1a --------------------------------------------------------------------

two.b <- test.dist(0,3) ; print (two.b[[1]])                                                                         # number of significant probes for the distribution in parts 2c/b

two.d <- test.dist(suggested.mean, suggested.sd) ;print (two.d[[1]])                                                 # number of significant probes for the distribution in part 2d


sig.probes.rand.event5 <- stat.test(event.5.outcomes = huc$vanvliet$clinical$event.5.random) ; print(length(sig.probes.rand.event5))    # number of significant probes for the distribution in part 2e

print(length(sig.probes))                                                                                             # number of significant probes for the distribution in part 1a


# The lowest number is seen for situation 2e and highest is for the suggested mean and sd distribution relative to the actual dataset. By using the actual mean and standarsd deviation the most accurate results are seen. 
# Accuracy could be increased by factoring in other information such as HER2 expression and determining their effect on probe significance. Also using a bonferroni corrected p value would likely help decrease the random significance difference between good and bad outcome patients seen in these distributions


#g-----------------------------------------------------------------------------------------------------------------------------

two.g <- test.dist(0,3, p.val = 0.001/length(huc$vanvliet$expr[,1]) )   # to determine the bonferroni corrected significant probes for 2b (note a typo in the question that says 2a which does not ask to tests a distribution)
print (two.g[[1]])                                                      # printing the number of significant probes found in the random distrbution N(0,3)

# the number of probes that are significant are : 0
# this is less than (as expected) the number of probes determined to be significant without the correction : ~20


