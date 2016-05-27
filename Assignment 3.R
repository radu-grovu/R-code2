# Q1 -----------------------------------------------------------------

install.packages("e1071")
library("e1071")


source("D:\\COMP 364 rep\\comp364\\src\\hucMini.R")  # installing package needed to run nieveBays function
setwd("D:\\COMP 364 rep\\comp364")

dataset.collections <- c("vanvliet")
huc <- huc.load(dataSets = dataset.collections, dataDir = "data")

# Classifier using ER , HER2 Lymphnode Status and Grade

#creating the classifier dataset
clin.rem.na <- subset(huc$vanvliet$clinical, !is.na(event.5))                                           # removing patients with no event.5 information
clin.var <-clin.rem.na[1:450, colnames(clin.rem.na) %in% c("event.5", "er", "her2", "lymph", "grade")]  # selecting out prdictive datasets
clin.var$er<-factor(clin.var$er)                                                                        # turning each unique element in the observed variables into a factor for proper classification
clin.var$her2<-factor(clin.var$her2)
clin.var$lymph <- factor(clin.var$lymph)
clin.var$grade <- factor(clin.var$grade)
clin.var$event.5<-factor(clin.var$event.5)


#creating the testing dataset
clin.test <-clin.rem.na[451:947, colnames(clin.rem.na) %in% c("event.5", "er", "her2", "lymph", "grade")]  # selecting out prdictive datasets
clin.test$er<-factor(clin.test$er)                                                                         # turning each unique element in the observed variables into a factor for proper classification
clin.test$her2<-factor(clin.test$her2)
clin.test$lymph <- factor(clin.test$lymph)
clin.test$grade <- factor(clin.test$grade)
clin.test$event.5<-factor(clin.test$event.5)


# creating the classifier
classifier1 <-   naiveBayes(   
                            event.5 ~ ., 
                            data = clin.var
                          ) 

# Here event.5 is modelled as c( "er", "her", "lymph" and "grade" )

outcome.predict1 <- predict(classifier1, clin.test)

summary.table <-table( 
                       outcome.predict1, 
                       clin.test$event.5, 
                       dnn=list('predicted','actual')
                     )


#determingin the accuracy of the classifier
accuracy.classifier <- (summary.table[1]+ summary.table[2])/ (summary.table[1] + summary.table[2]+ summary.table[3] + summary.table[4])


#Q2----------------------------------------------------------------------------------

# From assignment 2 : creating a function to run a t.test comparing good and bad outcome probe expressions and get the 20 most differentially expressed probes

stat.test<- function ( st.test = t.test,                       #type of statistical test (t.test or wocoxon.test)
                       p.max = 0.001,                 #p value needed for significance
                       expression.probes = huc$vanvliet$exprs,
                       event.5.outcomes = huc$vanvliet$clinical$event.5 # event 5 dataset 
                       
                      ){ 
  
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


              sig.probes <- stat.test()  # getting all probes in order of significance where there is a significant difference between good and bad outcome patients



get.gene.names <- function (significant.probes, # function gets gene names of probes
                            number = 10
                            ) {
  
              gene.names <- c() # vector to store gene neames of differentially expressed genes
              
              for (i in 1:length (significant.probes)) #gets genes neames  for probes 
                
              {
                
                gene.names <- c(gene.names, huc$vanvliet$probe.info$gene.name[which(huc$vanvliet$probe.info$probe.id == significant.probes[i])] )  
                
              }
              
              no.copies <- unique(gene.names) # removes duplicate gene names
              return (no.copies [1:number] ) #prints top 10 genes in orde of significance 
      }

            

            top.twenty.genes <- get.gene.names(sig.probes , number = 20) ; print (top.ten.genes) #printing top ten gene names that are most significantly different between good and bad outcome data sets
            print (top.twenty.genes)

            
# creating the dataset for the classifier

            expr.flip <- as.data.frame(t(huc$vanvliet$exprs)) 
            colnames (expr.flip) <- huc$vanvliet$probe.info$probe.id
            
            expr.select <- expr.flip [1:450, colnames(expr.flip) %in% sig.probes[1:20]] 
            expr.select[,length(expr.select[1,])+1] <- huc$vanvliet$clinical$event.5[1:450]
            colnames (expr.select)[21] <- "event.5"
            
            expr.select <- expr.select[!is.na(expr.select$event.5) , ] # removing NA value for undefined event.5 patients
            
            
#creating testing dataset
            
            expr.select.test <- expr.flip [451:947, colnames(expr.flip) %in% sig.probes[1:20]] 
            expr.select.test[,length(expr.select.test[1,])+1] <- huc$vanvliet$clinical$event.5[451:947]
            colnames (expr.select.test)[21] <- "event.5"
            
            expr.select.test <- expr.select.test[!is.na(expr.select.test$event.5) , ] # removing NA value for undefined event.5 patients
            
# expressing event.5 in both training and testing datasets as a factor
            
            expr.select$event.5<- factor (expr.select$event.5)      
            expr.select.test$event.5<- factor (expr.select.test$event.5) 
            
            
    
# Creating the classifier
            
classifier2<-naiveBayes(   # creating the classifier based on top 20 probes 
                          event.5 ~ ., 
                          data = expr.select
                       ) 
  
  
# Here event.5 is modelled by the top 20 probes
  
  outcome.predict2 <- predict(classifier2, expr.select.test)
  
  summary.table2 <-table( outcome.predict2, 
                         expr.select.test$event.5, 
                         dnn=list('predicted','actual'))
  
  accuracy.classifier2 <- (summary.table2[1]+ summary.table2[2])/ (summary.table2[1] + summary.table2[2]+ summary.table2[3] + summary.table2[4]) #accuracy of the classifier based on the top 20 probes
  print (accuracy.classifier2)
  
# getting gene names of genes associated with 10 most variable probes
  
  accuracy.probe <- list()
  for (i in 1: length(classifier2$tables))
  {
    
    temp.table<- as.data.frame(classifier2$tables[i])
    accuracy.probe$value[i] <- (temp.table[1,1]+ temp.table[2,2])/ (temp.table[1,1] + temp.table[2,1]+ temp.table[1,2] + temp.table[2,2]) #accurac
    accuracy.probe$name[i]<- names (classifier2$tables[i])
  }
  
  si.clas2.probes<- vector() # vector to store ordered probes 
  si.clas2.probes<- accuracy.probe$name[order(accuracy.probe$value, decreasing = FALSE)] # oprdering probes by accuracy values 
  print(si.clas2.probes[1:10])                        #printing lowest accuracy (most variable) probes
  sig.clas2.genes <- get.gene.names (si.clas2.probes) #getting lowest accuracy (most variable) genes 
  print(sig.clas2.genes[1:10])
  
  

#Q3--------------------------------------------------------------------------------------------

  #creating a function to build the 4 other classifiers to be made in this question
  
  ER.HER.Classifier.maker <- function ( er.select ,
                                        her2.select 
                                        
                                      ){
    
    valid.patients <- which(!is.na(huc$vanvliet$clinical$event.5 ) & er.select & her2.select ) #removes patients with no event.5 data and takes those with the desired ER and HER2 dataset values
    
    
    clin <- huc$vanvliet$clinical[valid.patients,]                  # clinical data fro only useable patients
    exprs <- huc$vanvliet$exprs[,valid.patients]                    # probe expression data for only useable patients
    
    
    ##  defining the training and validation datasets.
    
    half <- floor( nrow(clin) / 2 )                                 # defining the training datadet as the first half of the clinical data
    training.clin <- clin[1:half,]
    training.exprs <- exprs[,1:half]
    
    validation.clin <- clin[(half+1):nrow(clin),]                   # defining the validation dataset
    validation.exprs <- exprs[,(half+1):ncol(exprs)]
    
    ## doing the t-test in the training datase
    
    good.outcome.ind <- which(training.clin$event.5 == FALSE)       # seperating datasets into good and bad outcome 
    bad.outcome.ind <- which(training.clin$event.5 == TRUE)
    
    ## the t-test to get top 20 probes                                        
    ttest.res <- list()
    ttest.pvalues <- vector()
    for (i in 1:nrow(training.exprs)) {    ## for each probe ...
      
      good.exprs <- training.exprs[i,][good.outcome.ind]
      bad.exprs <- training.exprs[i,][bad.outcome.ind]
      ttest.res[[i]] <- t.test( good.exprs, bad.exprs, var.equal=TRUE )
      ttest.pvalues[i] <- ttest.res[[i]]$p.value
    }
    
    topProbes <- order(ttest.pvalues, decreasing=FALSE)[1:20]
    
    topProbes.expr <- huc$vanvliet$exprs[topProbes, valid.patients]
    rownames(topProbes.expr) <- huc$vanvliet$probe.info$probe.id[topProbes]
    
    ## get an expression matrix of only the top 20 probes and
    ## only the valid non-NA patients.
    
    
    # creating testing dataset
    tmp <- as.data.frame( t( topProbes.expr ) )
    
    
    # adding event.5 variable (as a factor)
    
    
    tmp$event.5 <- factor( clin$event.5 ) 
    
    classifier<-naiveBayes(   # creating the classifier
      event.5 ~ ., 
      data = tmp) 
    
    top.20.genes<- get.gene.names(rownames(topProbes.expr) , number=20)
    
    data.output <- list (classifier, tmp , top.20.genes )  
    
    return (data.output)  
    
  } 
  
  
  ER.pos.her2.pos <- ER.HER.Classifier.maker (huc$vanvliet$clinical$er, huc$vanvliet$clinical$her2 )      # storing classifier and gene name information
  ER.pos.her2.neg <- ER.HER.Classifier.maker (huc$vanvliet$clinical$er, !huc$vanvliet$clinical$her2 )
  ER.neg.her2.pos <- ER.HER.Classifier.maker (!huc$vanvliet$clinical$er, huc$vanvliet$clinical$her2 )
  ER.neg.her2.neg <- ER.HER.Classifier.maker (!huc$vanvliet$clinical$er, !huc$vanvliet$clinical$her2 )
  
  classifier3 <- ER.pos.her2.pos [[1]]   #labelling the classifiers
  classifier4 <- ER.pos.her2.neg [[1]]
  classifier5 <- ER.neg.her2.pos [[1]]
  classifier6 <- ER.neg.her2.neg [[1]]
  
  dataset3 <- ER.pos.her2.pos [[2]]   #labelling the datasets
  dataset4 <- ER.pos.her2.neg [[2]]
  dataset5 <- ER.neg.her2.pos [[2]]
  dataset6 <- ER.neg.her2.neg [[2]]
  
  genes3 <- ER.pos.her2.pos [[3]]   #labelling the genes
  genes4 <- ER.pos.her2.neg [[3]]
  genes5 <- ER.neg.her2.pos [[3]]
  genes6 <- ER.neg.her2.neg [[3]]
  
  
  # finidng common genes between top 20 sets
  
  tst <- c(unique(genes3 ),unique(genes4),unique(genes5), unique(genes6), unique(top.twenty.genes))
  tst <- tst[!is.na(duplicated(tst))]
  tst[duplicated(tst)]
  
  # From the above test we can see 3 genes repeated between datasets : "CCNB2"  "RRM2"   "KIF20A"
  # All of these genes are common between the top twenty differentially expressed genes list (Question 2) and the most differentially expressed genes between patients who were Er+ and HER2 -
  # meaning that the presence of estrogen receptorsand the absence of human epidermal growth factor receptor are likely to influence whether the patient is good or bad outcome
  # All of the common genes mentionned are involved in cell cycle and mitotic pathways, which makes sense since they are involved with cancer
  
  
  # function to determine the accracy of a classifier
  
  classifier.accuracy <- function (classif, data.set)
  
  {
    
          outcome <- predict(classif, data.set )
          
          summary.t <-table(     outcome, 
                                 data.set$event.5, 
                                 dnn=list('predicted','actual'))
          
          accuracy<- (summary.t[1]+ summary.t[4])/ (summary.t[1] + summary.t[2]+ summary.t[3] + summary.t[4])
          
          return (accuracy)
          
  }
  
  
  # creating the table to summarize classfiers tested aginst mutiple datasets as sepecifed int the question
  
  acc.table <- data.frame()
  
  clas.list <- list (classifier2, classifier3, classifier4, classifier5, classifier6 )
  dat.list <-  list (dataset3, dataset4, dataset5, dataset6 )
  
  for (i in 1:5) 
  { 
    for (j in 1:4) 
    {
      
      acc.table[i,j]<- classifier.accuracy(clas.list[[i]], dat.list[[j]])
      
    } 
    
    
  }
  
  rownames(acc.table) <- c( "classifier2", "classifier3", "classifier4", "classifier5", "classifier6")
  colnames(acc.table)<- c("dataset3", "dataset4", "dataset5", "dataset6")
  
  acc.table


# Accuracies are improved for the datasets which they were constrcuted for since they were trained on part of those datasets and so have factored in conditions common to these datasets.
# When tested on a different dataset factor imortance and trends in the data vary based on the selection so the accuracy as expected goes down.  















