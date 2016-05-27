#Q1 -----------------------------------------

#a) ---------------------------------------------------------------------------

# in terms of the ball analogy :
# There are 3 different ball types in the bag labelled A,B,C
# There are also hybrid balls that have the labels 2 labels on them A,B, A,c or B,C



total.probes <- 20000
A.probes <- 100
B.probes <- 250
C.probes <- 750

A.B.probes <- 10
B.C.probes <- 10
A.C.probes <- 20

A.probability <- A.probes / total.probes
B.probability <- B.probes / total.probes
C.probability <- C.probes / total.probes

# evaluating if the intersections are significant using the binomial distribution 

A.B <- binom.test(A.probes, total.probes, p= B.probability) # calculating the probability of sucesses seen in A via the model B 
A.B.intersect.predict <- A.B$p.value * total.probes

B.C <- binom.test(B.probes, total.probes, p= C.probability) # calculating the probability of sucesses seen in B via the model C 
B.C.intersect.predict <- B.C$p.value * total.probes

A.C <- binom.test(A.probes, total.probes, p= C.probability) # calculating the probability of sucesses seen in A via the model C 
A.C.intersect.predict <- A.C$p.value * total.probes

A.B.intersect.predict # orinting the number of probes that should intersect beween models at random 
B.C.intersect.predict
A.C.intersect.predict


#b)-----------Who to fire ?--------------------------------------------------------------------------

# I would fire the maker of model b since it has the least probes in common with the other meodels



# Q2-------------------------------------------------------------





