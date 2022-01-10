
allAspects1 <- sqldf("select a.replication as Rep1,MaxQueueLength as MaxQueue1,b.prec as tiredPr1,c.Prec as finishedPr1
                    from groudWorkoutRep1 as a join howMuchLeftOfTiredness1 as b on a.replication=b.replication join
                    finishedPrec1 as c on a.replication = c.n
                    group by a.replication,MaxQueueLength,b.prec,c.Prec")
allAspects2 <- sqldf("select a.replication as Rep2,MaxQueueLength as MaxQueue2,b.prec as tiredPr2,c.Prec as finishedPr2
                    from groudWorkoutRep2 as a join howMuchLeftOfTiredness2 as b on a.replication=b.replication join
                    finishedPrec2 as c on a.replication = c.n
                    group by a.replication,MaxQueueLength,b.prec,c.Prec")
allOftheAspects <- sqldf("select *
                         from allAspects as a join allAspects1 as b on a.replication=Rep1 join
                         allAspects2 as c on Rep2=a.replication
                         ")
allOftheAspects$Rep1 <- NULL
allOftheAspects$Rep2 <- NULL

pairedTest1 <- t.test(x=allOftheAspects$MaxQueueLength,
                      y=allOftheAspects$MaxQueue1,alternative = "two.sided",
                      paired = TRUE,var.equal = TRUE,conf.level = 0.99) 
print(pairedTest1)

pairedTest2 <- t.test(x=allOftheAspects$MaxQueueLength,
                      y=allOftheAspects$MaxQueue2,alternative = "two.sided",
                      paired = TRUE,var.equal = TRUE,conf.level = 0.99)
print(pairedTest2)


pairedTest3 <- t.test(x=allOftheAspects$MaxQueue1,
                      y=allOftheAspects$MaxQueue2,alternative = "two.sided",
                      paired = TRUE,var.equal = TRUE,conf.level = 0.99)
print(pairedTest3)

pairedTest4 <- t.test(x=allOftheAspects$prec,
                      y=allOftheAspects$tiredPr1,alternative = "two.sided",
                      paired = TRUE,var.equal = TRUE,conf.level = 0.99) 
print(pairedTest4)

pairedTest5 <- t.test(x=allOftheAspects$prec,
                      y=allOftheAspects$tiredPr2,alternative = "two.sided",
                      paired = TRUE,var.equal = TRUE,conf.level = 0.99) 
print(pairedTest5)

pairedTest6 <- t.test(x=allOftheAspects$tiredPr1,
                      y=allOftheAspects$tiredPr2,alternative = "two.sided",
                      paired = TRUE,var.equal = TRUE,conf.level = 0.99) 
print(pairedTest6)

pairedTest7 <- t.test(x=allOftheAspects$finishedPr,
                      y=allOftheAspects$finishedPr1,alternative = "two.sided",
                      paired = TRUE,var.equal = TRUE,conf.level = 0.99) 
print(pairedTest7)

pairedTest8 <- t.test(x=allOftheAspects$finishedPr,
                      y=allOftheAspects$finishedPr2,alternative = "two.sided",
                      paired = TRUE,var.equal = TRUE,conf.level = 0.99) 
print(pairedTest8)

pairedTest9 <- t.test(x=allOftheAspects$finishedPr1,
                      y=allOftheAspects$finishedPr2,alternative = "two.sided",
                      paired = TRUE,var.equal = TRUE,conf.level = 0.99) 
print(pairedTest9)






