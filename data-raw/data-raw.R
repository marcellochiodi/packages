# data for MLANP package
# install.packages("devtools")
## install.packages("usethis")
library(devtools)
use_data_raw()
library(etasFLP)
library(usethis)
antropometric=read.table("antropometric_Afull.txt", head=TRUE,sep=" ",dec=".")
## ese_antr1 similar to antropometric_Afull.txt
firms=read.table("azienda.txt", head=TRUE,sep=",")
soccer1=read.table("calcio2003.txt", sep=" ",dec=".")
exercise_child1 =read.table("ese_neo1.txt", sep=" ",dec=",",head=TRUE)
exercise_soccer1=read.table("ese_quote2011.txt", sep=" ",dec=".")
exercise1 =read.table("eserc1.txt", sep=";",dec=".",head=TRUE)
exercise2 =read.table("eserc2.txt", sep=";",dec=".",head=TRUE)
exercise.trial1 =read.table("esetrial1.txt", sep=" ",dec=".")
gaussian1_128 =scan("gaussian1-128.txt", sep=",",dec=".")
granprix =read.table("granpremio.csv", sep=";", head = TRUE,dec=",")
    granprix[,6]=granprix$Tempo*1440
    names(granprix)[6]="Minutes"
children1 =read.table("neonati2.csv", sep=";",dec=".",head=TRUE)
children2 =read.table("neonati_dati1.csv", sep=";",dec=".",head=TRUE)
## change kind of data

children.rid =read.table("neonatisel.csv", sep=";",dec=".")
exercise.antr1 =read.table("pub_ridotto.txt", sep=" ",dec=".",head=TRUE)
students.survey =read.table("quest_stud1.txt", sep=" ",dec=".",head=TRUE)
students.test =read.table("TEST_1.csv", sep=";",dec=",",head=TRUE)
#trial =read.table("trial1_rid.csv", sep=";",dec=".",head=TRUE)
exercise.trial2 =read.table("trial2_ese.csv", sep=";",dec=".",header=TRUE)

    
str(students1)
str(students2)
str(children1)
str(children2)
str(exercise_soccer1)
str(esetrial1)
str(gaussian1_128)
str(granprix)

## ese_neo_big1.csv dropped
## ese_neo1.txt mantained as exercise_child1 
usethis::use_data(exercise.trial1,exercise.trial2,students.survey,students.test,exercise.antr1,children1,children.rid,antropometric,firms,soccer1,exercise_child1,exercise_soccer1,exercise1,exercise2,gaussian1_128, granprix, compress = "xz", overwrite = T)



data(exercise.trial1)
str(exercise.trial1)

data(exercise.trial2)## check
str(exercise.trial2)
data(students.survey)## check
str(students.survey)
data(students.teststr(exercise.trial2)
)
str(students.test)
data(exercise.antr1)
str(exercise.antr1)## check
data(children1)
str(children1)
data(children.rid)
str(children.rid)
data(antropometric)
str(antropometric)
data(firms)
str(firms)
data(soccer1)
str(soccer1)
data(exercise_child1) ## check
str(exercise_child1)

data(exercise_soccer1)
str(exercise_soccer1)
data(exercise1)
str(exercise1)
data(exercise2)
str(exercise2)
data(granprix)
str(granprix)









