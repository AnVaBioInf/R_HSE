#Скачайте геном E.coli в формате fasta (тут -
#https://www.ncbi.nlm.nih.gov/nuccore/NC_011750.1?report=fasta), 
#прочитайте его методом read.dna из пакета ape. 
library(ape)

echColiDNA = read.dna('sequence.fasta', format = 'fasta', as.matrix  = F, 
                      as.character = T) 

#Получите последовательность генома в виде строки 
#(строковой вектор единичной длины).
seq.Ech.coli = c()
for (i in c(1:length(echColiDNA[[1]]))){
  seq.Ech.coli[i] = echColiDNA[[1]][i]
}
seq.Ech.coli.str = paste(seq.Ech.coli, sep="", collapse="")

#Найдите два самых частых и два самых редких 6-ти мера в геноме бактерии.
six.nucl = substring(seq.Ech.coli.str, 
                     1:(nchar(seq.Ech.coli.str)-5), 
                     6:nchar(seq.Ech.coli.str))

how.often = as.data.frame(table(six.nucl))

summary(how.often)
# из таблицы, мин: 
#cctagg 24
#ctagac 25

# max:
#cgccag 5883
#ctggcg 5666