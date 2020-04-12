#######################
## Analyzing Results ##
#######################

rm(list = ls())
setwd('/Users/nishantkumar/Documents/ProjectOutput')

## Loading test file
test = read.csv(file = 'test.csv', header = T, sep = ',')

## Loading predictions from chunks
res1 = read.table(file = 'chunk1.txt', header = T)
res2 = read.table(file = 'chunk2.txt', header = T)
res3 = read.table(file = 'chunk3.txt', header = T)
res4 = read.table(file = 'chunk4.txt', header = T)
res5 = read.table(file = 'chunk5.txt', header = T)
res6 = read.table(file = 'chunk6.txt', header = T)
res7 = read.table(file = 'chunk7.txt', header = T)
res8 = read.table(file = 'chunk8.txt', header = T)
res9 = read.table(file = 'chunk9.txt', header = T)
res10 = read.table(file = 'chunk10.txt', header = T)

res11 = read.table(file = 'chunk11.txt', header = T)
res12 = read.table(file = 'chunk12.txt', header = T)
res13 = read.table(file = 'chunk13.txt', header = T)
res14 = read.table(file = 'chunk14.txt', header = T)
res15 = read.table(file = 'chunk15.txt', header = T)
res16 = read.table(file = 'chunk16.txt', header = T)
res17 = read.table(file = 'chunk17.txt', header = T)
res18 = read.table(file = 'chunk18.txt', header = T)
res19 = read.table(file = 'chunk19.txt', header = T)
res20 = read.table(file = 'chunk20.txt', header = T)

## Combine the output files(Chunk 1 thru 20) created out from RCode (Kaggle_RCode1.R, Kaggle_Rcode1_Function.R) ran 1st place. 
## We read 20 millions records in chunks(1 chunk = 1million rows)from the training file and created the model and fit the model to the test file.  

res.tot = data.frame(res1, res2, res3, res4, res5, res6, res7, res8, res9, res10, 
                     res11, res12, res13, res14, res15, res16, res17, res18, res19, res20)

## Now average out, select 
res.tot = apply(res.tot, 1, function(x) ifelse(sum(x == 0) > 10, 0, ifelse(sum(x == 0) == 10, sample(c(0,1), 1, replace = F), 1)))

new.test = data.frame(test, pred = res.tot)

## Store the merged file on disk to analyse the data in deep 
write.table(new.test, file = 'test_pred.csv', row.names = F, sep = ',')
