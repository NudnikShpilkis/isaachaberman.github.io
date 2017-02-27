#habermodel Haberman
#Final Modeling data for 84-362

rm(list=ls(all=TRUE))
###CHANGE FOR YOUR COMPUTER
setwd("C:/Users/admin/OneDrive/Documents/Senior Fall/84-362/Jay Data/r/Modelling and Plotting")

data <- read.csv('C:/Users/admin/OneDrive/Documents/Senior Fall/84-362/Jay Data/r/Modelling and Plotting/mash.csv')

data.cw <- data
data.post <- data[data$postcw == 1,]

rm("data")

##Ulfelder Models

f.orig.cw.log <- formula(cou.a.d.1 ~
                        colbrit + colfrnc + colespn +
                          age.ln +
                          xxxcimrln + 
                          cou.tries5d +
                          cou.glo.t.ln + cou.reg.t.ln +
                          growth.imf.d +
                          polcat2 + polcat3 + polcat7 + 
                          durable.ln +
                          elceleth.d +
                          nld.any.1 +
                          civconc +
                          postcw)

f.type.cw.log <- formula(cou.a.d.1 ~
                        xxxcimrln +                   
                        cou.tries5d +               
                        gwf.party +                   
                        gwf.personal +                
                        gwf.military +              
                        gwf.monarchy +              
                        tenure.ln +                   
                        I(gwf.party * tenure.ln) +                   
                        I(gwf.personal * tenure.ln) +                    
                        I(gwf.military * tenure.ln) +                                      
                        nld.any.1 +                  
                        I(gwf.party * nld.any.1) +                   
                        I(gwf.personal * nld.any.1) +                    
                        I(gwf.military * nld.any.1) +                     
                        growth.imf.s +               
                        I(gwf.party * growth.imf.s) +                   
                        I(gwf.personal * growth.imf.s) +                    
                        I(gwf.military * growth.imf.s) +                    
                        gwf.duration.ln +            
                        I(gwf.party * gwf.duration.ln) +                   
                        I(gwf.personal * gwf.duration.ln) +                    
                        I(gwf.military * gwf.duration.ln) +
                        postcw)  


f.habermodel.cw.log <- formula(cou.a.d.1 ~
                          postcw +
                          xxxcimrln +                  
                          cou.tries5d +                
                          gwf.party +  
                          growth.imf.s
)

#Modeling with Post-Cold War data set

f.orig.post.log <- formula(cou.a.d.1 ~
                           colbrit + colfrnc + colespn +
                           age.ln +
                           xxxcimrln + 
                           cou.tries5d +
                           cou.glo.t.ln + cou.reg.t.ln +
                           growth.imf.d +
                           polcat2 + polcat3 + polcat7 +
                           durable.ln +
                           elceleth.d +
                           nld.any.1 +
                           civconc
)

f.type.post.log <- formula(cou.a.d.1 ~
                           xxxcimrln +                   
                           cou.tries5d +               
                           gwf.party +                   
                           gwf.personal +                
                           gwf.military +              
                           gwf.monarchy +              
                           tenure.ln +                   
                           I(gwf.party * tenure.ln) +                   
                           I(gwf.personal * tenure.ln) +                    
                           I(gwf.military * tenure.ln) +                                      
                           nld.any.1 +                  
                           I(gwf.party * nld.any.1) +                   
                           I(gwf.personal * nld.any.1) +                    
                           I(gwf.military * nld.any.1) +                     
                           growth.imf.s +               
                           I(gwf.party * growth.imf.s) +                   
                           I(gwf.personal * growth.imf.s) +                    
                           I(gwf.military * growth.imf.s) +                    
                           gwf.duration.ln +            
                           I(gwf.party * gwf.duration.ln) +                   
                           I(gwf.personal * gwf.duration.ln) +                    
                           I(gwf.military * gwf.duration.ln)
)  

f.habermodel.post.log <- formula(cou.a.d.1 ~
                            xxxcimrln +                  
                            cou.tries5d +                
                            gwf.party +  
                            growth.imf.s
)

#Ulfelder Cross-validation

# Cross-validation

# Make folds for 10-fold CV that are about as large as the time I've been forecasting,
# with period of obs starting in 1980 to match IMF growth data start
valdat.cw <- subset(data.cw, year >= 1980 & year <= 2014 & is.na(cou.a.d.1) == FALSE)
valdat.post <- subset(data.post, year >= 1980 & year <= 2014 & is.na(cou.a.d.1) == FALSE)
y.cw <- as.factor(valdat.cw$cou.a.d.1)
y.post <- as.factor(valdat.post$cou.a.d.1)
valdat.cw$k <- createFolds(y.cw, k = 10, list = FALSE)
valdat.post$k <- createFolds(y.post, k = 10, list = FALSE)

# Function to generate models & pred probs for each iteration
predit.cw <- function(foldnum) {
  train.cw <- subset(valdat.cw, k != foldnum)
  test.cw <- subset(valdat.cw, k == foldnum)
  
  #ORIG Models
  orig.cw.log <- glm(f.orig.cw.log, family = binomial, data = train.cw, na.action = na.exclude)
  test.cw$orig.cw.log.p <- predict(orig.cw.log, newdata = test.cw, type = "response")
  #TYPE Models
  type.cw.log <- glm(f.type.cw.log, family = binomial, data = train.cw, na.action = na.exclude)
  test.cw$type.cw.log.p <- predict(type.cw.log, newdata = test.cw, type = "response")
  #habermodel Models
  habermodel.cw.log <- glm(f.habermodel.cw.log, family = binomial, data = train.cw, na.action = na.exclude)
  test.cw$habermodel.cw.log.p <- predict(habermodel.cw.log, newdata = test.cw, type = "response")
  
  out.cw <- subset(test.cw, select = c(sftgcode, year, predyr, cou.a.d.1,
                                 orig.cw.log.p,
                                 type.cw.log.p,
                                 habermodel.cw.log.p,
                                 k))
  return(out.cw)
}

test1.cw <- predit.cw(1)
test2.cw <- predit.cw(2)
test3.cw <- predit.cw(3)
test4.cw <- predit.cw(4)
test5.cw <- predit.cw(5)
test6.cw <- predit.cw(6)
test7.cw <- predit.cw(7)
test8.cw <- predit.cw(8)
test9.cw <- predit.cw(9)
test10.cw <- predit.cw(10)
out.cw <- rbind(test1.cw, test2.cw, test3.cw, test4.cw, test5.cw, test6.cw, test7.cw, test8.cw, test9.cw, test10.cw)

predit.post <- function(foldnum) {
  train.post <- subset(valdat.post, k != foldnum)
  test.post <- subset(valdat.post, k == foldnum)
  
  #ORIG Models
  orig.post.log <- glm(f.orig.post.log, family = binomial, data = train.post, na.action = na.exclude)
  test.post$orig.post.log.p <- predict(orig.post.log, newdata = test.post, type = "response")
  #TYPE Models
  type.post.log <- glm(f.type.post.log, family = binomial, data = train.post, na.action = na.exclude)
  test.post$type.post.log.p <- predict(type.post.log, newdata = test.post, type = "response")
  #habermodel Models
  habermodel.post.log <- glm(f.habermodel.post.log, family = binomial, data = train.post, na.action = na.exclude)
  test.post$habermodel.post.log.p <- predict(habermodel.post.log, newdata = test.post, type = "response")
  
  out.post <- subset(test.post, select = c(sftgcode, year, predyr, cou.a.d.1,
                                    orig.post.log.p,
                                    type.post.log.p,
                                    habermodel.post.log.p,
                                    k))
  return(out.post)
}

test1.post <- predit.post(1)
test2.post <- predit.post(2)
test3.post <- predit.post(3)
test4.post <- predit.post(4)
test5.post <- predit.post(5)
test6.post <- predit.post(6)
test7.post <- predit.post(7)
test8.post <- predit.post(8)
test9.post <- predit.post(9)
test10.post <- predit.post(10)
out.post <- rbind(test1.post, test2.post, test3.post, test4.post, test5.post, test6.post, test7.post, test8.post, test9.post, test10.post)

rm("test1.cw",
   "test2.cw",
   "test3.cw",
   "test4.cw",
   "test5.cw",
   "test6.cw",
   "test7.cw",
   "test8.cw",
   "test9.cw",
   "test10.cw",
   "test1.post",
   "test2.post",
   "test3.post",
   "test4.post",
   "test5.post",
   "test6.post",
   "test7.post",
   "test8.post",
   "test9.post",
   "test10.post")

#Ulfelder Accuracy

# Distribution of AUC scores by fold
fun.auc <- function(df.cw, df.post, x) {
  require(verification)
  
  orig.cw.log <- roc.area(df.cw$cou.a.d.1[out.cw$k==x], df.cw$orig.cw.log.p[out.cw$k==x])
  
  type.cw.log <- roc.area(df.cw$cou.a.d.1[out.cw$k==x], df.cw$type.cw.log.p[out.cw$k==x])
  
  habermodel.cw.log <- roc.area(df.cw$cou.a.d.1[out.cw$k==x], df.cw$habermodel.cw.log.p[out.cw$k==x])
  
  orig.post.log <- roc.area(df.post$cou.a.d.1[out.post$k==x], df.post$orig.post.log.p[out.post$k==x])
  
  type.post.log <- roc.area(df.post$cou.a.d.1[out.post$k==x], df.post$type.post.log.p[out.post$k==x])
  
  habermodel.post.log <- roc.area(df.post$cou.a.d.1[out.post$k==x], df.post$habermodel.post.log.p[out.post$k==x])
  
  all <- c(x, 
           orig.cw.log$A, 
           type.cw.log$A,
           habermodel.cw.log$A,
           orig.post.log$A, 
           type.post.log$A,
           habermodel.post.log$A)
  
  names(all) <- c("fold", 
                  "orig.cw.log",
                  "type.cw.log", 
                  "habermodel.cw.log",
                  "orig.post.log",
                  "type.post.log", 
                  "habermodel.post.log")
  return(all)
}

auc1 <- fun.auc(out.cw, out.post, 1)
auc2 <- fun.auc(out.cw, out.post, 2)
auc3 <- fun.auc(out.cw, out.post, 3)
auc4 <- fun.auc(out.cw, out.post, 4)
auc5 <- fun.auc(out.cw, out.post, 5)
auc6 <- fun.auc(out.cw, out.post, 6)
auc7 <- fun.auc(out.cw, out.post, 7)
auc8 <- fun.auc(out.cw, out.post, 8)
auc9 <- fun.auc(out.cw, out.post, 9)
auc10 <- fun.auc(out.cw, out.post, 10)
auctab <- as.data.frame(rbind(auc1, auc2, auc3, auc4, auc5, auc6, auc7, auc8, auc9, auc10))
auctab <- auctab[,-1]
auctab <- rbind(auctab, colMeans(auctab))

rm("auc1", 
   "auc2",
   "auc3",
   "auc4",
   "auc5",
   "auc6",
   "auc7",
   "auc8",
   "auc9",
   "auc10")

## FORECASTING ###
## 

#Redo habermodel model due to lack of data
f.habermodel.post.log <- formula(cou.a.d.1 ~
                                 xxxcimrln +                  
                                 cou.tries5d +  
                                 growth.imf.s
)

train.cw <- subset(data.cw, predyr >= 1981 & predyr < 2015) # IMF growth data data start in 1980
test.cw <- subset(data.cw, predyr == 2015)

train.post <- subset(data.post, predyr >= 1981 & predyr < 2015) # IMF growth data data start in 1980
test.post <- subset(data.post, predyr == 2015)

oldmod <- glm(f.orig.cw.log, data = train.cw, family = binomial, na.action = na.exclude)
habermodelmod <- glm(f.habermodel.post.log, data = train.post, family = binomial, na.action = na.exclude)

test.cw$orig.p <- predict(oldmod, newdata = test.cw, type = "response")
test.post$habermodel.p <- predict(habermodelmod, newdata = test.post, type = "response")

# Function to get forecasts with CIs from data frame
# Generate responses with CIs (where m = model, nd = new data, & ci = c.i. size on 0-1 scale)
ppack <- function(m, nd, ci, idvars) {
  fitobj <- predict(m, newdata = nd, type = "link", se.fit = TRUE)
  ztop <- function(x) {exp(x)/(1 + exp(x))}
  p <- ztop(fitobj$fit)
  lo <- ztop(fitobj$fit - ( qnorm( ci + ((1 - ci)/2) ) * fitobj$se.fit ) )
  hi <- ztop(fitobj$fit + ( qnorm( ci + ((1 - ci)/2) ) * fitobj$se.fit ) )
  labels <- subset(nd, select = idvars)
  pack <- as.data.frame(cbind(labels, p, lo, hi))
  return(pack)
}

test.cw.2015 <- subset(test.cw, predyr == 2015)
test.post.2015 <- subset(test.post, predyr == 2015)

orig.2015 <- ppack(oldmod, test.cw.2015, 0.90, c("country", "sftgcode", "predyr", "cou.a.d.1"))
habermodel.2015 <- ppack(habermodelmod, test.post.2015, 0.90, c("country", "sftgcode", "predyr", "cou.a.d.1"))

orig.predictions <- predict(oldmod, newdata = test.cw.2015, type = "response")
habermodel.predictions <- predict(habermodelmod, newdata = test.post.2015, type = "response")
orig.predictions <- ifelse(orig.predictions >= 0.5, 1, 0)

meld.cw.2015 <- cbind(orig.2015[,1:3],
                   round(cbind(orig.2015$p), 3),
                   round(cbind(orig.2015$lo), 3),
                   round(cbind(orig.2015$hi), 3))
names(meld.cw.2015) <- c("country", "sftgcode", "predyr", "mean", "ci.90.lo", "ci.90.hi")

meld.post.2015 <- cbind(habermodel.2015[,1:3],
                      round(cbind(habermodel.2015$p), 3),
                      round(cbind(habermodel.2015$lo), 3),
                      round(cbind(habermodel.2015$hi), 3))
names(meld.post.2015) <- c("country", "sftgcode", "predyr", "mean", "ci.90.lo", "ci.90.hi")

# Reload data to get back to original labels
meld.cw.2015 <- meld.cw.2015[order(-meld.cw.2015$mean),]
meld.post.2015 <- meld.post.2015[order(-meld.post.2015$mean),]


#Dot Plot CI's Orig 
#meld.cw.2015$country[meld.cw.2015$country=="Congo-Kinshasa"] <- "DRC"
#meld.cw.2015$country[meld.cw.2015$country=="Congo-Brazzaville"] <- "Republic of Congo"
#meld.cw.2015$country[meld.cw.2015$country=="Gambia"] <- "The Gambia"

dotplot.orig.name <- "orig.dotplot.png"

png(file = dotplot.orig.name, width=14, height=18, units='cm', bg='white', res=150)
dotchart2(meld.cw.2015$mean[1:40], labels=meld.cw.2015$country[1:40],
          lines=TRUE, lwd=0.05, lty=3, sort=FALSE, dotsize=1.25, pch=20,
          col="black", cex.labels=0.75, xlim=c(0,0.5) )
dotchart2(meld.cw.2015$ci.90.lo[1:40], labels=meld.cw.2015$country[1:40],
          lines=TRUE, lwd=0.05, lty=3, sort=FALSE, dotsize=1.25, pch=20,
          col="gray", cex.labels=0.75, xlim=c(0,0.5), add = TRUE )
dotchart2(meld.cw.2015$ci.90.hi[1:40], labels=meld.cw.2015$country[1:40],
          lines=TRUE, lwd=0.05, lty=3, sort=FALSE, dotsize=1.25, pch=20,
          col="gray", cex.labels=0.75, xlim=c(0,0.5), add = TRUE )
title(main=list("Risk of Any Coup Attempts in 2015 (Top 40)", cex=1))

dev.off()

#Dot Plot CI's habermodel
#meld.post.2015$country[meld.post.2015$country=="Congo-Kinshasa"] <- "DRC"
#meld.post.2015$country[meld.post.2015$country=="Congo-Brazzaville"] <- "Republic of Congo"
#meld.post.2015$country[meld.post.2015$country=="Gambia"] <- "The Gambia"

dotplot.habermodel.name <- "habermodel.dotplot.png"

png(file = dotplot.habermodel.name, width=14, height=18, units='cm', bg='white', res=150)
dotchart2(meld.post.2015$mean[1:40], labels=meld.post.2015$country[1:40],
          lines=TRUE, lwd=0.05, lty=3, sort=FALSE, dotsize=1.25, pch=20,
          col="black", cex.labels=0.75, xlim=c(0,0.5) )
dotchart2(meld.post.2015$ci.90.lo[1:40], labels=meld.post.2015$country[1:40],
          lines=TRUE, lwd=0.05, lty=3, sort=FALSE, dotsize=1.25, pch=20,
          col="gray", cex.labels=0.75, xlim=c(0,0.5), add = TRUE )
dotchart2(meld.post.2015$ci.90.hi[1:40], labels=meld.post.2015$country[1:40],
          lines=TRUE, lwd=0.05, lty=3, sort=FALSE, dotsize=1.25, pch=20,
          col="gray", cex.labels=0.75, xlim=c(0,0.5), add = TRUE )
title(main=list("Risk of Any Coup Attempts in 2015 (Top 40)", cex=1))

dev.off()


