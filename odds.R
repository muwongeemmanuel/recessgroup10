
pois <- fifa18 %>% 
  group_by(club) %>%                            # multiple group columns
  summarise( sum_overall = sum(overall) )  # multiple summary columns
pois <- as.data.frame(pois)
pois <- tbl_df(pois)
pois$club <- sub("^$", "No Club", pois$club)

#pois$club[pois$club == ""]<-"No Club"
#pois[is.na(pois)] <- "No Club"
#pois$club[which(pois$club == "")] <- "No Club"
#pois$club[which(pois$club == "")] <- "No Club"
#pois <-pois[!(pois$club == ""), ]
#pois <- pois[!is.na(pois$club), ]

split1 <- pois[1:324,]
split2 <- pois[325:648,]

names(split1) <- c("team", "sum_overall")
names(split2) <- c("opponent", "sum_overall")

pois1 <- cbind(split1,split2)
pois2 <- cbind(split2,split1)

 
indx <- sapply(pois1, is.numeric)#check which columns are numeric
nm1 <- which(indx)#get the numeric index of the column
indx2 <- duplicated(names(nm1))#check which among the
# integer columns are duplicated
#use `Map` after splitting the "nm1" with its "names", do the `rowSums`
pois1[ nm1[!indx2]] <- Map(function(x,y) rowSums(x[y]), list(pois1),
                         split(nm1, names(nm1)))
pois1 <- pois1[ -nm1[indx2]]

indx <- sapply(pois2, is.numeric)#check which columns are numeric
nm1 <- which(indx)#get the numeric index of the column
indx2 <- duplicated(names(nm1))#check which among the
# integer columns are duplicated
#use `Map` after splitting the "nm1" with its "names", do the `rowSums`
pois2[ nm1[!indx2]] <- Map(function(x,y) rowSums(x[y]), list(pois2),
                           split(nm1, names(nm1)))
pois2 <- pois2[ -nm1[indx2]]


names(pois2) <- c("team", "sum_overall" , "opponent")

pois1$home <- rep(1,nrow(pois1)) # make new column
pois2$home <- rep(0,nrow(pois2)) # make new column

pois_final <- rbind(pois1,pois2)

#pois_final$sum_overall <- sub( 18649, 4000, pois_final$sum_overall)
pois_final$sum_overall <-  as.numeric(pois_final$sum_overall) / 2900

model <- glm(sum_overall ~ team + opponent + home, family=poisson(link=log), data=pois_final)
summary(model)

#Chelsea
predictHome <- predict(model, data.frame(home=1, team="Inter", opponent="KAA Gent"), type="response")
# 0.9453705 

#for sunderland. note that Home=0.
predictAway <- predict(model, data.frame(home=0, team="KAA Gent", opponent="Inter"), type="response")
# 0.999 

#Away
sum(dskellam(-100:-1, predictHome, predictAway)) #0.3574468
#Home
sum(dskellam(1:100, predictHome, predictAway)) #0.3289164
#Draw
sum(dskellam(0, predictHome, predictAway)) #0.3136368

set.seed(915706074)
nsim <- 10000
homeGoalsSim <- rpois(nsim, predictHome) 
awayGoalsSim <- rpois(nsim, predictAway)
goalDiffSim <- homeGoalsSim - awayGoalsSim
#Home
home <- (sum(goalDiffSim > 0) / nsim) + 0.010 #0.3275
#Draw
draw <- (sum(goalDiffSim == 0) / nsim) + 0.033 # 0.3197
#Away
away <- (sum(goalDiffSim < 0) / nsim) + 0.132 #0.3528
