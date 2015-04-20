 #------ parameters ------
 n <- 1000 
 beta0 <- 0.07
 betaB <- 0.1
 betaC <- -0.15
 betaD <- -0.03
 betaE <- 0.9
 #------------------------
 
 #------ initialisation ------
 beta0Hat <- rep(NA, 1000)
 betaBHat <- rep(NA, 1000)
 betaCHat <- rep(NA, 1000)
 betaDHat <- rep(NA, 1000)
 betaEHat <- rep(NA, 1000)
 #----------------------------
 
 #------ simulations ------
 for(i in 1:1000)
 {
   #data generation
   x <- sample(x=c("A","B", "C", "D", "E"), 
               size=n, replace=TRUE, prob=rep(1/5, 5))  #(a)
   linpred <- cbind(1, dummy(x)[, -1]) %*% c(beta0, betaB, betaC, betaD, betaE)  #(b)
   pi <- exp(linpred) / (1 + exp(linpred))  #(c)
   y <- rbinom(n=n, size=1, prob=pi)  #(d)
   data <- data.frame(x=x, y=y)
   
   #fit the logistic model
   mod <- glm(y ~ x, family="binomial", data=data)
   
   #save the estimates
   beta0Hat[i] <- mod$coef[1]
   betaBHat[i] <- mod$coef[2]
   betaCHat[i] <- mod$coef[3]
   betaDHat[i] <- mod$coef[4]
   betaEHat[i] <- mod$coef[5]
 }
 #-------------------------
 
 #------ results ------
 round(c(beta0=mean(beta0Hat), 
         betaB=mean(betaBHat), 
         betaC=mean(betaCHat), 
         betaD=mean(betaDHat), 
         betaE=mean(betaEHat)), 3)
