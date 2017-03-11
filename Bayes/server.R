library(shiny)

shinyServer(function(input, output) {
  library(MASS)  
  library(ellipse)  
  library(mvtnorm)
  
  data("iris")
  
  xl = iris[3:5]
  
  n = dim(xl)[2]
  l = dim(xl)[1]
  classes = levels(xl[,n])
  
  # find parametrs mus, sigmas and probability
  L = length(classes)
  mus = matrix(0, nrow=L, ncol=n-1)
  sigmas = matrix(0, nrow=L, ncol=n-1)
  prob = rep(0, times=L)
  for (i in 1:L) {
    cur = factor(classes[i], levels=classes)
    class = xl[xl[,n] == cur,]
    prob[i] = length(class)/l
    for (j in 1:n-1) {
      mus[i,j] = mean(class[,j])
      sigmas[i,j] = sqrt(var(class[,j]))
    }
  }
  
  
  N = function(x, mu, sigma) {
    return (exp(-1/2*(x-mu)^2/sigma^2)/(sqrt((2*pi)^n*(sigma))))
  }
  
  NaiveBayes = function(x) {
    res = rep(0, times = L)
    for (i in 1:L) {
      for (j in 1:(n-1)) {
        res[i] = res[i] + log(N(x[j],mus[i,j],sigmas[i,j]))
      }
      res[i] = res[i] + log(prob[i])
    }
    return(which.max(res))
  }
  
  estimateMu <- function(objects) {
    rows <- dim(objects)[1]
    cols <- dim(objects)[2]
    mu <- matrix(NA, 1, cols)
    for (col in 1:cols)      {
      mu[1, col] = mean(objects[,col])
    }
    return(mu)
  }

  estimateCovarianceMatrix <- function(objects, mu) {
    rows <- dim(objects)[1]
    cols <- dim(objects)[2]
    sigma <- matrix(0, cols, cols)
    for (i in 1:rows)     {
      sigma <- sigma + (t(objects[i,] - mu) %*% (objects[i,] - mu)) / (rows - 1)
    }
    return (sigma)
  }

  estimateFisherCovarianceMatrix <- function(objects1, objects2, mu1, mu2) {     
    rows1 <- dim(objects1)[1]     
    rows2 <- dim(objects2)[1]     
    rows <- rows1 + rows2     
    cols <- dim(objects1)[2]          
    sigma <- matrix(0, cols, cols)          
    for (i in 1:rows1)     {         
      sigma <- sigma + (t(objects1[i,] - mu1) %*% (objects1[i,] - mu1)) / (rows + 2)     
    }          
    for (i in 1:rows2)     {         
      sigma <- sigma + (t(objects2[i,] - mu2) %*% (objects2[i,] - mu2)) / (rows + 2)     
    }          
    return (sigma)
  }
  

  getPlugInDiskriminantCoeffs <- function(mu1, sigma1, mu2, sigma2) {
    ## Line equation: a*x1^2 + b*x1*x2 + c*x2 + d*x1 + e*x2 + f = 0
    invSigma1 <- solve(sigma1)
    invSigma2 <- solve(sigma2)
    f <- log(abs(det(sigma1))) - log(abs(det(sigma2))) + mu1 %*% invSigma1 %*% t(mu1) - mu2 %*% invSigma2 %*% t(mu2)
    alpha <- invSigma1 - invSigma2
    a <- alpha[1, 1]
    b <- 2 * alpha[1, 2]
    c <- alpha[2, 2]
    beta <- invSigma1%*%t(mu1) - invSigma2%*%t(mu2)
    d <- -2 * beta[1, 1]
    e <- -2 * beta[2, 1]
    return (c("x^2" = a, "xy" = b, "y^2" = c, "x" = d, "y" = e, "1" = f)) }


 # observeEvent(input$NaiveMap, {
    output$plot1 <- renderPlot({
      colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
      plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)
      xf <- seq(from=1, to=7, by=0.1)
      yf <- seq(from=0, to=2.5, by=0.1)
      for (i in xf) {
        for (j in yf) {
          p <- c(i, j, 0.0)
          answ <- NaiveBayes(p)
          points(i, j, col=colors[answ], pch=3)
        }
      }
      legend(1,0, c("Setosa", "Versicolor", "Virginica"), pch = c(15,15,15), col = c("red", "green3", "blue"))
    })
 # })
    
    output$plot2 <- renderPlot({
      sigma1_pl <- matrix(as.numeric(unlist(strsplit(input$sigma1_plugin,","))),2,2)
      sigma2_pl <- matrix(as.numeric(unlist(strsplit(input$sigma2_plugin,","))),2,2)
      mu1_pl <- as.numeric(unlist(strsplit(input$mu1_plugin,",")))
      mu2_pl <- as.numeric(unlist(strsplit(input$mu2_plugin,",")))
      ObjectsCountOfEachClass <- input$obj1  
      
      xy1 <- mvrnorm(n=ObjectsCountOfEachClass, mu1_pl, sigma1_pl)
      xy2 <- mvrnorm(n=ObjectsCountOfEachClass, mu2_pl, sigma2_pl)

      xl <- rbind(cbind(xy1, 1), cbind(xy2, 2))

      colors <- c(rgb(0/255, 162/255, 232/255), rgb(0/255, 200/255, 0/255))
      plot(xl[,1], xl[,2], pch = 21, bg = colors[xl[,3]], asp = 1)

      objectsOfFirstClass <- xl[xl[,3] == 1, 1:2]
      objectsOfSecondClass <- xl[xl[,3] == 2, 1:2]
      
      mu1 <- estimateMu(objectsOfFirstClass)
      mu2 <- estimateMu(objectsOfSecondClass)
      sigma1 <- estimateCovarianceMatrix(objectsOfFirstClass, mu1)
      sigma2 <- estimateCovarianceMatrix(objectsOfSecondClass, mu2)
      coeffs <- getPlugInDiskriminantCoeffs(mu1, sigma1, mu2, sigma2)

      for (i in 1:5) {
        lines(ellipse(sigma1, scale = c(i/2, i/2), centre = mu1, level = 0.95, npoints = 1000))
        lines(ellipse(sigma2, scale = c(i/2, i/2), centre = mu2, level = 0.95, npoints = 1000))
      }
      
      x <- y <- seq(-10, 20, len=100)
      z <- outer(x, y, function(x, y) coeffs["x^2"]*x^2 + coeffs["xy"]*x*y + coeffs["y^2"]*y^2 + coeffs["x"]*x + coeffs["y"]*y + coeffs["1"])
      contour(x, y, z, levels=0, drawlabels=FALSE, lwd = 3, col = "red", add = TRUE)

    })
    
    output$plot3 <- renderPlot({
      sigma_LDF <- matrix(as.numeric(unlist(strsplit(input$sigma_LDF,","))),2,2)
      mu1_LDF <- as.numeric(unlist(strsplit(input$mu1_LDF,",")))
      mu2_LDF <- as.numeric(unlist(strsplit(input$mu2_LDF,",")))
      ObjectsCountOfEachClass <- input$obj2
      
      xy1 <- mvrnorm(n=ObjectsCountOfEachClass, mu1_LDF, sigma_LDF)
      xy2 <- mvrnorm(n=ObjectsCountOfEachClass, mu2_LDF, sigma_LDF)
      
      xl <- rbind(cbind(xy1, 1), cbind(xy2, 2))
      
      colors <- c(rgb(0/255, 162/255, 232/255), rgb(0/255, 200/255, 0/255))
      plot(xl[,1], xl[,2], pch = 21, bg = colors[xl[,3]], asp = 1)
      
      objectsOfFirstClass <- xl[xl[,3] == 1, 1:2]
      objectsOfSecondClass <- xl[xl[,3] == 2, 1:2]
      
      mu1 <- estimateMu(objectsOfFirstClass)
      mu2 <- estimateMu(objectsOfSecondClass)
      Sigma <- estimateFisherCovarianceMatrix(objectsOfFirstClass, objectsOfSecondClass, mu1, mu2)  
      ## Получаем коэффициенты ЛДФ 
      inverseSigma <- solve(Sigma) 
      alpha <- inverseSigma %*% t(mu1 - mu2) 
      mu_st <- (mu1 + mu2) / 2 
      beta <- mu_st %*% alpha 
      
      for (i in 1:5) {
        lines(ellipse(sigma1, scale = c(i/2, i/2), centre = mu1, level = 0.95, npoints = 1000))
        lines(ellipse(sigma2, scale = c(i/2, i/2), centre = mu2, level = 0.95, npoints = 1000))
      }
      
      abline(beta / alpha[2,1], -alpha[1,1]/alpha[2,1], col = "red", lwd = 3) 
      
    })
    
    output$plot4 <- renderPlot({
      sigma1_lev <- matrix(as.numeric(unlist(strsplit(input$sigma1_levels,","))),2,2)
      sigma2_lev <- matrix(as.numeric(unlist(strsplit(input$sigma2_levels,","))),2,2)
      mu1_lev <- as.numeric(unlist(strsplit(input$mu1_levels,",")))
      mu2_lev <- as.numeric(unlist(strsplit(input$mu2_levels,",")))
      ObjectsCountOfEachClass <- input$obj3

      xy1 <- mvrnorm(n=ObjectsCountOfEachClass, mu1_lev, sigma1_lev)
      xy2 <- mvrnorm(n=ObjectsCountOfEachClass, mu2_lev, sigma2_lev)

      xl <- rbind(cbind(xy1, 1), cbind(xy2, 2))

      colors <- c(rgb(0/255, 162/255, 232/255), rgb(0/255, 200/255, 0/255))
      plot(xl[,1], xl[,2], pch = 21, bg = colors[xl[,3]], asp = 1)

      objectsOfFirstClass <- xl[xl[,3] == 1, 1:2]
      objectsOfSecondClass <- xl[xl[,3] == 2, 1:2]
      mu1 <- estimateMu(objectsOfFirstClass)
      mu2 <- estimateMu(objectsOfSecondClass)
      sigma1 <- estimateCovarianceMatrix(objectsOfFirstClass, mu1)
      sigma2 <- estimateCovarianceMatrix(objectsOfSecondClass, mu2)

      for (i in 1:5) {
        lines(ellipse(sigma1, scale = c(i/2, i/2), centre = mu1, level = 0.95, npoints = 1000))
        lines(ellipse(sigma2, scale = c(i/2, i/2), centre = mu2, level = 0.95, npoints = 1000))
      }
      getColor <- function(i, mini, maxi){ 
        x <- 255/(maxi - maxi/30) 
        if (i >= maxi - maxi/30) return(255) 
        else  return(x*i) 
      }
      
      for (i in 1:1000) {         
        lines(ellipse(sigma1, scale = c(i/500, i/500), centre = mu1, level = 0.95, npoints = 1000, col = red), col = rgb((255 - getColor(i, 0, 1000))/255, 0/255, 0/255))
        lines(ellipse(sigma2, scale = c(i/500, i/500), centre = mu2, level = 0.95, npoints = 1000, col = red), col = rgb((255 - getColor(i, 0, 1000))/255, 0/255, 0/255))
      }
    })
    
    output$plot3 <- renderPlot({
      sigma_LDF <- matrix(as.numeric(unlist(strsplit(input$sigma_LDF,","))),2,2)
      mu1_LDF <- as.numeric(unlist(strsplit(input$mu1_LDF,",")))
      mu2_LDF <- as.numeric(unlist(strsplit(input$mu2_LDF,",")))
      ObjectsCountOfEachClass <- input$obj2
      
      xy1 <- mvrnorm(n=ObjectsCountOfEachClass, mu1_LDF, sigma_LDF)
      xy2 <- mvrnorm(n=ObjectsCountOfEachClass, mu2_LDF, sigma_LDF)
      
      xl <- rbind(cbind(xy1, 1), cbind(xy2, 2))
      
      colors <- c(rgb(0/255, 162/255, 232/255), rgb(0/255, 200/255, 0/255))
      plot(xl[,1], xl[,2], pch = 21, bg = colors[xl[,3]], asp = 1)
      
      objectsOfFirstClass <- xl[xl[,3] == 1, 1:2]
      objectsOfSecondClass <- xl[xl[,3] == 2, 1:2]
      
      mu1 <- estimateMu(objectsOfFirstClass)
      mu2 <- estimateMu(objectsOfSecondClass)
      Sigma <- estimateFisherCovarianceMatrix(objectsOfFirstClass, objectsOfSecondClass, mu1, mu2)  
      ## Получаем коэффициенты ЛДФ 
      inverseSigma <- solve(Sigma) 
      alpha <- inverseSigma %*% t(mu1 - mu2) 
      mu_st <- (mu1 + mu2) / 2 
      beta <- mu_st %*% alpha 
      
      for (i in 1:5) {
        lines(ellipse(sigma1, scale = c(i/2, i/2), centre = mu1, level = 0.95, npoints = 1000))
        lines(ellipse(sigma2, scale = c(i/2, i/2), centre = mu2, level = 0.95, npoints = 1000))
      }
      
      abline(beta / alpha[2,1], -alpha[1,1]/alpha[2,1], col = "red", lwd = 3) 
      
    })
    
    output$plot5 <- renderPlot({
      sigma1_em <- matrix(as.numeric(unlist(strsplit(input$sigma1_em,","))),2,2)
      sigma2_em <- matrix(as.numeric(unlist(strsplit(input$sigma2_em,","))),2,2)
      mu1_em <- as.numeric(unlist(strsplit(input$mu1_em,",")))
      mu2_em <- as.numeric(unlist(strsplit(input$mu2_em,",")))
      ObjectsCountOfEachClass <- input$obj4
      
      xy1 <- mvrnorm(n=ObjectsCountOfEachClass, mu1_em, sigma1_em)
      xy2 <- mvrnorm(n=ObjectsCountOfEachClass, mu2_em, sigma2_em)
      
      xl <- rbind(cbind(xy1, 1), cbind(xy2, 2))
      
      colors <- c(rgb(0/255, 162/255, 232/255), rgb(0/255, 200/255, 0/255))
      plot(xl[,1], xl[,2], pch = 21, bg = colors[xl[,3]], asp = 1)
      
      eps <- input$eps 
      K <- 2
      N <- nrow(xl)
      M <- ncol(xl)-1
      
      #initial values
      Mu <- matrix(data = xl[sample(1:N, M),1:M],nrow = K, ncol = M)
      W <- rep(1/K, times = K)
      Sigma <- list(diag(M), diag(M))
      
      iter <- 0
      
      while (TRUE) {
        #E-step
        Pxn <- rep(0, times = N)
        for(i in 1:K) {
          for (j in 1:N) {
            z <- xl[j,1:2]
            Pxn[j] <- Pxn[j]+ W[i]*dmvnorm(z,Mu[i,],Sigma[[i]])
          }
        }
        
        Pnk <- matrix(data = 0, nrow = N, ncol= K) #the probability of presence in k-th distribution.
        for(i in 1:K) {
          for (j in 1:N) {
            z <- xl[j,1:2]
            Pnk[j,i] <- W[i]*dmvnorm(z,Mu[i,],Sigma[[i]])/Pxn[j]
          }
        }
        if (iter > 1) loglike_prev <- loglike
        if (iter > 0) loglike <- sum(log(Pxn))
        
        if (iter > 1) if (abs(loglike - loglike_prev) < eps) break
        
        #M-step
        for (i in 1:K) {
          sum1 <- 0
          sum2 <- 0
          
          W[i] <- sum(Pnk[,i])/N
          
          for (j in 1:N) sum1 <- sum1 + Pnk[j,i] %*% xl[j,1:M]
          Mu[i,] <- sum1/sum(Pnk[,i])
          
          for (j in 1:N) sum2 <- sum2 + Pnk[j,i] * t(t(xl[j,1:M] - Mu[i,])) %*% t(xl[j,1:M] - Mu[i,]) 
          # t(t()) - is transposed matrix and t() is initial (just  because t() doesn't work for transposition)
          
          Sigma[[i]] <- sum2/sum(Pnk[,i])
        }
        
        iter <- iter+1
      }
      print(iter)
      output$iterations <- renderPrint({iter})
      
      
      for (i in 1:5) {
        lines(ellipse(Sigma[[1]], scale = c(i/2, i/2), centre = Mu[1,], level = 0.95, npoints = 1000))
        lines(ellipse(Sigma[[2]], scale = c(i/2, i/2), centre = Mu[2,], level = 0.95, npoints = 1000))
      }
      
      lines(ellipse(Sigma[[1]], scale = c(2, 2), centre = Mu[1,], level = 0.95, npoints = 1000), col = "red")
      lines(ellipse(Sigma[[2]], scale = c(2, 2), centre = Mu[2,], level = 0.95, npoints = 1000), col = "red")
      
    })

})
