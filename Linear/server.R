library(shiny)
library(kernlab) # Support Vector Machines 
library(pROC)

shinyServer(function(input, output) {
  library(MASS)  
  library(kernlab)
  
  Normalization <- function(xl) {     
    n <- dim(xl)[2] - 1     
    for(i in 1:n)      {         
      xl[,i] <- (xl[,i]-mean(xl[,i]))/sd(xl[,i])      
    }     
    return(xl) 
  }
  
  Prepare <- function(xl) {     
    l <- dim(xl)[1]     
    n <- dim(xl)[2]-1     
    xl <- cbind(xl[,1:n], seq(from = -1, to = -1, length.out = l), xl[,n+1]) 
  } 
  
  lossQuad <- function(x) {     
    return ((x-1)^2) 
  }
  
  lossPerceptron <- function(x) { 
    return (max(-x, 0)) 
  }  
  
  lossLog <- function(x) {     
    return (log2(1 + exp(-x))) 
  }  
  
  sigmoidFunction <- function(z) {     
    return (1 / (1 + exp(-z))) 
  }  
  
  SGradient <- function(xl, eta = 1, lambda = 1/5, id) {
    l <- dim(xl)[1]     
    n <- dim(xl)[2] - 1        
    w <- c(1/2, 1/2, 1/2)     
    iterCount <- 0        
    Q <- 0     
    for (i in 1:l){       
      wx <- sum(w*xl[i,1:n])            
      margin <- wx*xl[i,n+1] 
      if (id == 1) Q <- Q+lossQuad(margin)  
      else if (id == 2) Q <- Q + lossPerceptron(margin)
      else if (id == 3) Q <- Q + lossLog(margin)
    }        
    repeat {         
      margins <- array(dim=l)                  
      for (i in 1:l) {             
        xi <- xl[i,1:n]             
        yi <- xl[i,n+1]                          
        margins[i] <- crossprod(w,xi)*yi          
      }              
      errorIndexes <- which(margins <= 0)              
      if (length(errorIndexes) > 0) {             
        i <- sample(errorIndexes, 1)             
        iterCount <- iterCount+1                           
        xi <- xl[i,1:n]             
        yi <- xl[i,n+1]     
        wx <- sum(w*xi)                         
        margin <- wx*yi      
        if (id == 1) {
          ex <- lossQuad(margin)     
          eta <- 1/sqrt(sum(xi*xi))  
          w <- w-eta*(wx-yi)*xi  
        }
        else if (id == 2) {
          ex <- lossPerceptron(margin)
          eta <- 1/iterCount   
          w <- w+eta*yi*xi 
        }
        else if (id == 3) {
          ex <- lossLog(margin)          
          eta <- 0.3         
          w <- w-eta*xi*yi*sigmoidFunction(wx*yi)  
        }
        print(iterCount)
        Qprev <- Q    
        Q <- (1-lambda)*Q+lambda*ex  
        if (abs(Qprev-Q)/abs(max(Qprev,Q)) < 1e-5) break 
        if (iterCount > 1000) break
      }         
      else break         
    }
    return (w)
  }
  
  output$plot1 <- renderPlot({
    sigma1 <- matrix(as.numeric(unlist(strsplit(input$sigma1_adaline,","))),2,2)
    sigma2 <- matrix(as.numeric(unlist(strsplit(input$sigma2_adaline,","))),2,2)
    mu1 <- as.numeric(unlist(strsplit(input$mu1_adaline,",")))
    mu2 <- as.numeric(unlist(strsplit(input$mu2_adaline,",")))
    ObjectsCountOfEachClass <- input$obj1  
    
    xy1 <- mvrnorm(n=ObjectsCountOfEachClass, mu1, sigma1) 
    xy2 <- mvrnorm(n=ObjectsCountOfEachClass, mu2, sigma2)  
    xl <- rbind(cbind(xy1, -1), cbind(xy2, +1))  
    colors <- c(rgb(155/255, 130/255, 165/255), "white", rgb(55/255, 250/255, 175/255))  
    
    xlNorm <- Normalization(xl) 
    xlNorm <- Prepare(xlNorm) 
    
    ## ADALINE 
    plot(xlNorm[, 1], xlNorm[, 2], pch = 21, bg = colors[xl[,3] + 2], asp = 1, xlab = "x1", ylab = "x2", main = "ADALINE") 
    w <- SGradient(xlNorm, id = 1)  
    abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 3, col = "blue")  
    
    # count errors
    l <- dim(xlNorm)[1]     
    n <- dim(xlNorm)[2] - 1 
    errs = 0
    for (i in 1:l) {
      if (xlNorm[i,n+1] != sign(sum(w*xlNorm[i,1:n]))) {
        errs = errs + 1
      }
    }
    print("error")
    print(errs)
    
  })
  
  output$plot2 <- renderPlot({
    sigma1 <- matrix(as.numeric(unlist(strsplit(input$sigma1_hebb,","))),2,2)
    sigma2 <- matrix(as.numeric(unlist(strsplit(input$sigma2_hebb,","))),2,2)
    mu1 <- as.numeric(unlist(strsplit(input$mu1_hebb,",")))
    mu2 <- as.numeric(unlist(strsplit(input$mu2_hebb,",")))
    ObjectsCountOfEachClass <- input$obj2  
    
    xy1 <- mvrnorm(n=ObjectsCountOfEachClass, mu1, sigma1) 
    xy2 <- mvrnorm(n=ObjectsCountOfEachClass, mu2, sigma2)  
    xl <- rbind(cbind(xy1, -1), cbind(xy2, +1))  
    colors <- c(rgb(155/255, 130/255, 165/255), "white", rgb(55/255, 250/255, 175/255))  
    
    xlNorm <- Normalization(xl) 
    xlNorm <- Prepare(xlNorm) 
    
    ##Hebb
    plot(xlNorm[, 1], xlNorm[, 2], pch = 21, bg = colors[xl[,3] + 2], asp = 1, xlab = "x1", ylab = "x2", main = "Hebb's rule") 
    w <- SGradient(xlNorm, id = 2)     
    abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 3, col = "green3") 
    
    # count errors
    l <- dim(xlNorm)[1]     
    n <- dim(xlNorm)[2] - 1 
    errs = 0
    for (i in 1:l) {
      if (xlNorm[i,n+1] != sign(sum(w*xlNorm[i,1:n]))) {
        errs = errs + 1
      }
    }
    print("error")
    print(errs)
    
  })
  
  output$plot3 <- renderPlot({
    sigma1 <- matrix(as.numeric(unlist(strsplit(input$sigma1_regr,","))),2,2)
    sigma2 <- matrix(as.numeric(unlist(strsplit(input$sigma2_regr,","))),2,2)
    mu1 <- as.numeric(unlist(strsplit(input$mu1_regr,",")))
    mu2 <- as.numeric(unlist(strsplit(input$mu2_regr,",")))
    ObjectsCountOfEachClass <- input$obj3 
    
    xy1 <- mvrnorm(n=ObjectsCountOfEachClass, mu1, sigma1) 
    xy2 <- mvrnorm(n=ObjectsCountOfEachClass, mu2, sigma2)  
    xl <- rbind(cbind(xy1, -1), cbind(xy2, +1))  
    colors <- c(rgb(155/255, 130/255, 165/255), "white", rgb(55/255, 250/255, 175/255))  
    
    xlNorm <- Normalization(xl) 
    xlNorm <- Prepare(xlNorm) 
    
    ##LogRegression
    plot(xlNorm[, 1], xlNorm[, 2], pch = 21, bg = colors[xl[,3] + 2], asp = 1, xlab = "x1", ylab = "x2", main = "Logistic regression") 
    w <- SGradient(xlNorm, id = 3)     
    abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 3, col = "red")
    
    # count errors
    l <- dim(xlNorm)[1]     
    n <- dim(xlNorm)[2] - 1 
    errs = 0
    for (i in 1:l) {
      if (xlNorm[i,n+1] != sign(sum(w*xlNorm[i,1:n]))) {
        errs = errs + 1
      }
    }
    print("error")
    print(errs)
    
  })
  output$plot4 <- renderPlot({
    sigma1 <- matrix(as.numeric(unlist(strsplit(input$sigma1_all,","))),2,2)
    sigma2 <- matrix(as.numeric(unlist(strsplit(input$sigma2_all,","))),2,2)
    mu1 <- as.numeric(unlist(strsplit(input$mu1_all,",")))
    mu2 <- as.numeric(unlist(strsplit(input$mu2_all,",")))
    ObjectsCountOfEachClass <- input$obj4 
    
    xy1 <- mvrnorm(n=ObjectsCountOfEachClass, mu1, sigma1) 
    xy2 <- mvrnorm(n=ObjectsCountOfEachClass, mu2, sigma2)  
    xl <- rbind(cbind(xy1, -1), cbind(xy2, +1))  
    colors <- c(rgb(155/255, 130/255, 165/255), "white", rgb(55/255, 250/255, 175/255))  
    
    xlNorm <- Normalization(xl) 
    xlNorm <- Prepare(xlNorm) 
    
    ## ADALINE 
    plot(xlNorm[, 1], xlNorm[, 2], pch = 21, bg = colors[xl[,3] + 2], asp = 1, xlab = "x1", ylab = "x2", main = "Linear classifiers") 
    w <- SGradient(xlNorm, id = 1)  
    abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 3, col = "blue")  
    
    ##Hebb
    w <- SGradient(xlNorm, id = 2)     
    abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 3, col = "green3") 
    
    ##LogRegression
    w <- SGradient(xlNorm, id = 3)     
    abline(a = w[3] / w[2], b = -w[1] / w[2], lwd = 3, col = "red")
    
    ##SVM
    y <- matrix(c(rep(1, ObjectsCountOfEachClass), rep(-1, ObjectsCountOfEachClass))) 
    svp <- ksvm(xlNorm[1:(ObjectsCountOfEachClass*2),1:2],y,type="C-svc",C = 100, kernel="vanilladot",scaled=c())
    ymat <- ymatrix(svp)
    points(xlNorm[-SVindex(svp),1], xlNorm[-SVindex(svp),2], pch = ifelse(ymat[-SVindex(svp)] < 0, 2, 1))
    points(xlNorm[SVindex(svp),1], xlNorm[SVindex(svp),2], pch = ifelse(ymat[SVindex(svp)] < 0, 17, 16))
    w <- colSums(coef(svp)[[1]] * xlNorm[SVindex(svp),])
    b <- b(svp)
    abline(b/w[2],-w[1]/w[2], lwd = 3, col = "orange")
    
    legend("bottomleft", c("ADALINE", "Hebb's rule", "Logistic regression", "SVM"), pch = c(15,15,15,15), col = c("blue", "green3", "red", "orange"))
    
  })
  output$plot5 <- renderPlot({
    sigma1 <- matrix(as.numeric(unlist(strsplit(input$sigma1_svm,","))),2,2)
    sigma2 <- matrix(as.numeric(unlist(strsplit(input$sigma2_svm,","))),2,2)
    mu1 <- as.numeric(unlist(strsplit(input$mu1_svm,",")))
    mu2 <- as.numeric(unlist(strsplit(input$mu2_svm,",")))
    ObjectsCountOfEachClass <- input$obj5  
    
    xy1 <- mvrnorm(n=ObjectsCountOfEachClass, mu1, sigma1) 
    xy2 <- mvrnorm(n=ObjectsCountOfEachClass, mu2, sigma2)  
    xl <- rbind(cbind(xy1, -1), cbind(xy2, +1))  
    colors <- c(rgb(155/255, 130/255, 165/255), "white", rgb(55/255, 250/255, 175/255))  
    
    xlNorm <- Normalization(xl) 
    xlNorm <- Prepare(xlNorm) 
    
    y <- matrix(c(rep(1, ObjectsCountOfEachClass), rep(-1, ObjectsCountOfEachClass))) 
    svp <- ksvm(xlNorm[1:(ObjectsCountOfEachClass*2),1:2],y, kernel="vanilladot",type = "C-svc") 
    #svp <- ksvm(xlNorm[1:ObjectsCountOfEachClass*2,1:2],y, kernel="rbfdot", type = "C-svc")
    plot(svp, data = xlNorm[1:(ObjectsCountOfEachClass*2),1:2]) 
  })
  
  output$plot6 <- renderPlot({
    sigma1 <- matrix(as.numeric(unlist(strsplit(input$sigma1_svm,","))),2,2)
    sigma2 <- matrix(as.numeric(unlist(strsplit(input$sigma2_svm,","))),2,2)
    mu1 <- as.numeric(unlist(strsplit(input$mu1_svm,",")))
    mu2 <- as.numeric(unlist(strsplit(input$mu2_svm,",")))
    ObjectsCountOfEachClass <- input$obj5  
    
    xy1 <- mvrnorm(n=ObjectsCountOfEachClass, mu1, sigma1) 
    xy2 <- mvrnorm(n=ObjectsCountOfEachClass, mu2, sigma2)  
    xl <- rbind(cbind(xy1, -1), cbind(xy2, +1))  
    colors <- c(rgb(155/255, 130/255, 165/255), "white", rgb(55/255, 250/255, 175/255))  
    
    xlNorm <- Normalization(xl) 
    xlNorm <- Prepare(xlNorm) 
    
    y <- matrix(c(rep(1, ObjectsCountOfEachClass), rep(-1, ObjectsCountOfEachClass))) 
    
    svp <- ksvm(xlNorm[1:(ObjectsCountOfEachClass*2),1:2],y,type="C-svc",C = 100, kernel="vanilladot",scaled=c())
    plot(c(min(xlNorm[,1]), max(xlNorm[,1])),c(min(xlNorm[,2]), max(xlNorm[,2])),type='n',xlab='x1',ylab='x2')
    title(main='Linear Separable Features')
    ymat <- ymatrix(svp)
    points(xlNorm[-SVindex(svp),1], xlNorm[-SVindex(svp),2], pch = ifelse(ymat[-SVindex(svp)] < 0, 2, 1))
    points(xlNorm[SVindex(svp),1], xlNorm[SVindex(svp),2], pch = ifelse(ymat[SVindex(svp)] < 0, 17, 16))
    
    w <- colSums(coef(svp)[[1]] * xlNorm[SVindex(svp),])
    b <- b(svp)
    
    # Draw the lines
    abline(b/w[2],-w[1]/w[2])
    abline((b+1)/w[2],-w[1]/w[2],lty=2)
    abline((b-1)/w[2],-w[1]/w[2],lty=2)
  })
  output$plot7 <- renderPlot({
    sigma1 <- matrix(as.numeric(unlist(strsplit(input$sigma1_svm,","))),2,2)
    sigma2 <- matrix(as.numeric(unlist(strsplit(input$sigma2_svm,","))),2,2)
    mu1 <- as.numeric(unlist(strsplit(input$mu1_svm,",")))
    mu2 <- as.numeric(unlist(strsplit(input$mu2_svm,",")))
    ObjectsCountOfEachClass <- input$obj5  
    
    xy1 <- mvrnorm(n=ObjectsCountOfEachClass, mu1, sigma1) 
    xy2 <- mvrnorm(n=ObjectsCountOfEachClass, mu2, sigma2)  
    xl <- rbind(cbind(xy1, -1), cbind(xy2, +1))  
    colors <- c(rgb(155/255, 130/255, 165/255), "white", rgb(55/255, 250/255, 175/255))  
    
    xlNorm <- Normalization(xl) 
    xlNorm <- Prepare(xlNorm) 
    
    y <- matrix(c(rep(1, ObjectsCountOfEachClass), rep(-1, ObjectsCountOfEachClass))) 
    
    if(input$do2) {
      par(mfrow=c(1,2))
      f <- vector()
      
      for(i in 1:nrow(xlNorm)) 
      { 
        x1<-xlNorm[i,1:2] 
        n<-length(x1) 
        f[i] <- sum(w[1:n]*x1)+b
      } 
      
      XL<-cbind(xlNorm,f) 
      XL<-XL[order(XL[,5]),] 
      
      mneg<-0 
      for(i in 1:nrow(XL)){ 
        if(XL[i,4]<1) mneg<-mneg+1 
      } 
      mpos<-0 
      for(i in 1:nrow(XL)){ 
        if(XL[i,4]<1) mpos<-mpos+1 
      } 
      AUC <- 0
      FPR<-vector()
      TPR<-vector()
      for(i in 1:nrow(XL)) 
      { 
        if(XL[i,4] == -1) 
        { 
          if(i==1) 
          { 
            FPR[i] <- 1/mneg 
            TPR[i] <- 0 
            AUC <- 1/mneg*TPR[i]
          } 
          else{ 
            FPR[i] <- FPR[i-1]+1/mneg 
            TPR[i] <- TPR[i-1] 
            AUC <- AUC+1/mneg*TPR[i] 
          } 
        } 
        else 
        { 
          if(i==1) 
          { 
            FPR[i] <- 0 
            TPR[i] <- 1/mpos 
          } 
          else{ 
            FPR[i] <- FPR[i-1] 
            TPR[i] <-TPR[i-1]+1/mpos 
          } 
        } 
      } 
      plot(data.frame(FPR,TPR),type="l", col="blue",lwd=3, main="ROC-curve")
      
      plot(roc(y, f, direction="auto"), print.auc=TRUE, col="blue", lwd=3, main="ROC-curve")
    }
  })
  
})
