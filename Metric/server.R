library(shiny)

shinyServer(function(input, output,session) {
  
  x1 <- reactive({ input$x1  })
  y1 <- reactive({ input$y1  })
  k1 <- reactive({  input$k1  })
  x2 <- reactive({ input$x2  })
  y2 <- reactive({ input$y2  })
  k2 <- reactive({  input$k2  })
  
  
  data("iris")
  
  #draw circle for Parzen and Potential
  drawCircle <- function(xc, yc, r, color){
    t <- 0
    while(t <= 2 * pi){
      x <- xc + r * cos(t)
      y <- yc + r * sin(t)
      
      points(x, y, pch = ".", col = color, asp = 1) 
      t <- t + 0.01
    }
  }
  
  #kernels
  Krectangle <- function(x) {
    if (abs(x) <= 1) return(0.5)
    else return(0)
  }
  
  Kepanchnikov <- function(x) {
    if (abs(x) <= 1) return(3*(1-x^2)/4)
    else return(0)
  }
  
  Ktriangle <- function(x) {
    if (abs(x) <= 1) return(1-abs(x))
    else return(0)
  }
  
  Kgauss <- function(x) {
    return(((2*pi)^(-0.5))*exp(-0.5*x^2))
  }
  
  Kkvartich <- function(x) {
    if (abs(x) <= 1) return(15*(1-x^2)^2/16)
    else return(0)
  }
  
  ########################################################################################
  euclideanDistance <- function(u, v)  {
    sqrt(sum((u - v)^2))
  }
  
  sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance) {
    l <- dim(xl)[1]
    n <- dim(xl)[2] - 1
    distances <- matrix(NA, l, 2)
    for (i in 1:l)      {
      distances[i, ] <- c(i, metricFunction(xl[i, 1:n], z))
    }
    
    orderedXl <- xl[order(distances[, 2]), ]
    return (orderedXl);
  }
  
  min <- function(a,b) {
    if (a < b) return(a)
    else return(b)
  }
  
  max <- function(a,b) {
    if (a > b) return(a)
    else return(b)
  }
  
  #########################################algorithms#############################
  
  kNN <- function(xl, z, k) {       
    orderedXl <- sortObjectsByDist(xl, z)     
    n <- dim(orderedXl)[2] - 1          
    classes <- orderedXl[1:k, n + 1]             
    counts <- table(classes)         
    class <- names(which.max(counts))      
    return (class) 
  } 
  
  wkNN <- function(xl, z, k, q) {       
    orderedXl <- sortObjectsByDist(xl, z) 
    n <- dim(orderedXl)[2] - 1          
    classes <- orderedXl[1:k, n + 1]  
    #res <- levels(orderedXl[,n+1])
    l <- length(names(table(classes)))
    vote <- matrix(0, l)
    for (i in 1:k) {
      vote[orderedXl[i,n+1]] = vote[orderedXl[i,n+1]] + q^(i-1) 
    }
    print(vote)
    class <- (which.max(vote))      
    return (class) 
  }
  
  #for Parzen with temp h
  Findh <- function(k, xl, z) {
    n <- dim(xl)[2] - 1  
    orderedXl <- sortObjectsByDist(xl, z) 
    d <- euclideanDistance(orderedXl[k,1:n],z)
    return (d)
  }
  
  Parzen <- function(xl, z, K, h) {  
    L <- dim(xl)[1]
    n <- dim(xl)[2]
    classes <- xl[1:L, n]  
    l <- length(names(table(classes)))
    vote <- matrix(0, l)
    if (K == "Krectangle") K <- Krectangle 
    else if (K == "Kepanchnikov") K <- Kepanchnikov 
    else if (K == "Ktriangle") K <- Ktriangle 
    else if (K == "Kgauss") K <- Kgauss 
    else if (K == "Kkvartich") K <- Kkvartich 
    for (i in 1:L) {
      vote[xl[i,n]] = vote[xl[i,n]] + K(euclideanDistance(xl[i,1:n-1],z)/h) 
    }
    class <- (which.max(vote)) 
    if (sum(vote) == 0) return (0)
    else return (class) 
  }
  
  Potential <- function(xl, z, K, h, g) {  
    L <- dim(xl)[1]
    n <- dim(xl)[2]
    classes <- xl[1:L, n]  
    l <- length(names(table(classes))) #number of classes
    vote <- matrix(0, l)
    for (i in 1:L) {
      vote[xl[i,n]] = vote[xl[i,n]] + g[i]*K(euclideanDistance(xl[i,1:n-1],z)/h) #score for each class
    }
    class <- (which.max(vote)) 
    if (sum(vote) == 0) return (0)
    else return (class) #class id
  }
  
  #for Potential functions (better to choose cnt_err > 1)
  FindGammas <- function(XL, h, K) {
    L <- dim(XL)[1]
    N <- dim(XL)[2]
    g <- rep(0,times = L) #gammas
    cnt_err <- 0 
    cnt_err <- 150
    while(cnt_err > 15 && g[which.max(g)] < 7) { #second condition is just to fill gammas 
      cnt_err <- 0
      for(i in 1:L){ 
        y <- Potential(XL[-i,], XL[i,-N], K, h, g) 
        if (y != as.integer(XL[i,N])) {
          g[i] <- g[i] + 1 
          cnt_err <- cnt_err + 1 #find error 
        }
      } 
      print(cnt_err) 
    } 
    return(g)
  }
  
  #################################plots##################################################
  
  #draw plot for kNN (first is for classification of one object and second is for clas.map)
  observeEvent(input$kNNCl, {
    output$plot1 <- renderPlot({
      par(mar = c(5.1, 4.1, 0, 1))
      colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
      plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species])
      p <- c(input$x1, input$y1) 
      xl <- iris[, 3:5] 
      class <- kNN(xl, p, input$k1)
      points(p[1], p[2], pch = 22, bg = colors[class], col = "black", cex = 2)
    })
  })
  observeEvent(input$kNNMap, {
    output$plot1 <- renderPlot({
      ideal <- STOLP(iris[,3:5], 0, 5, input$k1, 1) #xl, delta, l0
      print(ideal)
      colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
      plot(iris[,3:5], pch = 19, bg = "grey", col = "grey", asp = 1, cex = 2)
      plot(ideal[, 1:2], pch = 19, bg = colors[ideal$Species], col = colors[ideal$Species], asp = 1, cex = 2)
      xf <- seq(from=1, to=7, by=0.1)
      yf <- seq(from=0, to=2.5, by=0.1)
      for (i in xf) {
        for (j in yf) {
          p <- c(i, j, 0.0)
          res <- kNN(ideal, p, input$k1)
          points(i, j, col=colors[res], pch=3)
        }
      }
      legend(1,2.5, c("Setosa", "Versicolor", "Virginica"), pch = c(15,15,15), col = c("red", "green3", "blue"))
    })
  })
  
  #draw plot for wkNN (first is for classification of one object and second is for clas.map)    
  observeEvent(input$wkNNCl, {
    output$plot2 <- renderPlot({
      par(mar = c(5.1, 4.1, 0, 1))
      colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
      plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species])
      p <- c(input$x2, input$y2)
      xl <- iris[, 3:5]
      class <- wkNN(xl, p, input$k2, 0.9) #q = 0.9
      points(p[1], p[2], pch = 22, bg = colors[class], col = "black", cex = 2)
    })
  })
  observeEvent(input$wkNNMap, {
    output$plot2 <- renderPlot({
      ideal <- STOLP(iris[,3:5], 0, 5, input$k2, 2) #xl, delta, l0
      print(ideal)
      colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
      plot(ideal[, 1:2], pch = 19, bg = colors[ideal$Species], col = colors[ideal$Species], asp = 1, cex = 2)
      xf <- seq(from=1, to=7, by=0.1)
      yf <- seq(from=0, to=2.5, by=0.1)
      for (i in xf) {
        for (j in yf) {
          p <- c(i, j, 0.0)
          res <- wkNN(ideal, p, input$k2, 0.9)
          points(i, j, col=colors[res], pch=3)
        }
      }
      legend(1,2.5, c("Setosa", "Versicolor", "Virginica"), pch = c(15,15,15), col = c("red", "green3", "blue"))
    })
  })
  
  #draw plot for Parzen (first is for classification of one object and second is for clas.map)
  observeEvent(input$ParzenCl, {
    output$plot3 <- renderPlot({
      colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
      plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species])
      p <- c(input$x3, input$y3)
      xl <- iris[, 3:5]
      class <- Parzen(xl, p, input$Kernel, input$h)
      if (class == 0) points(p[1], p[2], bg="yellow", pch=22, cex = 2)
      else {
        points(p[1], p[2], bg = colors[class], pch=22, cex = 2)
        #points(p[1], p[2], col = colors[class], pch = 1, cex = input$h*10)
      }
      drawCircle(p[1],p[2],input$h, "black")
    })
  })
  observeEvent(input$ParzenMap, {
    output$plot3 <- renderPlot({
      ideal <- STOLP(iris[,3:5], 0, 5, input$Kernel, 3) #xl, delta, l0
      print(ideal)
      colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
      plot(ideal[, 1:2], pch = 19, bg = colors[ideal$Species], col = colors[ideal$Species], asp = 1, cex = 2)
      xf <- seq(from=1, to=7, by=0.1)
      yf <- seq(from=0, to=2.5, by=0.1)
      for (i in xf) {
        for (j in yf) {
          p <- c(i, j, 0.0)
          res <- Parzen(iris[,3:5], p, input$Kernel, input$h)
          if (res == 0) points(i, j, col="yellow", pch=3)
          points(i, j, col=colors[res], pch=3)
        }
      }
      legend(1,2.5, c("Setosa", "Versicolor", "Virginica"), pch = c(15,15,15), col = c("red", "green3", "blue"))
    })
  })
  
  #draw plot for Parzen with temp h (here without classification map and STOLP)
  observeEvent(input$ParzenTempHCl, {
    output$plot4 <- renderPlot({
      colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
      plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species])
      p <- c(input$x4, input$y4) 
      xl <- iris[, 3:5] 
      h <- Findh(input$k, iris[3:5], p)
      print(h)
      if (h == 0) h <- 0.01
      output$hval <- renderPrint({h})
      class <- Parzen(xl, p, input$Kernel_2, h)
      if (class == 0) points(p[1], p[2], bg="yellow", pch=22, cex = 2)
      else {
        points(p[1], p[2], bg = colors[class], pch=22, cex = 2)
        #points(p[1], p[2], col = colors[class], pch = 1, cex = input$h*10)
      }
      drawCircle(p[1],p[2],h, "black")
    })
  })
  observeEvent(input$ParzenTempHMap, {
    output$plot4 <- renderPlot({
      colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue") 
      plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1) 
      xf <- seq(from=1, to=7, by=0.1)
      yf <- seq(from=0, to=2.5, by=0.1)
      for (i in xf) {
        for (j in yf) {
          p <- c(i, j, 0.0)
          h <- Findh(input$k, iris[3:5], p)
          if (h == 0) h <- 0.1
          res <- Parzen(iris[3:5], p, input$Kernel_2, h) 
          #Krectangle 1, Kepanchnikov 1, Ktriangle 1.5, Kgauss 0.9, Kkvartich 0.5
          if (res == 0) points(i, j, col="yellow", pch=3)
          else points(i, j, col=colors[res], pch=3)
        }
      }
      legend(1,0, c("Setosa", "Versicolor", "Virginica"), pch = c(15,15,15), col = c("red", "green3", "blue"))
    })
  })
  
  #draw plot for Potential functions 
  observeEvent(input$PotencialCl, {
    output$plot5 <- renderPlot({
      Kkern <- input$Kernel_potential
      if (Kkern == "Krectangle") Kkern <- Krectangle 
      else if (Kkern == "Kepanchnikov") Kkern <- Kepanchnikov 
      else if (Kkern == "Ktriangle") Kkern <- Ktriangle 
      else if (Kkern == "Kgauss") Kkern <- Kgauss 
      else if (Kkern == "Kkvartich") Kkern <- Kkvartich 
      g <- FindGammas(iris[3:5], input$h_potential, Kkern)
      print(g)
      colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
      plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species])
      L <- dim(iris[3:5])[1]
      N <- dim(iris[3:5])[2]
      ir <- iris[3:5]
      for (i in 1:L) {
        if (g[i] != 0) {
          points(iris[i,3],iris[i,4],  col = colors[ir[i,N]], pch = 19, cex = g[i]+5)
          drawCircle(iris[i,3],iris[i,4],input$h_potential, colors[ir[i,N]])
        }
      }
      p <- c(input$x5, input$y5) 
      xl <- iris[, 3:5] 
      class <- Potential(iris[3:5], p, Kkern, input$h_potential, g)
      if (class == 0) points(p[1], p[2], bg="yellow", pch=22, cex = 2)
      else {
        points(p[1], p[2], bg = colors[class], pch=22, cex = 2)
        #points(p[1], p[2], col = colors[class], pch = 1, cex = input$h*10)
      }
    })
  })
  observeEvent(input$PotencialMap, {
    output$plot5 <- renderPlot({
      Kkern <- input$Kernel_potential
      if (Kkern == "Krectangle") Kkern <- Krectangle 
      else if (Kkern == "Kepanchnikov") Kkern <- Kepanchnikov 
      else if (Kkern == "Ktriangle") Kkern <- Ktriangle 
      else if (Kkern == "Kgauss") Kkern <- Kgauss 
      else if (Kkern == "Kkvartich") Kkern <- Kkvartich 
      g <- FindGammas(iris[3:5], input$h_potential, Kkern)
      colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
      plot(iris[, 3:4], pch = 1, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)
      L <- dim(iris[3:5])[1]
      N <- dim(iris[3:5])[2]
      ir <- iris[3:5]
      for (i in 1:L) {
        if (g[i] != 0) {
          points(iris[i,3],iris[i,4],  col = colors[ir[i,N]], pch = 19, cex = g[i])
          #points(iris[i,3],iris[i,4],  col = colors[ir[i,N]], pch = 1, cex = height*16.5) #its window
        }
      }
      xf <- seq(from=1, to=7, by=0.1)
      yf <- seq(from=0, to=2.5, by=0.1)
      for (i in xf) {
        for (j in yf) {
          p <- c(i, j, 0.0)
          res <- Potential(iris[3:5], p, Kkern, input$h_potential, g)
          #Krectangle 1, Kepanchnikov 1, Ktriangle 1.5, Kgauss 0.9, Kkvartich 0.5
          if (res == 0) points(i, j, col="yellow", pch=3)
          else points(i, j, col=colors[res], pch=3)
        }
      }
      legend(1,0, c("Setosa", "Versicolor", "Virginica"), pch = c(15,15,15), col = c("red", "green3", "blue"))
      
    })
  })
  
  
  #########################################STOLP##########################################  
  
  
  #find best q for wknn (dont use it at shiny because of low speed)
  LOOfuncq <- function(XL, k, l, r, step) {
    L <- dim(XL)[1]
    N <- dim(XL)[2]
    qtest <- seq(l, r, step)
    len = length(qtest)
    mas <- rep(0, times = len)
    for(i in 1:L) {
      orderedXL <- sortObjectsByDist(XL[-i,],XL[i,-N])
      for(j in 1:len) {
        y <- wkNNfast(orderedXL, k, qtest[j])
        if (y != as.integer(XL[i,N])) mas[j] <- mas[j] + 1
      }
    }
    LOOres <- data.frame(q = qtest, LOO = mas/L)
    return(LOOres)
  }
  
  Margin <- function(xl, z, y, param, method) {
    l <- dim(iris[3:5])[1]
    n <- dim(iris[3:5])[2]
    if (method == 1) { #knn
      k <- param
      orderedXl <- sortObjectsByDist(xl, z)
      kNeighboorsClasses <- orderedXl[1:k, n]
      marginval <- array(0, dim = length(table(kNeighboorsClasses)))
      for (i in 1:k) {
        marginval[kNeighboorsClasses[i]] <- marginval[kNeighboorsClasses[i]]+1
      }
      mOwn <- marginval[y]
      marginval <- marginval[-as.integer(y)]
      mOthers <- marginval[which.max(marginval)]
      m <- mOwn - mOthers
    }
    if (method == 2) { #kwnn
      k <- param
      q <- 0.9
      # step <- 0.1
      # left <- 0.1
      # right <- 1
      #loo <- LOOfuncq(iris[3:5], k, left, right, step) #find best q for this k
      #q <- loo[which.min(loo[,2]),1]
      
      classes <- xl[1:l, n]  
      len <- length(names(table(classes)))
      orderedXl <- sortObjectsByDist(xl, z)
      marginval <- array(0, dim = len)
      for (i in 1:k) {
        marginval[orderedXl[i,n]] <- marginval[orderedXl[i,n]] + q^(i) 
      }
      mOwn <- marginval[as.integer(y)]
      marginval <- marginval[-as.integer(y)]
      mOthers <- marginval[which.max(marginval)]
      m <- mOwn - mOthers
    }
    if (method == 3) { #parzen
      if (param == "Krectangle") param <- Krectangle 
      else if (param == "Kepanchnikov") param <- Kepanchnikov 
      else if (param == "Ktriangle") param <- Ktriangle 
      else if (param == "Kgauss") param <- Kgauss 
      else if (param == "Kkvartich") param <- Kkvartich 
      h <- input$h
      classes <- xl[1:l, n]  
      l <- dim(xl)[1]
      len <- length(names(table(classes)))
      marginval <- array(0, dim = len)
      for (i in 1:l) {
        #print(xl[i,1:n])
        marginval[xl[i,n]] <- marginval[xl[i,n]] + param(euclideanDistance(xl[i,1:n-1],z)/h)
        #print(marginval)
        #print(i)
      }
      #print(marginval)
      mOwn <- marginval[as.integer(y)]
      marginval <- marginval[-as.integer(y)]
      mOthers <- marginval[which.max(marginval)]
      m <- mOwn - mOthers
    }
    return(m)
  }
  
  STOLP <- function(xl, delta, l0, param, method) {
    l <- dim(iris[3:5])[1]
    n <- dim(iris[3:5])[2]
    classes <- xl[1:l, n]  
    cnt <- length(names(table(classes))) #find count of classes
    good <- rep(T, times = l)
    inf <- 10000
    
    #remove emissions 
    for (i in 1:l) {
      class <- xl[i,n]
      M <- Margin(xl[-i,], xl[i,1:n-1], class, param, method) #xl,z,y,param,method
      #print(M)
      if (M < delta) good[i] <- FALSE
    }
    xl <-  xl[good,] #new xl without emissions
    l <- dim(xl)[1]
    print(l)
    
    #find initial etalons
    etalon <- vector()
    Mc = rep(-inf,cnt) 
    idc = rep(-inf,cnt)
    for (i in 1:l) {
      class <- xl[i,n]
      M <- Margin(xl[-i,], xl[i,1:n-1], class, param, method)
      if(M > Mc[class]) #find one etalon from each class
      {
        Mc[class] <- M #save margin of etalon object for 'class'
        idc[class] <- i #save its index
      }
    }
    #save first etalons to 'etalon'
    good <- rep(T, times = l) 
    for (i in 1:cnt) 
    {
      cur <- idc[i]
      if (cur != -inf) { #it is etalon object
        etalon <- rbind(etalon, xl[cur,]) #save this etalon object
        print("etalon")
        print(etalon)
        good[cur] <- FALSE #we should delete etalon from xl
      }
    }
    xl <-  xl[good,] #new xl without founded etalons
    l <- dim(xl)[1] #new size of xl
    print(xl)
    print("__________________________________")
    
    error <- l0+1
    # iter <- 0
    l1 <- l
    while(l1 != dim(etalon)[1])
    {
      Mc <- rep(inf,cnt)
      idc <- rep(inf,cnt)
      error <- 0
      good <- rep(T, times = l)
      
      for (i in 1:l) 
      {
        class <- xl[i,n]
        M <- Margin(etalon, xl[i,1:n-1], class, param, method)
        print(M)
        if(M <= 0) error <- error+1
      }
      print(sprintf("error %f", error))
      if (error < l0) break;
      #add object with min margin
      for (i in 1:l) 
      {
        class <- xl[i,n]
        M <- Margin(etalon, xl[i,1:n-1], class, param, method)
        if(M < Mc[class]) #find an object with min margin
        {
          Mc[class] <- M 
          idc[class] <- i
        }
      }
      for (i in 1:cnt) 
      {
        cur <- idc[i]
        if (cur != inf) {
          etalon <- rbind(etalon, xl[cur,])
          good[cur] <- FALSE
        }
      }
      xl <-  xl[good,]
      l <- dim(xl)[1]
    }
    return(etalon)
  }
  
  ################################################################################
  # ideal <- STOLP(iris[,3:5], 0, 5) #xl, delta, l0
  # print("ideal")
  # print(ideal)
  
  ################################################################################
  # colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
  # plot(ideal[, 1:2], pch = 19, bg = colors[ideal$Species], col = colors[ideal$Species], asp = 1)
  # xf <- seq(from=1, to=7, by=0.1)
  # yf <- seq(from=0, to=2.5, by=0.1)
  # for (i in xf) {
  #   for (j in yf) {
  #     p <- c(i, j, 0.0)
  #     res <- Parzen(ideal,p,kernel,0.6)
  #     #res <- KNN(ideal, p, k)
  #     #    Krectangle 1, Kepanchnikov 1, Ktriangle 1.5, Kgauss 0.9, Kkvartich 0.5
  #     if (res == 0) points(i, j, col="yellow", pch=3)
  #     points(i, j, col=colors[res], pch=3)
  #   }
  # }
  # legend(1,0, c("Setosa", "Versicolor", "Virginica"), pch = c(15,15,15), col = c("red", "green3", "blue"))
  
  
  
})
