#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls())
library(ggplot2)
library(shiny) 
require(LearnBayes)
library(tidyverse)
library(shinyWidgets)
library(shinythemes)  # more funky looking apps
library(rstan)
library(DT)
require(rms) # freq logistic regression
library(shinyalert)

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
options(max.print=1000000)
fig.width <- 400
fig.height <- 300
fig.width2 <- 1400
fig.height2 <- 300#50
fig.width3 <- 1300  
fig.height3 <- 350#400
fig.width4 <- 1380
fig.height4 <- 300
p1 <- function(x) {formatC(x, format="f", digits=1)}
p2 <- function(x) {formatC(x, format="f", digits=2)}
p5 <- function(x) {formatC(x, format="f", digits=5)}

options(width=200)
set.seed(12345) # reproducible

is.even <- function(x){ x %% 2 == 0 } # function to id. odd maybe useful


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- fluidPage(theme = shinytheme("journal"), #https://www.rdocumentation.org/packages/shinythemes/versions/1.1.2
                # paper
                useShinyalert(),  # Set up shinyalert
                setBackgroundColor(
                    color = c( "#2171B5", "#F7FBFF"), 
                    gradient = "linear",
                    direction = "bottom"
                ),
                
                h2("Bayesian bootstrap"),
                
                h4("xxxxxxxxxxxx
              "), 
                
                h3("  "), 
                
                
                sidebarLayout(
                    
                    sidebarPanel( width=3 ,
                                  
                                  tags$style(type="text/css", ".span8 .well { background-color: #00FFFF; }"),
                                  
                                  
                                  actionButton(inputId='ab1', label="R code",   icon = icon("th"),   
                                               onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/Bayesian-proportions-logistic-regression/master/monte-carlo-two-proportions/app.R', '_blank')"),    
                                  actionButton("resample", "Rerun the Monte Carlo simulations"),
                                  br(), # br(), 
                                  tags$style(".well {background-color:#b6aebd ;}"), ##ABB0B4AF
                                  
                                  h4("xxxxxxxxxxxxxxxxxxxxxxxx."),
                                  div(
                                      
                                      tags$head(
                                          tags$style(HTML('#ab1{background-color:orange}'))
                                      ),
                                      
                                      tags$head(
                                          tags$style(HTML('#resample{background-color:orange}'))
                                      ),
                                      
                                      
                                      textInput('vec3', 
                                                div(h5("Enter the number of samples")), "100"),
                                      
                                      textInput('vec4', 
                                                div(h5("Enter the true mean and sd for normal dist.")), "10, 5"),
                                      
                                      textInput('n1y1', 
                                                div(h5("Enter the number of samples")), "101"),
                                      
                                      textInput('n2y2', 
                                                div(h5("Enter the true correlation")), ".9"),
                                      
                                      textInput('sims', 
                                                div(h5("Monte Carlo simulations")), "1000"),
                                      div(h5("References:")),  
                                      
                                      tags$a(href = "https://projecteuclid.org/download/pdf_1/euclid.aos/1176344552", "[1] Efron"),
                                      div(p(" ")),
                                      tags$a(href = "https://projecteuclid.org/download/pdf_1/euclid.aos/1176345338", "[2] Rubin"),
                                      div(p(" ")),
                                      tags$a(href = "https://en.wikipedia.org/wiki/Beta_distribution", "[3] Beta distribution"),
                                      div(p(" ")),
                                      tags$a(href = "https://www.tjmahr.com/bayesian-fisher-exact-test/", "[4] Blog article"),
                                      div(p(" ")),
                                      
                                      
                                  )
                                  
                    ),
                    
                    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~tab panels
                    mainPanel(width=9,
                              
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              #    tabsetPanel(type = "tabs", 
                              navbarPage(       
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
                                  tags$style(HTML("
                            .navbar-default .navbar-brand {color: orange;}
                            .navbar-default .navbar-brand:hover {color: blue;}
                            .navbar { background-color: #b6aebd;}
                            .navbar-default .navbar-nav > li > a {color:black;}
                            .navbar-default .navbar-nav > .active > a,
                            .navbar-default .navbar-nav > .active > a:focus,
                            .navbar-default .navbar-nav > .active > a:hover {color: pink;background-color: purple;}
                            .navbar-default .navbar-nav > li > a:hover {color: black;background-color:yellow;text-decoration:underline;}
                            .navbar-default .navbar-nav > li > a[data-value='t1'] {color: red;background-color: pink;}
                            .navbar-default .navbar-nav > li > a[data-value='t2'] {color: blue;background-color: lightblue;}
                            .navbar-default .navbar-nav > li > a[data-value='t3'] {color: green;background-color: lightgreen;}

                   ")),
                                  
                                  tabPanel("1 Estimating the uncertanty in one sample mean", value=7, 
                                           
                                           
                                           fluidRow(
                                               
                                               
                                               column(width = 7, offset = 0, style='padding:0px;',
                                                      
                                                      h4("Prior distributions"), 
                                                      #      div(plotOutput("trt.plot", width=fig.width2, height=fig.height2)), 
                                                      
                                                      
                                                      h4("Posterior distributions updated with the observed data"), 
                                                      #    div(plotOutput("trt.plot1", width=fig.width2, height=fig.height2)),       
                                                      
                                                      
                                                      
                                               ))
                                           
                                  ) ,
                                  
                                  tabPanel("2 Estimating the uncertanty in a correlation coefficient", value=7, 
                                           
                                           
                                           fluidRow(
                                               column(width = 5, offset = 0, style='padding:1px;',
                                                      
                                                      h4("Frequentist bootstrap and Bayesian bootstrap posterior summary of distributions, median and 95% intervals and T-test estimates"), 
                                                      div( verbatimTextOutput("reg.summary2"))
                                               )),
                                           
                                           
                                           
                                           h4(paste("Frequentist and Bayesian posterior distributions")), 
                                           div(plotOutput("diff", width=fig.width4, height=fig.height4)),       
                                           
                                           h6(paste("Blue vertical lines demark 95% credible intervals, red dashed lines are population estimates and the black dashed lines true population mean")), 
                                           
                                           
                                  ) ,
                                  
                                  
                                  
                                  
                                  tabPanel("2 Same results using datatable function", value=3, 
                                           
                                           h6("Sort and filter on the fly."),
                                           h4("Posterior distributions summaries, p(efficacy) will be judged by p(trt>ctrl)"), 
                                           #  DT::dataTableOutput("tablex"),
                                           h4(paste("Posterior distributions : 1 risk difference (trt-ctrl);","2 relative risk (trt/ctrl); 3 odds ratio [odds(trt)/odds(ctrl)]")), 
                                           div(plotOutput("diff2", width=fig.width4, height=fig.height4)),  
                                           h6(paste("Blue vertical lines demark 95% credible intervals, red dashed lines are population values of interest")), 
                                  ),
                                  
                                  tabPanel("3 Frequentist analysis", value=3, 
                                           h3("No prior information is used ! Also don't forget the confidence intervals cannot be interpreted
                                              that there is the stated probability that the true population parameter lies in the interval !"),
                                           h4("Fisher's Exact Test for Count Data"),
                                           #   div( verbatimTextOutput("fisher")),
                                           h4("2-sample test for equality of proportions without continuity correction"), 
                                           #   div( verbatimTextOutput("prop")),
                                           h4("Logistic regression odds ratio"), 
                                           #   div( verbatimTextOutput("logregx")),
                                           
                                           
                                           
                                  )
                                  
                              )
                              #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    )
                    
                ) 
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end tab panels 
                
)


server <- shinyServer(function(input, output   ) {
    
    shinyalert("You better believe it, there's a Bayesian bootstrap!",
               "And It's pretty, pretty good",
               type = "info")
    # --------------------------------------------------------------------------
    # ---------------------------------------------------------------------------
    # --------------------------------------------------------------------------
    # This is where a new sample is instigated 
    random.sample <- reactive({
        
        foo <- input$resample
        
        #sample sizes
        trt <- as.numeric(unlist(strsplit(input$vec3,",")))
        # mean and sD
        ctr <- as.numeric(unlist(strsplit(input$vec4,",")))
        
        #sample size for corelation
        n1y1 <- as.numeric(unlist(strsplit(input$n1y1,","))) #trt
        # R
        n2y2 <- as.numeric(unlist(strsplit(input$n2y2,",")))
        
        sims <- as.numeric(unlist(strsplit(input$sims,",")))
        
        return(list( #prob1=i[1],prob2=j[1],prob3=i[2],prob4=j[2],prob5=i[3],prob6=j[3],
            trt.alpha=trt[1],  
            ctr.alpha=ctr[1], ctr.beta=ctr[2],
            n1=n1y1[1], 
            n2=n2y2[1],#y2=n2y2[2],
            sims=sims#, prio=prio
        )) 
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    mcmc <- reactive({
        
        sample <- random.sample()
        
        n   <- trt.alpha <- sample$trt.alpha
        mu1 <- sample$ctr.alpha
        sd1 <- sample$ctr.beta
        
        #n1   <- sample$n1  
        # r    <- sample$n2
        reps <- sims <- sample$sims
        
        x <- rnorm(n, mean=mu1, sd=sd1)
        #reps <- 10
        
        # simulations
        
        require(Hmisc)
        dboot <- function(data.set) {
            u <- c(0, sort(runif(length(data.set) - 1)), 1)
            g <- diff(u)
            #return( wtd.quantile(data.set, weights=g, probs=.5, normwt=T))
            return( wtd.mean(data.set, weights=g))
        }
        
        #http://rsnippets.blogspot.ie/2012/11/simple-bayesian-bootstrap.html
        library(gtools)
        
        # Bayesian bootstrap
        mean.bb <- function(x, n) {
            apply(rdirichlet(n, rep(1, length(x))), 1, weighted.mean, x = x)
        }
        
        # standard bootstrap
        mean.fb <- function(x, n) {
            replicate(n, mean(sample(x, length(x), TRUE)))
        }
        
        
        
        
        
        
        
        #set.seed(2131)
        #reps <- I
        #reps <- 10
       # x <- rnorm(100, mean=0, sd=1)
      #  x <- rnorm(n, mean=mu1, sd=sd1)
        
        
        fbq<-bbq<-bb2<-NULL
        
        system.time(A <- mean.fb(x, reps))
        system.time(B <- mean.bb(x, reps))
        system.time(C <- replicate(reps, dboot(x)))
        
        fbq <- quantile(A, c(.5,0.025, 0.975)) 
        bbq <- quantile(B, c(.5,0.025, 0.975))
        bb2 <- quantile(C, c(.5,0.025, 0.975))
        
        y <- t.test(x)
        A1 <- cbind("Frequentist Median, 95% CI "   ,  p5(fbq[1]), p5(fbq[2]),  p5(fbq[3])  )
        B1 <- cbind("Bayesian Median, 95% CI "      ,  p5(bbq[1]), p5(bbq[2])  ,p5(bbq[3]))
        C1 <- cbind("EOB's Bayesian Median, 95% CI ",  p5(bb2[1]), p5(bb2[2]) , p5(bb2[3]))
        D1 <- cbind("T-test Mean, 95%CI ",p5(y$estimate[1][[1]]), p5(y$conf.int[1]), p5(y$conf.int[2]))
        
        res <- rbind(A1,B1,C1,D1)
        rownames(res) <- NULL
        
        return(list(res=res , A=A, B=B, C=C , mu1=mu1)) 
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$diff <- renderPlot({         
        
        z <- mcmc()$A
        mu1 <- mcmc()$mu1
        
        q <- quantile(z,c(.025, 0.25, 0.5, 0.75, 0.975))
        par(bg = 'lightgoldenrodyellow') 
        par(mfrow=c(1,3))
        plot(density(z),
             xlab="Freq. Bootstrap, Mean estimate",
             ylab="Density",
             main="",
             ylim=c(0,max(density(z)$y)),
             frame.plot=FALSE,cex.lab=1.5,lwd=3,yaxt="no")
        abline(v=q[1], col="blue") #95% credible interval
        abline(v=q[5], col="blue")
        abline(v=q[3], col="red", lty='dashed')
        abline(v=mu1, col="black", lty='dashed')
        
        z <- (mcmc()$B)
        
        q <- quantile(z,c(.025, 0.25, 0.5, 0.75, 0.975))
        
        plot(density(z), #log="x",
             xlab="Bayesian Bootstrap, Mean estimate",
             ylab="Density",
             main="",
             ylim=c(0, max(density(z)$y)),##
             frame.plot=FALSE,cex.lab=1.5,lwd=3,yaxt="no")
        abline(v=q[1], col="blue") #95% credible interval
        abline(v=q[5], col="blue")
        abline(v=q[3], col="red", lty='dashed')
        abline(v=mu1, col="black", lty='dashed')
        
        
        z <- mcmc()$C
        
        q <- quantile(z,c(.025, 0.25, 0.5, 0.75, 0.975))
        
        plot(density(z), #  log="x",
             xlab="Bayesian Bootstrap 2, Mean estimate",
             ylab="Density",
             main="",
             ylim=c(0, max(density(z)$y)),
             frame.plot=FALSE,cex.lab=1.5,lwd=3,yaxt="no")
        abline(v=q[1], col="blue") #95% credible interval
        abline(v=q[5], col="blue")
        abline(v=q[3], col="red", lty='dashed')
        abline(v=mu1, col="black", lty='dashed')
        
        captio=("xxxx")
        
        
        
        par(mfrow=c(1,1))
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$trt.plot <- renderPlot({         
        
        # sample <- random.sample()
        # 
        # x<- seq(0.001,.999, length.out=10000)
        # 
        # trt.alpha<- sample$trt.alpha
        # trt.beta<-  sample$trt.beta
        # ctr.alpha<- sample$ctr.alpha
        # ctr.beta<-  sample$ctr.beta
        # 
        # 
        # tmp1 <- max(c(dbeta(x, trt.alpha, trt.beta)  ) )
        # tmp2 <- max(c(dbeta(x, ctr.alpha, ctr.beta)))
        # tmp <- max(tmp1, tmp2)
        # 
        # par(bg = 'lightgoldenrodyellow')
        # 
        # 
        # curve(dbeta(x, trt.alpha, trt.beta),col = "blue", xlab = c("Probabiity"), 
        #       main=paste0("The Beta distribution for treatment in blue with shape parameters (",p2(trt.alpha),", ",p2(trt.beta),") and control in black (",p2(ctr.alpha),", ",p2(ctr.beta),")             "  
        #       ),
        #       ylab = "Density", xlim=c(0.0,1),  ylim=c(0, (tmp)*1.1) #ylim=c(0, max(
        # )
        # curve(dbeta(x, ctr.alpha, ctr.beta),col = "black", xlab = c("Probabiity"), 
        #       
        #       ylab = "Density",  add=TRUE
        # )
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$reg.summary2 <- renderPrint({
        
        return(print(mcmc()$res, digits=4))
        
    })
    # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    cor1 <- reactive({
        
        sample <- random.sample()
        
        n    <- sample$n1  
        r    <- sample$n2
        reps <- sims <- sample$sims
        
        
        CorrNorm <- function(n, rho) {
            X1 = rnorm(n); X2 = rnorm(n)
            Z = cbind(X1, rho*X1+sqrt(1-rho^2)*X2)
            return(Z)
        } 
        
        z<-CorrNorm(n=n,rho=r)
        cor.test(z[,1],z[,2])
        
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        sboot <- function() {
            cor(data.set[sample(1:9, replace=T),])[1,2]
        }
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        bboot <- function() {
            cov.wt(data.set, diff(c(0,sort(runif(8)),1)), cor=T)$cor[1,2]
        }
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        X <- matrix(c(z[,1],z[,2]), length(z[,1]), 2)
        library(LaplacesDemon)
        BB <- BayesianBootstrap(X=X, n=sims,
                                Method=function(x,w) cov.wt(x, w, cor=TRUE)$cor[1,2]) 
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        library(bayesboot)
        # Using the weighted correlation (corr) from the boot package.
        library(boot)
        b4 <- bayesboot(data.set, corr, R = sims, use.weights = TRUE)
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        f <- replicate(sims, sboot())
        b <- replicate(sims, bboot())
        BB <- unlist(BB)
        xx1 <- b4$V1
        
        return(list(f=f, b=b, BB=BB, xx1=xx1)) 
        
    })
    
    
    output$diff2 <- renderPlot({         
        
        z <- mcmc()$A
        
        q <- quantile(z,c(.025, 0.25, 0.5, 0.75, 0.975))
        par(bg = 'lightgoldenrodyellow') 
        par(mfrow=c(1,3))
        plot(density(z),
             xlab="risk differnece trt - ctrl",
             ylab="p(trt - ctrl | y, n)",
             main="",
             ylim=c(0,max(density(z)$y)),
             frame.plot=FALSE,cex.lab=1.5,lwd=3,yaxt="no")
        abline(v=q[1], col="blue") #95% credible interval
        abline(v=q[5], col="blue")
        abline(v=0, col="red", lty='dashed')
        
        z <- (mcmc()$B)
        
        q <- quantile(z,c(.025, 0.25, 0.5, 0.75, 0.975))
        
        plot(density(z), #log="x",
             xlab="relative risk trt / ctrl",
             ylab="p(trt / ctrl | y, n)",
             main="",
             ylim=c(0, max(density(z)$y)),##
             frame.plot=FALSE,cex.lab=1.5,lwd=3,yaxt="no")
        abline(v=q[1], col="blue") #95% credible interval
        abline(v=q[5], col="blue")
        abline(v=1, col="red", lty='dashed')
        
        z <- mcmc()$C
        
        q <- quantile(z,c(.025, 0.25, 0.5, 0.75, 0.975))
        
        plot(density(z),   #log="x",
             xlab="odds ratio",
             ylab="p(odds trt / odds ctrl | y, n)",
             main="",
             ylim=c(0, max(density(z)$y)),
             frame.plot=FALSE,cex.lab=1.5,lwd=3,yaxt="no")
        abline(v=q[1], col="blue") #95% credible interval
        abline(v=q[5], col="blue")
        abline(v=1, col="red", lty='dashed')
        captio=("xxxx")
        
        
        
        par(mfrow=c(1,1))
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
    output$tablex <- DT::renderDataTable({
        
        # mc <- mcmc()$f1
        # # s1 <- stan()$f
        # # s2 <- stan2()$res
        # 
        # A <- (mc)
        # # B <- (s1)
        # # C <- (s2)
        # 
        # A <- data.frame(A)
        # # B <- data.frame(B)
        # # C <- data.frame(C)
        # 
        # A$model <- "Bayesian Monte Carlo"
        # # B$model <- "Bayes proportions"
        # # C$model <- "Bayes logistic reg."
        # # 
        # A$parameter = rownames(A)
        # # B$parameter = rownames(B)
        # # C$parameter = rownames(C)
        # 
        # x <-A# rbind(A,B,C)
        # #x$parameter = rownames(x)
        # 
        # names(x) <- c("Mean","p2.5","p25","p50","p75","p975","Model","parameter")
        # x <- x[,c("Model","parameter","Mean","p2.5","p25","p50","p75","p975")]
        # 
        # 
        # rownames(x) <- NULL
        # x$Model <- NULL
        # foo <- x
        # datatable(x,
        #           
        #           rownames = FALSE,
        #           
        #           options = list(
        #               searching = TRUE,
        #               #pageLength = 20,
        #               paging=FALSE,
        #               lengthMenu = FALSE ,
        #               lengthChange = FALSE,
        #               autoWidth = FALSE
        #               # colReorder = TRUE,
        #               # deferRender = TRUE,
        #               # scrollY = 200,
        #               # scroller = T
        #           ))  %>% 
        #     formatRound(
        #         columns= c("parameter","Mean","p2.5","p25","p50","p75","p975"), digits=c(0,3,3,3,3,3,3)  )
        
    })
    
    # --------------------------------------------------------------------------
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    fisher <- reactive({
        
        # sample <- random.sample()
        # 
        # n1 <- sample$n1  
        # y1 <- sample$y1
        # n2 <- sample$n2
        # y2 <- sample$y2
        # 
        # n1 <- 50
        # y1 <- 15
        # n2 <- 50
        # y2 <- 10
        # 
        # 
        # #~~~~~~~~~~~~~~~~~~~~~~~~~~
        # data <- matrix(c(y1, n1-y1,y2,n2-y2),nr=2,dimnames=list(c("response","nonresponse"), c("trt","ctrl")))
        # f <- fisher.test(data)
        # 
        # #~~~~~~~~~~~~~~~~~~~~~~~~~~
        # res <- prop.test(x = c(y1, y2), n = c(n1, n2), correct = FALSE)
        # pr <- res 
        # 
        # #~~~~~~~~~~~~~~~~~~~~~~~~~~
        # # # logistic regression
        # 
        # Table <- t( matrix(c(y1,y2,n1-y1,n2-y2), nc=2))
        # f99 <- glm(as.table(Table) ~ c(1,0), family=binomial)
        # f100 <- exp(coef(f99))[2][[1]]
        # 
        # #~~~~~~~~~~~~~~~~~~~~~~~~~~
        # 
        # d = as.data.frame(as.table(as.matrix(data)))
        # 
        # # from stack exchange
        # countsToCases <- function(x, countcol = "Freq") {
        #     # Get the row indices to pull from x
        #     idx <- rep.int(seq_len(nrow(x)), x[[countcol]])
        #     # Drop count column
        #     x[[countcol]] <- NULL
        #     # Get the rows from x
        #     x[idx, ]
        # }
        # 
        # 
        # dd <- countsToCases(d)
        # rownames(dd)<-NULL
        # 
        # 
        # dd$y <- ifelse(dd$Var1 %in% "response",1,0)
        # dd$x <- ifelse(dd$Var2 %in% "trt",1,0)
        # dd$Var1 <- dd$Var2 <- NULL
        # 
        # #y <- dd$y; x <- dd$x
        # # summary(glm(y ~ x, family=binomial))
        # 
        # ddd <<- datadist(dd)
        # options( datadist = "ddd" )
        # harrell <- summary(lrm(y~x,dd))
        # 
        # 
        # #~~~~~~~~~~~~~~~~~~~~~~~~~~
        # return(list(f= f, pr=pr , logreg = harrell  ))   #, logreg = f99
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # output$fisher <- renderPrint({
    #     
    #     return(print(fisher()$f, digits=4))
    #     
    # })
    # output$prop <- renderPrint({
    #     
    #     return(print(fisher()$pr, digits=4))
    #     
    # })
    # 
    # output$logregx <- renderPrint({
    #     
    #     return(print(fisher()$logreg, digits=4))
    #     
    # })
    #~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # ---------------------------------------------------------------------------
    
    
})

# Run the application 
shinyApp(ui = ui, server = server)