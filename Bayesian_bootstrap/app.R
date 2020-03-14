#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls())
library(ggplot2)
library(shiny) 
#require(LearnBayes)
#library(tidyverse)
library(shinyWidgets)
library(shinythemes)  # more funky looking apps
#library(rstan)
library(DT)
#require(rms) # freq logistic regression
library(shinyalert)
library(gtools)
library(Hmisc)
library(scales)
library(LaplacesDemon)
library(bayesboot)
library(boot)
 #options(mc.cores = parallel::detectCores())
#rstan_options(auto_write = TRUE)
options(max.print=1000000)
fig.width <- 400
fig.height <- 300
fig.width2 <- 1400
fig.height2 <- 300#50
fig.width3 <- 1400  
fig.height3 <- 600#400
fig.width4 <- 1380
fig.height4 <- 450

fig.width5 <- 1380
fig.height5 <- 600

p1 <- function(x) {formatC(x, format="f", digits=1)}
p2 <- function(x) {formatC(x, format="f", digits=2)}
p3 <- function(x) {formatC(x, format="f", digits=3)}
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
                
                h2("The Bayesian Bootstrap (philosophic numbers smooth)"),
                
                h4("There is a Bayesian analogue of the familiar frequentist bootstrap! [1]. We investigate the Bayesian bootstrap using functions coded up by me and 
                functions from R packages. We also run a frequentist bootstrap. We look at (i) estimating a mean from one sample from a normal distribution and 
                (ii) estimating a correlation coefficient between two groups of samples. In the case of the one sample mean, n-1 uniform samples are drawn and the size of the n gaps then become 
                the probabilities for incorporation of the original n samples into the boostrap sample [2]. On the third tab we also use a published dataset to estimate correlation [1]. 
                An observation with this app. is that the Bayesian approach works better with small samples. A more general advantage of the Bayesian bootstrap with respect to the resulting 
                inferences about parameters is that 
                the Bayesian approach generates likelihood statements about parameters, rather than frequency statements about statistics under assumed values for parameters.

              "), 
                
                h3("  "), 
                
                
                sidebarLayout(
                    
                    sidebarPanel( width=3 ,
                                  
                                  tags$style(type="text/css", ".span8 .well { background-color: #00FFFF; }"),
                                  
                                  
                                  actionButton(inputId='ab1', label="R code",   icon = icon("th"),   
                                               onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/Bayesian_bootstrap/master/Bayesian_bootstrap/app.R', '_blank')"),    
                                  actionButton("resample", "Rerun the Monte Carlo simulation"),
                                  br(), # br(), 
                                  tags$style(".well {background-color:#b6aebd ;}"), ##ABB0B4AF
                                  
                                  h4("The first input below is the number of Monte Carlo simulations which impacts tabs 1 to 3. 
                                  The next two inputs impact only tab 1, the number of samples and
                                     the true mean and standard deviation. Tab 2 estimates the uncertainty in a correlation 
                                     coefficient and the last two inputs, the number of samples (in each of two groups) and the true correlation between
                                     the two groups come into play here. The fouth tab presents the published data."),
                                  div(
                                      
                                      tags$head(
                                          tags$style(HTML('#ab1{background-color:orange}'))
                                      ),
                                      
                                      tags$head(
                                          tags$style(HTML('#resample{background-color:orange}'))
                                      ),
                                     # h4("------------------------------------All tabs------------------------------------"),
                                      textInput('sims', 
                                                div(h5("Monte Carlo simulations (all tabs)")), "1000"),
                                      #h4("------------------------------------tab 2 inputs only-------------------------"),
                                      textInput('vec3', 
                                                div(h5("Number of samples for mean (tab 1)")), "100"),
                                      
                                      textInput('vec4', 
                                                div(h5("Enter the true mean and sd for a normal distribution (tab 1)")), "10, 5"),
                                      #h4("------------------------------------tab 3 inputs only-------------------------"),
                                      textInput('n1y1', 
                                                div(h5("Number of samples for Correlation (tab 2)")), "10"),
                                      
                                      textInput('n2y2', 
                                                div(h5("Enter the true correlation (tab 2)")), ".8"),
                                      #h4("-----------------------------------------------------------------------------------"),
                                 
                                      div(h5("References:")),  
                                      
                                      tags$a(href = "https://projecteuclid.org/download/pdf_1/euclid.aos/1176344552", "[1] Efron's paper"),
                                      div(p(" ")),
                                      tags$a(href = "https://projecteuclid.org/download/pdf_1/euclid.aos/1176345338", "[2] Rubin's paper"),
                                      div(p(" ")),
                                      tags$a(href = "https://blogs.sas.com/content/iml/2017/09/20/fishers-transformation-correlation.html", "[3] Fisher's z transformation"),
                                      div(p(" ")),
                                      # tags$a(href = "https://www.tjmahr.com/bayesian-fisher-exact-test/", "[4] Blog article"),
                                      # div(p(" ")),

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
                                  
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  tabPanel("1 Estimating a one sample mean", value=7, 
                                           
                                                    h4("Using the the inputs left we can control the number of Monte Carlo simulations, the sample size and 
                                                    the true population mean and standard deviation. We present from the left, the familiar frequentist bootstrap approach. 
                                                    With the first Bayesian bootstrap approach samples are selected with replacement by drawing n-1 random uniform values between 0 and 1 
                                              and the n gap sizes are then the probability for inclusion of the original samples into a bootstrap sample, the mean is then estimated. 
                                              The next Bayesian bootstrap distribution is generated using the Dirichlet distribution with n draws from this distribution
                                              (the gaps between uniform random variables follow the Dirichlet distribution) to derive the probability of inclusion.
                                               The result from a t-test is also provided in the table. The median and 2.5 and 97.5 percentiles are presented for each distribution."),
                                                    
                                           h4(paste("Figure 1. Bayesian and frequentist bootstrap distributions, estimating one sample mean")), 
                                           div(plotOutput("diff", width=fig.width4, height=fig.height4)),       
                                            
                                           fluidRow(
                                               column(width = 5, offset = 0, style='padding:1px;',
                                                      h4("T-test mean and 95% confidence interval"), 
                                                      div( verbatimTextOutput("reg.summary2"))
                                               )),
                                   ) ,
                        
                                  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  tabPanel("2 Estimating correlation using simulated data", value=3, 
                h4("Using the the inputs left we can specify the number of Monte Carlo simulations, the sample size and the true population correlation.
               We present clockwise from top left, 
                                              a Bayesian bootstrap approach in which n paired samples are selected with replacement by drawing n-1 random uniform values between 0 and 1
                                              and the
                                              n gap sizes are then the probability for inclusion in a bootstrap sample, for which correlation is then estimated. The next approach 
                                              is from the LaplaceDemon package, followed by the frequentist bootstrap and lastly the function in the bayesboot package. 
                    We use the Fisher z transformation to plot each distribution. The median and 2.5 and 97.5 percentiles are presented for each distribution."),
                h4(paste("Figure 2. Bayesian and frequentist bootstrap distributions, estimating correlation")), 
                                           div(plotOutput("diff3", width=fig.width5, height=fig.height5)),
                                           
                                  ),
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                  tabPanel("3 Estimating correlation using Efron's dataset", value=3, 
                                           h4("We use the dataset quoted in Efron's bootstrap paper, a small dataset of two groups, each
                                              of 9 samples [1]. Figure 1 in the paper is very similar to the frequentist histogram here.
                                              We use the Fisher's z transformation to plot each distribution. The only user input that has
                                              impact here are the number of Monte Carlo simulations. We present clockwise from top left, 
                                              an approach in which n paired samples are selected with replacement by drawing n-1 random uniform values between 0 and 1 and the
                                              n gap sizes are then the probability for inclusion in a bootstrap sample. for which correlation is then estimated. The next approach 
                                              is from the LaplaceDemon package, followed by the frequentist bootstrap and lastly the function in the bayesboot package.
                                                  The median and 2.5 and 97.5 percentiles are presented for each distribution."),
                                           h4(paste("Figure 3. Bayesian and frequentist bootstrap distributions, estimating correlation")),  
                                           div(plotOutput("diff4", width=fig.width5, height=fig.height5)),
                                           
                                  ),
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                tabPanel("4 Efron's dataset", value=3, 
                         h4("Efron's dataset."),
                         
                         fluidRow(
                           column(
                         DT::dataTableOutput("tablex"),width = 6)
                         )
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~             
                           ) 
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                        )
                 #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                    )
                 ) 
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~end tab panels 
                
)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

server <- shinyServer(function(input, output   ) {
    
    shinyalert("You better believe it, there's a Bayesian bootstrap!",
               "And It's pretty, pretty good",
               type = "info")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # This is where a new sample is instigated 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    random.sample <- reactive({
        
        foo <- input$resample
        
        #sample sizes
        trt <- as.numeric(unlist(strsplit(input$vec3,",")))
        # mean and sD
        ctr <- as.numeric(unlist(strsplit(input$vec4,",")))
        
        #sample size for correlation
        n1y1 <- as.numeric(unlist(strsplit(input$n1y1,","))) #trt
        # R
        n2y2 <- as.numeric(unlist(strsplit(input$n2y2,",")))
        
        sims <- as.numeric(unlist(strsplit(input$sims,",")))
        
        return(list(  
            trt.alpha=trt[1],  
            ctr.alpha=ctr[1], ctr.beta=ctr[2],
            n1=n1y1[1], 
            n2=n2y2[1], 
            sims=sims 
        )) 
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # tab 1 one sample mean estimation
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    mcmc <- reactive({
        
        sample <- random.sample()
        
        n   <- trt.alpha <- sample$trt.alpha
        mu1 <- sample$ctr.alpha
        sd1 <- sample$ctr.beta
 
        reps <- sims <- sample$sims
        
        x <- rnorm(n, mean=mu1, sd=sd1)
          
        #require(Hmisc)
        dboot <- function(data.set) {
            u <- c(0, sort(runif(length(data.set) - 1)), 1)
            g <- diff(u)
            #return( wtd.quantile(data.set, weights=g, probs=.5, normwt=T))
            return( wtd.mean(data.set, weights=g))
        }
        
        #http://rsnippets.blogspot.ie/2012/11/simple-bayesian-bootstrap.html
       # library(gtools)
        
        # Bayesian bootstrap
        mean.bb <- function(x, n) {
            apply(rdirichlet(n, rep(1, length(x))), 1, weighted.mean, x = x)
        }
        
        # standard bootstrap
        mean.fb <- function(x, n) {
            replicate(n, mean(sample(x, length(x), TRUE)))
        }
        
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
        
        res <- res[4,]
        
        return(list(res=res , A=A, B=B, C=C , mu1=mu1)) 
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # tab 1 one sample mean ggplot
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$diff <- renderPlot({         

      sample <- random.sample()
      mu1 <- sample$ctr.alpha
      sd1 <- sample$ctr.beta
      n    <- sample$n1
 
      A <- mcmc()$A
      B <- mcmc()$B
      C <- mcmc()$C
      
      foo <- cbind(A,B,C)
      foo1 <- reshape::melt(foo)

      levels(foo1$X2)
      
      Ae <-  est <- quantile(A, c(.025,.5,.975)) 
      A <-  paste("Frequentist : Median",p3(est[2][[1]]),", 95%CI (", p3(est[1][[1]]) ,", ",  p3(est[3][[1]]) ,")")  
      
      Be <-   est <- quantile(B, c(.025,.5,.975))  
      B <-  paste("Bayesian : Median",p3(est[2][[1]]),", 95%CI (", p3(est[1][[1]]) ,", ",  p3(est[3][[1]]) ,")") 
      
      Ce<-  est <- quantile(C, c(.025,.5,.975))  
      C <-  paste("Bayesian2 : Median",p3(est[2][[1]]),", 95%CI (", p3(est[1][[1]]) ,", ",  p3(est[3][[1]]) ,")") 
      
        #make a dataset to add lines to ggplot facets
      
      dummy2 <- data.frame(X2=c(paste("",A),
                                paste("",B),
                                paste("",C)
      ),  
      q1 =  c(Ae[1], Be[1], Ce[1]),
      q50 = c(Ae[2], Be[2], Ce[2]),
      q3 =  c(Ae[3], Be[3], Ce[3])
      ) 
      
      
      levels(foo1$X2) <- c(paste("",A),
                           paste("",B),
                           paste("",C) 
      )
      
       r <-  mu1+5* sd1
       p <-  seq(-r, r, sd1/n) #(r--r)/10)
      
       #library(scales)
      g0 <- ggplot(data=foo1, aes(x = value)) +#
        geom_vline(data = dummy2, aes(xintercept = q1,  colour="red", linetype = "dotdash")) +
        geom_vline(data = dummy2, aes(xintercept = q50, colour="red", linetype = "dotdash")) +
        geom_vline(data = dummy2, aes(xintercept = q3,  colour="red", linetype = "dotdash")) +
        geom_vline( aes(xintercept = mu1,  colour="black", linetype = "dash")) +
        geom_histogram( aes(y = ..density..), bins=100, colour="black" , fill=rainbow(300))+     ylab("")+
        geom_density(alpha = 0.1, fill = "red") +
        facet_wrap(X2~ .) 
      
      
      g0 <- g0  + scale_x_continuous(breaks= p, 
                                     xlab("Mean"),
                                     oob=discard) +
        #xlim(-Inf,Inf) +
        scale_y_continuous(breaks = NULL) +
        
        theme_bw()  
      
      g0 <- g0 + theme(#axis.line=element_blank(),
        #axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        #axis.ticks=element_blank(),
        #axis.title.x=element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        #axis.title.y=element_blank(),
        legend.position="none",
        #anel.background=element_blank(),
        #panel.grid.major=element_blank(),
        #panel.grid.minor=element_blank(),
        # plot.background=element_blank())
        #plot.margin = unit(c(1,1,1,1), "cm")
        plot.title = element_text(size = 16),
        strip.text.x = element_text(size = 16, colour = "black", angle = 0),
        strip.background = element_rect(fill="ivory"),
        panel.border = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")
      )
      
      g0 <- g0 + labs(#title = "MLB run scoring, 1901-2015",
                      #subtitle = "Run scoring has been falling for 15 years, reversing a 30 year upward trend",
                      caption = "The dotted lines indicate the median and 2.5 and 97.5 percentiles, the red line is the true population mean" 
        ) 
      
      print(g0)

    })
  
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # not used replaced by diff, ggplot 
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    output$diffx <- renderPlot({         
      
      z <- mcmc()$A
      mu1 <- mcmc()$mu1
      
      q <- quantile(z,c(.025, 0.25, 0.5, 0.75, 0.975))
      par(bg = 'lightgoldenrodyellow') 
      par(mfrow=c(1,3))
      plot(density(z),
           xlab="Frequentist Bootstrap, Mean estimate",
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
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$reg.summary2 <- renderPrint({
        
        return(print(mcmc()$res, digits=4))
        
    })
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # run the correlation analysis for tab 2
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
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
        
        data.set <- data.frame(z[,1],z[,2])
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        sboot <- function() {
            cor(data.set[sample(1:n, replace=T),])[1,2]
        }
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        bboot <- function() {
            cov.wt(data.set, diff(c(0,sort(runif(n-1)),1)), cor=T)$cor[1,2]
        }
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        X <- matrix(c(z[,1],z[,2]), length(z[,1]), 2)
        #library(LaplacesDemon)
        BB <- BayesianBootstrap(X=X, n=sims,
                                Method=function(x,w) cov.wt(x, w, cor=TRUE)$cor[1,2]) 
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        #library(bayesboot)
        # Using the weighted correlation (corr) from the boot package.
        #library(boot)
        b4 <- bayesboot(data.set, corr, R = sims, use.weights = TRUE)
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        f <- replicate(sims, sboot())
        b <- replicate(sims, bboot())
        BB <- unlist(BB)
        xx1 <- b4$V1
        
        return(list(f=f, b=b, BB=BB, xx1=xx1)) 
        
    })

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #NOT USED REPLACED BY FACET PLOT
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    output$diff2 <- renderPlot({         
    
        
        f <- cor1()$f
        b  <- cor1()$b
        BB <-cor1()$BB
        xx1 <- cor1()$xx1
        
        ff <- qlogis(f)
        bb <- qlogis(b)
        BBB <- qlogis(BB)
        xx <- qlogis(xx1)
        
        all <- c(f,b,BB,xx1)
        minx <- min(all)
        maxx <- max(all)
        bz <- c(minx, maxx)
         

        a <- x <- c(0.001, 0.003,0.01, 0.05,seq(.1,0.9,0.1),.95,.98,.99,0.995,0.999, 0.9995,0.9999) 
        
        q <- qlogis(x)   
        
        lims <- unique(a[sapply(bz,function(x) which.min(abs(x-a)))])
        
        indx <- which(x %in% lims)
        #limz <- x[indx[1]:indx[2]]
         i = indx[1]-1
         if (i %in% 0) {i=1} 
         j=  indx[2]+1
         limitz = c(q[i], q[j])
         breakz = q[i:j]
         labelz = x[i:j]
        
        require(ggplot2)
        x1 <- xlab("")
        est <- quantile(f, c(.025,.5,.975)) 
        ff <- as.data.frame(ff)
        
        pL1<- ggplot(data = ff, aes(x = ff)) + x1+   
            geom_histogram(bins = 100, fill = rainbow(100))+
            scale_x_continuous(limits =limitz,
                                breaks= breakz,  # this is where the values go
                                labels= labelz)   + 
          
          
            labs(title = paste("Frequentist bootstrap: Median",p3(est[2][[1]]),", 95%CI ("
                               , p3(est[1][[1]]) ,", ",  p3(est[3][[1]]) ,")") ) +
            theme_bw()  
        pL1 <- pL1 + theme(axis.line=element_blank(),
                           #axis.text.x=element_blank(),
                           #axis.text.y=element_blank(),
                           #axis.ticks=element_blank(),
                           #axis.title.x=element_blank(),
                           axis.text=element_text(size=14),
                           axis.title=element_text(size=12,face="bold"),
                           #axis.title.y=element_blank(),
                           # legend.position="none",
                           panel.background=element_blank(),
                           panel.border=element_blank(),
                           #panel.grid.major=element_blank(),
                           #panel.grid.minor=element_blank(),
                           # plot.background=element_blank())
                           #plot.margin = unit(c(1,1,1,1), "cm")
                           plot.title = element_text(size = 14)
                           
        )
        
        est <- quantile(b, c(.025,.5,.975))  
        bb <- as.data.frame(bb)
        pL2<- ggplot(data = bb, aes(x = bb)) +x1 +
            geom_histogram(bins = 100, fill = rainbow(100))+
          scale_x_continuous(limits =limitz,
                             breaks= breakz,  # this is where the values go
                             labels= labelz)   + 
            labs(title = paste("Bayesian bootstrap: Median",p3(est[2][[1]]),", 95%CI (", p3(est[1][[1]]) ,", ",  p3(est[3][[1]]) ,")") ) +
            theme_bw()  
        pL2 <- pL2 + theme(axis.line=element_blank(),
                           #axis.text.x=element_blank(),
                           #axis.text.y=element_blank(),
                           #axis.ticks=element_blank(),
                           #axis.title.x=element_blank(),
                           axis.text=element_text(size=14),
                           axis.title=element_text(size=12,face="bold"),
                           #axis.title.y=element_blank(),
                           # legend.position="none",
                           panel.background=element_blank(),
                           panel.border=element_blank(),
                           #panel.grid.major=element_blank(),
                           #panel.grid.minor=element_blank(),
                           # plot.background=element_blank())
                           #plot.margin = unit(c(1,1,1,1), "cm")
                           plot.title = element_text(size = 14)
        )
        
        est <- quantile(BB, c(.025,.5,.975))  
        BBB<- as.data.frame(BBB)
        pL3<- ggplot(data = BBB, aes(x = BBB)) +x1+ 
            geom_histogram(bins = 100, fill = rainbow(100))+
          scale_x_continuous(limits =limitz,
                             breaks= breakz,  # this is where the values go
                             labels= labelz)   + 
            labs(title = paste("LaplaceDemon bootstrap: Median",p3(est[2][[1]]),", 95%CI (", p3(est[1][[1]]) ,", ",  p3(est[3][[1]]) ,")") ) +
            theme_bw()  
        pL3 <- pL3 + theme(axis.line=element_blank(),
                           #axis.text.x=element_blank(),
                           #axis.text.y=element_blank(),
                           #axis.ticks=element_blank(),
                           #axis.title.x=element_blank(),
                           axis.text=element_text(size=14),
                           axis.title=element_text(size=12,face="bold"),
                           #axis.title.y=element_blank(),
                           # legend.position="none",
                           panel.background=element_blank(),
                           panel.border=element_blank(),
                           #panel.grid.major=element_blank(),
                           #panel.grid.minor=element_blank(),
                           # plot.background=element_blank())
                           #plot.margin = unit(c(1,1,1,1), "cm")
                           plot.title = element_text(size = 14)
        )
        
        
        x1 <- xlab(" ")
        est <- quantile(xx1, c(.025,.5,.975))  
        xx<- as.data.frame(xx)
        pL4<- ggplot(data = xx, aes(x = xx)) +x1+ 
            geom_histogram(bins = 100, fill = rainbow(100))+
          scale_x_continuous(limits =limitz,
                             breaks= breakz,  # this is where the values go
                             labels= labelz)   + 
            labs(title = paste("bayesboot bootstrap: Median",p3(est[2][[1]]),", 95%CI (", p3(est[1][[1]]) ,", ",  p3(est[3][[1]]) ,")") ) +
            theme_bw()  
        pL4 <- pL4 + theme(axis.line=element_blank(),
                           #axis.text.x=element_blank(),
                           #axis.text.y=element_blank(),
                           #axis.ticks=element_blank(),
                           #axis.title.x=element_blank(),
                           axis.text=element_text(size=14),
                           axis.title=element_text(size=12,face="bold"),
                           #axis.title.y=element_blank(),
                           # legend.position="none",
                           panel.background=element_blank(),
                           panel.border=element_blank(),
                           #panel.grid.major=element_blank(),
                           #panel.grid.minor=element_blank(),
                           # plot.background=element_blank())
                           #plot.margin = unit(c(1,1,1,1), "cm")
                           plot.title = element_text(size = 14)
        )
        
        
        
        
        
        gridExtra::grid.arrange(pL1,  pL2, pL3,  pL4, nrow=2) 
        
    })
        
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # correlation plot tab 2
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    output$diff3 <- renderPlot({         
          
          f <- cor1()$f
          b  <- cor1()$b
          BB <-cor1()$BB
          xx1 <- cor1()$xx1
          
          sample <- random.sample()
       
          r    <- sample$n2

        ff <-  (f)
        bb <- (b)
        BBB <- (BB)
        xx <- (xx1)
        foo <- cbind(ff,bb,BBB,xx)
        foo1 <- reshape::melt(foo)
 
        
        levels(foo1$X2)
        
      Ce <-  est <- quantile(f, c(.025,.5,.975)) 
      C <-  paste("Frequentist : Median",p3(est[2][[1]]),", 95%CI (", p3(est[1][[1]]) ,", ",  p3(est[3][[1]]) ,")")  

     Ae <-   est <- quantile(b, c(.025,.5,.975))  
      A <-  paste("Bayesian : Median",p3(est[2][[1]]),", 95%CI (", p3(est[1][[1]]) ,", ",  p3(est[3][[1]]) ,")") 

      Be<-  est <- quantile(BB, c(.025,.5,.975))  
      B <-  paste("LaplaceDemon : Median",p3(est[2][[1]]),", 95%CI (", p3(est[1][[1]]) ,", ",  p3(est[3][[1]]) ,")") 

     De<-    est <- quantile(xx1, c(.025,.5,.975))  
     D <-   paste("bayesboot : Median",p3(est[2][[1]]),", 95%CI (", p3(est[1][[1]]) ,", ",  p3(est[3][[1]]) ,")")  

     # make a dataset to add lines to ggplot facets
     
     dummy2 <- data.frame(X2=c(paste("",A),
                            paste("",B),
                            paste("",C),
                            paste("",D)
     ),  
                          q1 =  c(Ae[1], Be[1], Ce[1], De[1]),
                          q50 = c(Ae[2], Be[2], Ce[2], De[2]),
                          q3 =  c(Ae[3], Be[3], Ce[3], De[3])
                          )
    
     
         levels(foo1$X2) <- c(paste("",A),
                              paste("",B),
                              paste("",C),
                              paste("",D)
                              )
         
        p <- sort(c(-.9, .9,-.99,.99 ,.999,.9999,1,-.95, .95,.8,-.8,seq(-.6,.6,0.3))  )
        
       # library(scales)
       g0 <- ggplot(data=foo1, aes(x = value)) +#
          geom_vline(data = dummy2, aes(xintercept = q1,  colour="red", linetype = "dotdash")) +
          geom_vline(data = dummy2, aes(xintercept = q50, colour="red", linetype = "dotdash")) +
          geom_vline(data = dummy2, aes(xintercept = q3,  colour="red", linetype = "dotdash")) +
         geom_vline( aes(xintercept = r,  colour="black", linetype = "dash")) +
         
         geom_histogram( aes(y = ..density..), bins=100, colour="black" , fill=rainbow(400))+     ylab("")+
          geom_density(alpha = 0.1, fill = "red") +
          facet_wrap(X2~ .) 
        
   
          g0 <- g0  + scale_x_continuous(trans = atanh_trans()  ,
                                        breaks= p, 
                                        xlab("Correlation"),
                                        oob=discard) +
            #xlim(-Inf,Inf) +
            scale_y_continuous(breaks = NULL) +

      theme_bw()  
   
      g0 <- g0 + theme(#axis.line=element_blank(),
                         #axis.text.x=element_blank(),
                         #axis.text.y=element_blank(),
                         #axis.ticks=element_blank(),
                         #axis.title.x=element_blank(),
                         axis.text=element_text(size=12),
                         axis.title=element_text(size=12,face="bold"),
                         #axis.title.y=element_blank(),
                         legend.position="none",
                         #anel.background=element_blank(),
                         #panel.grid.major=element_blank(),
                         #panel.grid.minor=element_blank(),
                         # plot.background=element_blank())
                         #plot.margin = unit(c(1,1,1,1), "cm")
                         plot.title = element_text(size = 16),
                       strip.text.x = element_text(size = 16, colour = "black", angle = 0),
                       strip.background = element_rect(fill="ivory"),
                       panel.border = element_blank(),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       axis.line = element_line(colour = "black")
                       )
        print(g0)
        
    })
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # ruben's data
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
      ruben <- reactive({
          
          dye <- c(1.15, 1.7, 1.42, 1.38, 2.8, 4.7, 4.8, 1.41, 3.9)
          efp <- c(1.38, 1.72, 1.59, 1.47, 1.66, 3.45, 3.87, 1.31, 3.75)
          data.set <- data.frame(dye,efp)
          
          return(list(data.set=data.set)) 

        })

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # ruben's data analysis correlation plot tab 3
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      
        output$diff4 <- renderPlot({      
          
          sample <- random.sample()
          
          data.set <- ruben()$data.set
          
          reps <- sims <- sample$sims
          
         # library(LaplacesDemon)
          len <- length(data.set$dye)
          
            sboot <- function() {
              cor(data.set[sample(1:len, replace=T),])[1,2]
            }
            
            bboot <- function() {
              cov.wt(data.set, diff(c(0,sort(runif(len-1)),1)), cor=T)$cor[1,2]
            }
            
           A <- data.set$dye
           B <- data.set$efp
            
          X <- matrix(c(A, B), len, 2)
          colnames(X) <- c("dye","efp")
          BB <- BayesianBootstrap(X=X, n=sims,
                                  Method=function(x,w) cov.wt(x, w, cor=TRUE)$cor[1,2]) 
           
         # library(bayesboot)
          # Using the weighted correlation (corr) from the boot package.
        #  library(boot)
          b4 <- bayesboot(data.set, corr, R = sims, use.weights = TRUE)
           
          
          f<-replicate(sims, sboot())
          b<-replicate(sims, bboot())
          BB <- unlist(BB)
          xx1 <- b4$V1
            
          ff <-  (f)
          bb <- (b)
          BBB <- (BB)
          xx <- (xx1)
          foo <- cbind(ff,bb,BBB,xx)
          foo1 <- reshape::melt(foo)
          
          levels(foo1$X2)
          
          Ce <-  est <- quantile(f, c(.025,.5,.975)) 
          C <-  paste("Frequentist : Median",p3(est[2][[1]]),", 95%CI (", p3(est[1][[1]]) ,", ",  p3(est[3][[1]]) ,")")  
          
          Ae <-   est <- quantile(b, c(.025,.5,.975))  
          A <-  paste("Bayesian : Median",p3(est[2][[1]]),", 95%CI (", p3(est[1][[1]]) ,", ",  p3(est[3][[1]]) ,")") 
          
          Be<-  est <- quantile(BB, c(.025,.5,.975))  
          B <-  paste("LaplaceDemon : Median",p3(est[2][[1]]),", 95%CI (", p3(est[1][[1]]) ,", ",  p3(est[3][[1]]) ,")") 
          
          De<-    est <- quantile(xx1, c(.025,.5,.975))  
          D <-   paste("bayesboot : Median",p3(est[2][[1]]),", 95%CI (", p3(est[1][[1]]) ,", ",  p3(est[3][[1]]) ,")")  
          
          # make a dataset to add lines to ggplot facets
          
          dummy2 <- data.frame(X2=c(paste("",A),
                                    paste("",B),
                                    paste("",C),
                                    paste("",D)
          ),  
          q1 =  c(Ae[1], Be[1], Ce[1], De[1]),
          q50 = c(Ae[2], Be[2], Ce[2], De[2]),
          q3 =  c(Ae[3], Be[3], Ce[3], De[3])
          )
          
          levels(foo1$X2) <- c(paste("",A),
                               paste("",B),
                               paste("",C),
                               paste("",D)
          )
          
          p <- c(-.9, -.99,.99 , -.95, .95, .999, .9999,c(-.8,-.4,0,.5,.8))  
          #library(scales)
          g0 <- ggplot(data=foo1, aes(x = value)) +#
            geom_vline(data = dummy2, aes(xintercept = q1,  colour="red", linetype = "dotdash")) +
            geom_vline(data = dummy2, aes(xintercept = q50, colour="red", linetype = "dotdash")) +
            geom_vline(data = dummy2, aes(xintercept = q3,  colour="red", linetype = "dotdash")) +
            geom_histogram(aes(y = ..density..), bins=100, colour="black" , fill=rainbow(400))+  ylab("")+
            geom_density(alpha = 0.1, fill = "red") +
            facet_wrap(X2~ .) 
          
           
          
          g0 <- g0  + scale_x_continuous(trans = atanh_trans()  ,
                                         breaks= p, xlab("Correlation"),
                                         oob=discard) +
            scale_y_continuous(breaks = NULL) +
            
            theme_bw()  
          
          g0 <- g0 + theme(#axis.line=element_blank(),
            #axis.text.x=element_blank(),
            #axis.text.y=element_blank(),
            #axis.ticks=element_blank(),
            #axis.title.x=element_blank(),
            axis.text=element_text(size=12),
            axis.title=element_text(size=12,face="bold"),
            #axis.title.y=element_blank(),
            legend.position="none",
            #anel.background=element_blank(),
            #panel.grid.major=element_blank(),
            #panel.grid.minor=element_blank(),
            # plot.background=element_blank())
            #plot.margin = unit(c(1,1,1,1), "cm")
            plot.title = element_text(size = 16),
            strip.text.x = element_text(size = 16, colour = "black", angle = 0),
            strip.background = element_rect(fill="ivory"),
            panel.border = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black")
          )
          print(g0)
          
          
        })
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # print Ruben's data
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

       output$tablex <- DT::renderDataTable({
        
        d <- ruben()$data.set
     
        x <- d
 
        foo <- x
         datatable(x,
                   
                   rownames = TRUE,
                   
                   options = list(
                       searching = TRUE,
                       #pageLength = 20,
                       paging=FALSE,
                       lengthMenu = FALSE ,
                       lengthChange = FALSE,
                       autoWidth = TRUE
                       # colReorder = TRUE,
                       # deferRender = TRUE,
                       # scrollY = 200,
                       # scroller = T
                   ))  %>% 
             formatRound(
                 columns= c("dye","efp"), digits=c(2,2)  ) 
        
    })
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
})

# Run the application 
shinyApp(ui = ui, server = server)