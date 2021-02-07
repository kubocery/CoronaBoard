#rm(list = ls())



model_fn <- function(r0, pop, days, rho, gamma,delta,  deathrate,  cum=FALSE){
  require(plotly)
  
  # r0<-2
  # pop<- 100000
  # days <- 400
  # startpop <- 30
  # 
  # 
  # kappa <- 0.03
  # lambda <- 1-kappa
  # gamma <- 1/2
  # beta <- r0*gamma
  # delta <- 1/4
  # rho <- 0.2


  r0<-r0
  pop<- pop
  days <- days
  startpop <- 100
  
  kappa <- deathrate
  lambda <- 1-kappa
  gamma <- 1/gamma
  delta <- 1/delta
  beta <- r0*delta
  rho <- rho
  
  dt <- 0.1
  t <- seq(0,days,by=dt)
  
  S<- numeric(length = length(t))
  E<- numeric(length = length(t))
  I<- numeric(length = length(t))
  Q<- numeric(length = length(t))
  D<- numeric(length = length(t))
  R<- numeric(length = length(t))
  P<- numeric(length = length(t))  
  
  
  S[1] <- 1-(startpop+10)/pop
  E[1] <- (startpop+10)/pop
  I[1] <-  startpop/pop
  R[1] <- 0
  Q[1]<-0
  D[1]<-0
  #P[1]<-0
  
  
  
  for (i in 1:(days/dt)) {
    S[i+1] <- S[i]-(rho*beta*S[i]*I[i])*dt
    E[i+1] <- E[i]+(rho*beta*S[i]*I[i] - gamma*E[i])*dt
    I[i+1] <- I[i]+(gamma*E[i]-delta*I[i])*dt
    Q[i+1] <- Q[i]+(delta*I[i] - lambda*Q[i]-kappa*Q[i])*dt
    R[i+1] <- R[i]+lambda*Q[i]*dt
    D[i+1] <- D[i]+kappa*Q[i]*dt
   # P[i+1] <- P[i]+rho*S[i]*dt
  }
  
  df <- data.frame(t=t, S=S, E=E*pop, I=I*pop,Q=Q, D=D, R=R)
  
  
  
  df$CSI <- 1-df$S
  
  
    hovertxt1 <- paste0("<b>Days since outbreak: </b>", df$t, "<br>",
                       "<b>Infected population: </b>", round(df$I, 0), "<br>")
    hovertxt2 <- paste0("<b>Days since outbreak: </b>", df$t, "<br>",
                        "<b>Exposed population: </b>", round(df$E, 0), "<br>")
    
    
   pl1 <-  plot_ly(df, x = ~t) %>%
      add_lines(y = ~I, name = 'Infected',line=list(color='#e31a1c'), text = hovertxt1, hoverinfo = "text") %>% 
      add_lines(y=~E, name = 'Exposed', line=list(color= '#1f78b4',dash='dash'),
                text = hovertxt2, hoverinfo = "text") %>%
    layout(title = "SEIR model simulation", font = list(size = 12),
            xaxis = list(title = '', showgrid = F, showline = T,
                         ticklen = 3,zeroline = T),
            yaxis = list(title = "Population affected", showline = F, ticks = "outside", showgrid = TRUE, 
                         rangemode = "tozero",ticklen = 0,zeroline = TRUE),
            #legend = list(x = 0.7, y = 0.99),
            margin = list(l=10,r=10,b=50,t=70))  
 
    hovertxt3 <- paste0("<b>Days since outbreak: </b>", df$t, "<br>",
                        "<b>Number of cases: </b>", round(df$CSI, 2)*100,'%',"<br>")
    hovertxt4 <- paste0("<b>Days since outbreak: </b>", df$t, "<br>",
                        "<b>Recovered patiens: </b>", round(df$R, 2)*100,'%', "<br>")
    hovertxt5 <- paste0("<b>Days since outbreak: </b>", df$t, "<br>",
                        "<b>Deceased patiens: </b>", round(df$D, 2)*100,'%', "<br>")
    
   pl2 <- plot_ly(df, x = ~t) %>%
      add_lines(y=~CSI, name = 'Infected',line=list(color='#1f78b4'), text = hovertxt3, hoverinfo = "text") %>% 
      add_lines(y=~R, name = 'Recovered',line=list(color='#33a02c'), text = hovertxt4, hoverinfo = "text") %>% 
      add_lines(y=~D, name = 'Deceased',line=list(color='#000000'), text = hovertxt5, hoverinfo = "text") %>% 
    layout(showlegend = FALSE,
           xaxis = list(title = 'Days since outbreak', showgrid = F, showline = T,
                        ticklen = 3,zeroline = T),
          yaxis = list(title = "Fraction of population", showline = F, ticks = "outside", showgrid = TRUE, 
                        rangemode = "tozero",ticklen = 0,zeroline = FALSE, range=c(0,1), tickformat = "%"),
           #legend = list(x = 0.8, y = 0.5))
           margin = list(l=10,r=10,b=50, t=70)) 
   
   subplot(list(pl1,pl2),nrows=2, shareX = F , margin = 0.05, titleY = T, titleX = T)
   
   }

