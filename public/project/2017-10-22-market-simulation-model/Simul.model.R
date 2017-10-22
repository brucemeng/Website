# Part 1 - Market Simulation Model
# Model will simulate various market indexes ~N(mean, sd)

setwd("H:/BMO WM/Financial Modeling/Model/Part 1 - Market Simulation")

library(forecast)
library(magrittr)
library(readxl)
library(readr)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(quantmod)
library(Quandl)
set.seed(1)

## Global Model and Parameters
    # Number of future paths
    paths <- 10 #8 was original number
    
    # Number of months into the future
    nbr.of.months <- 12*5
    
    # Function to simulate based on parameters
    simul.model <- function(init.value = 0, mean.input = 0, sd.input = 0, num.month = 1) {
        change <- rnorm(num.month, mean.input, sd.input) #GLDEX or normal????
        simul.path <- cumprod(c(init.value, exp(change)))
        return(simul.path)
    }
    
    
## TSX simulation
    # Obtain TSX data
    getSymbols("^GSPTSE")
    data.TSX <- to.monthly(GSPTSE)
    data.TSX <- data.frame(data.TSX, Date = index(data.TSX))
    data.TSX$Date <- as.yearmon(data.TSX$Date)
    
    # Filter for only 2010 onwards
    data.TSX.2010 <- to.monthly(GSPTSE)
    data.TSX.2010 <- data.TSX.2010["2010::"]
    
    data.TSX.2010 <- data.frame(data.TSX.2010, Date = index(data.TSX.2010))
    data.TSX.2010$Date <- as.yearmon(data.TSX.2010$Date)
    
    # Compute change in levels
    data.TSX.2010.change <- data.TSX.2010[-c(1),]
    data.TSX.2010.change$change <- log(data.TSX.2010$GSPTSE.Close) %>% diff()
    #data.TSX.2010.change <- data.TSX.2010.change[-c(1),]
    
    TSX.last.price <- data.TSX.2010[nrow(data.TSX.2010),] %>%
                        data.frame()
    
    # Calculate mean and sd of log returns
    TSX.simul.params <- data.frame(mean = NA, sd = NA)
    TSX.simul.params$mean <- mean(data.TSX.2010.change$change)
    TSX.simul.params$sd <- sd(data.TSX.2010.change$change)
    
    # Replicate for MC simulation
    mc.TSX <- replicate(paths, simul.model(init.value = TSX.last.price$GSPTSE.Close,
                                         mean.input = TSX.simul.params$mean,
                                         sd.input = TSX.simul.params$sd,
                                         num.month = nbr.of.months)) %>%
              data.frame()
    
    mc.TSX <- rbind(replicate(paths, data.TSX[nrow(data.TSX)-1, "GSPTSE.Close"]), mc.TSX)
                
    
    mc.TSX$Avg <- apply(mc.TSX, 1, mean)
    
    # Generate Dates
    mc.TSX$Date <- (data.TSX$Date[nrow(data.TSX)-1])
    mc.TSX$Date <- as.yearmon(mc.TSX$Date)
    
    for (m in 2:(nrow(mc.TSX))) {
        mc.TSX$Date[m] <- mc.TSX$Date[m - 1] + 1/12
    }

## S&P500 Simulation
    # Obtain S&P500 data
    getSymbols("^GSPC")
    data.SP500 <- to.monthly(GSPC)
    data.SP500 <- data.frame(data.SP500, Date = index(data.SP500))
    data.SP500$Date <- as.yearmon(data.SP500$Date)
    
    # Compute change in levels
    data.SP500.change <- data.SP500[-c(1),]
    data.SP500.change$change <- log(data.SP500$GSPC.Close) %>% diff()
    #data.SP500.change <- data.SP500.change[-c(1),]
    
    SP500.last.price <- data.SP500[nrow(data.SP500),] %>%
        data.frame()
    
    # Calculate mean and sd of log returns
    SP500.simul.params <- data.frame(mean = NA, sd = NA)
    SP500.simul.params$mean <- mean(data.SP500.change$change)
    SP500.simul.params$sd <- sd(data.SP500.change$change)
    
    # Replicate for MC simulation
    mc.SP500 <- replicate(paths, simul.model(init.value = SP500.last.price$GSPC.Close,
                                           mean.input = SP500.simul.params$mean,
                                           sd.input = SP500.simul.params$sd,
                                           num.month = nbr.of.months)) %>%
                data.frame()
    
    mc.SP500 <- rbind(replicate(paths, data.SP500[nrow(data.SP500)-1, "GSPC.Close"]), mc.SP500)
    
    mc.SP500$Avg <- apply(mc.SP500, 1, mean)
    
    # Generate Dates
    mc.SP500$Date <- (data.SP500$Date[nrow(data.SP500)-1])
    mc.SP500$Date <- as.yearmon(mc.SP500$Date)
    
    for (m in 2:(nrow(mc.SP500))) {
        mc.SP500$Date[m] <- mc.SP500$Date[m - 1] + 1/12
    }

## Get FX data
    getSymbols("USD/CAD", src = "oanda")
    data.FX <- to.monthly(USDCAD)
    data.FX <- data.frame(data.FX, Date = index(data.FX))
    data.FX$Date <- as.yearmon(data.FX$Date)
    
    # Compute change in levels
    data.FX.change <- data.FX[-1,]
    data.FX.change$change.FX <- log(data.FX$USDCAD.Close) %>% diff()
    
## BoC Overnight rates
    Quandl.api_key("q7RtD2yYMyRABD32n5_4")
    boc.or <- Quandl("BOC/V39079", collapse = "monthly", type = "ts", start_date = "2009-12-01")/100
    
    boc.fit <- boc.or %>%
                    auto.arima()
    
    boc.model <- forecast(boc.fit, h = 90)
    boc.model.df <- data.frame(Date = index(boc.model$mean), OR = boc.model$mean)

## Write data
    write_csv(mc.TSX, "./Output/mc.TSX.csv")
    write_csv(mc.SP500, "./Output/mc.SP500.csv")
    write_csv(data.FX.change, "./Output/data.FX.csv")
    write_csv(boc.model.df, "./Output/boc.model.csv")
    