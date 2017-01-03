library(tidyr)
library(dplyr)
library(ggplot2)

vei <- read.csv("gap2.csv", header = TRUE, sep = ";")
cpi <-read.csv("cpi.csv", header = TRUE, sep = ";")
cdr <-read.csv("cdr.csv", header = TRUE, sep = ";")
cbr <-read.csv("cbr.csv", header = TRUE, sep = ";")
gdp <- read.csv("gdp.csv", header = TRUE, sep = ";")


vei$X2002 <- round(vei$X2002, 0)
vei$X2003 <- round(vei$X2003, 0)
vei$X2004 <- round(vei$X2004, 0)
vei$X2005 <- round(vei$X2005, 0)
vei$X2006 <- round(vei$X2006, 0)
vei$X2007 <- round(vei$X2007, 0)

vei$X2002 <- as.integer(vei$X2002)
vei$X2003 <- as.integer(vei$X2003)
vei$X2004 <- as.integer(vei$X2004)
vei$X2005 <- as.integer(vei$X2005)
vei$X2006 <- as.integer(vei$X2006)
vei$X2007 <- as.integer(vei$X2007)



gap <- gather(vei,"Country")
names(gap)[2] <- paste("ano")

gap <- inner_join(gap,cpi, "Country")
gap <- inner_join(gap,cbr,"Country")
gap <- inner_join(gap,cdr,"Country")
gap <- inner_join(gap,gdp,"Country")

gap$GDP_decimal <- gap$GDP
gap$cbr_decimal <- gap$cbr
gap$cdr_decimal <- gap$cdr
gap$cpi_decimal <- gap$CPI

ggplot(gap, aes(x = value)) +
  geom_histogram()

ggplot(gap, aes(x = value)) +
  geom_histogram(binwidth = 50)

gap$classe <- NA
gap$GDP <- round(gap$GDP, 0)
gap$GDP <- as.integer(gap$GDP)

gap$classe[gap$GDP <= 52056] <- "muito alto"
gap$classe[gap$GDP <= 12000] <- "alto"
gap$classe[gap$GDP <= 3000] <- "médio"
gap$classe[gap$GDP <= 700] <- "baixo"
gap$classe[gap$GDP <= 110] <- " muito baixo"

ggplot(gap, aes(x = classe, y = value)) +
  geom_boxplot()

gap$CPI <- round(gap$CPI, 0)
gap$CPI <- as.integer(gap$CPI)

gap$cbr <- round(gap$cbr, 0)
gap$cbr <- as.integer(gap$cbr)

gap$cdr <- round(gap$cdr, 0)
gap$cdr <- as.integer(gap$cdr)

gap$classe_CPI <- "pouco corrupto"
gap$classe_CPI[gap$CPI <= 6] <- "médio"
gap$classe_CPI[gap$CPI <= 2] <- "alta corrupção"


gap$classe_cbr <- "alto"

gap$classe_cbr[gap$cbr <= 28] <- "médio"
gap$classe_cbr[gap$cbr <= 12] <- "baixo"


gap$classe_cdr <- "alto"
gap$classe_cdr[gap$cdr <= 11] <- "médio"
gap$classe_cdr[gap$cdr <= 6] <- "baixo"

ggplot(gap, aes(x = classe_CPI, y = value)) +
  geom_boxplot()

ggplot(gap, aes(x = classe_cbr, y = value)) +
  geom_boxplot()

ggplot(gap, aes(x = classe_Cdr, y = value)) +
  geom_boxplot()


p1 <- ggplot(gap, aes(y = value, x = cpi_decimal)) +
          geom_point() +
        geom_smooth()
 
 p2 <- ggplot(gap, aes(y = value, x = cdr_decimal)) +
            geom_point() +
        geom_smooth()
 p3 <- ggplot(gap, aes(y = value, x = cbr_decimal)) +
        geom_point() +
        geom_smooth()
 
 p4 <- ggplot(gap, aes(y = value, x = GDP_decimal)) +
            geom_point() +
        geom_smooth()
 library(gridExtra)
 
 grid.arrange(p1,p2,p3,p4, ncol = 2)
 
 cor.test(gap$value, gap$cpi_decimal)
 cor.test(gap$value, gap$cdr_decimal)
 cor.test(gap$value, gap$cbr_decimal)
 cor.test(gap$value, gap$GDP_decimal)
 
 
 #cpi - correlação 0.75
 #cdr - correlação 0.037
 #cbr - correlação 0.062
 #gdp - correlação 0.75
 
 poor_coutries <- subset(gap, gap$classe == 'baixo')
 cor.test(poor_coutries$value, poor_coutries$GDP_decimal)
 
 poor_coutries <- subset(gap, gap$classe == 'médio')
 poor_coutries <- subset(gap, gap$classe == 'alto')
 poor_coutries <- subset(gap, gap$classe == 'muito alto')
 
 ggplot(gap, aes(y = value, x = cbr_decimal)) +
        geom_point(aes(colour = classe))
 
 ggplot(gap, aes(y = value, x = cdr_decimal)) +
        geom_point(aes(colour = classe))
 
 ggplot(gap, aes(y = value, x = cbr_decimal)) +
   geom_point(aes(colour = classe_CPI))
 
 
 