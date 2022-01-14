#Loading graphic libraries
install.packages("ggplot2")
library(ggplot2)

TaxDat <- read.csv("IRS Stats of Income Readable.csv",stringsAsFactors = FALSE)
TaxDat2 <- subset(TaxDat, TaxDat$Year <= 1925)

TaxDat2$Year <- as.Date(paste(TaxDat2$Year,01,01,sep="-"), format= "%Y-%m-%d")
TaxDat2$Ratio.Dividends.Tax.To.Tax.Reported <- as.numeric(TaxDat2$Ratio.Dividends.Tax.To.Tax.Reported)
TaxDat2$Ratio.Dividends.Tax.To.Tax.Reported[1] <- NA
TaxDat2$Profits.From.Sales[2] <- NA

TaxDat2$Income.Tax.Rate <- c(7,15,67,77,73,73,73,58,43.5,46,25)
TaxDat2$Disposition.Tax.Rate <- c(7,15,67,77,73,73,73,12.5,12.5,12.5,12.5)

TaxDat2$Gross.Income <- as.numeric(TaxDat2$Gross.Income)
TaxDat2$Profits.From.Sales <- as.numeric(TaxDat2$Profits.From.Sales)
TaxDat2$Dividends.Received <- as.numeric(TaxDat2$Dividends.Received)
TaxDat2$Interest.Income <- as.numeric(TaxDat2$Interest.Income)

TaxDat2$WholeYear <- c(1915,1916,1917,1918,1919,1920,1921,1922,1923,1924,1925)
DFGraph <- as.data.frame(c(7,15,67,77,73,73,73,58,43.5,46,25,7,15,67,77,73,73,73,12.5,12.5,12.5,12.5))
colnames(DFGraph) <- "Value"
DFGraph$WholeYear <- c(1915,1916,1917,1918,1919,1920,1921,1922,1923,1924,1925)
DFGraph$Income.Or.Capital.Rate <- c("Income Tax Rate","Income Tax Rate","Income Tax Rate","Income Tax Rate","Income Tax Rate",
             "Income Tax Rate","Income Tax Rate","Income Tax Rate","Income Tax Rate","Income Tax Rate",
             "Income Tax Rate","Capital Gains Rate","Capital Gains Rate","Capital Gains Rate","Capital Gains Rate","Capital Gains Rate",
             "Capital Gains Rate","Capital Gains Rate","Capital Gains Rate","Capital Gains Rate","Capital Gains Rate",
             "Capital Gains Rate")
             
### Graphs

#Figure 1 - Top Bracket Profit From Disp., Interest Income, and Dividends

plot(x=TaxDat2$WholeYear, y=TaxDat2$Gross.Income/1500000, type="o", xlab="Year",ylab="Amount (Millions)", ylim=c(0,500),
     main="Figure 1: Top Bracket Profit from Dispositions,\n Interest Income, and Dividends",pch=16,xaxt="n",yaxs="i",
     col=alpha(rgb(0,0,0),0))
lines(x=TaxDat2$WholeYear, y=TaxDat2$Profits.From.Sales/1000000, type="o",pch=17,col=alpha(rgb(0,0,0), 0.9))
lines(x=TaxDat2$WholeYear,y=TaxDat2$Dividends.Received/1000000, type="o",pch=15, col=alpha(rgb(0,0,0), 0.7))
lines(x=TaxDat2$WholeYear, y=TaxDat2$Interest.Income/1000000, type="o", pch=8, col=alpha(rgb(0,0,0), 0.5))
axis(1, at=seq(1915, 1926, by=1))
abline(h=200, col=alpha(rgb(0,0,0), 0.1))
abline(h=0, col=alpha(rgb(0,0,0), 0.1))
abline(h=400, col=alpha(rgb(0,0,0), 0.1))
abline(h=100, col=alpha(rgb(0,0,0), 0.1))
abline(h=300, col=alpha(rgb(0,0,0), 0.1))
abline(h=500, col=alpha(rgb(0,0,0), 0.1))
abline(v=1915, col=alpha(rgb(0,0,0), 0.1))
abline(v=1916, col=alpha(rgb(0,0,0), 0.1))
abline(v=1917, col=alpha(rgb(0,0,0), 0.1))
abline(v=1918, col=alpha(rgb(0,0,0), 0.1))
abline(v=1919, col=alpha(rgb(0,0,0), 0.1))
abline(v=1920, col=alpha(rgb(0,0,0), 0.1))
abline(v=1921, col=alpha(rgb(0,0,0), 0.1))
abline(v=1922, col=alpha(rgb(0,0,0), 0.1))
abline(v=1923, col=alpha(rgb(0,0,0), 0.1))
abline(v=1924, col=alpha(rgb(0,0,0), 0.1))
abline(v=1925, col=alpha(rgb(0,0,0), 0.1))
legend("topright",legend=c("Income from Dividends","Income from Dispositions of Property","Interest Income"),
       text.col=c(
         alpha(rgb(0,0,0), 0.9),
         alpha(rgb(0,0,0), 0.7),
         col=alpha(rgb(0,0,0), 0.5)),lty=c(1,1,1,1),col=c(
           alpha(rgb(0,0,0), 0.9),
           alpha(rgb(0,0,0), 0.7),
           col=alpha(rgb(0,0,0), 0.5)),pch=c(15,17,8))

#Figure 1 - Income and Cap Gains Rate by Year

ggplot(DFGraph, aes(WholeYear, y=Value, fill=Income.Or.Capital.Rate,))+
  geom_bar(position="dodge",stat="identity",colour="black")+
  ylab("Tax Rate (%)")+
  ylim(0,100)+
  xlab("Year")+
  labs(fill="Rate by Source")+
  ggtitle("Figure 2: Income and Capital Gains Tax Rate by Year")+
  scale_x_continuous(labels=TaxDat2$WholeYear, breaks=TaxDat2$WholeYear)+
  theme_bw()+
  scale_fill_manual("Rate by Source", values = c("Capital Gains Rate" = "black", "Income Tax Rate" = "grey"))+
  theme(legend.justification=c(1,1), legend.position=c(1,1))+
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))+
  theme(plot.title = element_text(hjust = 0.5))

#Figure 3 - DJI 1915-1925
DJI <- read.csv("DJI 1915-1925 Data.csv")
DJI$date <- as.Date(DJI$date, "%m/%d/%Y")

aggregatevolume <- read.csv("Yearly Stock Trade Volume.csv")
aggregatevolume <- aggregatevolume[-c(26,27)]
aggregatevolume2 <- subset(aggregatevolume, is.na(aggregatevolume$Year)==FALSE)
aggregatevolume2$Year <- as.Date(paste(aggregatevolume2$Year,01,01,sep="-"), format= "%Y-%m-%d")
aggregatevolume2$Year2 <- as.Date(paste(c(1915,1916,1917,1918,1919,1920,1921,1922,1923,1924,1925),12,31,sep="-"), format= "%Y-%m-%d")
aggregatevolume2$YearNum <- c(1915,1916,1917,1918,1919,1920,1921,1922,1923,1924,1925)

plot(x=DJI$date, y=DJI$DJI3D.open, type="l", ylim=c(40,180), xlab="Year",ylab="Value", 
     main="Figure 3: Dow Jones Industrial Average 1915-1925")
axis(1, at=seq(1915L, 1926L, by = 1L))

#Figure 4 - NYSE Trade Volume

plot(x=aggregatevolume2$YearNum, y=aggregatevolume2$NYSE.Stock, xlab="Year",ylab="Trade Volume (Millions)", type="b",
     main="Figure 4: NYSE Trade Volume 1915-1925", ylim=c(100,550), xaxt="n")
axis(1, at=seq(1915,1925, by=1))

#Figure 5 - Bond Yields
BondsDF <- read.csv("NBER Bond Data.csv")
BondsDF <- subset(BondsDF, BondsDF$Year<1926)

plot(x=BondsDF$Year, y=BondsDF$Muni, type="l", xlab="Year",ylab="Yield", ylim=c(0,7),yaxs="i",
     main="Figure 5: Corporate and Municipal Bond Yields 1915-1925", col=alpha(rgb(0,0,0), 0.3),lwd=2,xaxt="n")
lines(x=BondsDF$Year, y=BondsDF$Corporate, type="l",lwd=2)
lines(x=BondsDF$Year,y=BondsDF$Spread, type="l",lwd=2,lty=5)
legend("topleft",legend=c("Corporate","Municipal","Corp-Muni Spread"),
       text.col=c("black",alpha(rgb(0,0,0), 0.5),"black"),lty=c(1,1,5),col=c("black",alpha(rgb(0,0,0), 0.3),"black"))
axis(1, at=seq(1915, 1926, by=1))

#Figure 6 - Corporate Muni Bond Spread

plot(x=BondsDF$Year,y=BondsDF$Spread, type="l",lwd=2,xlab="Year",ylab="Spread",yaxs="i", ylim=c(0.5,1.5),xlim=c(1918,1926),
     main="Figure 6: Corporate-Municipal Bond Spread 1919-1925",xaxt="n")
     axis(1, at=seq(1918, 1926, by=1))
     
     
     
     
### Scripts for Inline Version
## Note use 700x500
## Figure 1:
     
     ggplot(DFGraph, aes(WholeYear, y=Value, fill=Income.Or.Capital.Rate,))+
       geom_bar(position="dodge",stat="identity",colour="black")+
       ylab("Tax Rate (%)")+
       ylim(0,100)+
       xlab("Year")+
       labs(fill="Rate by Source")+
       ggtitle("Figure 1: Income and Capital Gains Tax Rate by Year")+
       scale_x_continuous(labels=TaxDat2$WholeYear, breaks=TaxDat2$WholeYear)+
       theme_bw()+
       scale_fill_manual("Rate by Source", values = c("Capital Gains Rate" = "black", "Income Tax Rate" = "grey"))+
       theme(legend.justification=c(1,1), legend.position=c(1,1))+
       theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))+
       theme(axis.text=element_text(size=11))+
       theme(plot.title = element_text(hjust = 0.5))
     
     
     #Figure 2 - Top Bracket Profit From Disp., Interest Income, and Dividends
     
     dev.new(width=7, height=5)
     plot(x=TaxDat2$WholeYear, y=TaxDat2$Gross.Income/1500000, type="o", xlab="Year",ylab="Amount (Millions)", ylim=c(0,500),
          main="Figure 2: Top Bracket Profit from Dispositions,\n Interest Income, and Dividends",pch=16,xaxt="n",yaxs="i",
          col=alpha(rgb(0,0,0),0))
     lines(x=TaxDat2$WholeYear, y=TaxDat2$Profits.From.Sales/1000000, type="o",pch=17,col=alpha(rgb(0,0,0), 0.9))
     lines(x=TaxDat2$WholeYear,y=TaxDat2$Dividends.Received/1000000, type="o",pch=15, col=alpha(rgb(0,0,0), 0.7))
     lines(x=TaxDat2$WholeYear, y=TaxDat2$Interest.Income/1000000, type="o", pch=8, col=alpha(rgb(0,0,0), 0.5))
     axis(1, at=seq(1915, 1926, by=1))
     abline(h=200, col=alpha(rgb(0,0,0), 0.1))
     abline(h=0, col=alpha(rgb(0,0,0), 0.1))
     abline(h=400, col=alpha(rgb(0,0,0), 0.1))
     abline(h=100, col=alpha(rgb(0,0,0), 0.1))
     abline(h=300, col=alpha(rgb(0,0,0), 0.1))
     abline(h=500, col=alpha(rgb(0,0,0), 0.1))
     abline(v=1915, col=alpha(rgb(0,0,0), 0.1))
     abline(v=1916, col=alpha(rgb(0,0,0), 0.1))
     abline(v=1917, col=alpha(rgb(0,0,0), 0.1))
     abline(v=1918, col=alpha(rgb(0,0,0), 0.1))
     abline(v=1919, col=alpha(rgb(0,0,0), 0.1))
     abline(v=1920, col=alpha(rgb(0,0,0), 0.1))
     abline(v=1921, col=alpha(rgb(0,0,0), 0.1))
     abline(v=1922, col=alpha(rgb(0,0,0), 0.1))
     abline(v=1923, col=alpha(rgb(0,0,0), 0.1))
     abline(v=1924, col=alpha(rgb(0,0,0), 0.1))
     abline(v=1925, col=alpha(rgb(0,0,0), 0.1))
     legend("topright",legend=c("Income from Dividends","Income from Dispositions of Property","Interest Income"),
            text.col=c(
              alpha(rgb(0,0,0), 0.9),
              alpha(rgb(0,0,0), 0.7),
              col=alpha(rgb(0,0,0), 0.5)),lty=c(1,1,1,1),col=c(
                alpha(rgb(0,0,0), 0.9),
                alpha(rgb(0,0,0), 0.7),
                col=alpha(rgb(0,0,0), 0.5)),pch=c(15,17,8),
            cex= 0.9,
            xjust=1)

     #Figure 3
     
     DJI <- read.csv("DJI 1915-1925 Data.csv")
     DJI$date <- as.Date(DJI$date, "%m/%d/%Y")

     aggregatevolume <- read.csv("Yearly Stock Trade Volume.csv")
     aggregatevolume <- aggregatevolume[-c(26,27)]
     aggregatevolume2 <- subset(aggregatevolume, is.na(aggregatevolume$Year)==FALSE)
     aggregatevolume2$Year <- as.Date(paste(aggregatevolume2$Year,01,01,sep="-"), format= "%Y-%m-%d")
     aggregatevolume2$Year2 <- as.Date(paste(c(1915,1916,1917,1918,1919,1920,1921,1922,1923,1924,1925),12,31,sep="-"), format= "%Y-%m-%d")
     aggregatevolume2$YearNum <- c(1915,1916,1917,1918,1919,1920,1921,1922,1923,1924,1925)
     

     
     par(mfrow=c(1,1))
     plot(x=DJI$date, y=DJI$DJI3D.open, type="l", ylim=c(40,180), xlab="Year",ylab="Value",yaxs="i",
          main="Figure 3: Dow Jones Industrial Average 1915-1925")
     axis.Date(1, at=seq(min(DJI$date), max(DJI$date), by="years"), format="%Y")
     abline(v=axis.Date(1, at=seq(min(DJI$date), max(DJI$date), by="years"), format="%Y"), col=alpha(rgb(0,0,0), 0.1))
     abline(v=axis.Date(1, DJI$date), col=alpha(rgb(0,0,0), 0.1))
     abline(h=40, col=alpha(rgb(0,0,0), 0.1))
     abline(h=60, col=alpha(rgb(0,0,0), 0.1))
     abline(h=80, col=alpha(rgb(0,0,0), 0.1))
     abline(h=100, col=alpha(rgb(0,0,0), 0.1))
     abline(h=120, col=alpha(rgb(0,0,0), 0.1))
     abline(h=140, col=alpha(rgb(0,0,0), 0.1))
     abline(h=160, col=alpha(rgb(0,0,0), 0.1))
     abline(h=180, col=alpha(rgb(0,0,0), 0.1))
     abline(v=as.Date("1926-01-01"),col=alpha(rgb(0,0,0), 0.1))
     abline(v=as.Date("1925-01-01"),col=alpha(rgb(0,0,0), 0.1))
     abline(v=as.Date("1924-01-01"),col=alpha(rgb(0,0,0), 0.1))
     abline(v=as.Date("1923-01-01"),col=alpha(rgb(0,0,0), 0.1))
     abline(v=as.Date("1922-01-01"),col=alpha(rgb(0,0,0), 0.1))
     abline(v=as.Date("1921-01-01"),col=alpha(rgb(0,0,0), 0.1))
     abline(v=as.Date("1920-01-01"),col=alpha(rgb(0,0,0), 0.1))
     abline(v=as.Date("1919-01-01"),col=alpha(rgb(0,0,0), 0.1))
     abline(v=as.Date("1918-01-01"),col=alpha(rgb(0,0,0), 0.1))
     abline(v=as.Date("1917-01-01"),col=alpha(rgb(0,0,0), 0.1))
     abline(v=as.Date("1916-01-01"),col=alpha(rgb(0,0,0), 0.1))
     abline(v=as.Date("1915-01-01"),col=alpha(rgb(0,0,0), 0.1))
     
     
     
     #Figure 4 - Bond Yields
     BondsDF <- read.csv("NBER Bond Data.csv")
     BondsDF <- subset(BondsDF, BondsDF$Year<1926)
     
     plot(x=BondsDF$Year, y=BondsDF$Muni, type="l", xlab="Year",ylab="Yield", ylim=c(0,7),yaxs="i",
          main="Figure 4: Corporate and Municipal Bond Yields 1915-1925", col=alpha(rgb(0,0,0), 0.3),lwd=2,xaxt="n")
     lines(x=BondsDF$Year, y=BondsDF$Corporate, type="l",lwd=2)
     lines(x=BondsDF$Year,y=BondsDF$Spread, type="l",lwd=1,lty=2)
     axis(1, at=seq(1915, 1926, by=1))
     abline(h=1, col=alpha(rgb(0,0,0), 0.1))
     abline(h=2, col=alpha(rgb(0,0,0), 0.1))
     abline(h=3, col=alpha(rgb(0,0,0), 0.1))
     abline(h=4, col=alpha(rgb(0,0,0), 0.1))
     abline(h=5, col=alpha(rgb(0,0,0), 0.1))
     abline(h=6, col=alpha(rgb(0,0,0), 0.1))
     abline(v=1915, col=alpha(rgb(0,0,0), 0.1))
     abline(v=1916, col=alpha(rgb(0,0,0), 0.1))
     abline(v=1917, col=alpha(rgb(0,0,0), 0.1))
     abline(v=1918, col=alpha(rgb(0,0,0), 0.1))
     abline(v=1919, col=alpha(rgb(0,0,0), 0.1))
     abline(v=1920, col=alpha(rgb(0,0,0), 0.1))
     abline(v=1921, col=alpha(rgb(0,0,0), 0.1))
     abline(v=1922, col=alpha(rgb(0,0,0), 0.1))
     abline(v=1923, col=alpha(rgb(0,0,0), 0.1))
     abline(v=1924, col=alpha(rgb(0,0,0), 0.1))
     abline(v=1925, col=alpha(rgb(0,0,0), 0.1))
     abline(v=1926, col=alpha(rgb(0,0,0), 0.1))
          legend("topleft",legend=c("Corporate","Municipal","Corp-Muni Spread"),
            text.col=c("black",alpha(rgb(0,0,0), 0.5),"black"),lty=c(1,1,5),col=c("black",alpha(rgb(0,0,0), 0.3),"black"))
