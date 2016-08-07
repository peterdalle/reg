# För att se vad regression m.m. faktiskt gör under huven har jag skrivit egna
# funktioner för att räkna ut regressionskoefficienter och annan typ av
# statistik. Det finns funktioner för:
#
# - Varians
# - Kovarians
# - Standardavvikelse
# - Pearsons korrelationskoeffiecient r
# - Pearsons r konfidensintervall
# - Signifikanstest av r
# - Enkel linjär OLS regression (Y = α + βX)
# - Analys av varians (one-way ANOVA)
# - (T-test)
# 
# Dessa jämförs sedan med de inbyggda funktionerna i R för att se att rätt värde
# räknats ut. Funktionerna är långsamma, överdrivet explicita och uppdelade något
# felaktigt (kvadratsummorna ligger exempelvis i predicera-funktionen), men
# tanken är att man någorlunda enkelt ser vad som händer i varje steg.
#
# Peter M. Dahlgren 2016-04-09
# -----------------------------------------------------------------------------

# Först skapar vi ett simpelt dataset för regressionen (formel: y ~ x).
x <- c(1,6,5,4,6,8,6,5,7,8)
y <- c(1,3,4,4,7,8,6,4,6,9)

# Dataset för variansanalys. Vi skiter i att specificera faktorer, utan antar dem implicit.
group1 <- c(5,4,3)
group2 <- c(6,5,4)
group3 <- c(14,12,10)

# Skapa även ett dataset som passar R:s aov-funktion som vi ska jämföra med.
groups.df <- data.frame(y=c(group1, group2, group3),      
                        group=as.factor(c(rep(1, length(group1)), # Notera att vi måste säga till aov() vilken som är
                                rep(2, length(group2)),           # gruppvariabel ("group") genom att passera as.factor-funktionen.
                                rep(3, length(group3)))))         # Testa utan så får du se det skeva resultatet.


# Räkna ut variansen.
varians <- function(x)
{
  SumX <- 0 # Sigma av X
  N <- length(x)
  for(i in 1:N)
  {
    SumX <- (SumX + (x[i] - mean(x)) ^ 2) # För varje x, subtrahera från medelvärdet och kvadrera.
  }
  return(SumX / (N - 1))
}


# Räkna ut kovariansen.
kovarians <- function(x, y)
{
  SumXY <- 0 # Sigma av X
  N <- length(x)
  for(i in 1:N)
  {
    SumX <- (x[i] - mean(x))  # Avvikelse från medelvärdet.
    SumY <- (y[i] - mean(y))  # Avvikelse från medelvärdet.
    SumXY <- SumXY + (SumX * SumY) # Multiplicera avvikelserna från medelvärdena, och summera.
  }
  return((SumXY) / (N - 1))
}


# Räkna ut Pearsons r.
pearsonkorrelation <- function(x, y)
{
  SumXY <- 0
  SumXSquared <- 0
  SumYSquared <- 0
  N <- length(x)
  for(i in 1:N)
  {
    SumXY <- SumXY + ((x[i] - mean(x)) * (y[i] - mean(y)))  # Multiplicera avvikelserna från medelvärdena för X och Y.
    SumXSquared <- SumXSquared + ((x[i] - mean(x)) ^ 2)     # Kvadrera avvikelserna från medelvärdet för X.
    SumYSquared <- SumYSquared + ((y[i] - mean(y)) ^ 2)     # Kvadrera avvikelserna från medelvärdet för Y.
  }
  return(SumXY / sqrt(SumXSquared * SumYSquared))
}


# Räkna ut korrelationens (r) konfidensintervall vid en givan sampelstorlek (n).
korrelationkonfidensintervall <- function(r, n, z.level=1.96)
{
  # Förutsätter 95 % konfidensnivå (z.level=1.96) om inget annat anges.
  # Baserad på formlerna som finns http://davidmlane.com/hyperstat/B8544.html

  # Konvertera r till z (kallas också Fisher Z transformation). 
  z = 0.5 * (log(1 + r) - log(1 - r))

  # Variansen på z.
  sigmaz <- (1 / sqrt(n - 3))
  
  # Nedre och övre gränsen för z (på 95 %-nivån eller dylikt).
  z.lower <- z - (z.level * sigmaz)
  z.upper <- z + (z.level * sigmaz)
  
  # Konvertera z tillbaka till r (inverse Fisher Z transformation).
  r.lower <- (exp(2 * z.lower) - 1) / (exp(2 * z.lower) + 1)
  r.upper <- (exp(2 * z.upper) - 1) / (exp(2 * z.upper) + 1)
  
  return(c(r.lower, r.upper))
}


# Räkna ut signifikans av r (vilket är ett t-test).
pearsonkorrelationsignifikanstest <- function(x, y)
{
  r <- pearsonkorrelation(x, y)                # Först tar vi fram r.
  N <- length(x)                               # Antal observationer.
  t <- (r * sqrt(N - 2)) / sqrt(1 - r ^ 2)     # Räkna ut t-värde.
  df <- N - 2                                  # Degree of freedom.
  t.crit <- qt(0.975, df)                      # Kritisk t-värde. 0.975 får vi fram genom 1 - 0.05 / 2 (alltså dubbelsidigt test med alpha=0.025).
  sig <- t >= t.crit                           # Är det signifikant på 0.05-nivån? (Dock: använd aldrig dikotom signifikans, utan använd fullständiga p-värdet!).
  p <- pt(t, df, lower=F) * 2                  # Tvåsidigt p-värde.
  return(paste("Korrelation p:", p))           # Returnera text.
}


# Räkna ut standardavvikelse.
standardavvikelse <- function(x)
{
  # Standardavvikelsen är helt enkelt roten ur variansen.
  return(sqrt(varians(x)))
}


# Räkna ut intercept+b+beta i enkel linjär regression (1 prediktorvariabel, OLS).
enkellinjarregression <- function(y, x)
  {
  N <- length(x)
    
  # Ostandardiserad (b) regressionskoefficient.
  b.SumXY <- 0
  b.SumXSquared <- 0
  for(i in 1:N)
  {
    b.SumXY <- b.SumXY + ((x[i] - mean(x)) * (y[i] - mean(y))) # Multiplicera avvikelserna från medelvärdena för X och Y.
    b.SumXSquared <- b.SumXSquared + ((x[i] - mean(x)) ^ 2)
  }
  b <- (b.SumXY / b.SumXSquared) # Slutliga ostandardiserad regressionskoefficienten (b).
    
  # Standardiserad (beta) regressionskoefficient.
  beta.SumXY <- 0
  beta.SumXSquared <- 0
  for(i in 1:N)
  {  
    # Konvertera till z-score: Z=(X-M)/SD. Notera att z-score alltid har M=0 och SD=1.
    # Därefter räknas kvadratsummorna för regression+residual ut på samma sätt som för b.
    z.x <- (x[i] - mean(x)) / standardavvikelse(x)
    z.y <- (y[i] - mean(y)) / standardavvikelse(y)
    beta.SumXY <- beta.SumXY + (z.y * z.x)           # Multiplicera Z-scores för X och Y.
    beta.SumXSquared <- beta.SumXSquared + (z.x ^ 2) # Vi kvadrerar därmed prediktorns z-score snarare än x.
    }
  beta <- (beta.SumXY / beta.SumXSquared) # Slutliga beta-koefficienten.
    
  # Intercept/constant.
  intercept <- mean(y) - (b * mean(x))
  
  # Gör lättläst text.
  print(paste("Intercept: ", round(intercept, 3), "  b: ", round(b, 3), "  beta: ", round(beta, 3), sep=""))
  
  return(c(intercept, b, beta))
}


# Predicera Y med enkel linjär regression.
predicera <- function(y, x)
{
  # Gör regression och ta fram intercept + b + beta.
  reg <- enkellinjarregression(y, x)
  intercept <- reg[1] # constant.
  b <- reg[2]         # b-koefficient/slope.
  beta <- reg[3]      # beta-koefficient.
  N <- length(y)      # Antal observationer.
  k <- 1              # Antal oberoende variabler. Vi har ju bara en prediktor (x).
    
  # Räkna ut residualerna.
  ss.res <- 0 # Residualkvadratsumma (residual sum of squares).
  ss.reg <- 0 # Regressionskvadratsumma (regression sum of squares).
  ss.tot <- 0 # Total sum of squares (reg + res).
  for(i in 1:N)
  {
    yhat <- (intercept + (b * x[i]))          # Y-hat är predicerade Y-värden (Y=a+b*x).
    ss.res <- ss.res + ((y[i] - yhat) ^ 2)    # Residualkvadratsumma.
    ss.reg <- ss.reg + ((yhat - mean(y)) ^ 2) # Regressionskvadratsumma.
  }
  ss.tot <- (ss.reg + ss.res)   # Total sum of squares.
  e.msr <- (ss.res / (N-k-1))   # Mean square residual (MSR).
  err.sd <- sqrt(e.msr)         # Residualstandardavvikelse.
  
  # Räkna ut förklarad varians/determinationskoefficient (vilket enkelt
  # uttryckt är systematisk variation dividerat med total variation).
  R2 <- ss.reg / ss.tot                       # Vanligt R2.
  R2adj <- (1 - ((1-R2) * ((N-1) / (N-k-1)))) # R2-justerat som tar hänsyn till antalet variabler/analysenheter.
  
  # Räkna ut regressionskoefficientens standardfel.
  x.sum <- 0
  for(i in 1:N)
  {
    x.sum <- x.sum + ((x[i] - mean(x)) ^ 2)
  }
  b.stderr <- (err.sd / sqrt(x.sum))
  
  # t-testning.
  t <- (b / b.stderr)      # Dividera koefficienten med dess standardfel för att få ut t.
  df <- (N-k-1)            # Degree of freedom.
  #t.crit <- 2.306         # Kritiskt värde från t-distributionen (tvåsidig a=0.05 & df=8).
  t.crit <- qt(0.975, df)  # Hämta automatiskt kritiskt värde (1-0.975=0.025 vilket alltså motsvarar både vänster och höger svans).
  
  # Konfidensintervall för b.
  ci.lower <- b - (t.crit * b.stderr) # Nedre gräns.
  ci.upper <- b + (t.crit * b.stderr) # Övre gräns.
      
  # Fishers F för hela modellen (vilket alltid bör vara >1 om vi gjort rätt då distributionen bara har en svans).
  # Notera också att t^2 = F.
  Fval <- (R2 / k) / ((1-R2) / (N-k-1))
  
  # Räkna ut p-värde.
  p <- pt(-abs(t), df) * 2  # Tack till @krstoffr för denna! https://twitter.com/krstoffr/status/720895719561945088
    
  # ----------------
  # Frågor just nu:
  # Hur räknar man ut std err och t på intercept?
  # Hur räknar man ut p på intercept?
  # Hur räknar man ut p på hela modellen (från F)? Är inte p för b samma som för modellen då det är en enkel regression?
  # ----------------
    
  # Gör någorlunda läsbar text av resultaten.
  text <- paste("Residual standard error: ", round(err.sd, 3),
                  "  R-squared: ", round(R2, 3), 
                  "  R-squared adj: ", round(R2adj, 3),
                  "  b std.err: ", round(b.stderr, 3),
                  "  F: ", round(Fval, 3),
                  "  p: ", p,
                  "  df: ", N-k-1, ", ", k, 
                  "  t:", round(t, 3),
                  "  CI för b: [", round(ci.lower, 2) ,", ", round(ci.upper, 2),"]",
                sep="")
  return(text)
}


# Jämför medelvärden i två grupper (t-test).
ttest <- function(group1, group2)
{
  # TODO
}


# Analys av varians i tre grupper (one-way ANOVA).
onewayanova <- function(group1, group2, group3)
{
  N1 <- length(group1)    # Antal observationer i grupp 1.
  N2 <- length(group2)    # Antal observationer i grupp 2.
  N3 <- length(group3)    # Antal observationer i grupp 3.
  N <- N1+N2+N3           # Totalt antal observationer .
  groups <- 3             # Antal grupper.
  grandmean <- mean(c(group1, group2, group3))  # Totalmedelvärde.
  
  # Total varians inom alla grupper sammanlagt (total sum of squares).
  ss.tot <- 0
  for(i in 1:N1)
  {
    ss.tot <- ss.tot + ((group1[i] - grandmean) ^ 2) # För grupp 1.
    ss.tot <- ss.tot + ((group2[i] - grandmean) ^ 2) # För grupp 2.
    ss.tot <- ss.tot + ((group3[i] - grandmean) ^ 2) # För grupp 3.
  }
  
  # Variation inom varje grupp (sum of squares within).
  ss.w <- 0             # Sum of squares within.
  df.w <- N - groups    # Degree of freedom within (N - antal grupper).
  for(i in 1:N1)
  {
    ss.w <- ss.w + (group1[i] - mean(group1)) ^ 2 # För grupp 1.
    ss.w <- ss.w + (group2[i] - mean(group2)) ^ 2 # För grupp 2.
    ss.w <- ss.w + (group3[i] - mean(group3)) ^ 2 # För grupp 3.
  }
  ms.w <- ss.w / df.w   # Mean Square Within (MSW).
  
  # Variation mellan grupper (sum of squares bewteen).
  ss.b <- 0           # Sum of squares between.
  df.b <- groups - 1  # Degree of freedom between (df).
  ss.b <- ss.b + ((mean(group1) - grandmean) ^ 2) * N1 # Avvikelser från totalmedelvärdet, multiplicerat med antal personer för grupp 1.
  ss.b <- ss.b + ((mean(group2) - grandmean) ^ 2) * N2 # Samma för grupp 2.
  ss.b <- ss.b + ((mean(group3) - grandmean) ^ 2) * N3 # Samma för grupp 3.
  ms.b <- ss.b / df.b # Mean Square Between (MSB).
  
  # Fishers F för hela modellen.
  Fval <- ms.b / ms.w
  
  # Räkna ut p-värde från F-distribution.
  p <- pf(q=Fval, df1=df.b, df2=df.w, lower.tail=F)
  
  # Effektstorlekar: eta-squared och r.
  eta <- ss.b / ss.tot    # Eta-square (n2). Notera att denna är identisk med R2 från regressionen!
  r <- sqrt(eta)          # r är helt enkelt roten ur eta-square.
  
  # TODO: Lägg till Cohens d + Pooled Cohens d.
      
  # Skriv ut beskrivande text.
  text <- paste("SS between: ", round(ss.b, 2),
                "  SS within: ", round(ss.w, 2),
                "  df between: ", df.b,
                "  df within: ", df.w,
                "  Mean square between: ", round(ms.b, 2),
                "  Mean square within: ", round(ms.w, 2),
                "  F: ", round(Fval, 2),
                "  p: ", round(p, 3),
                "  Effektstorlek: (eta: ", round(eta, 3), ", r: ", round(r, 3), ")",
                sep="")
  return(text)
}


# Jämför med R:s inbyggda funktioner så att vi vet att vi gjort rätt.
varians(x)
var(x)

standardavvikelse(x)
sd(x)

kovarians(x, y)
cov(x, y)

pearsonkorrelation(x, y)
cor(x, y)

korrelationkonfidensintervall(r = .9, n = 100)
library(psychometric) # install.packages("psychometric")
CIr(r = .9, n = 100, level = .95)

pearsonkorrelationsignifikanstest(x, y)
cor.test(x, y)

enkellinjarregression(y, x) # Skriv ut intercept+b+beta
predicera(y, x) # Skriv ut R2, F, b stderr m.m.

# Jämför OLS regression med inbyggda linear model (lm).
fit <- lm(y ~ x)
fit$coef
summary(fit)
# > summary(fit)
# Coefficients:
#              Estimate   Std. Error   t value   Pr(>|t|)    
# (Intercept)  -0.6042     1.1895     -0.508    0.625226    
# x             1.0365     0.2005      5.169    0.000854 ***
# 
# Residual standard error: 1.242 on 8 degrees of freedom
# Multiple R-squared:  0.7696,  Adjusted R-squared:  0.7408 
# F-statistic: 26.72 on 1 and 8 DF,  p-value: 0.0008537

# One-way ANOVA.
onewayanova(group1, group2, group3)

# Jämför med inbyggda aov-funktionen.
anova.model <- aov(y ~ group, data=groups.df) # Glöm ej: se till att group är av typen factor.
summary(anova.model) 
# > summary(anova.model) 
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# group        2    114      57    28.5 0.000864 ***
# Residuals    6     12       2                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
