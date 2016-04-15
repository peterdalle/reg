# -----------------------------------------------------------------------------
# För att se vad regression m.m. faktiskt gör under huven har jag skrivit egna
# funktioner för att räkna ut regressionskoefficienter och annan typ av
# statistik. Det finns funktioner för:
#
# - Varians
# - Kovarians
# - Standardavvikelse
# - Pearsons korrelationskoeffiecient r
# - Enkel linjär regression (Y = α + βX)
# - (One-way ANOVA)
# 
# Dessa jämförs sedan med de inbyggda funktionerna i R för att se att rätt värde
# räknats ut. Funktionerna är långsamma, överdrivet explicita och uppdelade något
# felaktigt (kvadratsummorna ligger exempelvis i predicera-funktionen), men
# tanken är att man någorlunda enkelt ser vad som händer i varje steg.
# -----------------------------------------------------------------------------

# Först skapar vi ett simpelt dataset (tanken är att testa y~x med regressionen).
x <- c(1,6,5,4,6,8,6,5,7,8)
y <- c(1,3,4,4,7,8,6,4,6,9)


# Räkna ut variansen.
varians <- function(x)
{
  SumX <- 0 # Sigma av X
  N <- length(x)
  for(i in 1:N)
  {
    SumX <- (SumX + (x[i] - mean(x)) ^ 2)
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
    SumX <- (x[i] - mean(x))
    SumY <- (y[i] - mean(y))
    SumXY <- SumXY + (SumX * SumY)
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
    SumXY <- SumXY + ((x[i] - mean(x)) * (y[i] - mean(y)))
    SumXSquared <- SumXSquared + ((x[i] - mean(x)) ^ 2)
    SumYSquared <- SumYSquared + ((y[i] - mean(y)) ^ 2)
  }
  return(SumXY / sqrt(SumXSquared * SumYSquared))
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
    b.SumXY <- b.SumXY + ((x[i] - mean(x)) * (y[i] - mean(y)))
    b.SumXSquared <- b.SumXSquared + ((x[i] - mean(x)) ^ 2)
  }
  b <- (b.SumXY / b.SumXSquared) # Slutliga ostandardiserad regressionskoefficienten (b).
  
  
  # Standardiserad (beta) regressionskoefficient.
  beta.SumXY <- 0
  beta.SumXSquared <- 0
  for(i in 1:N)
  {  
    # Konvertera till z score: Z=(X-M)/SD. Notera att Z-score alltid har M=0 och SD=1.
    # Därefter räknas kvadratsummorna för regression+residual ut på samma sätt som för b.
    z.x <- (x[i] - mean(x)) / standardavvikelse(x)
    z.y <- (y[i] - mean(y)) / standardavvikelse(y)
    beta.SumXY <- beta.SumXY + (z.y * z.x)           # Multiplicera z scores för X och Y.
    beta.SumXSquared <- beta.SumXSquared + (z.x ^ 2) # Vi kvadrerar därmed prediktorns z-score snarare än x.
    }
  beta <- (beta.SumXY / beta.SumXSquared) # Slutliga beta koefficienten.
  
  
  # Intercept/constant.
  intercept <- mean(y) - (b * mean(x))
  
  # Gör lättläst text.
  print(paste("Intercept: ", round(intercept, 4), "   b: ", round(b, 4), "   beta: ", round(beta, 4), sep=""))
  
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
  t <- (b / b.stderr)      # Dividera koeffieicienten med dess standardfel för att få ut t.
  df <- (N-k-1)            # Degree of freedom.
  #t.crit <- 2.306         # Kritiskt värde från t-distributionen (tvåsidig a=0.05 & df=8).
  t.crit <- qt(0.975, df)  # Hämta automatiskt kritiskt värde (1-0.975=0.025 vilket alltså motsvarar både vänster och höger svans).
  
  # Konfidensintervall för b.
  ci.lower <- b - (t.crit * b.stderr) # Nedre gräns.
  ci.upper <- b + (t.crit * b.stderr) # Övre gräns.
      
  # Fishers F för hela modellen.
  Fval <- (R2 / k) / ((1-R2) / (N-k-1))
  
  
  # ----------------
  # Frågor just nu:
  # Hur räknar man ut std err och t på intercept?
  # Hur räknar man ut p på intercept och b?
  # Hur räknar man ut p på hela modellen (från F)? Är inte p för b samma som för modellen då det är en enkel regression?
  # ----------------
  
  
  # Gör läsbar text av resultaten.
  text <- paste("Residual standard error: ", round(err.sd, 3),
                  "  R-squared: ", round(R2, 3), 
                  "  R-squared adj: ", round(R2adj, 3),
                  "  b std.err: ", round(b.stderr, 3),
                  "  F: ", round(Fval, 3),
                  "  df: ", N-k-1, ", ", k, 
                  "  t:", round(t, 3),
                  "  CI för b: [", round(ci.lower, 2) ,", ", round(ci.upper, 2),"]",
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


enkellinjarregression(y, x) # Skriv ut intercept+b+beta
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

predicera(y, x) # Skriv ut R2, F, b stderr m.m.
