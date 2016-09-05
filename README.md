# reg
Funktioner för grundläggande deskriptiv och inferentiell statistik, skrivna i R för utbildningssyfte.

Just nu finns funktioner för:

- Varians
- Kovarians
- Standardavvikelse
- Pearsons korrelationskoeffiecient r
- Signifikanstest av Pearsons r
- Enkel linjär OLS regression (Y = α + βX)
- One sample t-test
- Two sample t-test (obeorende/unpaired)
- Analys av varians (oberoende one-way ANOVA med tre grupper)

Dessa funktioner jämförs sedan med de inbyggda funktionerna i R för att se att rätt värde räknats ut.

Funktionerna är långsamma, överdrivet explicita och uppdelade något märkligt (kvadratsummorna ligger exempelvis i predicera-funktionen av bekvämlighetsskäl), men tanken är att man någorlunda enkelt ser vad som händer i varje steg.

Att göra så småningom:

 - Paired t-test
 - Exakt binomialt test
 - Multipel regressionsanalys (tar lång tid att skriva...)
