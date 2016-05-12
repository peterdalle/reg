# reg
Enkel linjär regressionsanalys samt deskriptiv statistik i R för undervisningssyfte.

För att se vad regression m.m. faktiskt gör under huven har jag skrivit egna funktioner för att räkna ut regressionskoefficienter och annan typ av statistik.

Just nu finns funktioner för:

- Varians
- Kovarians
- Standardavvikelse
- Pearsons korrelationskoeffiecient r
- Signifikanstest av Pearsons r
- Enkel linjär OLS regression (Y = α + βX)
- Analys av varians (oberoende one-way ANOVA med tre grupper)
- Att göra så småningom:
 - T-test (obeorende)
 - One-sample t-test
 - Exakt binomialt test
 - Multipel regressionsanalys (tar lång tid att skriva...)

Dessa funktioner jämförs sedan med de inbyggda funktionerna i R för att se att rätt värde räknats ut.

Funktionerna är långsamma, överdrivet explicita och uppdelade något märkligt (kvadratsummorna ligger exempelvis i predicera-funktionen av bekvämlighetsskäl), men tanken är att man någorlunda enkelt ser vad som händer i varje steg.
