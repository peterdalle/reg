# reg
Enkel linjär regressionsanalys samt deskriptiv statistik i R för undervisningssyfte.

För att se vad regression m.m. faktiskt gör under huven har jag skrivit egna funktioner för att räkna ut regressionskoefficienter och annan typ av statistik.

Det finns funktioner för:
- Varians
- Kovarians
- Standardavvikelse
- Pearsons korrelationskoeffiecient r
- Enkel linjär OLS regression (Y = α + βX)
- (One-way ANOVA)

Dessa funktioner jämförs sedan med de inbyggda funktionerna i R för att se att rätt värde räknats ut.

Funktionerna är långsamma, överdrivet explicita och uppdelade något felaktigt (kvadratsummorna ligger exempelvis i predicera-funktionen), men tanken är att man någorlunda enkelt ser vad som händer i varje steg.
