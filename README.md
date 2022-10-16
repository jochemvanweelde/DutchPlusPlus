# DutchPlusPlus
 Dutch++ is een programmeertaal die is ontworpen door Jochem van Weelde voor ATP, een vak van de Hogeschool Utrecht 

 De taal lijkt op C++ maar is compleet "vernederlandst" waardoor je de code bijna in het Nederlands voor kan lezen. De taal werkt echter niet zoals C++. Het kan beter vergeleken worden met Python met C++ verplichte typing en style. Op dit moment is alleen de basis geimplementeerd om goede (eenvoudige) programma's te kunnen schrijven. Hieronder kunt u lezen wat deze taal allemaal ondersteunt. 

## Eisen van ATP
Het vak ATP heeft een paar eisen gesteld waaraan de taal maar ook de python-code waarmee de taal is geschreven, moet voldoen. Deze staan hieronder onder een apart kopje uitgelegd. 

### Turingvolledigheid (Turing Complete)
Dutch++ is Turingcompleet. De volgende dingen zijn namelijk mogelijk:
* Het is mogelijk door middel van logica om te 'branchen' in de code. (als-statements)
* Het is mogelijk om te loopen, zowel oneindig (totdat de maximale loop- of recursielengte is bereikt) als eindig door middel van logica. 
* Het is mogelijk om oneindig variabelen en functies aan te maken voor zover het geheugen het toelaat. 

### Functioneel programmeren
Het volledige project is functioneel geschreven. Hoewel de Lexer, Parser en Runner in een klasse zijn geschreven maken ze geen gebruik van lokale klassevariabelen. Ze hebben geen init functie en daardoor ook geen states waardoor functies zich anders gaan gedragen. De klasse zorgt er alleen voor dat de functies mooi bij elkaar blijven onder 1 kopje. 

### Klassen
Bijna alles in dit project is een klasse. Het beste gebruik van klasse en inheritance kan je zien in [ClassNodes.py](Parser\ClassNodes.py). Alle nodes erven van Node maar gebruiken ook elkaar. De abstract-functies `__repr__()` en `run()` moeten worden geimplementeerd door alle andere nodes.

### Decorators
Ik heb nog geen goede use case gevonden voor decorators in mijn code, maar ik heb wel een decorator geschreven om te laten zien dat ik het wel snap! Een decorator is geschreven in [ClassNodes.py](Parser/ClassNodes.py) voor debugging. Een simpele `@debug_decorator` boven een functie plaatsen is genoeg om te laten zien welke node wanneer wordt 'gerunned'.

### Type-annotaties
Ook zijn type-annotaties zo volledig mogelijk ingezet in dit project. Type-annotaties maakt de pythoncode enorm veel duidelijker. Het is duidelijk wat een functie wil ontvangen en wat je daarvoor terugkrijgt. Daarnaast heb je een blije intellisense en daardoor dus ook een blije IT-er. 

### Hogere Ordefuncties
Hogere ordefuncties zijn her en der gebruikt in dit project. De meeste kun je vinden in [ClassNodes.py](Parser/ClassNodes.py). Hierin wordt `zip()` en `map()` gebruikt om functies aan te roepen van parameters of nodes van functiebodies bijvoorbeeld.

## Functies van DutchPlusPlus
In dit onderdeel worden verschillende functies van DutchPlusPlus uitgelegd en gedemonstreerd via codevoorbeelden. 

### Variabelen
Variabelen kunnen gedeclareerd, gedefiniëerd, ge-assigned en gecalled worden. De werking hiervan lijkt op die van C++. 
```bash
# Declaration + Definition
janee a;
janee b is waar;

# Definition
getal c is 5;

# Reassign
zweef d is 4.5;
d is 5.6;

# Call
getal ookInteger is c;
```

### Functies
Functies kunnen gedeclareerd, gedefiniëerd en gecalled worden. De werking hiervan lijkt op dit van C++. Functies kunnen met `geefterug` een waarde returnen. Functies zonder return voeren hun body uit en returnen `None`.
```bash
# Declaration
getal geefGetal(getal n);

# Definition
getal geefGetal(getal n){
    geefterug n;
}

# Call
geefGetal(5);
```

### Wiskunde
Er kunnen verschillende wiskundige operaties worden uitgevoerd op variabelen maar ook op losse getallen. 
```bash
# Optellen
getal a is 5 plus 5;

# Aftrekken
getal b is 5;
getal c is 3;
getal d is a min b;

# Vermenigvuldigen
getal e is 5 keer 8;

# Delen
zweef f is 5 delendoor 2;

# Breien
getal g is 2 plus 2 keer 5;
# Wordt uitgevoerd als 2 + (2*5) = 12
# Haakjes worden nog niet ondersteund.
```

### Logica
Een heleboel logische operaties kunnen worden uitgevoerd. Een logische operatie geeft altijd `True` of `False` terug. 
```bash
# ==
5 gelijkaan 5;

# !=
5 nietgelijkaan 4;

# >
5 groterdan 3;

# >=
5 groterofgelijkaan 3;

# <
5 kleinerdan 7;

# <=
5 kleinerofgelijkaan 7;
```
Het breien van logische operaties wordt nog niet ondersteund. `&&`, `||`, `!`

### Voorwaarden (Conditional)
Een if statement kan worden gemaakt met het `als`-woord met daarbij een [logische operatie](#logica). 
```bash
als (n gelijkaan 0){
    n is 1;
}
```
Het gebruik van `alsanders` en `anders` wordt nog niet ondersteund. 

### Loopjes
Alleen een `zolang` statement kan gemaakt worden in DutchPlusPlus. Dit lijkt op een while-loop in c++.
```bash
zolang (n groterdan 0){
    n is n min 1;
}
```

### Printen
Het is mogelijk om te printen in D++. Dit kunnen variabelen zijn maar ook functiecalls en stukjes logica. 
```bash
# Print een waarde
print(5);

# Print een variabele
getal a is 5;
print(a);

# Print een functie
getal geefGetal(getal n){
    geefterug n;
}
print(geefGetal(5));

# Print logica
print(5 gelijkaan 5);

# Print wiskunde
print(3 plus 2);

# Sla helemaal door
print(geefGetal(a) gelijkaan 2 plus 3);
```
Je zult vast wel denken waarom er opeens print() wordt gebruikt in plaats van de klassieke c++ `std::cout <<`. Op dit moment is de python print overgenomen voor gemakkelijkheid. Wie weet heb ik later tijd om dit nog te veranderen naar een leuke Nederlandse variant. 

## Unit Testing
Unit tests zijn beschikbaar in dit project. Deze unit tests testen iedere functie en klasse van dit project.
UnitTests zijn te vinden in het mapje [UnitTests](UnitTests) en bevatten de volgende onderdelen:
* [Unittests voor de Lexer](UnitTests/UnitTestsLexer.py)
* [Unittests voor de Parser](UnitTests/UnitTestsParser.py)
* [Unittests voor de Runner](UnitTests/UnitTestsRunner.py)

### Unittests voor de Lexer
De Lexer vormt de basis voor alle functionaliteiten in deze taal. Er moeten dus goede tests zijn om de werking hier van te garanderen.

De klasse **TestLexer** bevat tests voor iedere functie van de Lexer en test de preciese werking van die functies. De tests gaan uit van een goed geformateerde D++ string. Invalide tokens worden ook getest, er wordt dus gecontroleerd voor een error, bijvoorbeeld als er een `=`-teken wordt meegegeven. Door deze tests kunnen we garanderen dat een string goed geconverteerd kan worden naar een lijst aan tokens.

De klasse **TestToken** bevat alle tests voor iedere token binnen D++. De tests controleren of een bepaalde string (met D++ formatering) goed wordt omgezet naar een token. Deze tests controleren ook of de inhoud van de token goed klopt zodat er gegarandeerd kan worden dat de juiste waardes binnen de juiste type tokens worden gezet.
Daarnaast wordt ook TokenData meegenomen in deze tests om te kijken of de TokenData klasse correct wordt geüpdatet.

### Unittests voor de Parser
De parser maakt van een super lange tokenlijst een georganiseerde boomstructuur (AST) van nodes.  

De klasse **TestParser** bevat alle tests voor iedere functie van de Parser en test de preciese werking van die functies. Zo wordt er gekeken of de juiste nodes worden gegenereerd op basis van een lijst aan tokens. Er wordt gekeken of de nodes de goede inhoud hebben en of de volgorde van de nodes goed wordt gegenereerd.   

De klasse **TestNodes** zorgt voor de preciese werking van alle Nodes die bestaan in D++. Ze testen iedere node die uit een string wordt gegenereerd. Deze tests zijn dus afhankelijk van de tests die worden gedaan in de **TestToken** en **TestLexer** klassen omdat ze de lexer gebruiken om tokens te genereren.  

Binnen deze tests wordt er gekeken naar de membervariabelen van iedere node en wordt er gekeken of daarbinnen de correcte waardes zijn gegenereerd. Hierdoor wordt er niet alleen gekeken het type van de gegenereerde node maar ook of de node correct zijn nested children goed heeft gegenereerd. Hierdoor kunnen we met nog meer zekerheid kijken of het programma correct werkt en er geen onverwachte nested nodes worden aangemaakt.  

### Unittests voor de Runner
Uiteindelijk nog tests voor de Runner en de library die wordt gebruikt om alle variabelen en functies op te slaan. 

De klasse **TestPrograms** test een geschreven DutchPlusPlus programma met een nagemaakt programma in python en kijkt of het resultaat hetzelfde is. Dit is wellicht één van de allerbelangrijkste tests binnen dit programma. Het test namelijk de complete functionaliteit binnen de zelfgeschreven taal die vergelijkt wordt met een veelgebruikte populaire taal. Door de programma's met verschillende waardes te laten runnen kunnen we met zekerheid zeggen dat de werking van de taal goed is als deze overeenkomt met die van Python.   

De klasse **TestLibrary** is een kleine reeks aan tests die de werking controleert van de Library functies. De tests checken of de location_list en de dictionary goed worden bijgehouden tijdens het runnen.   

## Filmpjes
Hieronder staan een paar filmpjes van de opdracht. Klik op de thumbnail om de video te bekijken op YouTube.    

❗LET OP! Dit filmpje is waarschijnlijk verouderd. De werking en structuur van dit project kan verschillen.❗  

### Filmpje Interpreter
25 augustus 2022  
[![Youtube Interpreter Filmpje](https://img.youtube.com/vi/KjqywITCSgE/0.jpg)](https://www.youtube.com/watch?v=KjqywITCSgE)