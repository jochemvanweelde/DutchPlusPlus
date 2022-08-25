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

## Funcites van DutchPlusPlus
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
Alleen een `terwijl` statement kan gemaakt worden in DutchPlusPlus. Dit lijkt op een while-loop in c++.
```bash
terwijl (n groterdan 0){
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

## Filmpjes
Hieronder staan een paar filmpjes van de opdracht. Klik op de thumbnail om de video te bekijken op YouTube.  

### Filmpje Interpreter
[![Youtube Interpreter Filmpje](https://img.youtube.com/vi/KjqywITCSgE/0.jpg)](https://www.youtube.com/watch?v=KjqywITCSgE)