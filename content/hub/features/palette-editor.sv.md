---
title: "Palettredigerare"
type: docs
---
Palettredigeraren är där du bygger och hanterar en Lumi-palett. Den håller din pigmentuppsättning, lagrar blandningarna du sparar från Palette Mixer, registrerar färgerna du faktiskt använde när du målade och låter dig konfigurera värdestrukturen och gradienterna för paletten.

## Välja en palett

En palett är mer än en samling pigment: det är ett stilistiskt engagemang. Många konstnärer arbetar med en liten, fast uppsättning pigment som de känner intimt: hur de blandar, de neutrala de producerar, temperaturen skiftar mellan dem. Den förtrogenheten blir en del av deras visuella röst. En målare kan ha en varm palett med låg kromatografi för figurarbete och en separat högtonad palett för landskap, eller så kan de göra allt sitt arbete inom en enda fyra-pigmentuppsättning som en avsiktlig begränsning som förenar ett verk.

Lumi stödjer detta sätt att arbeta. Varje palett har sina egna pigment, blandningar, värdestruktur och gradienter. Att byta paletter ändrar hela färgsystemet: kartan, mixern och de tillgängliga blandningarna uppdateras alla för att återspegla den nya uppsättningen.

En rullgardinsmeny högst upp i palettredigeraren väljer den aktiva paletten. Lumi levereras med tre paletter i gruppen **Standard**:

| Palett | Karaktär |
| :--- | :--- |
| **Standard** | En mångsidig varmlutande palett som täcker hjulet i full nyans. Bra utgångspunkt för de flesta ämnen. |
| **Mästare** | En stor fullspektrumpalett för målare som vill ha maximal färgtäckning och explicit kontroll över grånande yxor. |
| **Zorn** | En fyra-pigment begränsad palett baserad på Anders Zorns tillvägagångssätt. Täcker ett förvånansvärt brett utbud av varma kötttoner och neutrala färger med låg kromatografi från en minimal pigmentuppsättning. |

Paletter kan också skapas, importeras eller dupliceras från fliken Paletter.

## Palettpigment

Avsnittet **Palettpigment** högst upp i palettvyn listar dina primära poster: baspigmenten resten av paletten är byggd av. Dessa är ingångarna till det spektrala blandningssystemet. Sekundärer och tertiärer genereras automatiskt från dem och används för att fylla palettkartan

## Sparade mixar

Avsnittet **Sparade blandningar** innehåller färger som du uttryckligen har behållit från Palettmixern med **Lägg till i palett**. Det här är dina härledda färger: resultaten av spektralblandning, ton- och färgjusteringar sparade för återanvändning.

Sparade mixar är uppdelade i fem värdeband:

| Band | Standard ljusstyrka |
| :--- | :--- |
| High Key | 80 – 100 % |
| Övre mitten | 60 – 80 % |
| Mitten | 40 – 60 % |
| Lower Mid | 20 – 40 % |
| Djup | 0 – 20 % |

Lumi placerar varje sparad mix i lämpligt band automatiskt baserat på dess perceptuella lätthet (CIE L\*). Detta organiserar dina mixar efter värde snarare än att söka igenom en platt lista, och matchar vanligtvis hur en artist tänker på färg.

Sparade mixar kan döpas om via knappen **Byt namn på anpassad** eller snabbmenyn.

## Begagnade blandningar

Avsnittet **Använda blandningar** är en färgutlöst historia. Varje gång en färg från paletten appliceras på duken, registreras den här. Begagnade mixar beställs från de flesta till minst nya.

Det här avsnittet är användbart för att hämta en färg du målat med men inte explicit sparat. För att behålla en begagnad mix permanent, välj den och klicka på **Marknadsför** och den flyttas till sparade mixar i lämpligt värdeband.

Använda mixar lagras per palett och kvarstår mellan sessionerna.

## VärdebandVärdeband definierar var gränserna mellan de fem ljushetszonerna sitter. Som standard delar de ljusheten jämnt över 0–100 %-intervallet, men du kan justera dem för att matcha tonstrukturen i ditt motiv. Det är användbart för målare att definiera och hantera värdeband _och_ klyftorna mellan dem.

### Value Band Slider

Expanderaren **Värdeband** i palettredigeraren innehåller ett skjutreglage med fem dragbara avdelare. Dra valfri avdelare för att flytta gränsen mellan intilliggande band. Etiketten ovanför skjutreglaget visar namnet och det exakta procentintervallet för det aktiva bandet.

**Knappar:**

| Knapp | Effekt |
| :--- | :--- |
| **Avbryt** | Återställer skjutreglaget till det senast tillämpade läget |
| **Kopiera** | Kopierar den aktuella bandkonfigurationen till urklipp |
| **Klistra** | Klistrar in en kopierad bandkonfiguration från en annan palett |
| **Standardinställningar** | Återställer fabriksinställningarna för lika division |
| **Ansök** | Bekräftar ändringarna och regenererar paletten |

**Ansök** krävs för att göra ändringarna permanenta. Det utlöser en hel palettförnyelse och tar bort alla sparade mixar vars lätthet inte längre faller inom något band. Lumi visar en bekräftelsedialog som visar hur många mixar som skulle tas bort innan du fortsätter.

### Värdeband och palettkartan

Palettkartan visar paletten som ett nyanshjul med 36 nyanssektorer (10° vardera) och 15 ljushetsceller arrangerade som koncentriska ringar. Varje band motsvarar tre ringar: de fem banden × 3 ringar = totalt 15 celler.

Justering av värdebanden ändrar vilka ljushetsvärden som landar i varje ringnivå. Ett band komprimerat mot den mörka änden gör att dess tre ringar spänner över ett smalare tonområde; ett brett band ger sina tre ringar mer tonal spridning. Så här anpassar sig samma palettkartastruktur till paletter som är inställda för olika tonprioriteringar.

## Palettgradienter

Varje palett kan lagra en eller flera **Gradienter**: jämna fortgångar härledda från palettposter som kan appliceras på arbetsytan som övertoningsfyllningar eller användas som referensremsor.

Gradienter hanteras i **Gradient expander**. Kombinationen högst upp listar övertoningarna i den aktuella paletten. **Lägg till** skapar en ny övertoning. **Ta bort** tar bort den valda. **Byt namn** byter namn på den.

### Gradient Editor

**Gradient Editor expander** konfigurerar den valda övertoningen. Varje gradient har tre slutpunkter (**A**, **B** och **C**) som visas som färgrutor. Klicka på en färgruta för att göra den till den aktiva slutpunkten för redigering.

Varje slutpunkt kan ställas in genom att klicka på **Välj** och sedan klicka på en palettpost i palettkartan eller palettvyn. Slutpunkten är länkad till den palettposten av UID; om posten ändras uppdateras övertoningen.

**Kontroller per slutpunkt:**

| Kontroll | Effekt |
| :--- | :--- |
| **Styrka** | Hur starkt ändpunktsfärgen bidrar i förhållande till sina grannar |
| **Opacitet** | Alfa för slutpunktsfärgen i gradienten |
| **Kurva** | Gammajustering för färgavfallet från denna slutpunkt |

**Fördelningsreglage** (S1, S2, S3) ställs in där de tre mittpunkterna mellan ändpunkterna faller längs gradientremsan. Om du återställer dem återställs mittpunkterna till samma avstånd.

Gradientförhandsgranskningsremsan överst i Gradient Editor-blocket visar resultatet av den aktuella slutpunkten och distributionsinställningarna.

## Palett dockningsbarDen dockningsbara **Palett** (**Palett > Palett**) är en enklare läsfokuserad panel för att bläddra och välja färger från vilken palett som helst. Den visar samma tresektionsvy (palettpigment, sparade blandningar, begagnade blandningar) utan expanderarna för värdeband och gradienter.

En rullgardinsmeny för palettväljare längst upp låter dig växla mellan alla tillgängliga paletter. Klicka på valfri post för att ställa in den som förgrundsfärg. Dubbelklicka för att öppna färgnamnsredigeraren. För skrivbara paletter är åtgärderna Redigera färg, Ny färg från FG och Ta bort färg tillgängliga i knappraden.

Den dockningsbara paletten är avsedd för snabb färgåtkomst under målning när hela palettredigeraren skulle ta för mycket plats.

## Fliken Paletter

Fliken **Paletter** (tillgänglig som dockningsbar flik) visar den aktiva paletten i kompakt läge. Det utesluter pigmenten för att fokusera på sparade blandningar