---
title: "Lumi-o"
type: docs
weight: 1
translation_provenance: ai-reviewed
translation_lock: true
translation_source_sha256: 060cd1cd6261e7278b4bc6e6c7330a374ecb20b5368faedde03a6e15da424d12
url: "hub/about/lumi-o"
---

Lumi är en snabb, effektiv Linux-applikation för att skapa rasterbilder, utvecklad öppet. Designbeslut, arkitekturdokument och utvecklingshistorik är offentliga så att användare kan följa hur programmet utvecklas.

Lumi är inriktat på digital målning och illustration, men klarar också strukturerad bildredigering och fotografisk korrigering. Korrigerande justeringar, linsstilseffekter, tonbaserad färggradering och icke-förstörande filterlager stödjer ett brett spektrum av redigeringsflöden.

Lumi bygger på idén att digital målningsprogramvara ska bete sig som ett pålitligt studioverktyg: förutsägbart, transparent och fokuserat på själva bildskapandet.

## Syfte

Lumi stödjer en strukturerad, icke-förstörande metod för bildskapande — oavsett om källan är målad, ritad eller fotografisk. Det är ett fokuserat, tydligt alternativ till både allmänna bildredigerare och dedikerad målningsprogramvara: gratis och med öppen källkod, utan prenumerationer, inlåsning, molnberoende eller AI-bildgenerering.

Tillförlitlighet och långsiktig åtkomst är kärnfunktioner. Det öppna, katalogbaserade filformatet förblir läsbart utan proprietär programvara, med import och export av XCF och PSD.

## Konstnärlig grund

Lumi utvecklas av en oberoende konstnär med erfarenhet av pixelkonst, traditionellt tecknande och måleri, spelutveckling, teknisk konst, illustration och 3D-animation. Den bakgrunden formar inställningen till färg, linjearbete, lager, prestanda, dataåterställning, skript och användarupplevelse.

## Filosofi

Lumi kombinerar ett pigmentbaserat färgsystem med ett responsivt, icke-förstörande, lagerbaserat arbetsflöde. Färgsystemet är ett medvetet avsteg från konventionella HSV-reglage och godtyckliga RGB-väljare.

- **Pigmentcentrerad färg**: verkliga pigmentprofiler (Colour Index-koder) blandas spektralt så att paletter beter sig mer som riktig färg.
- **Palettstyrt arbetsflöde**: sparade, växlingsbara paletter organiserar pigment, blandningar, värdeband och gradienter och håller färgbesluten sammanhängande i en målning eller ett projekt.
- **Taktila, fokuserade verktyg**: penslar förenar penntryck, lutning och hastighet för direkt, nyanserad kontroll. Ytans textur kan ge ett drag gräng och tand från en yta, även på kalligrafiska drag, och en sväng kan förskjuta den grängen. Borstpenslar kan formas tätt och animeras längs ett drag.
- **Responsiv duk**: OpenGL kan rita lagerstacken och måla penseldrag för enskilda verktygsförinställningar. Stora airbrushar och stora texturstämplar följer handen, och rörelsen över duken förblir jämn.
- **Icke-förstörande tillförlitlighet**: lager och redigerbara filter skalar till komplexa projekt och förblir förutsägbara. Autosparning, snabbt sparande, inkrementella sparningar och återställning skyddar långa målningssessioner och stora projekt.
- **Live-arbetsytor**: namngivna profiler bevarar dockor, verktyg, förinställningar, paletter och enhetsbindningar och byter dem atomärt vid körning.
- **Scheme scripting**: Lumi utökar Script-Fu-traditionen med ett Scheme-baserat plug-in-språk och extra hjälpfunktioner för att bygga plug-ins och automatisera arbetsflöden.

[Filter](/hub/features/filters/) — inklusive formad bokeh-linsoskärpa, tilt-shift, tongradering, skärpa och brusreducering — kan förbli redigerbara vid sidan av penselarbete.

## Gränser

- **Fokuserat, inte uttömmande**: Lumi är inte avsett för webbdesign, desktop publishing eller varje nisch en bred redigerare som GIMP försöker täcka.
- **Endast Linux**: Lumi utvecklas på Debian med Cinnamon (X11). GNOME på Wayland har testats för programmets allmänna utseende och för OpenGL-rendering. Windows och macOS stöds inte.

## Erkännanden

Lumi bygger på grunden av GNU Image Manipulation Program (GIMP). Lumi erkänner och är djupt tacksam för utvecklarnas, konstnärernas och bidragsgivarnas många års arbete.

![Lumi logo placeholder](/images/lumi.png)
