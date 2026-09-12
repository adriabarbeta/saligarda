# La Saligarda: caracterització del vent catabàtic de la vall del Congost a la Garriga

*Anàlisi de 13 anys de registre a 5 minuts (2011–2024), 22 anys de registre diari
(2002–2024) i l'estació XEMA de Vic com a conca d'origen.*

---

## Resum

La Saligarda és un flux de drenatge nocturn que baixa la vall del Congost des de la
Plana de Vic fins a la Garriga. L'anàlisi de 1.355.318 registres de 5 minuts mostra
que:

1. És un flux **extraordinàriament canalitzat**: direcció mitjana **350,9° (NNW)**
   amb constància direccional |R| = 0,87. Aquest rumb coincideix amb el del Congost
   cap al Figaró (350,4°), el tram que la Garriga té just aigües amunt.
2. Bufa **gairebé cada nit de l'any** (el sector nord ocupa el 69–77 % de les
   observacions nocturnes en totes les estacions). El que canvia amb l'estació no és
   la freqüència sinó la **intensitat**: mediana de 10,4 km/h a l'hivern contra
   5,9 a l'estiu.
3. S'engega cap a les 18–20 UTC, culmina cap a les 4–5 UTC i s'apaga entre les 7 UTC
   (estiu) i les 10 UTC (hivern, = 11 h civils).
4. **Escalfa el termòmetre i refreda la pell.** Per a una mateixa massa d'aire, un
   episodi típic d'hivern deixa la matinada 0,86 °C més càlida i 2,7 punts més seca,
   però la rebaixa per vent (−1,92 °C) supera l'escalfament i la sensació de fred
   acaba sent 1,06 °C més baixa.
5. **No s'està ni intensificant ni afeblint.** Ho diuen tres vies independents: les
   observacions homogènies (11 anys), la sèrie llarga un cop descomptada una ruptura
   instrumental del 2011, i la reconstrucció de la circulació sinòptica amb 45 anys
   de reanàlisi NCEP/NCAR (−0,07 dies/any, IC 95 % [−0,20, +0,08]).
6. La configuració que la genera és un **anticicló centrat entre la Mànega i
   Biscaia** (anomalia de fins a +11 hPa sobre el domini), i el que en governa la
   força és el **refredament radiatiu de la Plana de Vic**: cada grau d'amplitud
   tèrmica a Vic val +0,45 km/h de drenatge.
7. **Es pot pronosticar la nit abans amb destresa notable**: R² = 0,57 per a la
   intensitat i AUC = 0,87 per a l'ocurrència, amb un sistema que s'executa sol cada
   vespre a partir d'Open-Meteo i que no necessita l'estació pròpia (§ 2.11–2.12).
8. **La penella estava girada +32° des del 2011** i ja s'ha corregit (§ 2.10). Que el
   drenatge del nord i la marinada del sud hagin girat el mateix demostra que és una
   rotació rígida de l'aparell, no un problema d'exposició. Cap resultat quantitatiu
   en depèn: l'índex queda invariant fins a la tercera xifra.

---

## 1. Dades i mètodes

### 1.1 Sèries

| Sèrie | Estació | Període | Pas | Registres |
|---|---|---|---|---|
| La Garriga 5 min | Davis (particular), ~250 m | 2011-03-01 → 2024-02-19 | 5 min | 1.355.318 |
| La Garriga diària | resum de la mateixa Davis | 2002-01-01 → 2024-02-19 | diari | 8.401 |
| Vic | XEMA **XO**, 41,935 N 2,240 E, 500 m | 2011-12-22 → 2025-12-22 | 30 min | 245.115 |

La sèrie de 5 minuts s'ha reconstruït llegint els **4.710 fitxers Excel diaris** de
`Saligarda/Dades/Dades/`, sense cap error de lectura i amb cobertura pràcticament
completa (105.120 registres l'any = 365 × 288). El lector gestiona els dos idiomes
de capçalera (`Date/Time` fins al maig de 2016, `Data/Hora` a partir del juny), el
nombre variable de columnes (11 a 14) i les files de peu de full (`MITJANES`,
`MÀX-MIN`, `PROMIG`).

El full de l'Ajuntament és un **resum de la mateixa Davis**, no una altra estació:
al període comú (4.705 dies) el biaix de la ratxa màxima diària és de +0,01 km/h amb
r = 0,9986, i el de la temperatura màxima, de +0,04 °C amb r = 0,9998.

Les estacions auxiliars del directori (Vic-1 `CX` 2000–2002, Tagamanent `VX`
2000–2008, Caldes de Montbui `VW`/`X9` 2000–2008) **no solapen** amb el registre de
la Garriga i no s'han fet servir.

### 1.2 Referència horària

Totes les hores d'aquest informe són **UTC**, que a la longitud de la Garriga
equival a l'hora solar menys uns 9 minuts. Per passar a hora civil, sumeu 1 h a
l'hivern i 2 h a l'estiu.

Que les dues sèries són en UTC i sense canvi d'hora s'ha verificat de tres maneres:

- A la Garriga, l'hora del mínim diari de temperatura coincideix amb la sortida del
  sol calculada **tots els mesos** (desfasament de −0,13 a +0,23 h; diferència
  hivern–estiu de només 0,19 h). Amb marca civil, aquesta corba baixaria 1 h a
  l'estiu.
- A Vic, el màxim d'irradiància en dies clars cau al mig-interval anterior al
  migdia solar, com correspon a mitjanes semihoràries en UTC.
- La correlació creuada de les dues sèries de temperatura és màxima a **lag 0**
  tant al DJF com al JJA (r = 0,881 i 0,951).

### 1.3 Índex de Saligarda

Les direccions estan **corregides en −32°** per la desalineació de la penella
posterior al 2011 (§ 2.10 i `config_direccio.R`). Sobre les dades corregides, l'eix
de drenatge es dedueix de les mateixes observacions: la direcció mitjana vectorial
del vent nocturn d'hivern amb velocitat ≥ 5 km/h és de **350,9° (NNW)**, i s'adopta
aquest valor empíric —no un rumb rodó— com a eix de la vall, de manera que la
projecció és exactament invariant a la correcció.

L'índex diari és la **component mitjana al llarg de l'eix, vall avall, entre les 00 i
les 10 UTC**:

```
u_dv = velocitat · cos(direcció − 350,9°)
```

positiva quan l'aire baixa del nord. La velocitat és la mitjana de l'interval,
derivada del recorregut de vent (`Wind Run`), no la ratxa. Es classifica com a dia
de Saligarda quan `u_dv ≥ 8 km/h` i la constància direccional `|R| ≥ 0,7`. Amb
aquest criteri surten 130 dies l'any (39,2 % dels dies amb dades).

L'índex és **continu**: el llindar només serveix per comptar dies, i totes les
conclusions s'han comprovat amb llindars de 5 a 15 km/h (§ 2.8 i script
`11_robustesa.R`).

### 1.4 Sensació de fred

Es calcula amb la fórmula JAG/TI (Environment Canada / NWS), vàlida per a T ≤ 10 °C
i V ≥ 4,8 km/h:

```
WC = 13,12 + 0,6215·T − 11,37·V^0,16 + 0,3965·T·V^0,16
```

Validada contra la columna `Wind Chill` de la Davis, dins del rang on la fórmula
s'aplica de debò: **biaix +0,053 °C, error absolut mitjà 0,100 °C, r = 0,9965** sobre
210.316 registres. Es calcula per a tota la sèrie en comptes de fer servir la columna
registrada, que només cobreix el 43 % dels registres.

---

## 2. Resultats

### 2.1 Direcció: una vall que no deixa opcions

Totes les direccions d'aquest informe estan **corregides en −32°** (§ 2.10) i són,
per tant, geogràfiques. Constància direccional |R| = 0,87 sobre 124.446 registres
nocturns d'hivern. Freqüència del sector de la vall (**NNO–N–NNE**):

| Estació | Nit i matí (00–10 UTC) | Tarda (12–20 UTC) |
|---|---|---|
| DJF | **77,0 %** | 40,5 % |
| MAM | 69,5 % | 10,9 % |
| JJA | 68,9 % | 5,4 % |
| SON | 77,4 % | 23,3 % |

El contrast dia/nit és màxim a l'estiu, quan la marinada del SSE domina completament
la tarda, i mínim a l'hivern, quan la component nord aguanta bona part del dia.
Figures **F1** (roses dels vents) i **F2** (cicle diari × mes).

### 2.2 Freqüència i cicle anual

Distribució de l'índex diari (km/h):

| Estació | P25 | Mediana | P75 | P95 | Màxim |
|---|---|---|---|---|---|
| DJF | 5,6 | **10,4** | 14,0 | 18,0 | 23,8 |
| SON | 4,9 | 7,9 | 11,0 | 15,3 | 27,5 |
| MAM | 3,3 | 6,2 | 9,1 | 13,2 | 18,1 |
| JJA | 3,8 | 5,9 | 8,2 | 11,9 | 15,9 |

Sensibilitat al llindar: ≥5 km/h → 185 dies/any; ≥8 → 130; ≥10 → 93; ≥12 → 59;
≥15 → 23.

### 2.3 Cicle de vida

| Estació | Inici | Pic | Final | Durada |
|---|---|---|---|---|
| DJF | 18 UTC | **5 UTC** | 10 UTC | 16 h |
| SON | 19 UTC | 5 UTC | 9 UTC | 13 h |
| MAM | 20 UTC | 5 UTC | 8 UTC | 12 h |
| JJA | 20 UTC | 4 UTC | 7 UTC | 11 h |

Patró de llibre de text: s'engega un parell d'hores després de la posta, culmina cap
a l'alba i s'apaga 2–3 h després de sortir el sol, quan l'escalfament del fons de
vall desfà la inversió. Els episodis d'hivern duren 5 h més que els d'estiu.

### 2.4 Dos règims

| Règim | Dies | u_dv nit | u_dv tarda | Ratxa màx. | Pressió | Tendència |
|---|---|---|---|---|---|---|
| Drenatge pur | 1.348 (85 %) | 12,0 | −1,6 | 34,0 | 1019,4 | +0,26 |
| Intermedi | 172 (11 %) | 12,9 | 4,8 | 37,3 | 1018,2 | +0,14 |
| Nord sinòptic | 60 (4 %) | 14,3 | **11,6** | **42,8** | 1020,0 | **+0,85** |

El drenatge pur inverteix el flux a la tarda; el nord sinòptic (postfrontal, amb
pressió pujant amb força) aguanta tot el dia, bufa més i es concentra a l'hivern
(49 dels 60 casos). Figura **F10**.

### 2.5 Efecte sobre temperatura, humitat i sensació de fred

**La comparació directa enganya.** Els dies sense Saligarda són sobretot dies
ennuvolats: 15,6 % amb pluja contra 2,2 %, pressió 1014 contra 1022 hPa, amplitud
tèrmica a Vic 9,6 contra 12,5 °C. Els núvols escalfen la matinada, i per això la
comparació crua dona matinades 1,8 °C *més fredes* amb Saligarda.

Controlant la temperatura simultània a Vic (la massa d'aire d'origen) i l'amplitud
tèrmica regional (el cel ras), **el signe s'inverteix**:

| Variable | Per km/h | IC 95 % | p | Episodi típic d'hivern |
|---|---|---|---|---|
| Temperatura | **+0,091** | [+0,081, +0,102] | <2e-16 | **+0,86 °C** |
| Humitat relativa | −0,192 | [−0,270, −0,115] | 1,2e-06 | −2,7 % |
| Punt de rosada | +0,045 | [+0,028, +0,063] | 2,2e-07 | +0,27 °C |
| Rebaixa per vent | −0,083 | [−0,089, −0,078] | <2e-16 | **−1,92 °C** |
| **Sensació de fred** | +0,008 | [−0,006, +0,021] | 0,25 | **−1,06 °C** |

Per a una mateixa massa d'aire, la Saligarda **escalfa la matinada** —compressió
adiabàtica en baixar 250 m i barreja mecànica que impedeix que es formi la inversió
local— i **l'asseca**. Però la rebaixa per vent supera l'escalfament: el resultat net
sobre la sensació és negatiu. **La Saligarda escalfa el termòmetre i refreda la
pell**, que és per què la memòria popular la recorda com un vent fred.

Dins de nits estrictament comparables (seques, anticiclòniques, cel ras; 1.127 dies),
comparant el terç de drenatge fort amb el fluix:

| Estació | Temperatura | Sensació | Humitat rel. | Punt de rosada |
|---|---|---|---|---|
| DJF | +0,25 (n.s.) | **−1,04** (p=0,024) | **−9,17** (p=9e-11) | −1,60 (p=1,5e-04) |
| JJA | +0,07 (n.s.) | +0,07 (n.s.) | −5,79 (p=7e-06) | −1,30 (p=6e-05) |

A l'hivern la temperatura no es mou tot i que l'aire d'origen és 1,9 °C més fred: el
drenatge compensa exactament el refredament. El senyal net més robust és
l'**assecament** (−9,2 punts d'humitat relativa).

Figures **F5** (cicles diaris) i **F7** (efecte dins de nits comparables).

### 2.6 Contrast amb Vic

A les 06 UTC la Garriga és sempre més càlida que Vic —la Plana de Vic és una conca
de 500 m amb una inversió molt més profunda— i la Saligarda amplia el contrast:

| Estació | ΔT amb Saligarda | ΔT sense | ΔHR amb | ΔHR sense |
|---|---|---|---|---|
| DJF | **+6,37 °C** | +4,15 | −20,2 % | −11,6 |
| MAM | +4,68 | +3,00 | −17,1 | −9,4 |
| JJA | +3,62 | +2,51 | −14,5 | −8,8 |
| SON | +5,00 | +3,48 | −17,8 | −11,3 |

L'increment del desnivell tèrmic (+1,4 a +2,2 °C) és de l'ordre de l'escalfament
adiabàtic sec que correspon a baixar 250 m (≈2,5 K). A MAM, JJA i SON l'acoblament
entre les dues estacions també és més estret amb Saligarda (r puja de 0,908 a 0,953
al JJA, i la desviació de la diferència baixa de 1,08 a 0,92 °C): les dues respiren
la mateixa massa d'aire. Figures **F6** i **F8**.

### 2.7 Situació sinòptica

Amb variables d'estació (no hi ha baròmetre a Vic ni reanàlisi a la carpeta original):

| | DJF amb Saligarda | DJF sense |
|---|---|---|
| Pressió mitjana | **1022,4 hPa** | 1014,4 |
| Tendència baromètrica | +0,49 hPa/dia | −0,81 |
| Oscil·lació diària de P | 5,22 hPa | 7,02 |
| Amplitud tèrmica a Vic | 12,5 °C | 9,6 |
| Irradiància relativa a Vic | 0,75 | 0,59 |
| Dies amb pluja | 2,2 % | 15,6 % |

Hi ha una relació dosi-resposta neta amb la pressió (hivern):

| Quintil de pressió | Pressió | u_dv | Fracció de dies amb Saligarda |
|---|---|---|---|
| Q1 | 1005,3 | 6,0 | 0,31 |
| Q3 | 1020,5 | 10,2 | 0,69 |
| Q5 | 1031,2 | **12,9** | **0,85** |

Correlació pressió–intensitat a l'hivern: r = 0,488 (p < 2e-16). Figura **F9**.

#### Reanàlisi NCEP/NCAR (1980–2024)

S'han baixat 45 anys de pressió al nivell del mar (2,5°, 6-horària, finestra
32,5–52,5 N × 20 W–20 E, 153 punts, 16.437 dies) amb `RNCEP`, promitjant els passos
de les 00 i les 06 UTC, que és la finestra de l'episodi.

**Composit d'hivern** (643 dies amb Saligarda contra 377 sense, figures **F15** i
**F15b**): l'anomalia és **positiva a tot el domini** —els dies de Saligarda són
simplement dies d'anticicló— amb un màxim de **+11,3 hPa al canal de la Mànega
occidental (50 N, 2,5 O)** que decreix monòtonament cap al sud-est: +10,2 a Biscaia,
+8,3 sobre Catalunya, +7,6 a les Balears, +4,8 a Algèria. Sobre Catalunya la
diferència de +8,3 hPa coincideix amb la que dona el baròmetre de la Garriga
(+8,0 hPa), obtinguda de manera independent.

Els mapes porten dibuixat el **vent geostròfic** calculat del mateix camp per
diferències centrades (`ug = −(1/ρf)·∂p/∂y`, `vg = (1/ρf)·∂p/∂x`, amb `f` variable
amb la latitud i correcció de la convergència dels meridians en passar de m/s a
graus). El conveni de signes s'ha verificat amb un anticicló gaussià sintètic: el
gir surt horari i les quatre comprovacions cardinals encerten al desè de grau.

Sobre Catalunya (42,5 N) el vent geostròfic del camp compost és **del 020° a
25,9 km/h** els dies de Saligarda i del 352° a 14,5 km/h els dies sense; l'anomalia
sola dona del 047° a 14,8 km/h. La coincidència amb l'eix de drenatge observat
(22,5°) indica que el flux sinòptic d'aquells dies està **alineat** amb la vall i no
la frena, no pas que la Saligarda sigui vent sinòptic: qui ho separa és el cicle
diari (§ 2.4), on el 85 % dels episodis inverteixen la direcció a la tarda.

Cal llegir el composit amb tres reserves: és la mitjana de 643 situacions i cap dia
concret s'hi assembla; el màxim cau a 2,5° de la vora nord del domini; i és només
pressió al nivell del mar, de manera que no hi surt el que realment governa la
intensitat, que és el refredament radiatiu local (§ 2.7b).

**Tipus de circulació.** PCA en mode S sobre les anomalies diàries (5 components,
93 % de la variància) i agrupament en 9 tipus:

| Tipus | Dies de Saligarda | u_dv mitjà | Fracció del total de dies |
|---|---|---|---|
| 1 | **66 %** | 10,8 km/h | 8,5 % |
| 7 | 49 % | 8,6 | 7,7 % |
| 4 | 41 % | 8,0 | 10,2 % |
| … | | | |
| 2 | 22 % | 5,6 | 13,2 % |

Només un tipus supera el 60 % de dies amb Saligarda. El contrast entre el tipus més
favorable i el menys favorable és de 66 % contra 22 % de dies i de 10,8 contra
5,6 km/h.

**Reconstrucció de 45 anys.** Amb un model de la component vall avall sobre les
components principals de la circulació més el cicle anual, validat deixant un any
fora cada vegada: r = 0,539 a escala diària (R² = 0,29; RMSE 3,84 km/h) i **r = 0,735
a escala anual**. La circulació de gran escala explica, doncs, prop d'un terç de la
variància diària —la resta la posa el refredament radiatiu local, que una graella de
2,5° no resol— però segueix prou bé les variacions d'any en any.

Aplicant el model a tot el període (figura **F17**):

| Mètrica | Pendent de Sen | IC 95 % | tau | p |
|---|---|---|---|---|
| Índex reconstruït | −0,0024 km/h/any | [−0,0078, +0,0029] | −0,08 | 0,42 |
| Dies de Saligarda esperats | −0,074 dies/any | [−0,202, +0,079] | −0,09 | 0,39 |
| Dies amb circulació favorable | −0,080 dies/any | [−0,357, +0,219] | −0,05 | 0,61 |
| Índex reconstruït, DJF | −0,0018 km/h/any | [−0,0222, +0,0192] | −0,02 | 0,85 |
| Dies esperats, DJF | −0,022 dies/any | [−0,168, +0,139] | −0,03 | 0,78 |

**Cap tendència en 45 anys.** La variabilitat interanual és gran (de 125 a 155 dies
esperats l'any) però no hi ha deriva. Amb la reserva que això demostra que *no han
canviat les condicions sinòptiques que generen la Saligarda*, no directament que no
hagi canviat el vent: la part local del senyal queda fora de la reanàlisi. Ara bé,
apunta en la mateixa direcció que les observacions directes (§ 2.8).

### 2.7b Què determina la força d'un episodi

Un flux de drenatge s'accelera per la flotabilitat negativa de l'aire fred acumulat a
la conca, de manera que la intensitat hauria de dependre sobretot del refredament
radiatiu. És exactament el que passa (n = 4.052 nits):

| Model | R² acumulat |
|---|---|
| Només cicle anual | 0,134 |
| + amplitud tèrmica diària a Vic (indicador de cel ras) | **0,331** |
| + pressió mitjana | 0,376 |
| + contrast tèrmic Vic − Garriga del vespre anterior | 0,409 |

| Predictor | Coeficient | p |
|---|---|---|
| Amplitud tèrmica a Vic | **+0,454 km/h per °C** | <2e-16 |
| Pressió | +0,157 km/h per hPa | <2e-16 |
| Contrast Vic − Garriga al vespre | −0,629 km/h per °C | <2e-16 |

El signe del tercer predictor és el que toca: com més freda ja és Vic respecte de la
Garriga en pondre's el sol, més fort baixa el drenatge aquella nit. Del quintil de
nits més ennuvolades al de més clares, l'índex passa de 6,0 a 9,8 km/h i la fracció
de dies de Saligarda de 0,29 a 0,49. El comportament és consistent a les quatre
estacions (R² de 0,31 a 0,36). Figura **F18**.

### 2.8 Tendència temporal

**Període homogeni (5 min, 2013–2023, n = 11 anys complets): cap tendència.**
El 2012 s'exclou perquè només té 152 dies amb recorregut de vent.

| Mètrica | Pendent de Sen | IC 95 % | tau | p |
|---|---|---|---|---|
| Dies de Saligarda | +0,50 dies/any | [−3,75, +3,67] | +0,07 | 0,81 |
| Índex mitjà | +0,003 km/h/any | [−0,145, +0,142] | +0,02 | 1,00 |
| Intensitat dels episodis | −0,002 km/h/any | [−0,060, +0,027] | −0,02 | 1,00 |
| Percentil 95 de l'índex | +0,027 km/h/any | [−0,130, +0,140] | +0,05 | 0,88 |
| Durada dels episodis | −0,037 h/any | [−0,170, +0,039] | −0,31 | 0,21 |
| Hora de finalització | −0,018 h/any | [−0,069, +0,034] | −0,20 | 0,44 |
| Sensació mínima en episodis | +0,144 °C/any | [−0,252, +0,404] | +0,31 | 0,21 |
| Dies amb sensació < 0 °C | −1,00 dies/any | [−3,71, +1,25] | −0,24 | 0,35 |
| *Ratxa màxima dels episodis* | *−0,114 km/h/any* | *[−0,226, −0,008]* | *−0,56* | *0,02* |

Per estacions, tampoc: DJF +0,50 dies/any (p = 0,64), MAM 0,00 (p = 1), JJA 0,00
(p = 1), SON +0,16 (p = 0,84).

L'**única** mètrica amb p < 0,05 és la ratxa màxima mitjana dels episodis
(−0,114 km/h/any, és a dir −1,25 km/h en 11 anys). Cal prendre-la amb reserves: no
sobreviu la correcció per comparacions múltiples (12 tests: amb Bonferroni caldria
p < 0,004), la velocitat mitjana no mostra res (−0,002 km/h/any, p = 1) i un
descens lent i monòton només de les ratxes és també el que produiria el desgast
progressiu dels coixinets d'un anemòmetre de cassoletes. Convindria comprovar
l'historial de manteniment de l'aparell abans de llegir-ho com un senyal atmosfèric.

**La sèrie llarga no serveix.** El full de l'Ajuntament semblava donar
+4,76 dies/any (p = 0,0028) i +0,32 km/h/any (p = 0,01), però:

- El test de **Pettitt detecta una ruptura el 2011** (K = 94, p = 0,0036).
- La ratxa màxima diària mitjana salta de **27,8 km/h (2004–2010) a 32,2 (2011–2023)**,
  un esglaó de +4,4 km/h.
- La fracció de dies amb ratxa ≥ 30 km/h passa de 0,25 a 0,49.
- `strucchange` situa el punt de ruptura el 2010.
- **Dins de cada tram no hi ha tendència**: +0,027 km/h/any (2002–2011) i
  −0,065 (2012–2023).
- La ruptura coincideix amb l'inici del registre de 5 minuts (març de 2011).

La temperatura mostra una anomalia coherent amb un canvi d'entorn: entre 2002 i 2023
la Tmín puja +0,087 °C/any mentre la Tmàx queda plana (−0,002 °C/any). Una
divergència d'aquesta magnitud no és climàtica.

**Conclusió: la Saligarda no s'està ni intensificant ni afeblint.** Les tres vies
independents hi convergeixen:

1. Observació directa homogènia (11 anys): res, amb tots els p ≥ 0,21 llevat de la
   ratxa dels episodis, que no resisteix la correcció per comparacions múltiples.
2. Sèrie llarga (22 anys): l'aparent intensificació és una ruptura instrumental del
   2011; dins de cada tram, res.
3. Circulació sinòptica reconstruïda (45 anys, § 2.7): res, amb IC estrets.

Onze anys homogenis serien massa pocs per concloure-hi res tot sols; el que dona
pes a la conclusió és que la reconstrucció de 45 anys, amb molta més potència
estadística, digui exactament el mateix. Figures **F11**, **F14** i **F17**.

### 2.9 Robustesa

| Prova | Resultat |
|---|---|
| Índex amb ratxa en comptes de velocitat mitjana | r = 0,990; tendència igual de nul·la |
| Finestra nocturna (5 variants de 00–10 a 04–10) | 127–189 dies/any; totes p ≥ 0,24 |
| Llindar de 5 a 15 km/h | totes les tendències p ≥ 0,27 |
| Exclusió del nord sinòptic | efecte sobre T: +0,093 vs +0,091 per km/h |
| Resolució de 16 rumbs de la penella | r = 0,99994; 0,4 % de dies reclassificats |

### 2.10 La penella està desalineada després del 2011

El rumb del Congost des de la Garriga cap amunt, calculat amb les coordenades dels
nuclis del fons de vall, és de **350° cap al Figaró, 344° cap a Aiguafreda i 338° cap
a Centelles**. La direcció mesurada del vent nocturn, en canvi, és de 22,9°: una
trentena llarga de graus de diferència, que la resta de la secció quantifica en
**+32°**.

El full de l'Ajuntament permet resoldre-ho, perquè hi consta la direcció **en graus
reals fins al 2010** (95–117 valors diferents per any) i en **rumbs de 16 a partir
del 2012** (20–24 valors). Direcció de les ratxes ≥ 30 km/h del semicercle nord:

| Període | Direcció mitjana | R |
|---|---|---|
| 2003–2008 | **342–344°** | 0,93 |
| 2009 | 354° | |
| 2010 | 2° | |
| 2011 | 11° | 0,87 |
| 2012–2023 | 15° (rang 7–27) | 0,82 |

Entre el 2003 i el 2008 la mesura coincideix **exactament** amb el rumb de la vall.
Després gira ~32° en sentit horari, de manera **gradual entre 2009 i 2011**. Per
octants, la cua del NO (31 % dels dies abans del 2011) es converteix en una cua del
NE (25 % després).

Una prova independent del terreny ho corrobora: en una vall canalitzada el flux
nocturn i el de tarda han de ser antiparal·lels, i ho són (MAM 163,8°, JJA 161,8°,
SON 179,1°), amb un eix intern coherent. L'aparell és consistent amb ell mateix; el
que no quadra és amb el terreny.

#### És una rotació rígida, no un problema d'exposició

Calia distingir entre **penella mal orientada** (rotació rígida, igual a tots els
sectors) i **obstacles a l'emplaçament** (deflexió que depèn del sector i de la
intensitat). El test decisiu és comparar dos règims físicament independents:

| Règim | 2003–2008 | 2012–2023 | Desfasament |
|---|---|---|---|
| Drenatge nocturn (sector nord) | 343,2° | 14,9° | **+31,8°** |
| Marinada d'estiu (sector sud) | 151,1° | 185,4° | **+34,3°** |

El drenatge del nord i la marinada del sud **han girat el mateix**. Això és una
rotació rígida: la penella. La correcció adoptada és de **−32°** (fitxer
`config_direccio.R`), amb una incertesa d'uns ±2° segons el llindar i el sector
que es faci servir per estimar-la.

Amb ratxes molt fortes (≥45 km/h) l'estimació baixa a +22°, però allà la
quantització en 16 rumbs atrapa la mitjana vectorial entre els compartiments de 0°
i 22,5° i el valor no és fiable.

**Validació de la correcció:** un cop aplicada, l'eix de drenatge deduït de les
dades passa a ser **350,9°**, que coincideix amb el rumb geogràfic del Congost cap
al Figaró (350,4°), el tram que la Garriga té just aigües amunt, a 4 km. I l'índex
`u_dv` queda **invariant** fins a la tercera xifra (mediana 7,18 contra 7,17 km/h;
p95 15,63 contra 15,64), com havia de ser: si giren les direccions, gira l'eix amb
elles.

En tot cas, **la ruptura del 2011 (§ 2.8) té dos símptomes independents** —el salt
de +4,4 km/h a la ratxa i aquesta rotació— i és pràcticament segur que hi va haver
una intervenció a l'estació cap al 2009–2011. Figura **F23**, scripts `13_`, `13b_`
i `config_direccio.R`.

### 2.11 Pronòstic diari

Model per emetre a les 18 UTC del dia D−1: cap predictor posterior a les 17 UTC,
excepte les components sinòptiques del dia D, que en operatiu vindrien d'un model
numèric (prognosi perfecta). Validació deixant un any sencer fora, 2013–2023,
3.750 nits.

**Intensitat (u_dv, km/h):**

| Model | R² | RMSE | Destresa vs climatologia |
|---|---|---|---|
| Climatologia (cicle anual) | 0,133 | 4,23 | — |
| Persistència (la nit d'ahir) | −0,073 | 4,71 | −0,24 |
| A1 sinòptic | 0,287 | 3,84 | +0,18 |
| A2 local (observacions del vespre) | 0,360 | 3,64 | +0,26 |
| A3 sinòptic + local | 0,393 | 3,54 | +0,30 |
| A4 bosc aleatori | **0,408** | **3,50** | **+0,32** |

Per estacions: DJF R² = 0,386, SON 0,323, MAM 0,277, JJA 0,275.

**Ocurrència (probabilitat de Saligarda):**

| Model | AUC | Brier | Encerts | POD | FAR | PSS |
|---|---|---|---|---|---|---|
| Climatologia | 0,722 | 0,203 | 0,691 | 0,54 | 0,39 | 0,33 |
| B1 sinòptic | 0,774 | 0,183 | 0,731 | 0,56 | 0,32 | 0,40 |
| B2 local | 0,806 | 0,172 | 0,745 | 0,59 | 0,29 | 0,43 |
| B3 sinòptic + local | **0,814** | **0,168** | 0,752 | 0,60 | 0,29 | **0,45** |
| B4 bosc aleatori | 0,814 | 0,168 | 0,753 | 0,57 | 0,27 | 0,44 |

**Sensació de fred mínima:** bosc aleatori, RMSE 2,11 °C, destresa +0,59 sobre la
climatologia (el R² de 0,92 està inflat pel cicle estacional i no s'ha de citar sol).

Predictors més importants: cicle anual, temperatura del vespre a Vic, pressió a la
Garriga, primera component sinòptica, i la intensitat de la nit anterior.

Dues lectures que compten. **La persistència no serveix**: saber què va fer ahir és
pitjor que no saber res, perquè els episodis van lligats al pas de situacions
sinòptiques i no a la inèrcia. I **el guany del model numèric és petit**: el model
només amb observacions del vespre (A2/B2) ja dona R² = 0,360 i AUC = 0,806, molt a
prop del model complet, i aquest sí que és plenament operatiu perquè no depèn de cap
pronòstic. Figures **F19**–**F21**, script `12_`.

### 2.12 Sistema operatiu de pronòstic

El model de § 2.11 depenia de les components principals de la reanàlisi NCEP, que en
operatiu no existeixen. S'ha refet amb **predictors físics directes d'Open-Meteo**,
que serveix l'**arxiu ERA5** per entrenar i el **pronòstic** per operar amb els
mateixos noms i unitats: el desajust entre entrenament i operació queda reduït a
l'error del model numèric, que és l'únic inevitable.

Quatre punts de graella (la Garriga, 45 N 2 E, 40,5 N 3 E i 41,65 N −0,9 E) donen
gradients regionals de pressió que fan la feina de les components principals sense
PCA. I s'hi incorpora el predictor que faltava: la **nuvolositat**.

| Objectiu | Model | Destresa (validació any a any, 3.855 nits) |
|---|---|---|
| Intensitat | climatologia | R² = 0,133 · RMSE 4,24 |
| | **OP-A (només Open-Meteo)** | **R² = 0,574 · RMSE 2,97 · destresa +0,51** |
| | OP-B (+ estació pròpia) | R² = 0,572 · RMSE 2,98 |
| Ocurrència | climatologia | AUC = 0,722 |
| | **OP-A** | **AUC = 0,866 · Brier 0,143 · POD 0,65 · FAR 0,22** |
| Sensació de fred | climatologia | RMSE 3,31 °C |
| | **OP-A** | **RMSE 1,42 °C · destresa +0,82** |

Comparat amb el model de § 2.11 (R² 0,408, AUC 0,814), el salt és gran, i ve
sobretot de la nuvolositat i del vent del model numèric. Per estacions, R² de 0,561
(DJF) a 0,442 (JJA).

**L'estació pròpia ja no aporta res** (0,572 contra 0,574): el sistema es pot
automatitzar del tot sense dependre dels Excel diaris de la Davis.

Predictors més importants: component nord-sud del vent del model (100), component
est-oest (46), nuvolositat baixa (42), cicle anual (38), temperatura i punt de rosada
(37), nuvolositat total (31), pressió (28).

**Circuit muntat:**

| Peça | Què fa |
|---|---|
| `14_baixa_openmeteo.R` | arxiu ERA5 2012–2024, quatre punts (es cacheja) |
| `15_model_operatiu.R` | entrena, valida i desa `derived/model_operatiu.rds` |
| `16_pronostic.R` | baixa el pronòstic, prediu i escriu el butlletí |
| `pronostic.bat` | embolcall per a la tasca programada |
| Tasca «Saligarda pronostic» | diària a les 19:00 h locals |

Sortides de cada execució: `pronostic.txt`, `pronostic.html`, una fila a
`derived/pronostics.csv` (per verificar la destresa real amb el temps) i el registre
a `derived/pronostic_execucions.log`.

L'hora d'emissió, les 19:00 locals, tanca la finestra de 12–17 UTC tant a l'hivern
com a l'estiu, i deixa la nit objectiu (00–10 UTC del dia següent) sencera per
davant.

#### Destresa real, no de laboratori

Els números anteriors surten d'alimentar el model amb ERA5, que és una anàlisi feta
a posteriori. Amb l'arxiu de **prediccions passades** d'Open-Meteo es pot mesurar la
degradació real: entrenant amb ERA5 i predint amb el que els models numèrics van
predir de debò, sobre 727 nits de 2022–2024, la intensitat perd només un **9 % de
R²** i l'ocurrència **no perd AUC**. A 6–16 h de marge, els models encerten prou bé
la nuvolositat i la pressió, que és el que mana. Scripts `17_` i `18_`/`18b_`.

Amb pronòstic real i segons com es defineixi l'episodi:

| Llindar | Diu que sí | Encert global | Precisió | Cobertura |
|---|---|---|---|---|
| ≥8 km/h | 39 % dels dies | 79 % | 75 % | 71 % |
| ≥12 km/h | 14 % | 89 % | **78 %** | 59 % |
| ≥15 km/h | 5 % | 95 % | 69 % | 53 % |

Compte amb l'encert global: un pronòstic que digués **sempre que no** encertaria el
61 %, el 82 % i el 93 % respectivament, de manera que el guany real és de +18, +7 i
+2 punts. Com més rar es fa l'esdeveniment, més fàcil és semblar encertat i menys
s'informa.

El model desplegat usa **300 arbres i mida mínima de node 10**, no 800/5: el
fitxer baixa de 24 a 6 MB —cosa que compta per fer-lo viatjar a un repositori— i
la destresa només cau 0,003 de R² i 0,002 d'AUC. La validació creuada s'ha fet
amb **els mateixos paràmetres** que el model desplegat: el que es valida ha de
ser exactament el que s'executa.

#### Bot de publicació diària

`19_bot_x.R` publica cada vespre a X el pronòstic per al **matí següent de 6 a 11
hora local**, la finestra que viu la gent, no la de l'estudi (00–10 UTC). Sense
enllaços, perquè a X un post amb enllaç costa 0,20 $ i un sense, 0,015 $ — un post
diari surt per uns 5,5 $ l'any.

La primera versió publicava la **probabilitat** d'episodi, que té la virtut de no
poder ser mai falsa: amb un model ben calibrat, un 62 % vol dir 62 %. Però un
número que mai no s'equivoca tampoc no diu gran cosa a qui ha de decidir si
s'emporta el tallavents, i deixava muda la brisa fluixa, que és la més freqüent.
La versió actual publica **la intensitat esperada en km/h i un adjectiu**, i
accepta equivocar-se. Els llindars de l'adjectiu són 6, 8, 11, 14 i 17 km/h
(pràcticament calma / molt suau / suau / moderada / forta / molt forta).

#### Calibratge de la intensitat publicada

Un bosc de regressió prediu la **mitjana condicional**, que per construcció
s'encongeix cap al centre de la distribució: la desviació típica de les
prediccions és de 3,3 km/h contra 5,2 de les observacions. Això, que és correcte
com a estimador, és dolent com a missatge. En validació any a any el biaix era
sistemàtic i anava tot en la mateixa direcció:

| Banda observada | Real (km/h) | El bot deia | Biaix |
|---|---|---|---|
| pràcticament calma | 2,8 | 5,4 | +2,7 |
| molt suau | 7,0 | 7,2 | +0,3 |
| suau | 9,4 | 8,5 | −0,9 |
| moderada | 12,4 | 10,1 | −2,4 |
| forta | 15,3 | 11,7 | −3,6 |
| molt forta | 19,2 | 13,1 | **−6,1** |

Els matins de Saligarda molt forta —els únics que la gent recorda i pels quals
jutjarà el bot— sortien anunciats com a moderats. La correcció aplicada estira la
dispersió al voltant de la mitjana d'entrenament, `x = m + (p − m)·k`. El factor
que igualaria del tot les variàncies seria k = 1,54, però costa precisió; amb
**k = 1,2** milloren les tres mesures alhora, cosa que no sol passar:

| k | RMSE | Adjectiu exacte | ±1 banda | Biaix dies forts |
|---|---|---|---|---|
| 1,00 (sense corregir) | 3,48 | 44 % | 83 % | −4,4 |
| **1,20** | **3,46** | **46 %** | 83 % | **−3,5** |
| 1,54 (inflació completa) | 3,70 | 46 % | 81 % | −2,0 |

S'aplica a les **dues** intensitats que publica el post, la de 6–11 i la del tram
6–9, amb el mateix factor i cadascuna al voltant del seu centre: la línia «més
marcat a primera hora» salta quan la diferència entre totes dues supera 2,5 km/h,
i inflar-ne només una l'hauria feta emmudir.

L'ordre de magnitud de l'error que veurà el lector: mediana 2,3 km/h, dins de
±4 km/h el 77 % dels dies i de ±6 el 92 %.

#### Operació

Les credencials no són al projecte: van a `%USERPROFILE%/.saligarda_x.json`. Si el
fitxer no hi és, el script redacta el post i el desa a `post_x.txt` però no envia
res. `derived/posts_x.csv` registra què s'ha publicat i quan, i hi ha un guard que
evita repetir la mateixa nit. Amb `--prova` es pot assajar sense enviar; amb
`--verifica`, comprovar les credencials sense publicar; i amb `--nit=` / `--p=` /
`--u=` / `--u1=` / `--wc=` es pot simular qualsevol nit per veure el format.

`derived/pronostics.csv` guarda **una fila per nit**: el workflow s'executa dos
cops al dia per cobrir els retards del cron de GitHub, i el segon intent
actualitza la predicció en lloc d'afegir-ne una de nova. Si acumulés duplicats,
la verificació posterior contra les observacions els comptaria dos cops.

---

## 3. Limitacions

1. **Longitud del registre.** Dotze anys homogenis. Cap conclusió sobre tendències
   climàtiques és possible amb aquesta longitud.
2. **Ruptura del 2011.** Ara amb dos símptomes: salt de +4,4 km/h a la ratxa i
   rotació de ~32° de la direcció (§ 2.10). Convindria buscar la metadada de
   l'estació —canvi d'anemòmetre, de pal o d'emplaçament— per confirmar-la i, si és
   possible, homogeneïtzar la sèrie 2002–2010, que és la que quadra amb el terreny.
3. **Direcció en 16 rumbs** (només des del 2012; abans en graus reals) i associada a
   la ratxa, no a la mitjana de l'interval. L'efecte de la quantització és
   menyspreable (§ 2.9); el de la desalineació, nul per als resultats però important
   per a la interpretació geogràfica (§ 2.10).
4. **Vic com a control.** El model de § 2.5 fa servir la temperatura simultània a Vic
   per aïllar el transport. Vic està a 25 km i el drenatge del Congost és una part
   petita del balanç d'aire de la Plana, però estrictament és una variable
   parcialment intermèdia i el coeficient s'ha de llegir amb aquesta reserva.
5. **Sense sondeigs ni perfils verticals**, la profunditat de la capa de drenatge i el
   número de Froude de l'escolament pel congost queden fora de l'abast.

---

## 4. Reproducció

```bash
Rscript 00_run_all.R          # tot (lent: llegeix 181 MB i 4.710 Excel)
Rscript 00_run_all.R rapid    # salta la reconstrucció de dades
```

| Script | Què fa |
|---|---|
| `config_direccio.R` | correcció de −32° de l'orientació de la penella |
| `01_build_vic.R` | XEMA cru → `vic.csv` (corregeix el bug que sobreescrivia `saligarda.csv`) |
| `02_build_garriga.R` | 4.710 Excel → `derived/la_garriga_5min.csv` |
| `03_build_ajuntament.R` | full de l'Ajuntament → sèrie diària 2002–2024 |
| `04_qc_temps.R`, `04b_qc_solar.R` | referència horària |
| `05_climatologia_vent.R` | eix, roses, cicle diari, sensació de fred |
| `06_index_saligarda.R` | índex diari i classificació d'episodis |
| `07_efecte_T_HR.R` | efecte directe sobre T, HR, rosada i sensació |
| `07b_efecte_identificat.R` | efecte identificat, controlant el cel ras |
| `08_sinoptica.R` | situació regional amb variables d'estació |
| `08b_intensitat.R` | què determina la força d'un episodi |
| `09_tendencies.R` | tendències (Mann-Kendall i pendent de Sen) |
| `09b_homogeneitat.R` | Pettitt i punts de ruptura |
| `10a`/`10b_sinoptica_reanalisi.R` | reanàlisi NCEP/NCAR: compostos i tipus |
| `11_robustesa.R` | proves de sensibilitat |
| `12_model_predictiu.R` | pronòstic diari validat any a any |
| `13_direccio_vs_vall.R`, `13b_offset_penella.R` | rumb de la vall i desalineació de la penella |
| `14_baixa_openmeteo.R` | arxiu ERA5 d'Open-Meteo per al model operatiu |
| `15_model_operatiu.R` | entrena i desa el model de producció |
| `16_pronostic.R` + `pronostic.bat` | pronòstic diari automàtic |

Dependències: `data.table`, `readxl`, `future.apply`, `ggplot2`, `Kendall`,
`strucchange`, `suncalc`, `RNCEP`, `sf`, `rnaturalearth`.

`10a`/`10b` van a part de `00_run_all.R`: la descàrrega de 45 anys de reanàlisi triga
prop d'una hora i es guarda a `derived/ncep_slp.rds`, de manera que només cal fer-la
un cop.

### Figures

| | |
|---|---|
| F1 | Roses dels vents per estació i franja horària |
| F2 | Cicle diari × mes de la component vall avall |
| F3 | Cicle diari de la velocitat per estació |
| F4 | Distribució de l'índex diari |
| F5 | Cicles diaris de T, sensació, rosada i HR segons hi hagi Saligarda |
| F6 | Contrast la Garriga − Vic hora a hora |
| F7 | Efecte del drenatge dins de nits comparables |
| F8 | Acoblament tèrmic amb la Plana de Vic |
| F9 | Situació regional (pressió, amplitud, irradiància, tendència) |
| F10 | Cicle de vida: drenatge pur contra nord sinòptic |
| F11 | Freqüència anual i tendència |
| F12 | Cicle anual de la intensitat |
| F13 | Tendència de la sensació de fred |
| F14 | Ruptura del 2011 a la sèrie de vent |
| F15, F15b | Composits i anomalia de pressió (reanàlisi), amb vent geostròfic |
| F16 | Nou tipus de circulació i propensió a la Saligarda, amb vent geostròfic |
| F17 | Reconstrucció de la freqüència, 1980–2024 |
| F18 | La intensitat contra el refredament radiatiu a Vic |
| F19 | Pronòstic de la intensitat: observat contra predit |
| F20 | Fiabilitat de la probabilitat de Saligarda |
| F21 | Importància dels predictors |
| F22 | Direcció nocturna segons la intensitat |
| F23 | Rotació de la direcció el 2011 |
| F24 | Importància dels predictors del model operatiu |
| qc_hora, qc_solar | Control de la referència horària |

### Nota sobre `synoptReg`

El paquet `synoptReg` (classificació sinòptica) **va ser retirat del CRAN
l'octubre de 2023** perquè el correu del mantenidor havia deixat de funcionar. Només
s'instal·la des de l'arxiu del CRAN o del GitHub de Lemus-Cánovas, i en tot cas no
descarrega dades: fa la classificació sobre NetCDF que li has de proporcionar tu
(típicament ERA5 via `ecmwfr`, que demana registre a la CDS). Aquí la classificació
es fa amb `prcomp` + `kmeans`, que és el mateix mètode (PCA en mode S i agrupament)
sense la dependència, i les dades es baixen amb `RNCEP`, que no necessita clau.
