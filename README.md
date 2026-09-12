# Saligarda

Estudi del vent catabàtic de la vall del Congost a la Garriga, i bot que en
publica el pronòstic diari.

- **[INFORME.md](INFORME.md)** — l'estudi complet: caracterització, efecte sobre
  temperatura, humitat i sensació de fred, situació sinòptica i tendències.
- **[figures/](figures/)** — les 24 figures.

## El bot

Cada vespre pronostica quina Saligarda farà **el matí següent, de 6 a 11 hora
local**, i en publica un post: la intensitat esperada en km/h (component mitjana
vall avall) i un adjectiu.

La finestra és la que viu la gent, no la de l'estudi (00–10 UTC). A l'hivern
coincideix amb el màxim del drenatge: entre les 6 i les 9 del matí la component
mitjana és de 10,5–10,7 km/h, el punt més fort de tot el cicle. Entrenar
directament sobre aquest tram el prediu millor que reciclar el model nocturn
(R² 0,533 contra 0,500), tot i que el matí és intrínsecament més difícil que la
nit sencera perquè és quan el drenatge s'apaga, i el moment exacte depèn de com
d'aviat escalfi el sol el fons de vall.

Destresa del model del matí, validada deixant un any sencer fora: **AUC 0,890**
i R² 0,533 per a la intensitat. Uns 48 episodis l'any.

Sobre el número que es publica: l'error mitjà és de 2,3 km/h i l'adjectiu és
exacte el 46 % dels dies, a una banda de distància el 83 %. La predicció porta
una correcció de l'encongiment cap a la mitjana (§ 2.12 de l'informe); sense
ella, els matins de Saligarda molt forta —19 km/h de mitjana real— sortien
anunciats a 13.

### Com funciona

```
16_pronostic.R   baixa el pronòstic d'Open-Meteo de 4 punts, hi aplica el model
                 i escriu pronostic.txt, pronostic.html i derived/pronostics.csv
19_bot_x.R       redacta el post i el publica a X
```

S'executa sol per GitHub Actions (`.github/workflows/saligarda.yml`), dos cops
al dia per si el `cron` s'endarrereix; hi ha guards que impedeixen publicar dos
cops la mateixa nit o publicar una nit que ja hagi començat.

### Provar-ho sense publicar

```bash
Rscript 19_bot_x.R --prova
Rscript 19_bot_x.R --prova --u=14 --u1=17 --wc=-2 --nit=2026-01-15
Rscript 19_bot_x.R --verifica
```

A Windows, `prova.bat` fa el mateix sense haver d'escriure la ruta d'R.

### Credencials

Mai al repositori. En local, a `%USERPROFILE%/.saligarda_x.json`; a GitHub
Actions, com a secrets: `X_API_KEY`, `X_API_SECRET`, `X_ACCESS_TOKEN`,
`X_ACCESS_TOKEN_SECRET`. Sense credencials el bot redacta el post però no
envia res.

## Reproduir l'estudi

Les dades brutes (4.710 Excel diaris de l'estació Davis, el CSV de la XEMA i la
reanàlisi NCEP) no són al repositori pel seu pes. Amb elles a lloc:

```bash
Rscript 00_run_all.R          # tot
Rscript 00_run_all.R rapid    # salta la reconstrucció de dades
```

Dependències: `data.table`, `readxl`, `future.apply`, `ggplot2`, `Kendall`,
`strucchange`, `suncalc`, `RNCEP`, `sf`, `rnaturalearth`, `ranger`, `pROC`,
`jsonlite`, `httr`.

## Dues coses que cal saber de les dades

1. El `saligarda.csv` de l'arrel **no** són dades de la Garriga: és el dataset
   de Vic, perquè un script antic hi va escriure a sobre. La reconstrucció bona
   és `derived/la_garriga_5min.csv`.
2. L'estació té una **ruptura instrumental el 2011**: la ratxa màxima diària
   salta de 27,8 a 32,2 km/h i la penella queda girada +32°. La rotació està
   corregida (`config_direccio.R`); el salt de magnitud no es pot corregir i
   invalida qualsevol tendència calculada sobre 2002–2023.
