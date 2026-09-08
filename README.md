# Saligarda

Estudi del vent catabàtic de la vall del Congost a la Garriga, i bot que en
publica el pronòstic diari.

- **[INFORME.md](INFORME.md)** — l'estudi complet: caracterització, efecte sobre
  temperatura, humitat i sensació de fred, situació sinòptica i tendències.
- **[figures/](figures/)** — les 24 figures.

## El bot

Cada vespre calcula la probabilitat que la matinada següent hi hagi Saligarda
(definida com una component mitjana vall avall ≥ 12 km/h entre les 00 i les
10 UTC) i en publica un post.

Publica la **probabilitat**, no un sí o un no: el model està ben calibrat, de
manera que un 62 % vol dir 62 % i el post no pot ser mai fals.

Destresa, validada deixant un any sencer fora i amb pronòstic real (no
reanàlisi), sobre 727 nits de 2022–2024: **AUC 0,90**, precisió del 78 % i
cobertura del 59 %.

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
Rscript 19_bot_x.R --prova --p=0.72 --u=14 --wc=-2 --nit=2026-01-15
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
