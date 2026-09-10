# ==============================================================================
# 19_bot_x.R -- Publica el pronostic diari de Saligarda a X
#
#   El crida pronostic.bat just despres de 16_pronostic.R, que ja ha deixat la
#   prediccio a derived/pronostics.csv.
#
#   DISSENY
#   - Publica la PROBABILITAT, no un si/no. El model esta ben calibrat (el
#     diagrama de fiabilitat cau sobre la diagonal), de manera que un 62% vol
#     dir 62% de debo i el post no pot ser mai "fals".
#   - L'episodi es el tram de 6 a 11 HORA LOCAL amb component vall avall
#     >= 14 km/h: uns 48 dies l'any. Es la finestra que viu la gent, i a
#     l'hivern coincideix amb el maxim del drenatge (10,5-10,7 km/h de mitjana
#     entre les 6 i les 9). L'estudi, en canvi, usa 00-10 UTC.
#   - SENSE ENLLACOS. A X un post amb enllac costa 0,20 $ i un sense, 0,015 $.
#   - Hores en hora local, que es un post public, no un informe tecnic.
#   - No publica dos cops la mateixa nit (ho comprova al registre).
#
#   CREDENCIALS
#   No son al projecte. Han d'anar a un fitxer fora del repositori:
#     C:/Users/<usuari>/.saligarda_x.json
#   amb aquest contingut (les quatre claus de l'app d'X, permis d'escriptura):
#     {"api_key":"...","api_secret":"...",
#      "access_token":"...","access_token_secret":"..."}
#   Si el fitxer no hi es, el script redacta el post i el desa, pero NO envia
#   res i ho diu al log. Aixi es pot deixar programat abans de tenir el compte.
#
#   Us:  Rscript 19_bot_x.R           (envia, si hi ha credencials)
#        Rscript 19_bot_x.R --prova   (mai envia: nomes redacta i mostra)
#        Rscript 19_bot_x.R --prova --p=0.72 --u=14 --wc=-2 --nit=2026-01-15
#          (simula una nit qualsevol per veure com quedaria el post)
# ==============================================================================
suppressMessages({library(data.table); library(jsonlite); library(httr)})

ruta <- grep("--file=", commandArgs(FALSE), value = TRUE)
if (length(ruta)) setwd(dirname(normalizePath(sub("--file=", "", ruta[1]))))
NOMES_PROVA <- "--prova" %in% commandArgs(trailingOnly = TRUE)

FITXER_CREDENCIALS <- file.path(Sys.getenv("USERPROFILE"), ".saligarda_x.json")
REGISTRE <- "derived/posts_x.csv"

# --------------------------------------------------------------- la prediccio
if (!file.exists("derived/pronostics.csv")) stop("no hi ha derived/pronostics.csv")
pr <- fread("derived/pronostics.csv")
p  <- pr[.N]                                     # l'ultima que ha escrit 16_
nit <- as.Date(p$nit)

if (is.null(p$p_bot) || is.na(p$p_bot))
  stop("la prediccio no porta p_bot; cal executar 16_pronostic.R actualitzat")

# --- simulacio, per veure el format d'una nit qualsevol sense esperar-la ------
arg_val <- function(nom) {
  a <- grep(paste0("^--", nom, "="), commandArgs(TRUE), value = TRUE)
  if (length(a)) sub(paste0("^--", nom, "="), "", a[1]) else NULL
}
if (!is.null(v <- arg_val("nit")))  nit <- as.Date(v)
if (!is.null(v <- arg_val("p")))    p$p_bot <- as.numeric(v)
if (!is.null(v <- arg_val("u")))    p$u_mati_pred <- as.numeric(v)
if (!is.null(v <- arg_val("wc")))   p$wc_min_pred <- as.numeric(v)
SIMULAT <- length(grep("^--(nit|p|u|wc)=", commandArgs(TRUE))) > 0
if (SIMULAT) {
  if (!NOMES_PROVA) stop("la simulacio nomes te sentit amb --prova")
  cat("[SIMULACIO: valors forcats des de la linia d'ordres]\n")
  # hores tipiques de l'estacio simulada
  cic <- data.table(mes = 1:12,
                    pic = c(5,5,5,5,5,4,4,4,5,5,5,5),
                    fin = c(10,10,8,8,8,7,7,7,9,9,9,10))
  m <- as.integer(format(nit, "%m"))
  p$pic_utc <- cic$pic[m]; p$final_utc <- cic$fin[m]
}

# ----------------------------------------------- encara arriba a temps?
# Si l'ordinador ha estat apagat i la tasca s'executa amb retard, el pronostic
# podria sortir quan la nit ja ha comencat o fins i tot acabat. Un post aixi no
# informa de res i queda malament. El drenatge s'engega cap a les 18 UTC del dia
# anterior, aixi que es posa el limit a les 22 UTC: prou marge per a l'emissio
# de les 19:00 locals i prou aviat per no publicar una nit ja en marxa.
limit <- as.POSIXct(sprintf("%s 22:00:00", format(as.Date(nit) - 1)), tz = "UTC")
ara <- Sys.time(); attr(ara, "tzone") <- "UTC"
if (!NOMES_PROVA && ara > limit) {
  cat(sprintf("Massa tard: son les %s UTC i la nit del %s ja ha comencat.\n",
              format(ara, "%Y-%m-%d %H:%M"), format(nit)))
  cat("No es publica res.\n")
  quit(save = "no")
}

# ------------------------------------------------------- ja s'ha publicat?
# Compte: la variable NO es pot dir 'nit', perque dins del data.table el nom
# quedaria capturat per la columna homonima i la condicio seria sempre certa,
# cosa que bloquejaria tots els posts a partir del primer enviat.
nit_txt <- as.character(nit)
if (file.exists(REGISTRE) && !NOMES_PROVA) {
  reg <- fread(REGISTRE)
  if (nrow(reg) && any(as.character(reg$nit) == nit_txt & as.logical(reg$enviat))) {
    cat("Ja s'havia publicat el post de la nit del", nit_txt, "- no es repeteix.\n")
    quit(save = "no")
  }
}

# ------------------------------------------------------------- redaccio
pc <- round(100 * p$p_bot)
hora_local <- function(h_utc, data) {           # sense zero al davant
  as.integer(format(as.POSIXct(sprintf("%s %02d:00:00", format(data), h_utc), tz = "UTC"),
                    tz = "Europe/Madrid", format = "%H"))
}
mesos <- c("gener","febrer","març","abril","maig","juny","juliol","agost",
           "setembre","octubre","novembre","desembre")
m_i   <- as.integer(format(nit, "%m"))
dia_n <- as.integer(format(nit, "%d"))
# Apostrofacio catalana, dues regles independents:
#  1) l'article "del" passa a "de l'" davant de vocal. Els unics dies que es
#     llegeixen comencant per vocal son l'1 (u) i l'11 (onze); tots els altres
#     comencen per consonant: dos, tres, quatre, cinc, sis, set, vuit, nou,
#     deu, dotze... vint, trenta.
#  2) la preposicio "de" passa a "d'" davant dels mesos que comencen per vocal:
#     abril, agost i octubre.
art_dia  <- if (dia_n %in% c(1, 11)) "de l'" else "del "
prep_mes <- if (m_i %in% c(4, 8, 10)) "d'" else "de "
data_txt <- sprintf("%s%d %s%s", art_dia, dia_n, prep_mes, mesos[m_i])

emoji <- if (pc >= 75) "\U0001F4A8" else if (pc >= 40) "\U0001F343" else "\U0001F634"
linia_int <- if (pc >= 25)
  sprintf("Intensitat esperada: %.0f km/h", p$u_mati_pred) else
  "Mat\u00ed tranquil, segurament"
linia_hora <- if (pc >= 40)
  sprintf("M\u00e0xim cap a les %d h, afluixa cap a les %d h",
          hora_local(p$pic_utc, nit), hora_local(p$final_utc, nit)) else NULL
# \u00b0 es el simbol de grau; \u00ba es l'ordinal masculi i no toca aqui
linia_wc <- if (!is.na(p$wc_min_pred) && p$wc_min_pred <= 5)
  sprintf("Sensaci\u00f3 m\u00ednima: %.0f \u00b0C", p$wc_min_pred) else NULL

text <- paste(c(
  sprintf("%s Saligarda \u00b7 mat\u00ed %s", emoji, data_txt),
  sprintf("Probabilitat: %d %%", pc),
  linia_int, linia_hora, linia_wc,
  "#laGarriga #Congost"), collapse = "\n")

cat("\n---------------- post ----------------\n"); cat(text, "\n")
cat(sprintf("--------------------------------------\n%d car\u00e0cters\n\n", nchar(text)))
if (nchar(text) > 280) stop("el post passa de 280 caracters")
writeLines(text, "post_x.txt")

# ------------------------------------------------------------- enviament
# Les credencials poden venir de dos llocs: un fitxer local (a l'ordinador de
# casa) o variables d'entorn (a GitHub Actions, que les injecta des dels
# secrets del repositori). Primer es miren les variables d'entorn.
llegeix_credencials <- function() {
  e <- list(api_key = Sys.getenv("X_API_KEY"),
            api_secret = Sys.getenv("X_API_SECRET"),
            access_token = Sys.getenv("X_ACCESS_TOKEN"),
            access_token_secret = Sys.getenv("X_ACCESS_TOKEN_SECRET"))
  if (all(nzchar(unlist(e)))) return(list(cr = e, font = "variables d'entorn"))
  if (file.exists(FITXER_CREDENCIALS))
    return(list(cr = fromJSON(FITXER_CREDENCIALS), font = FITXER_CREDENCIALS))
  NULL
}

# --------------------------------------------------------------- verificacio
# Comprova les credencials contra un endpoint de NOMES LECTURA i surt. Serveix
# per separar "les claus son dolentes" de "hi ha un problema en publicar", que
# des de fora es veuen igual (401 als dos casos).
if ("--verifica" %in% commandArgs(trailingOnly = TRUE)) {
  cr <- llegeix_credencials()
  if (is.null(cr)) { cat("No hi ha credencials enlloc.\n"); quit(save = "no", status = 1) }
  k <- cr$cr
  cat("Credencials de:", cr$font, "\n")
  # Llargades esperades a X. Si alguna no quadra, ja sabem quin secret revisar
  # sense haver de veure'n mai el valor.
  esperat <- c(api_key = 25L, api_secret = 50L,
               access_token = 50L, access_token_secret = 45L)
  for (nm in names(esperat)) {
    v <- k[[nm]]; n <- nchar(v)
    ok <- if (nm == "access_token") n >= 45 && n <= 60 else n == esperat[[nm]]
    cat(sprintf("  %-20s %3d caracters (esperat %d) %s\n", nm, n, esperat[[nm]],
                if (ok) "OK" else "<-- NO QUADRA"))
  }
  if (!grepl("-", k$access_token, fixed = TRUE))
    cat("  l'Access Token no porta guio: no sembla d'OAuth 1.0a\n")
  # Els espais al final son l'error mes frequent en enganxar secrets
  bruts <- names(k)[vapply(k, function(x) x != trimws(x), logical(1))]
  if (length(bruts))
    cat("  ATENCIO, amb espais o salts de linia al davant o darrere:",
        paste(bruts, collapse = ", "), "\n")
  U <- "https://api.x.com/2/users/me"
  ap <- oauth_app("saligarda", key = k$api_key, secret = k$api_secret)
  sg <- oauth_signature(U, "GET", ap, k$access_token, k$access_token_secret)
  ec <- function(x) URLencode(as.character(x), reserved = TRUE)
  au <- paste0("OAuth ", paste0(names(sg), '="', vapply(sg, ec, character(1)), '"',
                                collapse = ", "))
  rr <- GET(U, add_headers(Authorization = au))
  cat(sprintf("\n  GET /2/users/me -> HTTP %d\n", status_code(rr)))
  if (status_code(rr) == 200) {
    u <- content(rr)$data
    cat(sprintf("  CREDENCIALS CORRECTES: autenticat com a @%s (%s)\n", u$username, u$name))
    cat("  Si la publicacio falla amb 403, es el permis d'escriptura dels tokens.\n")
  } else {
    cat("  ", substr(content(rr, "text", encoding = "UTF-8"), 1, 300), "\n", sep = "")
    cat("\n  Un 401 aqui vol dir que les quatre claus no son valides com a joc.\n")
    cat("  Causa mes frequent: haver posat el Client ID i el Client Secret\n")
    cat("  (OAuth 2.0) en comptes de l'API Key i l'API Key Secret (OAuth 1.0a).\n")
  }
  quit(save = "no", status = if (status_code(rr) == 200) 0 else 1)
}

enviat <- FALSE; detall <- ""
credencials <- if (NOMES_PROVA) NULL else llegeix_credencials()
if (NOMES_PROVA) {
  detall <- "mode prova: no s'ha enviat"
} else if (is.null(credencials)) {
  # no s'hi posa la ruta completa: aquest registre es public al repositori
  detall <- "sense credencials (ni variables d'entorn ni fitxer local)"
} else {
  cr <- credencials$cr
  cat("Credencials llegides de:", credencials$font, "\n")
  URL <- "https://api.x.com/2/tweets"
  app <- oauth_app("saligarda", key = cr$api_key, secret = cr$api_secret)
  sig <- oauth_signature(URL, "POST", app, cr$access_token, cr$access_token_secret)
  # oauth_signature() retorna una LLISTA de parametres, no una configuracio de
  # peticio: passar-la a POST(config=) no genera cap capcalera i la peticio surt
  # sense autenticar (error 401). Cal muntar la capcalera Authorization a ma.
  # Amb OAuth 1.0a i cos JSON, el cos NO entra a la signatura.
  enc <- function(x) URLencode(as.character(x), reserved = TRUE)
  auth <- paste0("OAuth ", paste0(names(sig), '="', vapply(sig, enc, character(1)), '"',
                                  collapse = ", "))
  r <- try(POST(URL, add_headers(Authorization = auth),
                body = list(text = text), encode = "json"), silent = TRUE)
  if (inherits(r, "try-error")) {
    detall <- paste("error de xarxa:", conditionMessage(attr(r, "condition")))
  } else if (status_code(r) %in% c(200L, 201L)) {
    enviat <- TRUE
    detall <- paste("id", content(r)$data$id)
  } else {
    detall <- sprintf("HTTP %d: %s", status_code(r),
                      substr(content(r, "text", encoding = "UTF-8"), 1, 200))
  }
}
cat(if (enviat) "PUBLICAT: " else "NO publicat: ", detall, "\n", sep = "")

fwrite(data.table(moment = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                  nit = as.character(nit), probabilitat = pc,
                  u_mati_pred = p$u_mati_pred, enviat = enviat, detall = detall,
                  text = gsub("\n", " | ", text)),
       REGISTRE, append = file.exists(REGISTRE))

if (exists("codi_sortida") && codi_sortida != 0L)
  quit(save = "no", status = codi_sortida)
