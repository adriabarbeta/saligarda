# ==============================================================================
# 20_whatsapp.R -- pagina per publicar el post al canal de WhatsApp
#
#   Els canals de WhatsApp NO tenen API oficial de Meta: no es pot publicar de
#   manera automatica sense pilotar una sessio de WhatsApp Web amb serveis de
#   tercers, cosa que va contra els termes d'us i exposa el numero a un bloqueig.
#   L'API oficial (Cloud API) si que s'automatitza, pero factura per destinatari
#   i per missatge, de manera que amb un centenar de seguidors costaria mes en un
#   mes que X en deu anys.
#
#   La solucio es deixar el text a punt i que la publicacio sigui manual: aquest
#   script genera docs/index.html, una pagina amb el missatge del dia i un boto
#   de copiar, pensada per tenir-la a la pantalla d'inici del mobil.
#
#   El text surt de derived/posts_x.csv, que es el registre del que el bot ha
#   redactat per a X. Aixi les dues versions no poden divergir mai: si canvia la
#   plantilla del post, canvia sola la del canal. S'hi fan dos retocs propis de
#   WhatsApp: els hashtags no hi serveixen de res i el titol va en negreta, que
#   a WhatsApp s'escriu entre asteriscs.
#
#   Us:  Rscript 20_whatsapp.R
# ==============================================================================
suppressMessages(library(data.table))

ruta <- grep("--file=", commandArgs(FALSE), value = TRUE)
if (length(ruta)) setwd(dirname(normalizePath(sub("--file=", "", ruta[1]))))

REGISTRE <- "derived/posts_x.csv"
SORTIDA  <- "docs/index.html"
TEXT     <- "docs/missatge.txt"

if (!file.exists(REGISTRE)) stop("no hi ha ", REGISTRE)
reg <- fread(REGISTRE)
if (!nrow(reg)) stop("el registre de posts es buit")
p <- reg[.N]

# el registre desa el post amb " | " alla on hi havia salts de linia
linies <- trimws(strsplit(as.character(p$text), " | ", fixed = TRUE)[[1]])
linies <- linies[nzchar(linies) & !grepl("^#", linies)]
if (!length(linies)) stop("no s'ha pogut reconstruir el text del post")
linies[1] <- paste0("*", linies[1], "*")
missatge <- paste(linies, collapse = "\n")

escapa <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

nit <- as.character(p$nit)
html <- sprintf('<!doctype html>
<html lang="ca">
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<meta http-equiv="Cache-Control" content="no-store">
<title>Saligarda &middot; canal</title>
<link rel="icon" href="data:image/svg+xml,%%3Csvg xmlns=\'http://www.w3.org/2000/svg\' viewBox=\'0 0 16 16\'%%3E%%3Ctext y=\'14\' font-size=\'14\'%%3E&#127811;%%3C/text%%3E%%3C/svg%%3E">
<style>
  :root {
    --fons: #f7f7f5; --paper: #fff; --tinta: #1a1a1a; --fluix: #6b6b6b;
    --vora: #e2e2dd; --accent: #1f7a4d; --accent-t: #fff; --avis: #8a5a00;
    --avis-f: #fff6e0;
  }
  @media (prefers-color-scheme: dark) {
    :root {
      --fons: #16181a; --paper: #202325; --tinta: #e9e9e7; --fluix: #9a9a97;
      --vora: #33373a; --accent: #35a06a; --accent-t: #07130d; --avis: #f0c060;
      --avis-f: #2e2616;
    }
  }
  * { box-sizing: border-box; }
  body {
    margin: 0; padding: 1.1rem; background: var(--fons); color: var(--tinta);
    font: 16px/1.5 system-ui, -apple-system, "Segoe UI", sans-serif;
    -webkit-text-size-adjust: 100%%;
  }
  main { max-width: 32rem; margin: 0 auto; }
  h1 {
    font-size: .8rem; font-weight: 600; letter-spacing: .08em;
    text-transform: uppercase; color: var(--fluix); margin: 0 0 .9rem;
  }
  #avis {
    display: none; background: var(--avis-f); color: var(--avis);
    border: 1px solid currentColor; border-radius: .6rem;
    padding: .6rem .8rem; margin-bottom: .9rem; font-size: .88rem;
  }
  pre {
    background: var(--paper); border: 1px solid var(--vora);
    border-radius: .8rem; padding: 1rem; margin: 0;
    font: inherit; white-space: pre-wrap; word-wrap: break-word;
  }
  button {
    width: 100%%; margin-top: .9rem; padding: .95rem 1rem;
    background: var(--accent); color: var(--accent-t); border: 0;
    border-radius: .8rem; font: 600 1.05rem system-ui, sans-serif;
    cursor: pointer; -webkit-tap-highlight-color: transparent;
  }
  button:active { opacity: .82; }
  p.peu { color: var(--fluix); font-size: .82rem; margin: 1.1rem 0 0; }
</style>
<main>
  <h1>Saligarda &middot; missatge per al canal</h1>
  <div id="avis"></div>
  <pre id="msg">%s</pre>
  <button id="btn" type="button">Copiar</button>
  <p class="peu">Per al mat&iacute; del <strong>%s</strong>. Redactat el %s.<br>
     Copia, obre el canal de WhatsApp i enganxa-ho.</p>
</main>
<script>
  var NIT = "%s";
  var btn = document.getElementById("btn");
  btn.addEventListener("click", function () {
    var t = document.getElementById("msg").textContent;
    function fet() { btn.textContent = "Copiat"; setTimeout(function () { btn.textContent = "Copiar"; }, 1600); }
    if (navigator.clipboard && window.isSecureContext) {
      navigator.clipboard.writeText(t).then(fet, manual);
    } else { manual(); }
    function manual() {
      // Reserva per si writeText falla (Safari antic, permis denegat, o un clic
      // que el navegador no considera un gest de l\'usuari). execCommand esta
      // desaprovat pero funciona a tot arreu i copia de debò i sincronament.
      var el = document.getElementById("msg");
      var r = document.createRange(); r.selectNodeContents(el);
      var s = getSelection(); s.removeAllRanges(); s.addRange(r);
      var ok = false;
      try { ok = document.execCommand("copy"); } catch (e) { ok = false; }
      if (ok) { s.removeAllRanges(); fet(); }
      else { btn.textContent = "Selecciona-ho i copia"; }
    }
  });
  // El text es el de l\'últim post redactat. Si el workflow ha fallat, aquí hi
  // hauria el d\'un dia passat: val més dir-ho que no pas publicar-lo.
  (function () {
    var avui = new Date(); avui.setHours(0, 0, 0, 0);
    var nit = new Date(NIT + "T00:00:00");
    var dies = Math.round((nit - avui) / 86400000);
    var a = document.getElementById("avis"), t = null;
    if (dies < 0)  t = "Compte: aquest missatge és del matí del " + NIT + ", que ja ha passat. El pronòstic d\'avui no s\'ha generat.";
    if (dies === 0) t = "Compte: aquest missatge és del matí d\'avui, que ja ha passat.";
    if (t) { a.textContent = t; a.style.display = "block"; }
  })();
</script>
</html>
', escapa(missatge), escapa(nit), escapa(as.character(p$moment)), escapa(nit))

if (!dir.exists("docs")) dir.create("docs")
writeLines(html, SORTIDA, useBytes = TRUE)

# El mateix missatge en text pla. La pagina d'index depen que GitHub Pages
# estigui activat; aquest fitxer no depen de res, perque el visor de fitxers de
# GitHub ja hi posa un boto de copiar i el mobil l'obre sense mes. Serveix de
# reserva si algun dia Pages falla.
writeLines(missatge, TEXT, useBytes = TRUE)

cat("-> ", SORTIDA, " i ", TEXT, " (mati del ", nit, ")\n", sep = "")
cat("\n", missatge, "\n\n", sep = "")
