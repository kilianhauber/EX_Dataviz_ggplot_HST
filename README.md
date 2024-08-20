# EX_Dataviz_ggplot_HST

## Projektübersicht

Dieses Repository enthält den Code und die Daten für das Projekt "EX_Dataviz_ggplot_HST". Ziel des Projekts ist es, verschiedene Datenvisualisierungen mithilfe von `R` und der `ggplot2`-Bibliothek zu erstellen und in einer Präsentation darzustellen. Das Projekt wurde im Rahmen einer Gruppenarbeit entwickelt und behandelt verschiedene Aspekte der Datenanalyse und -visualisierung.

## Inhalt des Repositories

- **`EX_Dataviz_ggplot_Presentation_HST.qmd`**: Eine Quarto-Markdown-Datei, die den Text und den R-Code für die Erstellung einer Präsentation im `revealjs`-Format enthält.
- **`R/` Verzeichnis**: Enthält die folgenden R-Skripte:
  - `dataviz.R`: Hauptskript zur Erstellung von Visualisierungen.
  - `ColorPalettes.R`: Definiert die Farbschemata für die Visualisierungen.
  - `dataviz_HST.R`: Zusätzliche Funktionen und Anpassungen für die HST-Gruppe.
- **`data/` Verzeichnis**: Enthält die Daten, die für die Visualisierungen verwendet werden.
- **`figs/` Verzeichnis**: Hier werden die erzeugten Diagramme und Visualisierungen gespeichert.
- **`EX_Dataviz_ggplot_Presentation_HST.html`**: Die aus der QMD-Datei generierte HTML-Präsentation.

## Installation und Nutzung

1. **Repository klonen**: 
   ```bash
   git clone https://github.com/username/EX_Dataviz_ggplot_HST.git
   ```
2. **RStudio öffnen**: Öffnen Sie die Projektdatei `EX_Dataviz_ggplot_HST.Rproj` in RStudio.
3. **Abhängigkeiten installieren**: Stellen Sie sicher, dass die erforderlichen R-Pakete installiert sind:
   ```r
   install.packages(c("ggplot2", "quarto"))
   ```
4. **QMD-Datei rendern**: Um die Präsentation zu generieren, können Sie die QMD-Datei in RStudio rendern:
   ```bash
   quarto render EX_Dataviz_ggplot_Presentation_HST.qmd
   ```
5. **Visualisierungen ausführen**: Die R-Skripte im `R/`-Verzeichnis enthalten den Code zur Erstellung der Visualisierungen. Diese können individuell ausgeführt werden, um die Diagramme zu erzeugen.

## Projektstruktur

- **`EX_Dataviz_ggplot_Presentation_HST.qmd`**: Quarto-Dokument für die Präsentation.
- **`R/`**: R-Skripte zur Datenvisualisierung.
- **`data/`**: Daten, die für das Projekt verwendet werden.
- **`figs/`**: Gespeicherte Visualisierungen.
- **`EX_Dataviz_ggplot_HST.Rproj`**: RStudio-Projektdatei.
- **`EX_Dataviz_ggplot_Presentation_HST.html`**: HTML-Datei der gerenderten Präsentation.

## Autoren

Dieses Projekt wurde von der Gruppe HST erstellt:

- Kilian Hauber
- Danis Sisic
- Tim Trefzer

## Lizenz

Dieses Projekt steht unter der MIT-Lizenz. Weitere Informationen finden Sie in der [LICENSE](LICENSE)-Datei.

## Kontakt

Bei Fragen oder Anmerkungen können Sie uns über GitHub kontaktieren oder eine E-Mail an info@kilianhauber.com senden.
```

Dieser Text ist komplett in Markdown-Formatierung und kann direkt in eine `README.md`-Datei eingefügt werden.