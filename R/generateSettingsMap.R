# generateSettingsMap.R

#' \code{generateSettingsMap} generate a settings map for the sequence logo.
#'
#' Change the character, color, and the font for your sequence logo.
#'
#' @param residues a vector of residues for the sequence alignment.
#' @param character a vector of characters corresponding to \code{residues}.
#' @param colors a vector of acceptable R colors corresponding to
#' \code{residues}.
#' @param pathToFont the path to the font file to use for the characters. Must
#' have glyphs for all the characters.
#' @export
generateSettingsMap <- function(residues,
                                characters = residues,
                                colors = colorRampPalette(
                                  c("#C27E7E", "#816EBA", "#758AC9", "#82C9B6"))
                                  (length(residues)),
                                pathToFont = system.file("extdata/notosans",
                                              "NotoSans-Regular.ttf",
                                              package = "shi")) {
  settingsMap <- list()
  settingsMap[["font"]] <- pathToFont
  settingsMap[["residueSettings"]] <- list()
  for (i in 1:length(residues)) {
    residueSetting <- list()
    residueSetting[["residue"]] <- residues[i]
    residueSetting[["color"]] <- colors[i]
    residueSetting[["character"]] <- characters[i]
    settingsMap[["residueSettings"]][[residues[i]]] <- residueSetting
  }
  return(settingsMap)
}