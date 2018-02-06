if (!require(extrafont, quietly = TRUE)) {
  install.packages("extrafont")
  library(extrafont)
}

if (!require(grImport, quietly = TRUE)) {
  install.packages("grImport")
  library(grImport)
}

if (!require(gridBase, quietly = TRUE)) {
  install.packages("gridBase")
  library(gridBase)
}

font_import()
fonts()
loadfonts(device="postscript")

postscript("h.ps")
plot.new()
text(0, 0, labels="H")
dev.off()

postscript("a.ps")
plot.new()
text(0, 0, labels="A")
dev.off()

PostScriptTrace('h.ps', 'h.xml', charpath=FALSE)
PostScriptTrace('a.ps', 'a.xml', charpath=FALSE)
PostScriptTrace('hello.ps', 'hello.xml', charpath=FALSE)
hps <- readPicture('h.xml')
aps <- readPicture('a.xml')
hello <- readPicture('hello.xml')


# https://stackoverflow.com/a/20375766
scaleText <- function(text="hello world",
                       xscale=1, yscale=1, font='Courier',
                       tmp=tempfile()){
  tmp.ps <- paste0(tmp, ".ps")
  tmp.xml <- paste0(tmp, ".xml")
  string.ps <- paste0('%!PS
                    /', font, '             % name the desired font
                    5 selectfont        % choose the size in points and establish
                    % the font as the current one
                    ', xscale, ' ', yscale, ' scale            % scale axis
                    72 500 moveto        % position the current point at
                    % coordinates 72, 500 (the origin is at the
                    % lower-left corner of the page)
                    (', text, ') show  % stroke the text in parentheses
                    showpage             % print all on the page
                    ')
  cat(string.ps, file=tmp.ps)
  PostScriptTrace(tmp.ps, tmp.xml)
  readPicture(tmp.xml)
}


grid.newpage()
theText = c('A', 'B', 'C', 'D')
for (i in 1:4) {
  pushViewport(viewport(x=unit(10, "mm"), y=unit(10 * i, "mm"),
                        height=unit(10, "mm"), width=unit(10, "mm"),
                        just=c(0, 0)))
  letter <- scale_text(theText[i])
  grid.picture(letter)
  popViewport()
}

for (i in 1:4) {
  pushViewport(viewport(x=unit(20, "mm"), y=unit(10 + (20 * (i - 1)), "mm"),
                        height=unit(20, "mm"), width=unit(10, "mm"),
                        just=c(0, 0)))
  letter <- scale_text(theText[i], xscale=1, yscale=2)
  grid.picture(letter)
  popViewport()
}

for (i in 1:4) {
  pushViewport(viewport(x=unit(30, "mm"), y=unit(10 * i, "mm"),
                        height=unit(10, "mm"), width=unit(30, "mm"),
                        just=c(0, 0)))
  letter <- scale_text(theText[i], xscale=3)
  grid.picture(letter)
  popViewport()
}

for (i in 1:4) {
  pushViewport(viewport(x=unit(80, "mm"), y=unit(10 * i, "mm"),
                        height=unit(10, "mm"), width=unit(10, "mm"),
                        just=c(0, 0)))
  letter <- scale_text(theText[i])
  grid.picture(letter)
  popViewport()
}

for (i in 1:4) {
  pushViewport(viewport(x=unit(90, "mm"), y=unit(10 * i, "mm"),
                        height=unit(10, "mm"), width=unit(10, "mm"),
                        just=c(0, 0)))
  letter <- scale_text(theText[i], font="Times-Roman")
  grid.picture(letter)
  popViewport()
}

for (i in 1:4) {
  pushViewport(viewport(x=unit(100, "mm"), y=unit(10 * i, "mm"),
                        height=unit(10, "mm"), width=unit(10, "mm"),
                        just=c(0, 0)))
  letter <- scale_text(theText[i], font="Times-Italic")
  grid.picture(letter)
  popViewport()
}
