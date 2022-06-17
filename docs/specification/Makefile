OTT = ott

OTT_FLAGS_WRAP := -tex_show_meta false -picky_multiple_parses false
OTT_FLAGS_NO_WRAP := -tex_wrap false -tex_show_meta false -picky_multiple_parses false
OTT_FLAGS := $(OTT_FLAGS_WRAP)

OTTFileName := owl
OTTFile := $(OTTFileName).ott
OTTGen := $(OTTFileName).tex

all : pdf

$(OTTGen) : owl.ott
	$(OTT) $(OTT_FLAGS) -i $(OTTFile) -o $(OTTGen)

pdf : $(OTTGen)
	pdflatex $(OTTGen)

open: pdf
	open -a Skim $(OTTFileName).pdf

clean :
	rm -f *.aux *.dvi *.ps *.log *-ott.tex *-output.tex *.bbl *.blg \
	      *.rel note.pdf *~ *.vtc *.out *.spl *-inc.tex  *-ott.tex *-ottput.tex \
	      *.toc $(OTTGen)

