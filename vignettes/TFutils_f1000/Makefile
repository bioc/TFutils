PNGS=AMIGO.png	AMIGOplus.png	F1000header.png	ormdl3CEBPB.png	tfTargsApp.png
all:
	echo "library(BiocWorkflowTools); render('TFutils.Rmd', f1000_article())" | R --no-save >& /tmp/log

zip:
	zip -r tfutils.zip TFutils.Rmd TFutils.tex TFutils_files ${PNGS} f1000_styles.sty tfsrcs.bib
