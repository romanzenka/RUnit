##
##  RUnit
##  
##  utility 
##  create PDF document from Sweave rnw file
##  $Id$
##
rnwFile = RUnit

all:       $(rnwFile).pdf clean

$(rnwFile).pdf: $(rnwFile).ps
	ps2pdf  -dEncodeColorImages=false -dColorImageFilter=/FlateEncode -dAutoRotatePages=/None $(rnwFile).ps
	
$(rnwFile).ps:  $(rnwFile).dvi
	dvips $(rnwFile)
	
$(rnwFile).dvi: $(rnwFile).tex
	latex $(rnwFile)
	latex $(rnwFile)
	
$(rnwFile).tex: $(rnwFile).Rnw
	"$(R_HOME)/bin/R" CMD Sweave $(rnwFile).Rnw
	
clean:
	rm -f $(rnwFile).aux $(rnwFile).log $(rnwFile).toc $(rnwFile).out $(rnwFile).dvi $(rnwFile).tex
