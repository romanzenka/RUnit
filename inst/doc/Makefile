##
##  RUnit
##  
##  utility 
##  create PDF document from dvi (usefull if R CMD INSTALL fails to build the pdf)
##  $Id$
##
all:       RUnit.pdf clean
RUnit.pdf: RUnit.ps
	ps2pdf  -dEncodeColorImages=false -dColorImageFilter=/FlateEncode -dAutoRotatePages=/None RUnit.ps
RUnit.ps:  RUnit.dvi
	dvips RUnit
RUnit.dvi: RUnit.tex
									latex RUnit
	latex RUnit
clean:
	rm -f RUnit.aux RUnit.log RUnit.toc
