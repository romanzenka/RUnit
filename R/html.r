##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2003, 2004  Thomas Koenig, Matthias Burger, Klaus Juenemann
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, write to the Free Software
##  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

##  $Id$


writeRaw <- function(htmlStr,htmlFile,append=TRUE)
{
  ##@bdescr
  ## write raw text in a html file
  ##@bdescr
  ##@in htmlStr  : [character] text
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  
  cat(htmlStr,file=htmlFile,append=append)
  invisible(TRUE)
}

writeRawCR <- function(htmlStr,htmlFile,append=TRUE)
{
  ##@bdescr
  ## write raw text in a html file with a cr at end
  ##@bdescr
  ##@in htmlStr  : [character] text
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  
  writeRaw(htmlStr,htmlFile,append);
  cat("\n",file=htmlFile,append=TRUE)
  invisible(TRUE)
}

writeTitle <- function(htmlStr,htmlFile,append=TRUE)
{
  ##@bdescr
  ## write title tags and title text
  ##@bdescr
  ##@in htmlStr  : [character] title
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  
  writeRaw("<title>",htmlFile,append);
  writeRaw(htmlStr,htmlFile);
  writeRaw("</title>\n",htmlFile);
}

writeBeginHead <- function(htmlFile,append=TRUE)
{
  ##@bdescr
  ## write <head>
  ##@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  
  writeRaw("<head>",htmlFile,append);
}

writeEndHead <- function(htmlFile,append=TRUE)
{
  ##@bdescr
  ## write </head>
  ##@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  
  writeRaw("</head>\n",htmlFile,append);
}


writeBeginHtml <- function(htmlFile,append=TRUE)
{
  ##@bdescr
  ## write <html>
  ##@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  
  writeRaw("<html>",htmlFile,append);
}

writeEndHtml <- function(htmlFile,append=TRUE)
{
  ##@bdescr
  ## write </html>
  ##@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  
  writeRaw("</html>\n",htmlFile,append);
}

writeBeginBody <- function(htmlFile,append=TRUE)
{
  ##@bdescr
  ## write <body>
  ##@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  
  writeRaw("<body>",htmlFile,append);
}

writeEndBody <- function(htmlFile,append=TRUE)
{
  ##@bdescr
  ## write </body>
  ##@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  
  writeRaw("</body>\n",htmlFile,append);
}

writeBeginTag <- function(htmlTag,htmlFile,para="",append=TRUE)
{
  ##@bdescr
  ## write begin of a tag, with parameters
  ##@bdescr
  ##@in htmlTag  : [character] name of the tag
  ##@in htmlFile : [character] name of the html file
  ##@in para     : [character] parameters as string
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  
  if(para =="")
  {
    writeRaw(paste("<",htmlTag,">",sep=""),htmlFile,append);
  }
  else
  {
    writeRaw(paste("<",htmlTag," ",para,">",sep=""),htmlFile,append);
  }

}

writeEndTag <- function(htmlTag,htmlFile,append=TRUE)
{
  ##@bdescr
  ## write end of tag
  ##@bdescr
  ##@in htmlTag  : [character] name of the tag
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  
  writeRaw(paste("</",htmlTag,">",sep=""),htmlFile,append);
}

writeCR <- function(htmlFile,append=TRUE)
{
  ##@bdescr
  ## write CR in html file for better formatting of the html source
  ##@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  ##@ret         : [logical] TRUE if execution completes
  
  cat("\n",file=htmlFile,append=append)
  invisible(TRUE)
}

writeBeginTable <- function(header,htmlFile,border=1,
                            width="100%",append=TRUE,
                            columnWidth=NULL)
{

  ##@bdescr
  ## write begin of a table
  ##@bdescr
  ##@in header   : [character] title for columns
  ##@in htmlFile : [character] name of the html file
  ##@in border   : [integer] border of table
  ##@in width    : [character] width of table
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes

  tablePara <- paste("border=\"",border,"\" width=\"",width,"\"",sep="")
  writeRawCR(paste("<table ",tablePara," >",sep=""),htmlFile,append)

  ##  if header is provided
  if (length(header) > 0) {
    writeBeginTag("tr",htmlFile)

    
    for(i in seq(along=header)) {
      para <- ""
      if(!is.null(columnWidth)) {
        if (length(columnWidth) == length(header)) {
          para = paste("width=\"", columnWidth[i], "\"", sep="")
        } else {
          ##  recycle first
          para = paste("width=\"", columnWidth[1], "\"", sep="")
        }
      }
      writeBeginTag("th",htmlFile, para=para)
      writeRaw(header[i],htmlFile)
      writeEndTag("th",htmlFile)
      writeCR(htmlFile)
    }
    
    writeEndTag("tr",htmlFile,append)
  }
  
  writeCR(htmlFile)
}

writeTableRow <- function(row,htmlFile,append=TRUE,bgcolor="")
{

  ##@bdescr
  ## write a table row
  ##@bdescr
  ##@in row      : [character] data for table cells in row
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@in bgcolor  : [character] color for table cells
  ##@ret         : [logical] TRUE if execution completes
  
  writeBeginTag("tr",htmlFile);
  if(length(bgcolor) == 1)
  {
    bgcolor <- rep(bgcolor,length(row));
  }
  for(i in seq(along=row))
  {
    if(bgcolor[i] == "")
    {
      writeBeginTag("td",htmlFile);
    }
    else
    {
      writeBeginTag("td",htmlFile,para=paste("bgcolor=\"",bgcolor[i],"\"",sep=""));
    }
    writeRaw(row[i],htmlFile);
    writeEndTag("td",htmlFile);
    writeCR(htmlFile);
  }

  writeEndTag("tr",htmlFile,append);
  writeCR(htmlFile);
}


writeLink <- function(target,name,htmlFile,append=TRUE)
{
  ##@bdescr
  ##  write a link
  ##@bdescr
  ##@in target   : [character] target of the link
  ##@in name     : [character] name of the target
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  
  writeBeginTag("a",htmlFile,paste("href=\"",target,"\"",sep=""),append=append);
  writeRaw(name,htmlFile,append=TRUE);
  writeEndTag("a",htmlFile,append=TRUE);
}

writeEndTable <- function(htmlFile,append=TRUE)
{
  ##@bdescr
  ##  
  #@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  
  writeEndTag("table",htmlFile,append);
  writeCR(htmlFile);
}

writeHtmlHeader <- function(header,htmlFile)
{
  ##@bdescr
  ## write a HTML file header
  ##@bdescr
  ##@in header   : [character] title of the document
  ##@in htmlFile : [character] name of the link
  ##@ret         : [logical] TRUE if execution completes
  
  writeRawCR("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"",htmlFile,FALSE);
  writeRawCR("\"http://www.w3.org/TR/html4/transitional.dtd\">",htmlFile);
  writeBeginHtml(htmlFile);
  writeBeginHead(htmlFile);
  writeTitle(header,htmlFile);
  writeEndHead(htmlFile);
  writeBeginBody(htmlFile);
}

writeHtmlEnd <- function(htmlFile)
{
  ##@bdescr
  ## write end of html code
  ##@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@ret         : [logical] TRUE if execution completes
  
  writeEndBody(htmlFile);
  writeEndHtml(htmlFile);
}

writeHtmlSep <- function(htmlFile)
{
  ##@bdescr
  ## write horizontal seperator
  ##@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@ret         : [logical] TRUE if execution completes
  
  writeRawCR("<hr>",htmlFile);
}


writeImage <- function(img,htmlFile,append=TRUE)
{
  ##@bdescr
  ## write image tags
  ##@bdescr
  ##@in img :      [character] name of the image file
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  
  writeBeginTag("img",htmlFile,para=paste("src=\"",img,"\"",sep=""),append);
  writeEndTag("img",htmlFile);
}


writeHtmlSection <- function(title,sec,htmlFile,append=TRUE)
{
  ##@bdescr
  ## write titles for section
  ##@bdescr
  ##@in title    : [character] title of the section
  ##@in sec      : [integer] size of title (between 1-6)
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  
  secTag <- paste("h",sec,sep="")
  writeBeginTag(secTag,htmlFile,append);
  writeRaw(title,htmlFile,append);
  writeEndTag(secTag,htmlFile,append);
  writeCR(htmlFile,append);
}


writeHtmlTable <- function(dataFrame, htmlFile, border=1,
                           width="100%", append=TRUE)
{
  ##@bdescr
  ##  writes a data frame to a HTML table 
  ##@bdescr
  ##
  ##@in dataFrame : [data frame] size of title (between 1-6)
  ##@in htmlFile  : [character] name of the html file
  ##@in border    : [integer] 1 (default) table borders will be shown
  ##@in width     : [character] width of table
  ##@in append    : [logical] if TRUE append the tabel to an existing HTML file
  ##@ret          : [logical] TRUE if execution completed
  ##
  ##@codestatus   : untested

  header <- NULL
  colNames <- colnames(dataFrame)
  if (!is.null(colNames)) {
    if (length(colNames) == dim(dataFrame)[2]) {
      header <- colNames
    } else {
      ##  don't write column names
      header <- NULL
    }
  }

  rowNames <- rownames(dataFrame)
  if (!is.null(rowNames)) {
    header <- c("Name", header)
    dataFrame <- cbind(rowNames, dataFrame)
  }
  writeBeginTable(header, htmlFile, border=border,
                  width=width, append=append,
                  columnWidth=NULL)
  
  for (ti in 1:dim(dataFrame)[1]) {
    writeTableRow(dataFrame[ti, ], htmlFile, append=TRUE, bgcolor="")
  }
  writeEndTable(htmlFile,append=TRUE)
}
