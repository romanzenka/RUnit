writeRaw <- function(htmlStr,htmlFile,append=TRUE)
{
  ##@bdescr
  ## write raw text in a html file
  ##@bdescr
  ##@in htmlStr  : [character] text
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  
  cat(htmlStr,file=htmlFile,append=append);
}

writeRawCR <- function(htmlStr,htmlFile,append=TRUE)
{
  ##@bdescr
  ## write raw text in a html file with a cr at end
  ##@bdescr
  ##@in htmlStr  : [character] text
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code

  writeRaw(htmlStr,htmlFile,append);
  cat("\n",file=htmlFile,append=TRUE);
}

writeTitle <- function(htmlStr,htmlFile,append=TRUE)
{
  ##@bdescr
  ## write title tags and title text
  ##@bdescr
  ##@in htmlStr  : [character] title
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
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

  writeRaw("<head>",htmlFile,append);
}

writeEndHead <- function(htmlFile,append=TRUE)
{
  ##@bdescr
  ## write </head>
  ##@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code

  writeRaw("<head>\n",htmlFile,append);
}


writeBeginHtml <- function(htmlFile,append=TRUE)
{
  ##@bdescr
  ## write <html>
  ##@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code

  writeRaw("<html>",htmlFile,append);
}

writeEndHtml <- function(htmlFile,append=TRUE)
{
  ##@bdescr
  ## write </html>
  ##@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code

  writeRaw("<html>\n",htmlFile,append);
}

writeBeginBody <- function(htmlFile,append=TRUE)
{
  ##@bdescr
  ## write <body>
  ##@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code

  writeRaw("<body>",htmlFile,append);
}

writeEndBody <- function(htmlFile,append=TRUE)
{
  ##@bdescr
  ## write </body>
  ##@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code

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

  writeRaw(paste("</",htmlTag,">",sep=""),htmlFile,append);
}

writeCR <- function(htmlFile,append=TRUE)
{
  ##@bdescr
  ## write CR in html file for better formatting of the html source
  ##@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  
  cat("\n",file=htmlFile,append=append);
}
  
writeBeginTable <- function(header,htmlFile,border=1,width="100%",append=TRUE)
{

  ##@bdescr
  ## write begin of a table
  ##@bdescr
  ##@in header   : [character] title for columns
  ##@in htmlFile : [character] name of the html file
  ##@in border   : [integer] border of table
  ##@in width    : [character] width of table
  ##@in append   : [logical] append the html code

  
  tablePara <- paste("border=\"",border,"\" width=\"",width,"\"",sep="")
  writeRawCR(paste("<table ",tablePara," >",sep=""),htmlFile,append);
  writeBeginTag("tr",htmlFile);
  
  for(i in 1:length(header))
  {
    writeBeginTag("th",htmlFile);
    writeRaw(header[i],htmlFile);
    writeEndTag("th",htmlFile);
    writeCR(htmlFile);
  }
    
  writeEndTag("tr",htmlFile,append);
  writeCR(htmlFile);
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
  
  writeBeginTag("tr",htmlFile);
  if(length(bgcolor) == 1)
  {
    bgcolor <- rep(bgcolor,length(row));
  }
  for(i in 1:length(row))
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
  ## write a link
  ##@bdescr
  ##@in target   : [character] target of the link
  ##@in name     : [character] name of the target
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  
  writeBeginTag("a",htmlFile,paste("href=\"",target,"\"",sep=""),append=append);
  writeRaw(name,htmlFile,append=TRUE);
  writeEndTag("a",htmlFile,append=TRUE);
}
  
writeEndTable <- function(htmlFile,append=TRUE)
{
  writeEndTag("table",htmlFile,append);
  writeCR(htmlFile);
}

writeHtmlHeader <- function(header,htmlFile)
{
  ##@bdescr
  ## write a HTML table
  ##@bdescr
  ##@in header   : [character] title of the document
  ##@in htmlFile : [character] name of the link

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
  
  writeEndBody(htmlFile);
  writeEndHtml(htmlFile);
}

writeHtmlSep <- function(htmlFile)
{
  ##@bdescr
  ## write horizontal seperator 
  ##@bdescr
  ##@in htmlFile : [character] name of the html file
  
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

  secTag <- paste("h",sec,sep="")
  writeBeginTag(secTag,htmlFile,append);
  writeRaw(title,htmlFile,append);
  writeEndTag(secTag,htmlFile,append);
  writeCR(htmlFile,append);
}
  
