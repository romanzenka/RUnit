plotConnection.trackInfo <- function(con,pngfile,...)
{
  ## experimental 2nd order connections
  ## color for arrows
  color <- c("black","lightgreen","green","lightblue","blue","orangered","red")
  ## create nothing if 
  if(all(con==0))
  {
    
    ## open png device
    png(file=pngfile,width=1024,height=960);
    plot(1:10,axes=FALSE,xlab="",ylab="",main="",type="n");
    text(5,5,labels="No connection graph available");
    dev.off();
    return(NULL);
  }
  
  ## overall connections
  allCon <- sum(con);

  ## connections with percent
  con <- ceiling(con/sum(con)*100);

  ## normalize for colors
  con <- (con + 14) %/% 15;

  ## open png device
  png(file=pngfile,width=1024,height=960);
  
  ## basic plot
  plot(x=1:nrow(con),y=1:nrow(con),type="n",axes=FALSE,ylab="# line",xlab="",ylim=c(nrow(con),1));

  ## draw text lines
  text(x=1,y=1:nrow(con),labels=1:nrow(con));

  ## offset, to avoid complete overlay
  offset <- rep(3,length=nrow(con));

  ## minimal x
  xmin <- 2;
  
  ## check all connections
  for(i in 1:nrow(con))
  {
    for(j in 1:ncol(con))
    {

      ## check for an existing connection
      if(con[i,j] != 0)
      {
        colDraw <- color[con[i,j]];
        from <- j;
        to <- i;

        ## circular
        if(from == to)
        {
          top <- from+0.5;
          bot <- from-0.5;
          middle <- (xmin+offset[from])/2;

          ## top spline
          splTop <- spline(c(xmin,middle,offset[from]),c(from+0.2,top,from));

          ## bottom spline
          splBot <- spline(c(xmin,middle,offset[from]),c(from-0.2,bot,from));
          lines(splTop$x,splTop$y,col=colDraw);
          lines(splBot$x,splBot$y,col=colDraw);
          l <- length(splTop$y);
          ## draw arrow tips
          arrows(splTop$x[l-1],splTop$y[l-1],splTop$x[l],splTop$y[l],length=0.04,col=colDraw)
          offset[from] <- offset[from]+1;
        }
        else
        {
          ## "regular" case
          middle <- (i+j)/2;
          splxy <- spline(c(from-0.2,middle,to+0.2),c(xmin-0.2,offset[from],xmin+0.2));
          lines(splxy$y,splxy$x,col=colDraw);
          if(i < j)
          {
            l <- length(splxy$y);
            ## draw an arrow tip
            arrows(splxy$y[l-1],splxy$x[l-1],splxy$y[l],splxy$x[l],length=0.06,col=colDraw)
          }
          else
          {
            ## draw "inverse" arrow tip
            arrows(splxy$y[2],splxy$x[2],splxy$y[1],splxy$x[1],length=0.06,col=colDraw)
          }
          ## set offset higher
          offset[from] <- offset[from]+1;
        }
      }
    }
  }

  legposx <- nrow(con);
  leg.txt <- c("0-15%","15-30%","30-45%","45-60%","60-75%","75-90%","90-100%");
  legend(x=legposx,y=1,legend=leg.txt,lty=1,xjust=1,col=color)
  
  dev.off();

  return(NULL);
}

printHTML.trackInfo <- function(res,baseDir=".")
{


  writeRaw <- function(htmlStr,htmlFile,append=TRUE)
  {
    cat(htmlStr,file=htmlFile,append=append);
  }

  writeRawCR <- function(htmlStr,htmlFile,append=TRUE)
  {
    writeRaw(htmlStr,htmlFile,append);
    cat("\n",file=htmlFile,append=TRUE);
  }

  writeTitle <- function(htmlStr,htmlFile,append=TRUE)
  {
    writeRaw("<title>",htmlFile,append);
    writeRaw(htmlStr,htmlFile);
    writeRaw("</title>\n",htmlFile);
  }

  writeBeginHead <- function(htmlFile,append=TRUE)
  {
    writeRaw("<head>",htmlFile,append);
  }

  writeEndHead <- function(htmlFile,append=TRUE)
  {
    writeRaw("<head>\n",htmlFile,append);
  }

  writeBeginHtml <- function(htmlFile,append=TRUE)
  {
    writeRaw("<html>",htmlFile,append);
  }

  writeEndHtml <- function(htmlFile,append=TRUE)
  {
    writeRaw("<html>\n",htmlFile,append);
  }

  writeBeginBody <- function(htmlFile,append=TRUE)
  {
    writeRaw("<body>",htmlFile,append);
  }

  writeEndBody <- function(htmlFile,append=TRUE)
  {
    writeRaw("</body>\n",htmlFile,append);
  }

  writeBeginTag <- function(htmlTag,htmlFile,para="",append=TRUE)
  {
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
    writeRaw(paste("</",htmlTag,">",sep=""),htmlFile,append);
  }

  writeCR <- function(htmlFile,append=TRUE)
  {
    cat("\n",file=htmlFile,append=append);
  }
  
  writeBeginTable <- function(header,htmlFile,border=1,width="100%",append=TRUE)
  {

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
    writeEndBody(htmlFile);
    writeEndHtml(htmlFile);
  }

  writeHtmlSep <- function(htmlFile)
  {
    writeRawCR("<hr>",htmlFile);
  }
  
  writeImage <- function(img,htmlFile,append=TRUE)
  {
    writeBeginTag("img",htmlFile,para=paste("src=\"",img,"\"",sep=""),append);
    writeEndTag("img",htmlFile);
  }

  writeHtmlSection <- function(title,sec,htmlFile)
  {
    secTag <- paste("h",sec,sep="")
    writeBeginTag(secTag,htmlFile);
    writeRaw(title,htmlFile);
    writeEndTag(secTag,htmlFile);
    writeCR(htmlFile);
  }
  
  path <- paste(baseDir,"/results",sep="");
  dir.create(path);

  htmlFile <- paste(path,"/index.html",sep="");

  ## create index.html
  writeHtmlHeader("Overview",htmlFile);
  writeHtmlSection("Overview",2,htmlFile);
  writeBeginTable(c("categ.","Name","signature"),htmlFile)
  for(i in 1:length(res))
  {
    funcID <- strsplit(names(res)[i],"/")[[1]];
    funcCat <- funcID[1];
    funcName <- funcID[2];
    if(length(funcID) > 2)
    {
      sig <- funcID[3:length(funcID)];
      funcSig <- paste(funcName,"(",paste(sig,collapse=", "),")",sep="")
    }
    else
    {
      funcSig <- "";
    }

    writeBeginTag("tr",htmlFile);
    writeCR(htmlFile);

    ## write function category
    writeBeginTag("td",htmlFile);
    writeRaw(funcCat,htmlFile);
    writeEndTag("td",htmlFile);    
    writeCR(htmlFile);

    ## write function name
    writeBeginTag("td",htmlFile);
    writeLink(paste("./res",i,".html",sep=""),funcName,htmlFile)
    writeEndTag("td",htmlFile);    
    writeCR(htmlFile);

    ## write function signature
    writeBeginTag("td",htmlFile);
    writeRaw(funcSig,htmlFile);
    writeEndTag("td",htmlFile);    
    writeCR(htmlFile);
    writeEndTag("tr",htmlFile);
  }
  writeEndTable(htmlFile);
  writeHtmlEnd(htmlFile);


  writeLinkRef <- function(htmlFile,leftLink,leftName,rightLink,rightName)
  {
    writeBeginTable(c("",""),htmlFile,border=0,width="100%");

    writeBeginTag("tr",htmlFile);
    writeCR(htmlFile);

    writeBeginTag("td",htmlFile);
    writeLink(leftLink,leftName,htmlFile)
    writeEndTag("td",htmlFile);    
    writeCR(htmlFile);

    writeBeginTag("td",htmlFile,"align=\"right\"");
    writeLink(rightLink,rightName,htmlFile)
    writeEndTag("td",htmlFile);    
    writeEndTag("tr",htmlFile);    
    writeCR(htmlFile);

    writeEndTable(htmlFile)
  }
  
  ## create result pages
  for(i in 1:length(res))
  {
    absGraphImg <- paste(path,"/con",i,".png",sep="");
    absGraphFile <- paste(path,"/con",i,".html",sep="");
    relGraphImg <- paste("./con",i,".png",sep="");
    relGraphFile <- paste("./con",i,".html",sep="");
    relHTMLFile <- paste("res",i,".html",sep="");

    htmlFile <- paste(path,"/res",i,".html",sep="");
    ## begin result page
    writeHtmlHeader("Result",htmlFile);


    writeLinkRef(htmlFile,"index.html","index",relGraphFile,"graph");
    
    writeHtmlSep(htmlFile);
    writeHtmlSection("Result",2,htmlFile);

    funcName <- strsplit(names(res)[i],"/")[[1]][2]; 
    writeRaw("Function:",htmlFile);
    writeBeginTag("b",htmlFile);
    writeRaw(funcName,htmlFile);
    writeEndTag("b",htmlFile);
    writeCR(htmlFile);

    writeRaw("Runs:",htmlFile);
    writeBeginTag("b",htmlFile);
    writeRaw(res[[i]]$nrRuns,htmlFile);
    writeEndTag("b",htmlFile);
    writeCR(htmlFile);

    
    writeCR(htmlFile);

    writeBeginTable(c("line","code","calls","time"),htmlFile)
    for(j in 1:length(res[[i]]$src))
    {
      srcLine <- res[[i]]$src[j]
      leadingSpaceNr <- attr(regexpr("^( )*",srcLine),"match.length");
      if(leadingSpaceNr > 0)
      {
         srcLine <- gsub("^( )*","",srcLine)
         srcLine <- paste(paste(rep("&ensp;",leadingSpaceNr),collapse=""),srcLine,collapse="",sep="");
      }
      if(res[[i]]$run[j] > 0)
      {
        bgcolor <- "#00D000"
      }
      else
      {
        bgcolor <- "#D00000"
      }
      writeTableRow(c(j,srcLine,res[[i]]$run[j],round(res[[i]]$time[j],2)),htmlFile,bgcolor=bgcolor);
    }

    writeEndTable(htmlFile);
    writeHtmlSep(htmlFile);
    writeLinkRef(htmlFile,"index.html","index",relGraphFile,"graph");

    writeHtmlEnd(htmlFile);
    
    plotConnection.trackInfo(res[[i]]$graph,absGraphImg);
    writeHtmlHeader("Connection",absGraphFile);
    writeLinkRef(absGraphFile,"index.html","index",relHTMLFile,"Function");
    writeHtmlSep(absGraphFile);
    writeHtmlSection("Connection",2,absGraphFile);
    writeImage(relGraphImg,absGraphFile);
    writeCR(absGraphFile);
    writeHtmlSep(absGraphFile);
    writeLinkRef(absGraphFile,"index.html","index",relHTMLFile,"Function");
    writeHtmlEnd(absGraphFile);
    
  }
  

  
}



