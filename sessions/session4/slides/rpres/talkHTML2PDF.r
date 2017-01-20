# Set working directory
 setwd( "../slides/" )

# Load packages
require( knitr )
require( markdown )

# Create .md, .html, and .pdf files
#knit( "myTalk.Rmd")
#markdownToHTML( 'myTalk.md', 'talk.html', options = c( "use_xhml" ) )
system( "pandoc -s talk.html -o talk.pdf" )

## second ansatz
system( "pandoc -v" )
pandoc( 'talk.Rpres', format='latex' ) # PDF
pandoc( 'talk.Rpres', format='docx' ) # DOCX
