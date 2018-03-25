library(shiny)

shinyServer
(
  function(input, output,session)
    {
        
      require("rJava")
      require("RWeka")
      require("R.utils")
      require("stringi")
      require("stringr")
      require("textcat")
      require("tm")
      require("markovchain")
      require("hunspell")
      
      findnext = reactive({
                            load(file = "./forShiny.RData")
                            text = trimws(input$userSen)
                            if(is.na(text)|identical(text,"")){s = NULL}
                            else
                            {
                                s <<- predictNext(text,6,textPredictor) # see source files
                            }
                         })
      
      output$nextWord = renderText( 
                                {
                                    # for next word
                                    findnext()
                                    if(is.null(s)){"No sentence added"}
                                    else
                                    {
                                    g = s[1]
                                    
                                    if(!hunspell_check(g))
                                    {
                                        gh = unlist(hunspell_suggest(g))
                                        
                                        gh = gh[grepl(pattern = paste0("^",substring(g,1,1)),x = gh)]
                                        
                                        g = gh[1]
                                    }
                                    
                                    g
                                    }
                                }
                                  )
      
      output$otherWord = renderText(
                                    {
                                    findnext()
                                    # for other words
                                    if(is.null(s)|length(s)==1){"Not enough predicted words!"}    
                                    else
                                    {
                                    fg = ""
                                    sl = ifelse(length(s)==6,6,length(s)) 
                                     for(i in 2:sl)
                                     {
                                         gf = s[i]
                                         
                                         if(!hunspell_check(gf))
                                         {
                                             gh = unlist(hunspell_suggest(gf))
                                             
                                             gh = gh[grepl(pattern = paste0("^",substring(gf,1,1)),x = gh)]
                                             
                                             gf = gh[1]
                                         }
                                         
                                         fg = c(fg,gf)
                                     }
                                     
                                     fg = fg[-1]
                                     
                                     paste(fg,collapse = " | ")
                                    }
                                    }
                                   )
    
  }
)
