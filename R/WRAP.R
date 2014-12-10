library("R6")      #OOP
library("RCurl")   #Web Api
library("XML")     #Processing
library("RJSONIO") #processing

WRAP <- R6Class("WRAP",
# PUBLIC
#
  public = list(
    api = NA,
    version = "1.0",
    useragent = NA,  
    debug=FALSE,
    curl=NA,
    
    #
    #ctor
    #
    initialize = function(api,curl=NA){
      if(file.exists("user.R"))
        self$init_from_file("user.R")   #load config from file
      else
        self$init_interactive("user.R") #get config interactively

      if(!missing(api))
        self$api=api
      else              
        self$api=self$set_api_uri()        
      
      if(!missing(curl))
        self$curl=curl
      else                  
        self$curl=getCurlHandle()
      
      #a wmf complient user agent
      self$useragent = paste("WRAP",self$ver,"contact: ",private$user$email)
      #set global curl options
      curlSetOpt(
        cookiejar="cookies.txt",    #store cookies, 
        useragent = self$useragent,
        followlocation = TRUE,      #continuing queries
        ssl.verifypeer = FALSE,     #don't verify server cert
        # proxy=proxy,              #"136.233.91.120",
        # proxyusername=name,       #"mydomain\\myusername",  
        # proxypassword=pass,       #'whatever',
        # proxyport=port            #8080     
        verbose = self$debug,       #debug 
        #debugfunction = self$dfun,  #debug handler
        curl=self$curl              #handle is required
      )      
      #self$getMyLimits()
      #start up message
      self$greet()          
    },
    
    #
    #init configuration interactively and save to file
    #
    init_interactive=function(file_name="user.R"){
        
      name = readline(prompt="Enter your username: ")
      print(paste("name: ",name))
        
      password = readline(prompt="Enter your password: ")
      print(paste("password: ",password))
        
      url = readline(prompt="Enter your wiki's base url: ")
      print(paste("url: ",url))
      
      email= readline(prompt="Enter your email: ",)
      print(paste("email: ",email))          
      
      private$user=data.frame(name,password,url,email)
      print(paste("writing (to file: ",file_name))
      write.table(private$user,file_name,col.names=TRUE)        
    },
    
    #
    #init configuration from a file
    #
    init_from_file=function(file_name="user.R"){
        private$user<-read.table(file=file_name,header=TRUE)        
    },
    
    set_debug=function(mode=TRUE){
      self$debug=mode
      curlSetOpt(verbose=mode,                 
                 curl=self$curl)
    },
    
    set_api_uri=function(project="wikipedia",lang="en"){
      #todo check against legit wm wikis
      #todo add other wiki (if they support this api)
      
      if( nchar(lang)==0){
        uri=paste("https://www.",project,".org/w/api.php",sep = "")
      }else{
        uri=paste("https://",lang,".",project,".org/w/api.php",sep = "")
      }
      return(uri)
    },                      
    
    
    #debugging support
    dfun =function(msg, topic, curl){
      cat(topic, ":", length(msg), "\n")
      
    },

    greet = function() {
      cat(paste0("Hello, ", private$user$name, ".\n"))
      cat(paste0("email: ", private$user$email, ".\n"))
      cat(paste0("url: ", private$user$url, ".\n"))
  
    },
    
    github_parse = function(req){
      text <- content(req, as = "text")
      if (identical(text, "")) stop("Not output to parse", call. = FALSE)
      jsonlite::fromJSON(text, simplifyVector = FALSE)
    },            
    
    process_JSON=function(doc){
      result = tryCatch({
        fromJSON(I(doc), simplify = TRUE) 
      }, warning = function(w) {
        #warning-handler-code
        #print(paste("warning",w))
        
      }, error = function(e) {
        #error-handler-code
        print(paste("warning",e)) 
        res=""
      }, finally = {
        
      })        
    },#END tryCatch
    
    process_XML=function(doc,xpath,attribute){
      result = tryCatch({  
        res =xmlInternalTreeParse(doc, trim = TRUE)
        res =xpathApply(res,xpath)[[1]]
        
        if(nchar(attribute)){
          res <-xmlAttrs(res)[attribute][[1]]
        }
        #    res<-xmlAttrs(res)
        #    if(attribute %in% res ){#check attrib is in list
        #      res <-res[attribute][[1]]
        #    }else{
        #      e<-simpleError("attribute not found")
        #      signalCondition(e)
        #    }
        #  }
        
      }, warning = function(w) {
        #warning-handler-code
        #print(paste("warning",w))
      }, error = function(e) {
        #error-handler-code
        print(paste("warning",e)) 
      }, finally = {
        
      })
      return(result)
    },#END tryCatch
    #getUserInfo
    
  getMyLimits=function(uri){ 
    max_lag=5 #TODO lookup
  },
  
  #
  #doPost
  #
  doPost=function(uri, .params, style="post"){
    if(nchar(uri)==0){
      uri=self$api
    }
    raw=postForm(uri=uri,
                 .params=.params,
                 #assert=user,    make sure we are logged in
                 style=style,
                 curl=self$curl)
  },
  
  #getUserInfo
  getUserInfo=function(uri="",
                       userNameList,
                       userPropList,
                       responseFormat="json"){
    
    if(self$debug)print("getUserInfo()")
    .params = list(action="query",list="users",  
                   ususers=paste(userNameList, collapse = "|"),
                   usprop= paste(userPropList, collapse = "|"),
                   format=responseFormat)
    raw=self$doPost(uri=uri,.params=.params,style="post")  #query
    if(responseFormat=="xml"){  #parse the result
      doc=xmlInternalTreeParse(raw, trim = TRUE)
    }else{
      doc=self$process_JSON(doc=raw)
    }  
  },
  
  #
  #getTokens
  #  
  getTokens=function(uri="",responseFormat="xml"){
  if(self$debug)print("getTokens()")
    .params=list(action="query",
                 meta="tokens",
                 type="csrf|watch|patrol",
                 format=responseFormat)
    #get edit token
    raw=self$doPost(uri=uri,.params=.params,style="post")
    #parse the result
    editToken=raw
    if(responseFormat=="xml"){  
      editToken=self$process_XML(doc=raw,
                                 xpath="//api/query/tokens",
                                 attribute="csrftoken")
    }
    else if(responseFormat=="json"){  
      editToken=self$process_JSON(doc=raw)
    }
    return(editToken)
  },
 
  #
  #login
  #  
  login=function(uri="",name,password,responseFormat="xml"){ 
    if(self$debug)print("login()")
    .params= list(action="login",  
                lgname=name,
                lgpassword=password,
                format="xml")  

    #step 1 login
    x1=self$doPost(uri=uri,.params=.params) 
    #extract token attrib
    token<-self$process_XML(doc=x1,xpath="//api/login",attribute="token")
    
    #step 2 login check
    .params <-c(.params,lgtoken=token)           
    #add token to request
    x2=self$doPost(uri=uri,.params=.params,style="post")
    #extract result attrib  
    result<-self$process_XML(doc=x2,xpath="//api/login",attribute="result")
    
    retValue <- list(result=result,token=token)
    if(result!="Success" && self$debug)
      print(paste("login failed with message",result))
    return(retValue)
  }
  ),
  #
  # private
  # 
  
  private = list(
    user = NA,
    max_lag = 1 #in seconds),
  )#private
)#class