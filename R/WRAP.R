library("R6")      #OOP
library("RCurl")   #Web Api
library("XML")     #Processing
library("RJSONIO") #processing

#CURRENT: use case - onne wiki available per instance  OR
#TODO:    use case - multiple wikis available per instance per wiki!

WRAP <- R6Class("WRAP",
  public = list(        
    api = NA,
    version = "1.0",
    useragent = NA,  
    curl=NA,
    debug=TRUE,
    #ctor
    initialize = function(user,pass,email,api,curl=NA){
      if(file.exists("wrap.config"))
        init_from_file("wrap.conf")
      else{
        #TODO: code me
      }              
      #set ctor params
      if (!missing(api))    self$api=api
      else                  self$api=self$set_api_uri()        
      if (!missing(user))   private$user=user        
      if (!missing(pass))   private$pass=pass              
      if (!missing(email))  private$email=email
      if (!missing(curl))   self$curl=curl
      else                  self$curl=getCurlHandle()  
      
      #a wmf complient user agent
      self$useragent = paste("WRAP",self$ver,"contact: ",self$email)
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
    init_from_file=function(val){},
    set_hair = function(val) {
      self$hair <- val
    },
    #debugging support
    dfun =function(msg, topic, curl){
      cat(topic, ":", length(msg), "\n")
      
    },
    greet = function() {
      cat(paste0("Hello, my name is ", private$user, ".\n"))
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
  
  
  #doPost
  doPost=function(uri,
                  .params,
                  style="post"){
  
    if(nchar(uri)==0){
      uri=self$api
    }
    raw=postForm(uri=uri,
                 .params=.params,
                 style=style,
                 curl=self$curl) 
    
  },
  
  #getUserInfo
  getUserInfo=function(uri="",
                       userNameList,
                       userPropList,
                       responseFormat="json"){
    #responseFormat="json" #|"xml"
    .params = list(action="query",list="users",  
                    ususers=paste(userNameList, collapse = "|"),
                    usprop=paste(userPropList, collapse = "|"),
                    format=responseFormat)
    #query
    raw=self$doPost(uri=uri,.params=.params,style="post") 
    #parse the result
    if(responseFormat=="xml")   
    {          
      doc=xmlInternalTreeParse(raw, trim = TRUE)
    }else{
      doc=self$process_JSON(doc=raw)
    }      
  
  }
),
  

  private = list(
    user = NA,
    pass = NA,
    email= NA,  
    max_lag = NA)
)#class