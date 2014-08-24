library("R6")      #OOP
library("RCurl")   #Web Api
library("XML")     #Processing
library("RJSONIO") #processing

#CURRENT: use case - onne wiki available per instance  OR
#TODO:    use case - multiple wikis available per instance per wiki!

WRAP <- 
  R6Class("WRAP",
          private = list(
            user = NA,
            pass = NA,
            e-mail= NA,     
          ),
          
          public = list(        
            api = NA,
            version = "1.1",
            useragent = NA,
            
            #constructor
            initialize = function(user,pass,e-mail,api,curl) 
            {
              #TODO load df/table from config.file
              #if file.exists("wrap.config") initFromFile()
              
              #set ctor params
              if (!missing(api))    self$api <- api
              else                  self$api <- setApiUri()        
              if (!missing(user))   self$api <- api        
              if (!missing(pass))   self$api <- api              
              if (!missing(e-mail)) self$e-mail <- e-mail
              if (!missing(curl))   self$curl <- curl
              else                  self$curl <- getCurlHandle()  
              
              #a wmf complient user agent
              self$useragent = paste("WRAP",self$ver,"contact: ",self$email)
              
              #set global curl options
              curlSetOpt(
                cookiejar="cookies.txt",    #store cookies, 
                useragent = self$useragent
                paste("WRAP ver:",self$ver,
                      "contact: ",self$email),  #define the user agent
                followlocation = TRUE,      #continuing queries
                ssl.verifypeer = FALSE,     #don't verify server cert
                
                # proxy=proxy,           #"136.233.91.120",
                # proxyusername=name,    #"mydomain\\myusername",  
                # proxypassword=pass,    #'whatever',
                # proxyport=port         #8080 
                
                verbose = TRUE,             #debug 
                # debugfunction = dfun,          
                curl=self$curl          #handle is required
              )
              
              
              
              #self$getMyLimits()
              #start up message
              self$greet()          
            },
            
            setApiUri=function(project="wikipedia",lang="en"){
              #todo check against legit wm wikis
              #todo add other wiki (if they support this api)
              
              if( nchar(lang)==0){
                uri=paste("https://www.",project,".org/w/api.php",sep = "")
              }else{
                uri=paste("https://",lang,".",project,".org/w/api.php",sep = "")
              }
              return(uri)
            }
            
            
            
            set_hair = function(val) {
              self$hair <- val
            },
            greet = function() {
              cat(paste0("Hello, my name is ", self$name, ".\n"))
            }
          )
  )