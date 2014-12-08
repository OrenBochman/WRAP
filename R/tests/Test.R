library(testthat)
#library(WRAP)

setwd("~/Documents/R/WRAP/R")
source("WRAP.R")
#test_file("Test.R")

#testing functions
expect_that(10, equals(10))
expect_that(10, is_identical_to(10))
expect_that(c("one" = 1, "two" = 2),is_equivalent_to(1:2))

#expect_true(x)
#expect_false(x)
#expect_is(x, y)
#expect_equal(x, y)
#expect_equivalent(x, y)
#expect_identical(x, y)
#expect_matches(x, y)
#expect_output(x, y)
#expect_message(x, y)
#expect_warning(x, y)
#expect_error(x, y)

#create a test
#res=wmLogin(uri=setApiUri(project="mediawiki"),
#            "P-value",
#            "12345678",h,
#            .opts=c(debugfunction = dbg$update, verbose = TRUE))
#
w<-WRAP$new(user="P-value",
            pass="12345678",
            email="WRAP@gmail.com")

expect_equal(w$pass, NULL)
expect_equal(w$user, NULL)
expect_equal(w$email, NULL)

#testing set_api_uri
expect_equal(w$set_api_uri(project="wikipedia",lang="en"),"https://en.wikipedia.org/w/api.php")
expect_equal(w$set_api_uri(project="mediawiki",lang=""),"https://www.mediawiki.org/w/api.php")

a <- list(1:10, letters)
expect_that(str(a), prints_text("List of 2"))

listJSON<-list(
  logi<-c(TRUE,FALSE,TRUE),
  num<-c(1.00,3.00,4.15),
  chr<-c("a","b","def")
  )
#str(list4)

expect_equal(w$process_JSON('[ [true, false, true], [1, 3, 4.15], ["a", "b", "def" ] ]')
             ,listJSON)

testXml<-"<api><edit result=\"Success\" pageid=\"16283969\" title=\"Wikipedia:Sandbox\" contentmodel=\"wikitext\" oldrevid=\"621914847\" newrevid=\"621916124\" newtimestamp=\"2014-08-19T13:57:23Z\"/></api>"
expect_equal(w$process_XML(doc=testXml,"//api/edit","contentmodel"),"wikitext")

#test getUserInfo
properties = c("userid",
               "name",
               "blockinfo",               
               "implicitgroups",
               "rights",
               "userid",
               "editcount",
               "registration",
               "groups",
               "emailable",         
               "gender")


  res=w$getUserInfo(uri="https://en.wikipedia.org/w/api.php",
                    userNameList=c("OrenBochman"),
                    userPropList=properties)
  expect_match((res$query$users[[1]]["userid"]),"1526134")
  expect_match((res$query$users[[1]]["name"]),"OrenBochman")
  expect_match((res$query$users[[1]]["registration"]),"2006-05-29T22:11:46Z")
  expect_match((res$query$users[[1]]["gender"]),"unknown")
  expect_match((res$query$users[[1]]["emailable"]),"")
  expect_more_than(as.numeric(res$query$users[[1]]["editcount"]),12591)
 #print(res$query$users[[1]]["editcount"]) 

  expect_match((res$query$users[[1]]["groups"]),"user")
  expect_match((res$query$users[[1]]["groups"]),"autoconfirmed")
  expect_match((res$query$users[[1]]["groups"]),"*")
 # expect_match((res$query$users[[1]]["groups"]),"founder")

#  print(res$query$users[[1]]["groups"]) 

  #expect_match((res$query$users[[1]]["editcount"]),"12592")

res=w$getUserInfo(uri="https://en.wikipedia.org/w/api.php",
                    userNameList=c("Jimbo Wales"),                  
                    userPropList=properties)
 # print(res)
  expect_match((res$query$users[[1]]["userid"]),"24")
  expect_match(res$query$users[[1]]["name"],"Jimbo Wales")
  expect_more_than(as.numeric(res$query$users[[1]]["editcount"]),11599)
  expect_that(res$query$users[[1]]["registration"],matches("2001-03-27T20:47:31Z"))
  expect_that(res$query$users[[1]]["gender"],matches("unknown"))
  #print(res$query$users[[1]]["groups"])
  expect_that(res$query$users[[1]]["groups"],matches("user"))
  expect_that(res$query$users[[1]]["groups"],matches("autoconfirmed"))
  expect_that(res$query$users[[1]]["groups"],matches("*"))
  expect_that(res$query$users[[1]]["groups"],matches("founder"))
  expect_that(res$query$users[[1]]["groups"],matches("oversight"))
  expect_that(res$query$users[[1]]["groups"],matches("sysop"))
  expect_that(res$query$users[[1]]["groups"],matches("checkuser"),"has checkuser")

login=w$login(uri="https://en.wikipedia.org/w/api.php",
              name="P-value",
              password="12345678")
expect_that(login["result"],matches("Success"))



#testing getTokens
editToken=w$getTokens(uri="https://en.wikipedia.org/w/api.php",
                      responseFormat="xml")


print(editToken)
if(FALSE){

}

