library("testthat") 

setwd("~/Documents/R/WRAP")
#setup
source("R/WRAP.R")
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
w<-WRAP$new(user="P-value",pass="12345678",email="WRAP@gmail.com")

expect_equal(w$pass, NULL)
expect_equal(w$user, NULL)
expect_equal(w$email, NULL)

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
properties = c("blockinfo",
               #        "groups","implicitgroups","rights",
               "editcount","registration",
               "emailable","gender")

res=w$getUserInfo(userNameList=c("Jimbo Wales","OrenBochman"),
                  userPropList=properties)
print(res)
#print(res$query$users[[1]]$userid)
#print(res$query$users[[1]]$name)
#print(res$query$users[[1]]$editcount)
#print(res$query$users[[1]]$registration)
#print(res$query$users[[1]]$gender)