import 
  libcurl

var hCurl = easy_init()
if hCurl != nil: 
  discard easy_setopt(hCurl, OPT_VERBOSE, True)
  discard easy_setopt(hCurl, OPT_URL, "http://force7.de/nimrod")
  discard easy_perform(hCurl)
  easy_cleanup(hCurl)

