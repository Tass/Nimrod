# Test the new CGI module
import strtabs, cgi


#setTestData("name", "the andreas", "password", "rumpf\t\ttab")

var myData = readData()
validateData(myData, "name", "password")
writeContentType()

write(stdout, "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\">\n")
write(stdout, "<html><head><title>Test</title></head><body>\n")
writeln(stdout, "name: " & myData["name"])
writeln(stdout, "password: " & myData["password"])
writeln(stdout, "</body></html>")
