url_full = "http://www.numberfire.com/nba/players"
findclass = "all-players__indiv"

Set objIE = CreateObject("InternetExplorer.Application")
Set fso = CreateObject("Scripting.FileSystemObject")

objIE.navigate url_full
wscript.sleep 8000

Set fl = fso.OpenTextFile("C:\Users\Owner\Documents\GitHub\Fantasy-Hoops\NF All Players.txt", 2, True)
set ele_class = objIE.document.getElementsByClassName(findclass)


for x = 0 to ele_class.length-1 
with ele_class.item(x)
	fl.write(.getAttribute("data-first-name") & ";" & .getAttribute("data-last-name") & ";" & .getAttribute("data-team") & ";" & .children(0).getAttribute("href") & vbCrLf)
end with
next

fl.Close

''''''All done, close everything
msgbox "done"

Set fl = Nothing
Set fso = Nothing
objIE.application.Quit
set objIE = Nothing
WScript.Quit