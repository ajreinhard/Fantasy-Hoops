
Set fso = CreateObject("Scripting.FileSystemObject")

Set fl = fso.OpenTextFile("C:\Users\A097092\Desktop\Extra\Fantasy BB research\NumberFire_NBA_MasterList.txt")

 

dim xHttp: Set xHttp = createobject("Microsoft.XMLHTTP")

dim bStrm: Set bStrm = createobject("Adodb.Stream")

 

 

for i = 0 to 1000 step 50

 

xHttp.Open "GET", "http://games.espn.com/fba/freeagency?leagueId=201155&teamId=1&seasonId=2018&avail=-1&startIndex=" & i, False

xHttp.Send

 

with bStrm

    .type = 1

    .open

    .write xHttp.responseBody

    .savetofile "C:\Users\A097092\Desktop\Extra\Fantasy BB research\ESPN\players" & i/50 + 1 & ".txt", 2

    .close

end with

 

next

 

 

 

do until fl.atendofstream

 

xHttp.Open "GET", fl.readline, False

xHttp.Send

 

with bStrm

    .type = 1

    .open

    .write xHttp.responseBody

    .savetofile "C:\Users\A097092\Desktop\Extra\Fantasy BB research\Players\" & fl.readline & ".txt", 2

    .close

end with

 

loop

 

fl.close

 

xHttp.Open "GET", "http://games.espn.com/fba/leaguerosters?leagueId=201155", False

xHttp.Send

 

with bStrm

    .type = 1

    .open

    .write xHttp.responseBody

    .savetofile "C:\Users\A097092\Desktop\Extra\Fantasy BB research\Teams.txt", 2

    .close

end with

 

Set bStrm = Nothing

Set xHttp = Nothing

Set fso = Nothing

Set fl = Nothing

 

 

 

msgbox "Done"

 