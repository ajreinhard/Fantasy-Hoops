my_dir = "C:\Users\Owner\Documents\GitHub\Fantasy-Hoops\"

Set objIE = CreateObject("InternetExplorer.Application")
Set fso = CreateObject("Scripting.FileSystemObject")
Set nf_player_fl = fso.OpenTextFile(my_dir & "NF All Players.txt", 2, True)

dim xHttp: Set xHttp = createobject("Microsoft.XMLHTTP")
dim bStrm: Set bStrm = createobject("Adodb.Stream")

''''''''''''''''''''''''''''''''
''get all URLs from NumberFire''
''''''''''''''''''''''''''''''''
nf_url_full = "http://www.numberfire.com/nba/players"
nf_findclass = "all-players__indiv"

objIE.navigate nf_url_full
wscript.sleep 8000

Set ele_class = objIE.document.getElementsByClassName(nf_findclass)

for x = 0 to ele_class.length-1 
with ele_class.item(x)
	nf_player_fl.write(.getAttribute("data-first-name") & ";" & .getAttribute("data-last-name") & ";" & .getAttribute("data-team") & ";" & .children(0).getAttribute("href") & vbCrLf)
end with
next

nf_player_fl.close
Set nf_player_fl = fso.OpenTextFile(my_dir & "NF All Players.txt")

''''''''''''''''''''''''''''''''''''''
''get each player page on NumberFire''
''''''''''''''''''''''''''''''''''''''
do until nf_player_fl.atendofstream
player_line = nf_player_fl.readline
player_url = Right(player_line,Len(player_line)-InStrRev(player_line,"/"))
'msgbox InStrRev(";",player_line)

xHttp.Open "GET", "http://www.numberfire.com/nba/players/projections/" & player_url, False
xHttp.Send

with bStrm
    .type = 1
    .open
    .write xHttp.responseBody
    .savetofile my_dir & "NF Pages\" & player_url & ".txt", 2
    .close
end with

loop


'''''''''''''''''''''''''''''''''''
''get list of all players on ESPN''
'''''''''''''''''''''''''''''''''''
'for i = 0 to 1000 step 50

'xHttp.Open "GET", "http://games.espn.com/fba/freeagency?leagueId=201155&teamId=1&seasonId=2019&avail=-1&startIndex=" & i, False
'xHttp.Send

'with bStrm
'    .type = 1
'    .open
'    .write xHttp.responseBody
'    .savetofile my_dir & "ESPN Pages\players" & i/50 + 1 & ".txt", 2
'    .close
'end with

'next



'''''''''''''''''''''''
''get current rosters''
'''''''''''''''''''''''
xHttp.Open "GET", "http://fantasy.espn.com/basketball/league/rosters?leagueId=201155", False
xHttp.Send

with bStrm
    .type = 1
    .open
    .write xHttp.responseBody
    .savetofile my_dir & "Teams.txt", 2
    .close
end with

''''''''''''''''''''''''
''get ESPN Master List''
''''''''''''''''''''''''
xHttp.Open "GET", "http://fantasy.espn.com/apis/v3/games/fba/seasons/2019/segments/0/leagues/201155?&view=kona_player_info", False
xHttp.Send

with bStrm
    .type = 1
    .open
    .write xHttp.responseBody
    .savetofile my_dir & "ESPN.json", 2
    .close
end with
 

Set bStrm = Nothing
Set xHttp = Nothing
Set fso = Nothing
Set nf_player_fl = Nothing
set ele_class = Nothing
Set objIE = Nothing
msgbox "Done"

 