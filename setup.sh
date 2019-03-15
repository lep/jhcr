
set -e

cp test.w3x tmp.w3x

./clijasshelper common.j Blizzard.j tmp.w3x 
cp logs/output_war3map.j logs/orig_war3map.j
printf "init logs/output_war3map.j\nexit\n" | ./Main common.j Blizzard.j
./clijasshelper --scriptonly common.j Blizzard.j jasshelper.j asdasdunused.j
./MPQEditor.exe /add tmp.w3x logs/output_war3map.j war3map.j
cp tmp.w3x /cygdrive/c/Users/lep/Documents/Warcraft\ III/Maps/jhcr/
