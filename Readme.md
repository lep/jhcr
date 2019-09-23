# Jass Hot Code Reload

A compiler to allow hot code reload in WarCraft 3. This means you can update
your map script and see the changes in a running game without restarting it.
Do note though that this is alpha software. Expect bugs and make backups.
This works without memhack.

## Compiler usage


The compiler has two commands: init and update. The first command you execute
has to be the init command. The init command expects atleast three arguments:
Path to common.j, path to Blizzard.j and path to your maps war3map.j .
If your map was compiled by jasshelper you should use the --jasshelper flag
for the compiler to work correctly.

````
$ jhcr init common.j Blizzard.j war3map.j --jasshelper
````

The above usage will create a file jhcr_war3map.j. You should import this
as your maps new war3map.j.

Now you can make changes to your map. Once you've done this, get the new
war3map.j and call jhcr again.

````
$ jhcr update war3map.j --preload-path Path\To\CustomMapData --jasshelper
````

Again, if you've used jasshelper to compile your map, pass the --jasshelper flag.
The update command will create a file called JHCR.txt in the path you've specified.

As this tool operates only on the maps script and not on the map itself you
have to extract and insert the war3map.j yourself.

## Map Preparation

To load updates to the mapscript use `call ExecuteFunc("JHCR_Init_parse")` 
when appropiate. I use pressing escape. Do this after you've done an `update`
like above.


## Building JHCR

To build you need GHC 8.6, GNUMake, Bash, sed, cpp and a common.j file

