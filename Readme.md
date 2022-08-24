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

### Unix

To build jhcr you need to provide some things:

1. ghc, cabal-install, gnumake, sed, cpp (gcc)
2. A common.j file you want to target
3. A patch-level you want to target

After you installed all the build-tools and cloned this repo, go into your
freshly cloned repo and provide your common.j.

If your patch-level is below 1.29 build jhcr like this:

    COMMONJ=your-common.j PATCH_LVL=128 make configure-old-patch build

Otherwise build it like this:

    COMMONJ=your-common.j PATCH_LVL=133 make configure-new-patch build

This will use cabal to fetch all the required dependencies and compile jhcr
from scratch. If you want to switch between old and new patches a `make clean`
inbetween is recommended. If you just want to build the newest patch provide
a common.j file named like this common-1.33.j and simply do `make` as i try to
provide two makefile targets, one for patches pre 1.29 and one for the latest
patch on blizzard servers. The latter target of course changes over time.

### Windows

Building jhcr on windows is exactly the same as on a unix-system *except* that
the required tools aren't as easy installed. Personally i use
[chocolatey](https://chocolatey.org/) to install ghc, cabal and msys2 like this

    choco install ghc cabal msys2

Once msys2 is installed you have to make ghc and cabal available in msys2 shell.
To do that you have to edit your msys2.ini in your msys2 install folder to have
this line:

    MSYS2_PATH_TYPE=inherit

Any missing build-tools can either be installed via chocolatey or msys2.
To get cpp you most likely need to install a compiler suite like gcc or
llvm/clang (`pacman -S gcc`).
