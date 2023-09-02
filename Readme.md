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

After you installed all the build-tools and cloned this repo you probably should
also run a `cabal update`.

#### Building for the latest patch or patch 1.28

The Makefile provides two build targets for the latest patch (1.33 as of
writing) and patch 1.28 because these should be the most used patches.
If you put a file called common-1.33.j (or common-1.28.j)
in the directory you can simply type `make patch133` (or `make patch128`) and it
should just work. You can then find the executable deeply nested in the `dist-newstyle`
folder. To get the exact path you could either use `cabal exec which jhcr` or
if have a newer cabal version you can use `cabal list-bin jhcr`. If you're
building for an older patch you have to provide the `-f old-patch` flag to
`cabal list-bin` aswell.

#### Building for any other patch

If you want to target any other patch you have to use the `build` target and
provide two to three environment variables depending on your patch level ( any
patch lower than 1.29 is considered old).
Let's take two patch levels as an example: 1.26 and 1.31.


    # To build 1.31 aka a "new" patch
    PATCH_LVL=131 COMMONJ=common-1.31.j make clean build

    # To build 1.26 aka a "old" patch
    CABAL_FLAGS="-f old-patch" PATCH_LVL=126 COMMONJ=common-1.26.j make clean build

It is recommended to run `clean` if you switch the patch level.


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
