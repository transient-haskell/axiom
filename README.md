![Axiom logo](axiom.png)
==========
[![Hackage](https://img.shields.io/hackage/v/axiom.svg)](http://hackage.haskell.org/package/axiom)
[![Stackage LTS](http://stackage.org/package/axiom/badge/lts)](http://stackage.org/lts/package/axiom)
[![Stackage Nightly](http://stackage.org/package/axiom/badge/nightly)](http://stackage.org/nightly/package/axiom)
[![Build Status](https://travis-ci.org/transient-haskell/axiom.png?branch=master)](https://travis-ci.org/transient-haskell/axiom)


For some examples, see the [transient-examples](https://github.com/transient-haskell/transient-examples) repository: [distributedApps.hs](https://github.com/transient-haskell/transient-examples/blob/master/distributedApps.hs) and      [webapp.hs](https://github.com/transient-haskell/transient-examples/blob/master/webapp.hs)

The source code of these applications can be executed in the commmand line if you have docker installed.

[![Gitter](https://badges.gitter.im/theam/haskell-do.svg)](https://gitter.im/Transient-Transient-Universe-HPlay/Lobby?utm_source=share-link&utm_medium=link&utm_campaign=share-link)

Axiom (the new name of ghcjs-hplay) is also the Web user interface of [Transient](https://github.com/agocorona/transient). The Web functionality of transient will be called **Axiom**, like the cruise starship of Wall-e. Axiom is made to let you navigate the universe of nodes in the cloud through your browser while you are comfortably seated in your [hoverchair](https://www.youtube.com/watch?v=uOL2W9JQmo8).

Unlike his predecessor, [hplayground](http://github.com/agocorona/hplayground), Axiom has full integration with Transient and can run widgets that run code on the server, the client or both.

Axiom execute browser widgets that are reactive, can be composed monadically and algebraically (applicative, alternative, monoidal..). At the same time they participate in cloud computations. A widget can execute code in the server and, trough the server, in any node on the cloud using the same cloud primitives defined in transient-universe. The example applications include widgets that perform distributed map-reduce and federated chat servers as well as stream fibonacci numbers from server to client and from client to server.

To see how it integrates with Transient and how to create client-server applications, see the web paragraphs of the [transient tutorial](https://github.com/agocorona/transient/wiki/Transient-tutorial).

To see how to create client side applications and widgets (with no server code integration), look for  [hplayground](https://github.com/agocorona/hplayground) package. [Tutorial](https://www.airpair.com/haskell-tutorial/intro-to-haskell-web-apps)

How it works
============
The JS program compiled with GHCJS is sent to the browser, then it opens a websockets connection. Then  the most useful primitive is `atRemote` wich execute his argument in the server and return the result back to the client  (or viceversa, see below). The communication transport the variables necessary for executing the computation remotely. There is no explicit serialization neither communication. All is done implicitly.

`atRemote` can be executed inside itself, so a computation can jump from server to client and back. So a browser can be controlled by the server or the other way aroud. But the execution starts in the browser. Only the variable values already computed are transported to execute remotely.  In the other side, streaming  and reactivity in both directions is included since `atRemote` and other primitives are reactive (see the [transient tutorial](https://github.com/transient-haskell/transient/wiki/Transient-tutorial)).

In the other side, there is an experimental template editor to generate static HTML templates. The server can execute a rest route and bring the corresponding page template and the JS code to the browser, so web crawlers can find something to read.
Also in Axiom everithing compose algebraically with standard applicative, alternative and monoidal operators, and  also monadically:  

Larger widgets can be composed with algebraic combinations of smaller widgets. No limits. Widgets can have server side (they can use `atRemote`) so they are full stack, autonomous pieces down to the cloud. They make perfect software components.

Events do not bubble up to the top like in the case of React. An event within a widget produce a monadic response that executes widgets down trough the monad without affecting the surrounding rendering not affected by the event. That is why Axiom does not need a Virtual DOM, and the logic of the application and the execution flow match, so it produces a clean and understandable code. look at the TODO app (it is client-side only)

http://tryplayg.herokuapp.com/try/todo.hs/edit

Axiom also implement widgets that works as spreadsheet cells, with formulas depending on other cells. These formulas can be executed in the server, so they have full access to databases, mumber crunching, map-reduce etc. This functionality need some testing.

How to install & run fast
=========================
use `initNode` to initalize the application. Example below.
   

If you have docker
------------------
, you can run a transient image that has GHC, GHCJS and Transient installed. Then create this program with this content and save it to an executable location:
```
$ cat execthirdline.sh
command=`sed -n '3p' ${1} | sed 's/-- //'`
eval $command $1 $2 $3
```
Then add this to the head of your main source file:

```csh
#!/usr/bin/env ./execthirdline.sh
-- compile it with ghcjs and  execute it with runghc
-- set -e && port=`echo ${3} | awk -F/ '{print $(3)}'` && docker run -it -p ${port}:${port} -v $(pwd):/work agocorona/transient:24-03-2017  bash -c "cd work && mkdir -p static && ghcjs ${1} -o static/out && runghc ${1}  ${2} ${3}"
```

That header compiles the program with GHCJS and write the javascript code generated to the "static" folder and then executes the server program in interpreted mode with `runghc`. This is useful for rapid development, since you can modify the code and re-execute it very fast.

To fully compile and execute the program, you can susbstitute `runghc` by `ghc` and execute the binary. The header would look like:

```csh
#!/usr/bin/env ./execthirdline.sh
-- compile it with ghcjs and  execute it with runghc
-- set -e && port=`echo ${3} | awk -F/ '{print $(3)}'` && docker run -it -p ${port}:${port} -v $(pwd):/work agocorona/transient:24-03-2017  bash -c "cd work && mkdir -p static && ghcjs ${1} -o static/out && ghc ${1}  -o program && chmod 777 program && ./program ${2} ${3}"
```
That header, besides executing the application, it would also create a "program"  executable in your host machine  (as well as an "static" folder with files needed for the client-side application. You can execute it natively in a linux distro in the way it will be described below.

More complicated projects can be compiled and executed using `cabal` and `stack`. You can modify the header accordingly.

For example, this is a program that is directly executable with docker

```haskell
#!/usr/bin/env ./execthirdline.sh
-- compile it with ghcjs and  execute it with runghc
-- set -e && port=`echo ${3} | awk -F/ '{print $(3)}'` && docker run -it -p ${port}:${port} -v $(pwd):/work agocorona/transient:24-03-2017  bash -c "cd work && mkdir -p static && ghcjs ${1} -o static/out && runghc ${1}  ${2} ${3}"

import Prelude hiding (div, id, span)
import Transient.Base
import GHCJS.HPlay.View
import Transient.Move
import Transient.Indeterminism
import Data.IORef
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class
import Data.Monoid

main= keep . initNode . onBrowser $ do 
    local . render $  wlink () (h1 "hello fibonacci numbers")
    
    r <-  atRemote $ do
                r <- local . threads 1 . choose $ take 10 fibs
                localIO $ print r
                localIO $ threadDelay 1000000
                return r
    
    local . render . rawHtml $ (h2 r)
    where
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]  -- fibonacci numb. definition
```

To execute the program:
```
> chmod 777 YourSource.hs
> ./YourSource.hs -p start/<host>/<port>
```

where <host> <port>  are defined by you. for example `./YourSource.hs -p start/localhost/8080`

The program will be accessed from outside docker as a web application. Read the documentation of Docker for your platform about how to invoke it.

If you want to run it in a host Linux machine, you can generate the browser code and the executable from docker in the way described above. Then in in the host you can execute it:

```
> ./program - p start/localhost/8080
```

If you want to install Axiom in your host machine:
--------------------------------------------------

You need to install [stack](https://docs.haskellstack.org/en/stable/README/) and [ghcjs](https://github.com/ghcjs/ghcjs). The latter is not an easy task.

Then install Axiom in stack/ghc:

```
> stack install axiom
```

This should install ghc and compile everithing.

Alternatively, you can install [Haskell platform](https://www.haskell.org/platform/) and:

```
> cabal install axiom
```

In any case you need to install Axiom in GHCJS too:

```
> cabal install axiom --ghcjs
```

How to compile and run a program
================================
```
> mkdir -p static 
> ghcjs yourProgram.hs -o static/out
> ghc yourProgram.hs

> yourProgram -p start/yourhost/yourport

```

How to run Distributed applications
========================

If your program use `inputNodes` to connect N server nodes, you must use additional parameters in the command line:

in a computer or docker instance:
```
> yourProgram -p start/host1/port1
```
In the same or another computer or docker instance:
```
> yourProgram -p start/host2/port2/add/host1/port1/y
```
in the same or another computer or docker instance:
```
> yourProgram -p start/host3/port3/add/host1/port1/y
```

Be sure that the `host:port` ip addresses are reachable from all the machines.

This connect all the server nodes among them. 

The web browser can point to any host:port of them. You must have the static folder (wich contains the generated javascript files) as well as the executable in all the locations.

See [distrbutedApps](https://github.com/transient-haskell/transient-examples/blob/master/distributedApps.hs) that contain examples of distributed web applications.

Plans:
======

Axiom web nodes are client side applications. So dHTML rendering happens on the browser. It is intended to implement server side rendering as well as multipage navigation. The last release support page navigation and page templates for the creation of server-side content.


