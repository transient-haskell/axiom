[![Hackage](https://img.shields.io/hackage/v/ghcjs-hplay.svg)](http://hackage.haskell.org/package/ghcjs-hplay)
[![Stackage LTS](http://stackage.org/package/ghcjs-hplay/badge/lts)](http://stackage.org/lts/package/ghcjs-hplay)
[![Stackage Nightly](http://stackage.org/package/ghcjs-hplay/badge/nightly)](http://stackage.org/nightly/package/ghcjs-hplay)
[![Build Status](https://travis-ci.org/transient-haskell/ghcjs-hplay.png?branch=master)](https://travis-ci.org/agocorona/ghcjs-hplay)

Axiom (ghcjs-hplay)
==========
![](http://vignette3.wikia.nocookie.net/pixar/images/6/6d/Wall-E_Axiom_Deck_Analysis_Map.jpg/revision/latest/scale-to-width-down/185?cb=20120718160701)

For some examples, see the [transient-examples](https://github.com/transient-haskell/transient-examples) repository: [distributedApps.hs](https://github.com/transient-haskell/transient-examples/blob/master/distributedApps.hs) and      [webapp.hs](https://github.com/transient-haskell/transient-examples/blob/master/webapp.hs)

The source code can be executed in the commmand line if you have docker installed.

[![Gitter](https://badges.gitter.im/theam/haskell-do.svg)](https://gitter.im/Transient-Transient-Universe-HPlay/Lobby?utm_source=share-link&utm_medium=link&utm_campaign=share-link)

Axiom (the new name of ghcjs-hplay) is also the Web user interface of [Transient](https://github.com/agocorona/transient). The Web functionality of transient will be called **Axiom**, like the cruise starship of Wall-e. Axiom is made to let you navigate the universe of nodes in the cloud trough your browser while you are comfortably seated in your [hoverchair](https://www.youtube.com/watch?v=uOL2W9JQmo8).

Unlike his predecessor, [hplayground](http://github.com/agocorona/hplayground), Axiom has full integration with Transient and can run widgets that run code on the server, the client or both.

Axiom execute browser widgets that are reactive, can be composed monadically and algebraically (applicative, alternative, monoidal..). At the same time they participate in cloud computations. A widget can execute code in the server and, trough the server, in any node on the cloud using the same cloud primitives defined in transient-universe. The example applications include widgets that perform distributed map-reduce and federated chat servers as well as stream fibonacci numbers from server to client and from client to server.

To see how it integrates with Transient and how to create client-server applications, see the web paragraphs of the [transient tutorial](https://github.com/agocorona/transient/wiki/Transient-tutorial).

To see how to create client side applications and widgets (with no server code integration), look for  [hplayground](https://github.com/agocorona/hplayground) package. [Tutorial](https://www.airpair.com/haskell-tutorial/intro-to-haskell-web-apps) 

How it works
============
The JS program compiled with GHCJS is sent to the browser, then it opens a websockets connection. Then  the most useful primitive is `atRemote` wich execute his argument in the server and return the result back to the client  (or viceversa, see below). The communication transport the variables necessary for executing the computation remotely. There is no explicit serialization neither communication. All is done implicitly.

`atRemote` can be executed inside itself, so a computation can jump from server to client and back. So a browser can be controlled by the server or the other way aroud. But the execution starts in the browser. There is very little data to transport to execute remotely.  In the other side, streaming  and reactivity in both directions is included with no additional considerations with `atRemote` and other primitives. Isn't awesome?

In the other side, there is an experimental template editor to generate static HTML templates. The server can execute a rest route and bring the corresponding page template and the JS code to the browser, so web crawlers can find something to read.
Also in Axiom everithing compose algebraically with standard applicative, alternative and monoidal operators, and  also monadically:  

Larger widgets can be composed with algebraic combinations of smaller widgets. No limits. Widgets can have server side (they can use `atRemote`) so they are full stack, autonomous pieces down to the cloud. They make perfect software components.

Events do not bubble up to the top like in the case of React. An event within a widget produce a monadic response that executes widgets down trough the monad without affecting the surrounding rendering not affected by the event. That is why Axiom does not need a Virtual DOM, and the logic of the application and the execution flow match, so it produces a clean and understandable code. look at the TODO app (it is client-side only)

tryplayg.herokuapp.com/try/todo.hs/edit



Plans:
======

Axiom is in the process of becoming a Server-side as well as Client-side library for creating Web application. The last release support page templates for the creation of server-side content.

In the future it will manage routes in the server side besides client side, and will generate dinamic HTML content in the server as well as in the client.
