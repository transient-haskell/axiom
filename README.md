[![Hackage](https://img.shields.io/hackage/v/ghcjs-hplay.svg)](http://hackage.haskell.org/package/ghcjs-hplay)
[![Stackage LTS](http://stackage.org/package/ghcjs-hplay/badge/lts)](http://stackage.org/lts/package/ghcjs-hplay)
[![Stackage Nightly](http://stackage.org/package/ghcjs-hplay/badge/nightly)](http://stackage.org/nightly/package/ghcjs-hplay)
[![Build Status](https://travis-ci.org/agocorona/ghcjs-hplay.png?branch=master)](https://travis-ci.org/agocorona/ghcjs-hplay)

Axiom (ghcjs-hplay)
==========
![](http://vignette3.wikia.nocookie.net/pixar/images/6/6d/Wall-E_Axiom_Deck_Analysis_Map.jpg/revision/latest/scale-to-width-down/185?cb=20120718160701)

For some examples, see the [transient-examples](https://github.com/transient-haskell/transient-examples) repository: [distributedApps.hs](https://github.com/transient-haskell/transient-examples/blob/master/distributedApps.hs) and      [webapp.hs](https://github.com/transient-haskell/transient-examples/blob/master/webapp.hs)

The source code can be executed in the commmand line if you have docker installed.

[![Gitter](https://badges.gitter.im/theam/haskell-do.svg)](https://gitter.im/Transient-Transient-Universe-HPlay/Lobby?utm_source=share-link&utm_medium=link&utm_campaign=share-link)

Axiom (the new mame of ghcjs-hplay) is also the Web user interface of [Transient](https://github.com/agocorona/transient). 

Unlike his predecessor, [hplayground](http://github.com/agocorona/hplayground), Axiom has full integration with Transient and can run widgets that run code on the server, the client or both.

To see how it integrates with Transient and how to create client-server applications, see the web paragraphs of the [transient tutorial](https://github.com/agocorona/transient/wiki/Transient-tutorial).

To see how to create client side applications and widgets (with no server code integration), look for  [hplayground](https://github.com/agocorona/hplayground) package. [Tutorial](https://www.airpair.com/haskell-tutorial/intro-to-haskell-web-apps) 

Plans:
======

Axiom is in the process of becoming a Server-side as well as Client-side library for creating Web application. The last release support page templates for the creation of server-side content.

In the future it will manage routes in the server side besides client side, and will generate dinamic HTML content in the server as well as in the client.
