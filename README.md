# nefarious
NFS library

1. Introduction
-----------------

This is an attempt at implementing an NFS client and server in Common Lisp. 

See its sister project, FRPC, for the underlying ONCRPC implementation details.

2. Aims
--------

* Type in the RPC interface and handler stubs [DONE]
* Choose a reference implementation. I've chosen FreeNFS, because it's for Windows which is where I do most development. [DONE]
* Successfully call client RPCs against the reference NFS implementation. [ DONE ]
* Writing the server component is probably not going to be easy.

3. Client
----------

The various CALL- functions exported from the NEFARIOUS package provide client functionality. At present it's in 
a rather raw form, but appears to be reasonably functional. 

3. License
------------

Released under the terms of the MIT license.


Frank James 
Febuary 2015