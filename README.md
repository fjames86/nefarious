# nefarious
NFS library

1. Introduction
-----------------

This is an attempt at implementing an NFS client and server in Common Lisp. 

See its sister project, FRPC, for the underlying ONCRPC implementation details.

2. Aims
--------

* Type in the RPC interface and handler stubs [DONE]
* Choose a reference implementation. I've chosen FreeNFS, because it's for Windows. [DONE]
* Successfully call client RPCs against the reference NFS implementation. I can call the various
RPCs, but I need to add support for authentication to FRPC before I can progress further.
* Writing the server component is probably not going to be easy.

2. License
------------

Released under the terms of the MIT license.


Frank James 
Febuary 2015