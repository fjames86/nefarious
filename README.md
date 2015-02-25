# nefarious
An NFS implementation in Common Lisp.

1. Introduction
-----------------

This is an attempt at implementing an NFS client and server in Common Lisp. 

See its sister project, FRPC, for the underlying ONC/RPC implementation details.

2. Aims
--------

* Type in the RPC interface and handler stubs [DONE]
* Choose a reference implementation. I've chosen FreeNFS, because it's for Windows which is where I do most development. [DONE]
* Successfully call client RPCs against the reference NFS implementation. [ DONE ]
* Writing the server component is probably not going to be easy.

3. Client
----------

The various CALL- functions exported from the NEFARIOUS package provide client functionality. 
The client is working and has been used to mount, create, read, write and delete files. More work 
needs to be done to make the client functionality easier and more useful. This might include defining
a new stream type to make reading/writing remote files seamless.

4. Server 
-----------

The server component is progressing but not yet complete.

5. License
------------

Released under the terms of the MIT license.


Frank James 
Febuary 2015