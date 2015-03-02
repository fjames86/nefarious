# nefarious
An NFS implementation in Common Lisp.

1. Introduction
-----------------

This is an attempt at implementing an NFS client and server in Common Lisp. The main aim for
the project is to both as an educational tool for learning about implementing an NFS, and also
as a robust test platform for developing its sister project, FRPC, the underlying ONC/RPC implementation.

Performance and security are not at present significant concerns.

2. Aims
--------

Immediate project aims:
* Type in the RPC interface and handler stubs [DONE]
* Choose a reference implementation. I've chosen FreeNFS, because it's for Windows which is where I do most development. [DONE]
* Successfully call client RPCs against the reference NFS implementation. [ DONE ]
* Fill in the handler stubs [ IN-PROGRESS ]

More general aims:
* Write an NFS client and server which work in most situations
* Performance and security are not at all important
* This is an educational project only (at present)
* Provide a robust test platform for developing the ONC/RPC framework, frpc. This is one of main purposes of the project.

3. Client
----------

All the NFS and mount protocol messages are implemented and should be functioning. Look at the exported 
symbols startinn with CALL- from the nefarious and nefarious.mount packages.

```
(defvar dhandle (nfs.mount:call-mount "/"))

(nfs:call-read-dir dhandle)

(defvar fh (nfs:call-lookup dhandle "foo.txt"))

(nfs:call-read fh 0 1024)

(nfs.mount:call-unmount "/")
```

3.1 NFS file streams
---------------------

A new stream type which reads/writes to remote files, NFS-FILE-STREAM.

```
(with-nfs-mount (dhandle "/" :host "xxx")
  (with-nfs-file (dhandle f "foo.txt" :host "xxx")
    (let ((buffer (make-array 16)))
      (read-sequence buffer f)
      buffer)))
```

4. Server 
-----------

Run the server using:

```
(nfs:start "/")

(nfs:stop)
```

This starts an RPC server instance which listens on TCP and UDP ports 111, 635 and 2049.

The intention is to be able to successfully mount and access the files from a reference NFS client
implentation. I have chosen the standard Ubuntu NFS client. The aims are:
* successfully run: $ mount -t nfs -o nolock,nfsvers=3 host:/ /media/nefarious
* successfully enumerate files
* create, read, write, remove files
* create, remove directories

At present the PATH-CONF, FS-INFO and FS-STAT handlers have not been typed in. This prevents
the mount command from working. The actual NFS commands do work however.

5. License
------------

Released under the terms of the MIT license.


Frank James 
Febuary 2015