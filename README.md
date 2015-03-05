# nefarious
An NFS implementation in Common Lisp.

1. Introduction
-----------------

Nefarious is an NFS implemention in Common Lisp.

2. Aims
---------

* Provide a robust test-platform for the ONC-RPC implementation, frpc. These two packages have been
developed in tandem.
* Get the client-portion working. 
* Implement a robust, generalized server component. The server should be able to export
a generalized filesystem, not necessarily the underlying host filesystem (although it should be able
to do that too).


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

The server component is an RPC server which implements the port-mapper, mount and NFS programs. 
It listens on ports 111, 635 and 2049 (TCP and UDP).

Run the server using:

```
(nfs:start)

(nfs:stop)
```

4.1 Providers
--------------

Users should register providers to implement the NFS functionality. Providers are instances 
of classes which inherit from NFS-PROVIDER. Users should specialize the generic functions 
defined in providers.lisp to implement the functionality of each NFS method. 
See the  simple provider or the examples. It is possible to define providers which export
a virtual filesystem, the Windows registry etc.

4.1.1 Simple provider
----------------------

The default provider, SIMPLE-PROVIDER, can be used to export a directory from the local filesystem.

```
;; registers a simple provider that exports the directory named by *default-pathname-defaults* as "/"
(register-provider (make-simple-provider) "/")

;; registers a simple provider that exports the administrator home directory
(register-provider (make-simple-provider "C:\\Users\\administrator") "/admin")
```

4.1.2 Registry provider
-------------------------

Windows only. Exports the windows registry as an NFS filesystem.

```
;; export the HKEY_LOCAL_MACHINE hive as "/reg"
(register-provider (nfs.registry:make-registry-provider "HKLM\\") "/reg")
```

5. License
------------

Released under the terms of the MIT license.


Frank James 
Febuary 2015