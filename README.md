# nefarious
An NFS client and server in Common Lisp.

## 1. Introduction

Nefarious is an NFS implemention (both client and server) in Common Lisp, currently only NFSv3 is supported. 

## 2. Client

All the NFS and mount protocol messages are implemented. 

```
(defvar dhandle (nfs.mount:call-mount "/"))

(nfs:call-read-dir dhandle)

(defvar fh (nfs:call-lookup dhandle "foo.txt"))

(nfs:call-read fh 0 1024)

(nfs.mount:call-unmount "/")
```

## 2.1 NFS file streams

A stream type which reads/writes to remote files, NFS-FILE-STREAM.

```
(with-nfs-mount (dhandle "/" :host "xxx")
  (with-nfs-file (f dhandle "foo.txt" :host "xxx")
    (let ((buffer (make-array 16)))
      (read-sequence buffer f)
      buffer)))
```

## 3. Server 

The server component is an RPC server which implements the port-mapper, mount and NFS programs. 
It listens on ports 111, 635 and 2049 (TCP and UDP).

Run the server using:

```
(nfs:start)

(nfs:stop)
```

### 3.1 Providers

Users should register providers to implement the NFS functionality. Providers are instances 
of classes which inherit from NFS-PROVIDER. Users should specialize the generic functions 
defined in providers.lisp to implement the functionality of each NFS method. 
See the  simple provider or the examples. It is possible to define providers which export
a virtual filesystem, the Windows registry etc.

#### 3.1.1 Simple provider

The default provider, SIMPLE-PROVIDER, can be used to export a directory from the local filesystem.

```
;; registers a simple provider that exports the directory named by *default-pathname-defaults* as "/"
(register-provider (make-simple-provider) "/")

;; registers a simple provider that exports the administrator home directory
(register-provider (make-simple-provider "C:\\Users\\administrator") "/admin")
```

#### 3.1.2 Registry provider

Windows only. Exports the windows registry as an NFS filesystem. 

```
;; export the HKEY_LOCAL_MACHINE hive as "/reg"
(register-provider (nfs.registry:make-registry-provider :local-machine) "/reg")

;; export HKEY_LOCAL_MACHINE\SOFTWARE as "/hklm/software"
(register-provider (nfs.registry:make-registry-provider :local-machine "SOFTWARE") "/hklm/software")
```

## 4. Notes

Nefarious is based on [frpc](https://github.com/fjames86/frpc), the underlying ONCRPC implementation.

The primary purpose of both projects is to get something simple which works, rather than a high-performance, professional
product. At best, it should be considered an educational project which could be used to form the basis of something better.

## 5. License

Released under the terms of the MIT license.


Frank James 
Febuary 2015