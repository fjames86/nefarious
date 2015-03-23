# nefarious
Nefarious is an NFS client and server in Common Lisp. It is intended to be as portable as possible. 
It has been primarily developed using SBCL on Windows. Both client and server have been run on Windows 
and Linux and should perform equally.

## 1. Introduction

Nefarious is an NFS implemention in Common Lisp, supporting both client and server. Currently only NFSv3 is supported. 

The Nefarious client has been used successfully with the [FreeNFS](http://sourceforge.net/projects/freenfs/) on Windows 8.1.

The Nefarious server has been successfully mounted with [nekodrive](https://code.google.com/p/nekodrive/) and 
the NFS client that comes with Ubuntu Linux. 

The Nefarious server does not implement the "raw" filesystem interaction. Instead, it provides the interaction
with the RPC layer and defines an API that users should implement. This makes it easy to export arbitrary 
filesystems over NFS.

## 2. Client

All the NFS and mount protocol messages are implemented. Find the mount protocol messages 
in NEFARIOUS.MOUNT package and the NFS messages in NEFARIOUS package. For example:

```
(defvar dhandle (nfs.mount:call-mount "/" :host "myhost" :protocol :udp))

(nfs:call-read-dir dhandle :host "myhost" :protocol :udp)

(defvar fh (nfs:call-lookup dhandle "foo.txt" :host "myhost" :protocol :udp))

(nfs:call-read fh 0 1024 :host "myhost" :protocol :udp)

(nfs.mount:call-unmount "/" :host "myhost" :protocol :udp)
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

In order for the server to actually export any filesystems over NFS, users should first register
a provider. They should then simply start an RPC server.

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

Please note that this provider really is very "simple". It does not try to do anything fancy, it will 
not scale to large directories of files (because it builds up a list of file handles and makes no attempt to 
purge them). It's main aim is to provide an example of a working provider. More serious users
should endeavour to write their own provider.

#### 3.1.2 Registry provider

Windows only. Exports the windows registry as an NFS filesystem. 

```
;; export the HKEY_LOCAL_MACHINE hive as "/reg"
(register-provider (nfs.registry:make-registry-provider :local-machine) "/reg")

;; export HKEY_LOCAL_MACHINE\SOFTWARE as "/hklm/software"
(register-provider (nfs.registry:make-registry-provider :local-machine "SOFTWARE") "/hklm/software")
```

### 3.2 Example
The server component is an RPC server which implements the port-mapper, mount and NFS programs. 
It listens on ports 111, 635 and 2049 (TCP and UDP).

Run the server using:

```
;; if running on systems without a port mapper (such as Windows)
(nfs:start)

(nfs:stop)

;; if running on systems with a port mapper program (such as Linux)
(nfs:start :port-mapper nil)
```

Mount from Linux using the typical command,
```
$ mount -t nfs -o udp,nolock,nfsvers=3 192.168.0.1:/ /media/nfs
$ ls -l /media/nfs
$ umount /media/nfs 
```

## 4. Network status Monitor

The NSM protocol is implemented. Clients should register to receive notifications 
of state changes in remote hosts by providing a callback function. The callback will be executed 
when the status change is received.

The local server must also persist a state sequence number. By default nefarious uses the file named by 
*default-nsm-pathspec*. You should ensure this has been set before starting the nefarious server. 

## 5. Notes

Nefarious is based on [frpc](https://github.com/fjames86/frpc), the underlying ONC-RPC implementation.

The primary purpose of both projects is to get something simple which works, rather than a high-performance, professional
product. At best, it should be considered an educational project which could be used to form the basis of something better.
Please do not expect high performance or a bug-free experience, although it would be nice to have both eventually.

The NSM protocol, which supports state change notifications, has been implemented. 

The NLM protocol, which is used to implement file locking, has not been implemented.

Nefarious, like frpc, uses LOG4CL for debug logging. It also shares the unfortunate property of very poor performance
when debug logging is turned on. When not doing development, please turn the debug logging off. Going forward, 
it would probably be a good idea to use low-latency debug logging system (such as what?). 

If running nefarious on systems with a port-mapper program already running (such as all typical Linux systems), you should 
not run the port-mapper program from Lisp. Set the PORT-MAPPER option to NFS:START as nil. This will register the nefarious 
RPC programs with the local portmapper instead of the Lisp port mapper. 

## 6. License

Released under the terms of the MIT license.


Frank James 
Febuary 2015