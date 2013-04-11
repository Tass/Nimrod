# Nimrod Compiler
This repo contains the Nimrod compiler, Nimrod's stdlib, tools and 
documentation.

## Compiling
Compiling the Nimrod compiler is quite straightforward. Because
the Nimrod compiler itself is written in the Nimrod programming language
the C source of an older version of the compiler are needed to bootstrap the
latest version. The C sources are however included with this repository under
the build directory.

Pre-compiled snapshots of the compiler are also available on
[Nimbuild](http://build.nimrod-code.org/). Your platform however may not 
currently be built for.

The compiler currently supports the following platform and architecture 
combinations:
  
  * Windows (Windows XP or greater) - x86 and x86_64
  * Linux (most, if not all, distributions) - x86, x86_64, ppc64 and armv6l
  * Mac OS X 10.04 or higher - x86, x86_64 and ppc64
  
In reality a lot more are supported, however they are not tested regularly.

To build from source you will need:

  * gcc 3.x or later recommended. Other alternatives which may work
    are: clang, Visual C++, Intel's C++ compiler
  * unzip
  * git or wget

If you are on a fairly modern *nix system, the following steps should work:

```
$ git clone git://github.com/Araq/Nimrod.git
$ cd Nimrod
$ cd build
$ unzip csources.zip
$ cd ..
$ ./build.sh
$ bin/nimrod c koch
$ ./koch boot -d:release
```

``koch install [dir]`` may then be used to install Nimrod, or you
can simply add it to your PATH.

The above steps can be performed on Windows in a similar fashion, the
``build.bat`` and ``build64.bat`` (for x86_64 systems) are provided to be used
instead of ``build.sh``.

## Getting help
A [forum](http://forum.nimrod-code.org/) is available if you have any questions,
and you can also get help in the IRC channel
on [Freenode](irc://irc.freenode.net/nimrod) in #nimrod.

## License
The compiler is licensed under the GPLv2 license, the standard library is
licensed under the LGPL license with a linking exception so that you can link
to it statically. This means that you can use any license for your own programs 
developed with Nimrod, allowing you to create commercial applications.

Read copying.txt for more details.

Copyright (c) 2004-2013 Andreas Rumpf.
All rights reserved.
