# zonquerer

![Screenshot](https://github.com/death/zonquerer/raw/master/screenshots/zonquerer.png "Screenshot")

This is Zonquerer, a real-time strategy game, almost.

It is my submission to the Spring Lisp Game Jam 2021.

# development

It was developed in 7 days (except the `a-star` code, I guess).

## platform

```
Machine-Type               : X86-64
Machine-Version            : Intel(R) Core(TM) i5-4460  CPU @ 3.20GHz
Software-Type              : Linux
Software-Version           : 5.11.12-arch1-1
Lisp-Implementation-Type   : SBCL
Lisp-Implementation-Version: 2.1.3.86-780d9bfcf
```

## tools

- [aseprite](https://www.aseprite.org/)

- [tiled](https://www.mapeditor.org/)

# howto

Clone this repository into your local projects directory and type the
following at your toplevel:

```lisp
(ql:quickload "zonquerer")

(zonquerer:play)
```

# license

AGPL3
