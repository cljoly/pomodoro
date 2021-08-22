<!-- insert
---
title: "Light, simple, paper-like, Pomodoro timer"
date: 2021-08-22T04:48:15
---
end_insert -->

> Archived: the code is not compiling anymore (years of breaking changes in various dependencies)     
> You may be interested in https://github.com/open-pomodoro/openpomodoro-cli as an alternative.

<div align="center" class="badges">

<!-- remove -->
# Light, simple, paper-like, Pomodoro timer
<!-- end_remove -->

[![](https://img.shields.io/badge/licence-CeCILL--B-blue.svg)](http://cecill.info/licences/Licence_CeCILL-B_V1-en.html)
[![](https://img.shields.io/badge/opam-pomodoro-orange.svg)](http://opam.ocaml.org/packages/pomodoro/)

[![Version 0.2](https://download.tuxfamily.org/pomodoro/img/v02.png)](https://pomodoro.ml)

</div>

## Yet an other pomodoro timer…

…but this one different

### Paper like

 + Thanks to a simple design, you won't spend more time to manage your task list
   than you would need to actually do it!
 + Never change your log file (you edit it with your favorite text editor)

### Robust

 + Written in OCaml, a type-safe language
 + Internally use timer that remain accurate even when the programme is frozen
   for a while

### Light and fast

 + NCurse like interface
 + Careful design, to limit memory and CPU footprint

## Install

Just run

```
opam install pomodoro
```

If you want to use the latest version, pin the git repository.

```
opam pin add pomodoro git://git.tuxfamily.org/gitroot/pomodoro/paper_pomodoro.git
```

## [Licence](licence.en.html)

```
©  Clément Joly, 2016

foss@131719.xyz

This software is a computer program whose purpose is to use Pomodoro method.

This software is governed by the CeCILL-B license under French law and
abiding by the rules of distribution of free software.  You can  use,
modify and/ or redistribute the software under the terms of the CeCILL-B
license as circulated by CEA, CNRS and INRIA at the following URL
"http://www.cecill.info".

As a counterpart to the access to the source code and  rights to copy,
modify and redistribute granted by the license, users are provided only
with a limited warranty  and the software's author,  the holder of the
economic rights,  and the successive licensors  have only  limited
liability.

In this respect, the user's attention is drawn to the risks associated
with loading,  using,  modifying and/or developing or reproducing the
software by the user in light of its specific status of free software,
that may mean  that it is complicated to manipulate,  and  that  also
therefore means  that it is reserved for developers  and  experienced
professionals having in-depth computer knowledge. Users are therefore
encouraged to load and test the software's suitability as regards their
requirements in conditions enabling the security of their systems and/or
data to be ensured and,  more generally, to use and operate it in the
same conditions as regards security.

The fact that you are presently reading this means that you have had
knowledge of the CeCILL-B license and that you accept its terms.
```

> This application is not affiliated, associated or endorsed by the Pomodoro
> Technique® or Francesco Cirillo.
