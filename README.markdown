Pegout
======

Pegout is a Parsing Expression Grammer (PEG)-based parser which will 
parse a PEG and generate an HTML representation which allows easy, browser-based
navigation.

Pre-requisites
--------------

Pegout requires Erlang version R12B-3 (5.6.3) or later. The latest version of 
Erlang is available here:

[http://www.erlang.org/download.html](http://www.erlang.org/download.html)

You'll also need Neotoma installed, the latest version being available at 

[https://github.com/seancribbs/neotoma/](https://github.com/seancribbs/neotoma/)

plus Ruby and rake.

Compiling Pegout
----------------

To compile Pegout, type:

   rake

After compilation is complete, you'll see the test suite attempt to run but fail
because there is nothing in it ;-)

Implementation
--------------

Pegout is itself based on a PEG (derived from the Neotoma PEG developed by Sean Cribbs)
and generates the HTML is a fairly simplistic manner.
(In fact if I am honest it is a quick hack ;-)  - Handy, though.)

Links
-----

* Reia Home Page: [http://reia-lang.org](http://reia-lang.org)
* Reia Wiki: [http://wiki.reia-lang.org/](http://wiki.reia-lang.org/)
* Neotoma: [https://github.com/seancribbs/neotoma/](https://github.com/seancribbs/neotoma/)

About the Author
----------------

Pegout was created by Graeme Defty currently of Bangkok, Thailand. The need arose when working
on a PEG for the Reia programming language led to helping with the development of Neotoma itself.
Finding our way around someone else's PEG (if you will pardon the expression) can be difficult.

Graeme has a background in software development, starting as a programmer around 1827
(or nearly half past six).
His favorite programming languages are Ruby and Erlang.
