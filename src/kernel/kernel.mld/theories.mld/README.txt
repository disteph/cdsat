---------------------
Writing a new theory.
---------------------

A theory does not need to implement a specific interface, since the
only other components of Psyche that will directly interact with it
are the plugins specific to that theory.

In other words, write your theory as you want, and then write for your
theory a plugin (or several plugins) accordingly. The idea is that
your theory is heuristic-agnostic (typically it implements the search
primitives directly corresponding to a textbook inference system that
decides your theory), and the theory plugins are where you will
programme the heuristics driving the application of the search
primitives efficiently.

That being said, the plugins will eventually have to communicate back
to the kernel some answers like:
- the sequent is provable using this theory
- the sequent has a counter-model consistent with this theory

And you want those answers to be certified by the theory itself,
without your untrusted plugins messing around with them.

Psyche's kernel offers the support for producing *signed answers*,
i.e. answers authentificated as coming from a theory, and not
modifiable by any other piece of code.

These answers, or messages, are defined in Kernel.Top.Messages, where
you will find 5 primitives to produce them:

val thProvable   : 'sign -> 'tset -> ('sign,'tset,thProvable) thsays
val thNotProvable: 'sign -> 'tset -> ('sign,'tset,thNotProvable) thsays
val thStraight: 'sign -> 'tset -> ('tset->'tset)             -> ('sign,'tset,thStraight) thsays
val thAnd : 'sign -> 'tset -> 'tset -> ('tset->'tset->'tset) -> ('sign,'tset,thAnd) thsays
val thOr  : 'sign -> 'tset -> 'tset -> (bool->'tset->'tset)  -> ('sign,'tset,thOr) thsays

Every time, the type of the first argument ends up labelling the type
of the answer. So to produce an authentificated answer, your theory
should declare a private or abstract type (unit will do), and should
use an inhabitant of it as the first argument of its calls to the
above functions. Then make sure that you never leak any inhabitant of
your private/abstract type to the outside world, and no other piece of
code will ever be able to create messages that appear as being signed
by your theory. It will only be able to pass your messages around.

Then in order to declare your theory as a theory that the kernel can
trust, just add a handler for your theory in the type
sign Kernel.Theories_register.Sig.t
where sign is your theory's private or abstract type.

Hygiene policy:

- Write your theory in a subdirectory XXX of the present directory

- Add in the present directory an mlpack file to package the directory
  into a module XXX

- Add your handler in module Kernel.Theories_register.Sig

- Add your handler in the list of all theories
  Kernel.Theories_register.all_theories_list

- Possibly, edit Kernel.Theories_register.parse so that the user can
  select or unselect your theory in order to treat a problem, and in
  that case you can warn the user of this possibility by editing the
  the main file main.ml to add your theory to the list of known
  theory names

- To compile, do not forget to add to the top-level _tag file an entry
  to include your directory src/theories/XXX for pack XXX
  You may also need to add a line
  Pathname.define_context "src/kernel/theories/XXX" ["src/kernel/"; "src/kernel/theories/XXX"];
  in function mydispatch of the top-level file myocamlbuild.ml
