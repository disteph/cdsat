---------------------
Writing a new plugin.
---------------------

A plugin should implement Plugin.Type as defined in the file plugin.ml

To be usable with a theory, the type literals of the plugin should be the same as the type Atom.t of the theory

There are therefore two kinds of plugins:
- theory-specific plugins, which will be placed in the directory containing that theory, re-using its datatypes
- theory-generic plugins, which are functors taking as input a theory, and producing as output a module implementing Plugin.Type (with a type literals directly and blindly taken from the input theory)


The latter plugins will be placed in the present directory /generic-plugins, with the following hygiene policy:
- Write your generic plugin in a subdirectory XXX of the present directory
- It should contain a module MyPlugin (e.g. a file myPlugin.ml), with a functor GenPlugin of type
  functor (MyTheory: Theory.Type) -> (Plugin.Type with type literals = MyTheory.Atom.t)
- Add an mlpack file to package the directory into a module XXX
- Edit the main file main.ml to call XXX.MyPlugin.GenPlugin(MyTheory), where MyTheory is the desired theory
- To compile, do not forget to add to the top-level _tag file an entry to include your directory src/plugins/XXX for pack XXX
