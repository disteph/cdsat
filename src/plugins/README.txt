---------------------
Writing a new plugin.
---------------------

A plugin should implement Plugin.Type as defined in the file plugin.ml

Hygiene policy:
- Write your plugin in a subdirectory XXX of the present directory
- It should contain a module XXX of the same name (e.g. a file xXX.ml), with a submodule MyPlugin implementing Plugin.Type
- Edit the main file main.ml to call XXX.MyPlugin
- To compile, do not forget to add to the top-level _tag file an entry to include your directory XXX
