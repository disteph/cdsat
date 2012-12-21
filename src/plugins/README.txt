---------------------
Writing a new plugin.
---------------------

A plugin should implement Plugin.Type as defined in the file plugin.ml

Hygiene policy:
- Write your plugin in a subdirectory XXX of the present directory
- It should contain a module MyPlugin (e.g. a file myPlugin.ml), implementing Plugin.Type
- Add an mlpack file to package the directory into a module XXX
- Edit the main file main.ml to call XXX.MyPlugin
- To compile, do not forget to add to the top-level _tag file an entry to include your directory src/plugins/XXX for pack XXX
