---------------------
Writing a new plugin.
---------------------

There are three kinds of plugins:

- "theory plugins", that control the strategies of each theory-specific procedure
- "logic plugins", that control the strategies of proof-search in pure logic
- "combination plugins", that control the strategies for theory combinations

Theory plugins are to be placed in portfolio/pluginsTh
They should provide a module whose signature is the module type defined in spec/pluginTh.ml
That module should be made visible to the rest of Psyche by adding it to portfolio/pluginsTh.mlpack
It should also be listed in the bank of theory plugins in portfolio/pluginsTh_register.ml
...and in that file, get_default should select it when the appropriate theory handler is given 

Logic plugins are to be placed in portfolio/pluginsG
They should provide a module whose signature is the module type defined in spec/pluginG.ml
That module should be made visible to the rest of Psyche by adding it to portfolio/pluginsG.mlpack
It should also be listed in the bank of logic plugins in portfolio/pluginsG_register.ml
...and in that file, parse should select it when the appropriate string is given 

Combination plugins are to be placed in portfolio/plugins
They should provide a module whose signature is the module type defined in spec/plugin.ml
That module should be made visible to the rest of Psyche by adding it to portfolio/plugins.mlpack
It should also be listed in the bank of logic plugins in portfolio/plugins_register.ml
...and in that file, parse should select it when the appropriate string is given 

