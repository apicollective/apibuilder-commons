# apibuilder-commons

General scala utilities for interacting with Api Builder

## Configuration

Read the 'default' profile from the `~/.apibuilder/config` file:


```
    import apibuilder.config.{Config, Profile}
    val profile = Config.mustFindProfile()
```


