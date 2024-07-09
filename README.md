# apibuilder-commons

General scala utilities for interacting with API Builder

Versions 0.0.x are compatible with scala2

Version 0.1.x and greater compatible with scala3

##

Importing:
```
      "io.apibuilder" %% "apibuilder-commons" % "0.1.0"
```

## Configuration

Read the 'default' profile from the `~/.apibuilder/config` file:


```
    import apibuilder.config.{Config, Profile}
    val profile = Config.mustFindDefaultProfile
    println(s"profile apiUri: " + profile.apiUri)
```


