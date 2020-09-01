# apibuilder-commons

General scala utilities for interacting with Api Builder

##

Importing:
```
      "io.apibuilder" %% "apibuilder-commons" % "0.0.1"
```

## Configuration

Read the 'default' profile from the `~/.apibuilder/config` file:


```
    import apibuilder.config.{Config, Profile}
    val profile = Config.mustFindDefaultProfile
    println(s"profile apiUri: " + profile.apiUri)
```


