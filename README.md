# Molecule Admin

ScalaJS client app to administrate your Datomic/molecule database schema, data queries and updates etc.

Start Datomic transactor from Datomic distribution directory in separate process, something like

```
> cd <datomic-distribution-dir>
> bin/transactor -Xmx4g -Xms4g -Ddatomic.txTimeoutMsec=120000 config/samples/free-transactor-template.properties
```

Download MoleculeAdmin

```
> git clone https://github.com/scalamolecule/molecule-admin.git
> cd molecule-admin
```

Start application:

```
> sbt run
```

Then open localhost:9001 in browser. Might take a while to compile. 

After Molecule DSL boilerplate code has been generated, compiled and saved in jars in the lib folder, you might
want to comment out `.enablePlugins(MoleculePlugin)` in your build file to avoid re-generation/packaging of 
Molecule boilerplate code. Turn it on again when updating schemas.


## Test

Test all
```
> sbt test
```

Test
``` 
> sbt
> moleculeAdminJVM/test
> moleculeAdminJS/test
```

Individual test
```
> sbt
> moleculeAdminJVM/testOnly -- moleculeadmin.sharedtest2.util.DateTransformation
> moleculeAdminJS/testOnly -- moleculeadmin.sharedtest.util.DateTransformation
```

## Compile

```
> sbt clean compile
> sbt moleculeAdminJS/fastOptJS
```




Scala-js ideas:
- don't have play server running while testing in Intellij (double compiling)
- be aware not to double-render because of implicit Rx double-wrapping
- Client code: if "nothing happens", wrap code in try-catch and println to see possible undefined in console or debug in browser
- "LinkingException: There were linking errors": wipe out target folders and do `sbt clean compile`

Autowire:
- passing `None` results in `InvalidInput: null`
- default arguments only in interface, NOT in implementation! Otherwise causes weird compiler errors. 

Molecule:
- forgetting to add `update` after `assert(someValue)`

Shared code issues:
- devilish difference between .js/.jvm code: 
SimpleDateFormat.parse method is implemented on jvm side but not on js side! 
So testing on jvm won't expose it before client tries to call the method in vain  

           