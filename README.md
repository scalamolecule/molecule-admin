[![Gitter](https://badges.gitter.im/scalamolecule/Lobby.svg)](https://gitter.im/scalamolecule/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

# Molecule Admin

ScalaJS client app to administrate your Datomic/molecule database schema, data queries and updates etc.

Start the Datomic transactor from the Datomic distribution directory in separate process, something like

```
> cd <datomic-distribution-dir>
> bin/transactor -Xmx4g -Xms4g -Ddatomic.txTimeoutMsec=120000 config/samples/free-transactor-template.properties
```

Download MoleculeAdmin

```
> git clone https://github.com/scalamolecule/molecule-admin.git // if not already downloaded
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
> sharedJVM/test
> sharedJS/test
```

Individual test
```
> sbt
> sharedJVM/testOnly -- moleculeadmin.sharedtest2.util.DateTransformation
> sharedJS/testOnly -- moleculeadmin.sharedtest.util.DateTransformation
```

## Compile

```
> sbt clean compile
> sbt sharedJS/fastOptJS
```
