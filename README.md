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
