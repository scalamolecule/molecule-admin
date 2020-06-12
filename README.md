
![](project/resources/Molecule-logo-M.png)

# Molecule Admin

[![Gitter](https://badges.gitter.im/scalamolecule/Lobby.svg)](https://gitter.im/scalamolecule/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

An advanced web app (built with [ScalaJS](http://www.scala-js.org)) to 
administrate [Datomic](https://www.datomic.com/on-prem.html) databases:

- Build and update Datomic database schemas.
- Build and run complex queries from a graphical query builder or directly with [molecules](http://www.scalamolecule.org).
- Sort, filter, group and browse large data sets easily and fast.
- Add/edit data directly in table cells with easy key-command navigation.
- Perform custom group edits on any number of entities.
- Recursively expand entities with simple mouseovers for fast exploration.
- Watch/undo transaction history.
- See/edit grouped values.
- Shows queries as molecules and datalog queries (for easy copy/paste).
- Automatic query optimization for fast query performance.

MoleculeAdmin uses [molecules](http://www.scalamolecule.org) as a query representation. 
But it can be used without knowing about/using molecules or Scala for that matter 
(Clojure/Java folks are welcome!).

For performing Datalog queries, please use the official 
[Datomic Console](https://my.datomic.com/account/create) (requires registration 
or using a Pro version of Datomic).


## Download

To explore MoleculeAdmin with the mBrainz sample database,
1. Download a Datomic distribution like the [free version](https://my.datomic.com/downloads/free)
2. Download and untar the [mBrainz](https://github.com/Datomic/mbrainz-sample) sample database
   ```
   cd datomic-free-0.9.5703.21
   wget https://s3.amazonaws.com/mbrainz/datomic-mbrainz-1968-1973-backup-2017-07-20.tar -O mbrainz.tar
   tar -xvf mbrainz.tar
   ```
3. Start the transactor in its own process
   ```
   bin/transactor config/samples/free-transactor-template.properties
   ```
4. In another process (open new tab/window in your terminal), install the mbrainz database
   ```
   bin/datomic restore-db file:mbrainz-1968-1973 datomic:free://localhost:4334/mbrainz-1968-1973
   ```
5. Download MoleculeAdmin
   ```
   cd ..
   git clone https://github.com/scalamolecule/molecule-admin.git
   ```

## Use MoleculeAdmin

Having downloaded and installed all the necessary parts listed above,

1. Start the Datomic transactor in its own process, something like this (might 
already be running):
   ```
   cd datomic-free-0.9.5703.21
   bin/transactor -Xmx4g -Xms4g -Ddatomic.txTimeoutMsec=120000 config/samples/free-transactor-template.properties
   ```
2. In another process, start the application:
   ```
   cd molecule-admin
   sbt run
   ```
3. Wait for the server to have started:
   ```
   (Server started, use Enter to stop and go back to the console...)
   ```
4. Then open [localhost:9001](http://localhost:9001) - this can take a while the 
first time since all dependencies are resolved and the whole project compiled 
(check progress in terminal).
   
You should arrive at a list of current databases, something like this (with your 
paths):

![](project/resources/StartPage1.png)

A few small generic sample databases were installed that you can freely play 
around with by exploring their `Schema` or perform a `Query`:
- CoreTest - contains all possible data types. Note how the Schema defines those
and how the query builder takes each type into consideration and only allow 
valid settings.
- Partition/Partition1 - exploring multiple partitions that each contains one or 
more namespaces.
- Tree - Used to test building complex hierarchies with the query builder. 
- mBrainz - large real data set that can give a sense of the query builder
capabilities and the scrolling/editing features.

MoleculeAdmin is built using the Google Chrome browser. So it's recommended to 
use this for expected look and behaviour.


## Enable your own Datomic database

The first page you arrive to and that can be accessed by clicking on the M logo 
in the top left corner shows a list of all installed databases that the 
transactor knows of.

For MoleculeAdmin to be able to work with a database it needs a path to a
[schema definition file](http://www.scalamolecule.org/manual/schema/) which is
a `molecule` representation of a Datomic database schema. The schema definition
file is the template for molecule to generate Scala boilerplate code to make 
type safe molecule queries. 

So when you
have made changes to the schema definition file, you recompile your 
project code to have boilerplate code generated using the changed schema
definition file as a blueprint. That way you are guaranteed to only be able to 
make correct and type safe queries in your code.

In the first life cycles of a project, you'll likely develop your database schema
by writing a lot of changes directly to the schema definition file. But when the schema 
stabilizes and you start to enter real data into the database, you'll want a more
flexible way to make changes to the schema and at the same time have those
changes continuously propagated to your generated boilerplate. This is when
MoleculeAdmin can "take over" the responsibility of keeping your schema definition
file in sync with your database. So instead of you writing manually to the schema 
definition file, you let MoleculeAdmin write changes directly to it. That is why 
MoleculeAdmin asks for a path to the schema definition file for those 
databases/schema definition files that you want it to administrate for you.

You can also simply ignore giving a path to some database if you don't want to
work with it in MoleculeAdmin.

You also still have to recompile your project whenever you make schema changes this 
way via MoleculeAdmin and the schema definition file got changed.

For Clojure/Java users not using Scala, it would be cool if we could use the newer
Datomic feature of exporting the schema definition for a database
in order to automatically generate a schema definition file. Since the free
version of Datomic is not up to date and the export feature not available in 
the current older version, we can't yet automate this process and you'll have to 
make such [schema definition file](http://www.scalamolecule.org/manual/schema/) 
manually for now.


## Development

If you have questions or ideas, please feel free to discuss in the gitter channel:

[![Gitter](https://badges.gitter.im/scalamolecule/Lobby.svg)](https://gitter.im/scalamolecule/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

Or you can submit issues/pull requests too.


## Test

Run all internal MoleculeAdmin test by platform:
``` 
sbt
server> server/test
server> sharedJVM/test
server> sharedJS/test
```

.. or run individual tests:
```
sbt
server> server/testOnly moleculeadmin.servertest.log.LatestTxs
server> sharedJVM/testOnly moleculeadmin.sharedtest.util.DateTransformation
server> sharedJS/testOnly moleculeadmin.sharedtest.util.DateTransformation
```


## Compile

```
> sbt clean compile
> sbt sharedJS/fastOptJS
```

#### Author
Marc Grue

#### License
Molecule is licensed under the [Apache License 2.0](http://en.wikipedia.org/wiki/Apache_license)