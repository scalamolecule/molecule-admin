### Some things to remember...

Scala-js:
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

           