### Some things to remember...

Testing_
- Test classes are not automatically compiled with `sbt clean compile`!
  Running `sbt clean compile test` will compile test-classes too and then you can
  test them too from IntelliJ

Scala-js:
- be aware not to double-render because of implicit Rx double-wrapping
- Client code: if "nothing happens", wrap code in try-catch and println to see possible undefined in console or debug in browser
- "LinkingException: There were linking errors": wipe out target folders and do `sbt clean compile`
- molecule library as dependency needs to be compiled to both 2.12 and 2.13 to not 
    show as red in code (although it still compiles, oddly enough) 

Autowire:
- passing `None` results in `InvalidInput: null` 
maybe not anymore...
- default arguments only in interface, NOT in implementation! Otherwise you'll get weird compiler errors. 
maybe not anymore...

Molecule:
- forgetting to add `update` after `assert(someValue)`

Shared code issues:
- devilish difference between .js/.jvm code: 
SimpleDateFormat.parse method is implemented on jvm side but not on js side! 
So testing on jvm won't expose it before client tries to call the method in vain  

           