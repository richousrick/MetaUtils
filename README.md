# MetaUtils
A collection of tools and utilities to help with metaprogramming and reflection.

## Main Features:

### Runtime javac invocation
A wrapper has been created around the java compiler that works entirely in the JVM. 

This works without I/O, by passing a set of Class names mapped to a string representing thier source code. 
Returning a classloader storing the loaded classes and a map of the loaded classes.

Notable Methods:
```Scala
// com.richousrick.metautils.loader.JavaClassLoader

// compiles the given map of (Class name -> Source code) into a map of (Class name -> class)
loadClasses(Map[String, String]): Map[String, Class[_]]

// same as loadClasses, however it also returns the classloader used to load the classes
// the additional parameter is to specify the parent of the returned classloader
loadClassesWithLoader(Map[String, String]<, ClassLoader>): (Map[String, Class[_]], ByteArrayClassLoader)
```



### Functions wrapping invocation and instantiation
A collection of functions that allow easy invocation and instantiation of code not present at compile-time, have been implemented. 

Notable Methods:
```Scala
// new C(params)
build[C](params: Any*): C

// instance.funcName(params)
run[R](instance: AnyRef, funcName: String, params: Any*): R

// Similar to run, however the function name can be preceeded by a list of parameter-less functions
// i.e. runChain(myVar, "f1.f2.f3", params) => myVar.f1().f2().f3(params)
runChain[R](instance: AnyRef, funcList: String, params: Any*): R
```
Note: If any of these methods cannot find the desired method / constructor, they will throw Exceptions. 
If this is not desirable, they all have versions that return Options instead. These can be called by appending 'Opt' to the desired methods name. e.g. runOpt 


All of these functions support two methods to resolve the return type. 
The first is by providing a class of the desired return type as the first parameter.
The Second is by explicitly stating the return type using the type parameter.
Additionally, all methods except build support resolution via context. i.e. ignore both of the above.

#### Example Usage:
```Scala
class SomeClass private(val num: Number) {

	def this(f: Float) = this(f)

	def this(n: Number) = this(n)
}

// type resolved to the type of the provided class
// val floatClass: SomeClass = new SomeClass(2.5f)
val floatClass = build(classOf[SomeClass], 2.5f)

// type explicitly stated
// val doubleNum: Double = new SomeClass(2.5d).num
val doubleNum = build[SomeClass](2.5d).num

// val d: Double = doubleNum.doubleValue
val d = run[Double](doubleNum, "doubleValue")

// type resolved via context
// assert(floatClass.num.doubleValue.equals(d))
assert(runChain(floatClass, "num.doubleValue.equals", d))
```

