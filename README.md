# MetaUtils
A collection of tools and utilities to help with metaprogramming and reflection.

## Main Features:

### Functions wrapping invocation and instantiation
A collection of functions that allow easy invocation and instantiation at runtime, have been implemented. The main two of which are build and run.

```Scala
// new C(params)
build[C](<class: Class[C],> params: Any*): C

// instance.funcName(params)
run[R](instance: AnyRef, funcName: String, params: Any*): R
```
Note: If either of these methods cannot find the desired method / constructor, they will throw Exceptions. 
If this is not desirable, both have versions that return Options instead. These can be called by appending 'Opt' to the desired methods name. e.g. runOpt 


#### Example Usage:
```Scala
class SomeClass private(val num: Number) {

	def this(f: Float) = this(f)

	def this(n: Number) = this(n)
}

// build classes
val floatClass = build(classOf[SomeClass], 2.5f)
val doubleClass = build[SomeClass](2.5d)

// d = doubleClass.num.doubleValue
val d: Double = run[Double](run(doubleClass,"num"), "doubleValue")

// floatClass.num.doubleValue.equals(d)
val a: Boolean = run(run(floatClass.num, "doubleValue"), "equals", d)

// equals = true
assert(a)
```

