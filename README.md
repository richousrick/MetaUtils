# MetaUtils
A collection of tools and utilities to help with metaprogramming and reflection.

## Main Features:

### Functions wrapping invocation and instantiation
A collection of functions that allow easy invocation and instantiation at runtime, have been implemented. The most notable being:

```Scala
// new C(params)
build[C](<class: Class[C],> params: Any*): C

// instance.funcName(params)
run[R](instance: AnyRef, funcName: String, params: Any*): R

// Similar to run, however the function name can be preceeded by a list of parameter-less functions
// i.e. runChain(myVar, "f1.f2.f3", params) => myVar.f1().f2().f3(params)
runChain[R](instance: AnyRef, funcList: String, params: Any*): R
```
Note: If either of these methods cannot find the desired method / constructor, they will throw Exceptions. 
If this is not desirable, both have versions that return Options instead. These can be called by appending 'Opt' to the desired methods name. e.g. runOpt 


#### Example Usage:
```Scala
class SomeClass private(val num: Number) {

	def this(f: Float) = this(f)

	def this(n: Number) = this(n)
}

// val floatClass: SomeClass = new SomeClass(2.5f)
val floatClass = build(classOf[SomeClass], 2.5f)

// val doubleNum: Double = new SomeClass(2.5d).num
val doubleNum = build[SomeClass](2.5d).num

// val d: Double = num.doubleValue
val d = run[Double](doubleNum, "doubleValue")

// assert(floatClass.num.doubleValue.equals(d))
assert(runChain(floatClass, "num.doubleValue.equals", d))
```

