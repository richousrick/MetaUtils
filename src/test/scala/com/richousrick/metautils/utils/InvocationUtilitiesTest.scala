package com.richousrick.metautils.utils

import com.richousrick.metautils.utils.InvocationUtilities._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

class InvocationUtilitiesTest extends AnyFunSuite with BeforeAndAfterAll {
  // Use forName to mix usages between Java and scala class references
  val exampleClass: Class[_] = Class.forName("com.richousrick.metautils.utils.MetaUtilsTestExample")

  override def beforeAll(): Unit = {
    assert(exampleClass != null)
  }

  test("Find method returns Some for methods that exist") {
    val desiredMethod = exampleClass.getMethod("isSubClass", classOf[SuperClass], classOf[Int])
    assert(desiredMethod != null)

    // try to get the method using the correct types for parameters
    val foundMethod = findFunction(exampleClass,
      "isSubClass",
      classOf[Boolean],
      classOf[SuperClass],
      classOf[Int]).getOrElse(fail)
    assert(foundMethod == desiredMethod)

    // try to get the method using subtypes of the parameter
    val foundSubInstance = findFunction(exampleClass,
      "isSubClass",
      classOf[Boolean],
      classOf[AnotherSubClass],
      classOf[Int]).getOrElse(fail)
    assert(foundSubInstance == desiredMethod)
  }

  test("Find method returns None for methods that do not exist") {
    // try getting a method whoose name does not exist
    assert(findFunction(exampleClass, "NoSuchMethod", classOf[Boolean]).isEmpty)

    // try getting a method that has the same interface, except return type
    assert(findFunction(exampleClass, "isSubClass", classOf[String], classOf[SuperClass], classOf[Int]).isEmpty)

    // try getting a method that has the same interface except name
    assert(findFunction(exampleClass, "isSubClassNotReally", classOf[Boolean], classOf[SuperClass], classOf[Int])
      .isEmpty)

    // try getting a method that has the same interface except additional parameters
    assert(findFunction(exampleClass, "isSubClass", classOf[Boolean], classOf[SuperClass], classOf[Int], classOf[Int])
      .isEmpty)

    // try getting a method that has the same interface except different parameters
    assert(findFunction(exampleClass, "isSubClass", classOf[Boolean], classOf[Int], classOf[SuperClass]).isEmpty)

    // try getting a method that has the same interface except different parameters
    assert(findFunction(exampleClass, "isSubClass", classOf[Boolean], classOf[Int], classOf[SuperClass]).isEmpty)

    // try getting a method that has the same interface except missing parameters
    assert(findFunction(exampleClass, "isSubClass", classOf[Boolean], classOf[SuperClass]).isEmpty)
  }

  test("Find method can find methods in: the superclass") {
    findFunction(exampleClass, "toString", classOf[String]).getOrElse(fail)
  }

  test("Find method works with polymorphism: Different number of params") {
    val allParams = exampleClass.getMethod("isSubClass", classOf[SubClass], classOf[Int])
    assert(allParams != null)
    val lessParams = exampleClass.getMethod("isSubClass", classOf[SubClass])
    assert(lessParams != null)
    assert(allParams != lessParams)

    // get method using only subclass
    assert(findFunction(exampleClass, "isSubClass", classOf[Boolean], classOf[SubClass]).getOrElse(fail) == lessParams)

    // get method using subclass and int
    assert(findFunction(exampleClass,
      "isSubClass",
      classOf[Boolean],
      classOf[SubClass],
      classOf[Int]).getOrElse(fail) == allParams)
  }

  test("Find method works with polymorphism: Different types for params") {
    val subClass = exampleClass.getMethod("isSubClass", classOf[SubClass], classOf[Int])
    assert(subClass != null)
    val superClass = exampleClass.getMethod("isSubClass", classOf[SuperClass], classOf[Int])
    assert(superClass != null)
    assert(subClass != superClass)

    // get method using subclass
    assert(findFunction(exampleClass,
      "isSubClass",
      classOf[Boolean],
      classOf[SubClass],
      classOf[Int]).getOrElse(fail) == subClass)

    // get method using superclass
    assert(findFunction(exampleClass,
      "isSubClass",
      classOf[Boolean],
      classOf[SuperClass],
      classOf[Int]).getOrElse(fail) == superClass)

    // get method using wrong type fails
    assert(findFunction(exampleClass,
      "isSubClass",
      classOf[Boolean],
      classOf[AnotherSubClass]).isEmpty)
  }

  test("Build, Get, Run method are callable") {
    val mte = new MetaUtilsTestExample
    val sc = new SubClass

    // run function without return type
    assert(InvocationUtilities.run(mte, "isSubClass", sc))


    // run function with return type
    assert(!InvocationUtilities.run[Boolean](mte, "isSubClass", new AnotherSubClass(1), 0))

    // run non existent function
    assertThrows[NoSuchMethodError](InvocationUtilities.run[Boolean](mte, "NoSuchMethod"))
    assertThrows[NoSuchMethodError](InvocationUtilities.run(mte, "NoSuchMethod"))
  }

  test("Build new instance with generic type specification") {
    // build parameterless with generic type specification
    val sc: SubClass = build[SubClass]()
    assert(sc != null)
    assert(sc.isInstanceOf[SubClass])
    assert(sc.isInstanceOf[SuperClass])


    // build with parameters with generic type specification
    val as: AnotherSubClass = build[AnotherSubClass](5)
    assert(as != null)
    assert(as.isInstanceOf[AnotherSubClass])
    assert(as.isInstanceOf[SuperClass])
    assert(as.ID == 5)

    assertThrows[InstantiationException](build[AnotherSubClass]("hello"))
  }

  test("Build new instance with class type specification") {
    // build parameterless with class instance parameter
    val sc = build(classOf[SubClass])
    assert(sc != null)
    assert(sc.isInstanceOf[SubClass])
    assert(sc.isInstanceOf[SuperClass])

    // build with parameters with class instance parameter
    val as = build(classOf[AnotherSubClass], 6)
    assert(as != null)
    assert(as.isInstanceOf[AnotherSubClass])
    assert(as.isInstanceOf[SuperClass])
    assert(as.ID == 6)

    assertThrows[InstantiationException](build(classOf[SubClass], "hello"))
  }

  test("Mix build and run functions") {
    assert(InvocationUtilities.run(build[MetaUtilsTestExample](), "isSubClass", build(classOf[SubClass])))
  }

  test("Primitive types are comparable with Number") {
    assert(build[SomeClass](1).toString == "Int(1)")
    assert(build[SomeClass](1f).toString == "Float(1.0)")
    assert(build[SomeClass](1d).toString == "Misc(1.0)")
    assertThrows[InstantiationException](build[SomeClass]("Hello"))
    assertThrows[InstantiationException](build[SomeClass]('?'))
  }

  test("runChain works with multiple function calls and parameters") {
    assert(runChain(new SomeClass(-12), "num.intValue.getClass.getName.equals", "java.lang.Integer"))
    assert(!runChain[Boolean](new SomeClass(-12), "num.intValue.getClass.getName.equals", "java.lang.Intege"))
  }

  test("runChain works with multiple parameterless function calls") {
    // type required as cannot be inferred
    assert(runChain[String](new SomeClass(-12), "num.intValue.getClass.getName") == "java.lang.Integer")
  }

  test("runChain works with a single function call") {
    assert(runChain[String](new SomeClass(-12), "kind") == "Int")
  }
}

/**
 * Class with multiple constructors used to test
 * [[com.richousrick.metautils.utils.InvocationUtilities#build(scala.collection.immutable.Seq, scala.reflect.ClassTag) build]]
 * on types with polymorphic constructors
 */
class SomeClass private(val num: Number, val kind: String) {

  def this(i: Int) = this(i, "Int")

  def this(f: Float) = this(f, "Float")

  def this(n: Number) = this(n, "Misc")

  override def toString = s"$kind($num)"
}