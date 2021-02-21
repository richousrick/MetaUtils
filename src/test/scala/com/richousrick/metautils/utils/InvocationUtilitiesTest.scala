package com.richousrick.metautils.utils

import java.awt.Point
import java.lang.reflect.Modifier

import com.richousrick.metautils.utils.InvocationUtilities._
import org.scalatest.funsuite.AnyFunSuite

/**
 * Tests for [[com.richousrick.metautils.utils.InvocationUtilities#findFunction findFunction]] methods
 */
class FindMethodSuite extends AnyFunSuite {
  // Use forName to mix usages between Java and scala class references
  val exampleClass: Class[_] = Class.forName("com.richousrick.metautils.utils.MetaUtilsTestExample")

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
}

/**
 * Tests for [[com.richousrick.metautils.utils.InvocationUtilities#build build]] methods
 */
class BuildSuite extends AnyFunSuite {
  test("Build new instance: TypeTag reference") {
    // call parameterless constructor, using generic type referencing
    val sc: SubClass = build[SubClass]()
    assert(sc != null)
    assert(sc.isInstanceOf[SubClass])
    assert(sc.isInstanceOf[SuperClass])


    // call object constructor that takes parameters, using generic type referencing
    val as: AnotherSubClass = build[AnotherSubClass](5)
    assert(as != null)
    assert(as.isInstanceOf[AnotherSubClass])
    assert(as.isInstanceOf[SuperClass])
    assert(as.ID == 5)

    // call non-existent constructor, using generic type referencing
    assertThrows[InstantiationException](build[AnotherSubClass]("hello"))
  }

  test("Build new instance: Class reference") {
    // call parameterless constructor, using class parameter referencing
    val sc = build(classOf[SubClass])
    assert(sc != null)
    assert(sc.isInstanceOf[SubClass])
    assert(sc.isInstanceOf[SuperClass])

    // call object constructor that takes parameters, using class parameter referencing
    val as = build(classOf[AnotherSubClass], 6)
    assert(as != null)
    assert(as.isInstanceOf[AnotherSubClass])
    assert(as.isInstanceOf[SuperClass])
    assert(as.ID == 6)

    // call non-existent constructor, using class parameter referencing
    assertThrows[InstantiationException](build(classOf[SubClass], "hello"))
  }
}

/**
 * Tests for [[com.richousrick.metautils.utils.InvocationUtilities#run run]] methods
 */
class RunSuite extends AnyFunSuite {
  test("Run method works irregardless of return type specification") {
    val mte = new MetaUtilsTestExample
    val sc = new SubClass

    // run function without return type
    assert(InvocationUtilities.run(mte, "isSubClass", sc))

    // run function with return type
    // generic
    assert(!InvocationUtilities.run[Boolean](mte, "isSubClass", new AnotherSubClass(1), 0))
    // class provided
    assert(!InvocationUtilities.run(classOf[Boolean], mte, "isSubClass", new AnotherSubClass(1), 0))
  }

  test("Run non existant method throws NoSuchMethodError") {
    val mte = new MetaUtilsTestExample
    // inferred type
    assertThrows[NoSuchMethodError](InvocationUtilities.run(mte, "NoSuchMethod"))
    // generic specified type
    assertThrows[NoSuchMethodError](InvocationUtilities.run[Boolean](mte, "NoSuchMethod"))
    // provided type
    assertThrows[NoSuchMethodError](InvocationUtilities.run(classOf[Boolean], mte, "NoSuchMethod"))
  }
}

/**
 * Tests for [[com.richousrick.metautils.utils.InvocationUtilities#runChain runChain]] methods
 */
class RunChainSuite extends AnyFunSuite {
  test("runChain works with multiple function calls and parameters") {
    assert(runChain(new SomeClass(-12), "num.intValue.getClass.getName.equals", "java.lang.Integer"))
    assert(!runChain[Boolean](new SomeClass(-12), "num.intValue.getClass.getName.equals", "java.lang.Intege"))
    assert(!runChain(classOf[Boolean], new SomeClass(-12), "num.intValue.getClass.getName.equals", "java.lang.Intege"))
  }

  test("runChain works with multiple parameterless function calls") {
    // type required as cannot be inferred
    assert(runChain[String](new SomeClass(-12), "num.intValue.getClass.getName") == "java.lang.Integer")
    assert(runChain(classOf[String], new SomeClass(-12), "num.intValue.getClass.getName") == "java.lang.Integer")
  }

  test("runChain works with a single function call") {
    assert(runChain[String](new SomeClass(-12), "kind") == "Int")
    assert(runChain(classOf[String], new SomeClass(-12), "kind") == "Int")
  }
}

/**
 * Tests for [[com.richousrick.metautils.utils.InvocationUtilities#get get]] methods
 */
class GetSuite extends AnyFunSuite {
  /*TODO: Expand get methods to support:
     Private variables, (including those inherited)
   */

  test("Can get Static fields using class instance") {
    assert(get[Class[_]](classOf[Integer], "TYPE") == Integer.TYPE)
  }

  test("Can get accessible fields") {
    val p = new Point(1, 2)
    assert(Modifier.isPublic(p.getClass.getDeclaredField("x").getModifiers))
    assert(Modifier.isPublic(p.getClass.getDeclaredField("y").getModifiers))
    assert(get[Int](p, "x") == 1)
    assert(get[Int](p, "y") == 2)
    assert(p.x == 1)
    assert(p.y == 2)
  }

  test("Cannot get private fields") {
    // field exists but is private
    assert(Modifier.isPrivate(classOf[SomeClass].getDeclaredField("kind").getModifiers))
    // cannot get it
    assert(getOpt[String](new SomeClass(1), "kind").isEmpty)
  }
}

/**
 * Tests that apply to multiple functions
 */
class GeneralSuite extends AnyFunSuite {
  test("Mix build and run functions") {
    assert(InvocationUtilities.run(build[MetaUtilsTestExample](), "isSubClass", build(classOf[SubClass])))
  }

  test("Primitive numeric types can be used for Number parameters") {
    assert(build[SomeClass](new Integer(1)).toString == "Int(1)")
    assert(build[SomeClass](1).toString == "Int(1)")
    assert(build[SomeClass](1f).toString == "Float(1.0)")
    assert(build[SomeClass](1d).toString == "Misc(1.0)")
    assertThrows[InstantiationException](build[SomeClass]("Hello"))
    assertThrows[InstantiationException](build[SomeClass]('?'))
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