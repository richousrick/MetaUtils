package richousrick.lib

import java.lang

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import richousrick.lib.MetaUtils._

class MetaUtilsTest extends AnyFunSuite with BeforeAndAfterAll {

	// Use forName to mix usages between Java and scala class references
	val exampleClass: Class[_] = Class.forName("richousrick.lib.MetaUtilsTestExample")

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
	}

	test("Build, Get, Run method are callable") {
		val mte = new MetaUtilsTestExample
		val sc = new SubClass

		// run function without return type
		assert(MetaUtils.run(mte, "isSubClass", sc))


		// run function with return type
		assert(!MetaUtils.run[Boolean](mte, "isSubClass", new AnotherSubClass(1), 0))

		// run non existent function
		assertThrows[NoSuchMethodError](MetaUtils.run[Boolean](mte, "NoSuchMethod"))
		assertThrows[NoSuchMethodError](MetaUtils.run(mte, "NoSuchMethod"))
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

	test("Build new instance without class type specification cannot compile") {
		assertDoesNotCompile("val a:SubClass = build()")
		assertDoesNotCompile("val a:AnotherSubClass = build(6)")
	}

	test("Mix build and run functions") {
		assert(MetaUtils.run(build[MetaUtilsTestExample](), "isSubClass", build(classOf[SubClass])))
	}

	test("Primitive types are comparable with Number") {
		assert(build[SomeClass](1).toString == "Int(1)")
		assert(build[SomeClass](1f).toString == "Float(1.0)")
		assert(build[SomeClass](1d).toString == "Misc(1.0)")
		assertThrows[InstantiationException](build[SomeClass]("Hello"))
		assertThrows[InstantiationException](build[SomeClass]('?'))
	}

	test("Convert to primitive works on primitives") {
		assert(convertToPrimitive(classOf[Int]) == Integer.TYPE)
		assert(convertToPrimitive(classOf[Integer]) == Integer.TYPE)
		assert(convertToPrimitive(classOf[Byte]) == lang.Byte.TYPE)
		assert(convertToPrimitive(classOf[lang.Byte]) == lang.Byte.TYPE)
		assert(convertToPrimitive(classOf[Char]) == Character.TYPE)
		assert(convertToPrimitive(classOf[Character]) == Character.TYPE)
		assert(convertToPrimitive(classOf[Boolean]) == lang.Boolean.TYPE)
		assert(convertToPrimitive(classOf[lang.Boolean]) == lang.Boolean.TYPE)
	}

	test("Convert to primitive returns class if not primitive") {
		// test class without TYPE specified
		assert(convertToPrimitive(classOf[String]) == classOf[String])

		// test class with TYPE specified to a non class
		assert(convertToPrimitive(classOf[SuperClass]) == classOf[SuperClass])
		// test class with TYPE pointing to a primitive class
		assert(convertToPrimitive(classOf[AnotherSubClass]) == classOf[AnotherSubClass])
	}

	test("CompareI primitive incomparable with non primitive") {
		assert(compareClassI(classOf[Integer], Integer.TYPE) == -2)
		assert(compareClassI(lang.Boolean.TYPE, classOf[lang.Boolean]) == -2)
	}

	test("CompareI numberPatch works") {
		// enabled
		assert(compareClassI(classOf[Number], Integer.TYPE) == 1)
		assert(compareClassI(Integer.TYPE, classOf[Number]) == -1)

		// disabled
		assert(compareClassI(classOf[Number], Integer.TYPE, numPrimitivePatch = false) == -2)
		assert(compareClassI(Integer.TYPE, classOf[Number], numPrimitivePatch = false) == -2)
	}

	test("Compare unbox works") {
		assert(compareClass(classOf[Integer], Integer.TYPE) == 0)
		assert(compareClass(lang.Boolean.TYPE, classOf[lang.Boolean]) == 0)
		assert(compareClass(lang.Boolean.TYPE, classOf[Boolean]) == 0)
	}
}


class MetaUtilsTestExample {

	def isSubClass(superClass: SuperClass, targetID: Int): Boolean = superClass.ID == targetID

	def isSubClass(subClass: SubClass, targetID: Int): Boolean = targetID == 0 && subClass.secondCheck

	def isSubClass(subClass: SubClass): Boolean = subClass.secondCheck
}

abstract class SuperClass(val ID: Int)

class SubClass extends SuperClass(0) {
	val innerClass = new InnerClass
	val TYPE = 1

	def secondCheck: Boolean = innerClass.isValid

	class InnerClass {
		def isValid: Boolean = true
	}

}

class AnotherSubClass(ID: Int) extends SuperClass(ID) {
	val TYPE: Class[_] = Integer.TYPE
}


class SomeClass private(num: Number, kind: String) {

	def this(i: Int) = this(i, "Int")

	def this(f: Float) = this(f, "Float")

	def this(n: Number) = this(n, "Misc")

	override def toString = s"$kind($num)"
}