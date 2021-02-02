package richousrick.lib

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
}


class MetaUtilsTestExample {

	def isSubClass(superClass: SuperClass, targetID: Int): Boolean = superClass.ID == targetID

	def isSubClass(subClass: SubClass, targetID: Int): Boolean = targetID == 0 && subClass.secondCheck

	def isSubClass(subClass: SubClass): Boolean = subClass.secondCheck
}

abstract class SuperClass(val ID: Int)

class SubClass extends SuperClass(0) {
	val innerClass = new InnerClass

	def secondCheck: Boolean = innerClass.isValid

	class InnerClass {
		def isValid: Boolean = true
	}

}

class AnotherSubClass(ID: Int) extends SuperClass(ID)