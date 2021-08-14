package com.richousrick.metautils.loader

import com.richousrick.metautils.loader.JavaClassLoader._
import com.richousrick.metautils.utils.InvocationUtilities
import com.richousrick.metautils.utils.InvocationUtilities._
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters._
import scala.util.Try

class JavaClassLoaderTest extends AnyFunSuite {

	val smallClass: String = new String(Files.readAllBytes(Paths.get("src\\example\\src\\SmallClass.java")))

	val exampleProg: Map[String, String] =
		Files.list(Paths.get("src\\example\\src\\com\\richousrick\\exampleprog")).toScala(Set).map(p =>
			"com.richousrick.exampleprog." + p.toFile.getName.dropRight(5) -> new String(Files.readAllBytes(p))
		).toMap

	test("loadClasses can load single class") {
		val res = loadClasses(Map("SmallClass" -> smallClass))
		assert(res.size == 1)
		val sc: Class[_] = res("SmallClass")
		val ins = build(sc)
		println(sc.getMethod("getNext"))
		assert(InvocationUtilities.run[Int](ins, "getNext") == 0)
		assert(InvocationUtilities.run[Int](ins, "getNext") == 1)
	}

	test("Loaded classes are invokable with InvocationUtilities") {
		// assert core class is not present in current classloader
		assert(Try(Class.forName("com.richousrick.exampleprog.Core").getSimpleName).isFailure)

		// load both example classes
		val lc = loadClasses(exampleProg)

		// get core class
		val c: Class[_] = lc("com.richousrick.exampleprog.Core")

		// build new instance of Core
		val cIns = build(c)
		assert(cIns != null)

		// assert Core.testNestedIDIncrement is true
		assert(InvocationUtilities.run[Boolean](cIns.asInstanceOf[AnyRef], "testNestedIDIncrement"))
	}

	test("loadClasses throws without input") {
		assertThrows[IllegalArgumentException](loadClasses(Map()))
	}

	test("compileClasses can load single class") {
		// compile small class and assert it results in a nonempty Array of Byte
		val res = compileClasses(Map("SmallClass" -> smallClass))
		assert(res.size == 1)
		assert(res.contains("SmallClass"))
		assert(res("SmallClass").nonEmpty)
	}


	test("compileClasses works on complex class Structure") {
		// tests compileClasses on two classes with examples of:
		// cyclic references, nested classes, package declarations

		// assert example prog compiles successfully and results in a
		val res = compileClasses(exampleProg)
		assert(res.size == 4)
		assert(res.keySet == Set("Core", "NestedClasses$NestedClass", "NestedClasses$InnerClass", "NestedClasses")
			.map("com.richousrick.exampleprog." + _))
		assert(res.forall(_._2.nonEmpty))
	}

	test("compileClasses throws when given empty input") {
		assertThrows[IllegalArgumentException](compileClasses(Map()))
	}

	test("JavaSourceFromString getOutputStream") {
		// getOutputStream from readOnly variant throws exception on write
		assertThrows[UnsupportedOperationException](new JavaSourceFromString("SomeClass",
			"class SomeClass{}").openOutputStream())

		// getOutputStream from write variant returns empty stream
		assert(new JavaSourceFromString("SomeClass", new StreamMapper()).openOutputStream().size() == 0)
	}

	test("JavaSourceFromString cannot open multiple output streams") {
		val jSrc = new JavaSourceFromString("SomeClass", new StreamMapper())
		assert(jSrc.openOutputStream().size() == 0)
		assertThrows[UnsupportedOperationException](jSrc.openOutputStream())
	}
}
