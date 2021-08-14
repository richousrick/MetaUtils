package com.richousrick.metautils.loader

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*
import scala.util.Try

class ByteArrayClassLoaderTest extends AnyFunSuite {

	val smallClass: Array[Byte] = Files.readAllBytes(Paths.get("src\\example\\compiled\\SmallClass.class"))

	val exampleProg: Map[String, Array[Byte]] =
		Files.list(Paths.get("src\\example\\compiled\\exampleprog")).toScala(Set).map(p =>
			"com.richousrick.exampleprog." + p.toFile.getName.dropRight(6) -> Files.readAllBytes(p)
		).toMap

	test("Classloader hierarchy is correct") {
		// Create two ByteArrayClassLoader instances a parent extending the current classloader and child extending parent
		val parent = new ByteArrayClassLoader(exampleProg, getClass.getClassLoader)
		val child = new ByteArrayClassLoader(Map("SmallClass" -> smallClass), parent)

		// assert the child can load classes defined in the parent wrapper
		assertSuccessNotNull {
			child.loadClass("com.richousrick.exampleprog.Core")
		}
		assertSuccessNotNull {
			parent.loadClass("com.richousrick.exampleprog.Core")
		}

		// assert the parent cannot load classes defined in its child wrapper
		assert(Try(parent.loadClass("SmallClass")).getOrElse(fail()) == null)
		assertSuccessNotNull {
			child.loadClass("SmallClass")
		}

		// assert both can load classes defined in the current classloader, as parent extends the current classloader
		assertSuccessNotNull {
			child.loadClass("com.richousrick.metautils.loader.ByteArrayClassLoaderTest")
		}
		assertSuccessNotNull {
			parent.loadClass("com.richousrick.metautils.loader.ByteArrayClassLoaderTest")
		}
	}

	test("Can get defined class multiple times") {
		val bal = new ByteArrayClassLoader(exampleProg, getClass.getClassLoader)
		val a = bal.loadClass("com.richousrick.exampleprog.Core")
		val b = bal.loadClass("com.richousrick.exampleprog.Core")
		assert(a == b)
		assert(a != null)
	}

	test("Can access subclasses") {
		val bal = new ByteArrayClassLoader(exampleProg, getClass.getClassLoader)
		assert(Try(bal.loadClass("com.richousrick.exampleprog.NestedClasses$InnerClass")).isSuccess)
	}

	/**
	 * Assert the given block does not throw an exception and its result is not null
	 *
	 * @param func function to try to run
	 * @tparam R result type of func
	 */
	private def assertSuccessNotNull[R](func: => R): Unit =
		assert(Try(func).getOrElse(fail()) != null)

}
