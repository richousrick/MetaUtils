package com.richousrick.metautils.utils

import java.lang

import com.richousrick.metautils.utils.ClassUtilities._
import org.scalatest.funsuite.AnyFunSuite

class ClassUtilitiesTest extends AnyFunSuite {

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

  test("Compare primitive incomparable with non primitive") {
    assert(compareClass(classOf[Integer], Integer.TYPE, fuzzyPrimitive = false) == -2)
    assert(compareClass(lang.Boolean.TYPE, classOf[lang.Boolean], fuzzyPrimitive = false) == -2)
  }

  test("Compare numberPatch works") {
    // enabled
    assert(compareClass(classOf[Number], Integer.TYPE, fuzzyPrimitive = false) == 1)
    assert(compareClass(Integer.TYPE, classOf[Number], fuzzyPrimitive = false) == -1)

    // disabled
    assert(compareClass(classOf[Number], Integer.TYPE, fuzzyPrimitive = false, primitiveInherit = false) == -2)
    assert(compareClass(Integer.TYPE, classOf[Number], fuzzyPrimitive = false, primitiveInherit = false) == -2)
  }

  test("Compare unbox works") {
    assert(compareClass(classOf[Integer], Integer.TYPE) == 0)
    assert(compareClass(lang.Boolean.TYPE, classOf[lang.Boolean]) == 0)
    assert(compareClass(lang.Boolean.TYPE, classOf[Boolean]) == 0)
  }

  test("Compare works with primitive and Serializable") {
    assert(compareClass(classOf[Serializable], Integer.TYPE, fuzzyPrimitive = false, primitiveInherit = true) == 1)
    assert(compareClass(classOf[Serializable], Integer.TYPE, fuzzyPrimitive = false, primitiveInherit = false) == -2)
  }

  test("Compare works with primitive and Comparable") {
    assert(compareClass(classOf[Comparable[Integer]],
      Integer.TYPE,
      fuzzyPrimitive = false,
      primitiveInherit = true) == 1)
    assert(compareClass(classOf[Comparable[Integer]],
      Integer.TYPE,
      fuzzyPrimitive = false,
      primitiveInherit = false) == -2)
  }
}