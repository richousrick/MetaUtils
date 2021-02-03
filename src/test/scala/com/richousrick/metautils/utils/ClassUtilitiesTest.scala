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