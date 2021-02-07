package com.richousrick.metautils.utils

import scala.util.Try

/**
 * Collection of utility functions to help working with functions
 */
object ClassUtilities {

  /**
   * Compares two classes to one another, taking into account primitive classes.
   * i.e. if only one of the two classes are primitive, it will attempt to convert the other to a primitive too.
   *
   * @param c1 the first class to compare
   * @param c2 the second class to compare
   * @return
   * 1 if c2 <: c1
   * 0 if c1 == c2
   * -1 if c1 <: c2
   * -2 otherwise
   */
  def compareClass(c1: Class[_], c2: Class[_]): Int =
    (c1.isPrimitive, c2.isPrimitive) match {
      case (true, false) => compareClassI(c1, convertToPrimitive(c2))
      case (false, true) => compareClassI(convertToPrimitive(c1), c2)
      case _ => compareClassI(c1, c2)
    }

  /**
   * Compares two classes to one another
   *
   * @param c1                the first class to compare
   * @param c2                the second class to compare
   * @param numPrimitivePatch what Class[Number].isAssignableFrom(Class[primitive number]) should return.
   * @return
   * 1 if c2 <: c1
   * 0 if c1 == c2
   * -1 if c1 <: c2
   * -2 otherwise
   */
  def compareClassI(c1: Class[_], c2: Class[_], numPrimitivePatch: Boolean = true): Int =
    if (c1 == c2)
      0
    else if (c1.isAssignableFrom(c2))
      1
    else if (c2.isAssignableFrom(c1))
      -1
    else (c1, c2) match { // Check that a primitive numeric type is not being compared to Number
      case _ if !numPrimitivePatch => -2
      case (c1, c2) if c1 == classOf[Number] && c2.isPrimitive && numericPrimitives.contains(c2) => 1
      case (c1, c2) if c2 == classOf[Number] && c1.isPrimitive && numericPrimitives.contains(c1) => -1
      case _ => -2
    }

  /**
   * Attempts to get the primitive class from a given class.
   * Note: this returns clazz if clazz is not a primitive wrapper class
   *
   * @param clazz class to attempt to convert to a primitive class
   * @return either the primitive version of clazz, or clazz if no primitive was found
   */
  def convertToPrimitive(clazz: Class[_]): Class[_] =
    if (clazz.getName.startsWith("java.lang"))
      Try(clazz.getField("TYPE")).toOption.map(_.get(clazz)) match {
        case Some(c: Class[_]) => c
        case _ => clazz
      } else clazz

  /**
   * Set of all numeric primitive classes.
   * Used in compare class, to fix problem comparing Number and primitives.
   * i.e. Number.isAssignableFrom(int) == false
   */
  private val numericPrimitives: Set[Class[_]] = Set(
    convertToPrimitive(classOf[Byte]),
    convertToPrimitive(classOf[Short]),
    convertToPrimitive(classOf[Int]),
    convertToPrimitive(classOf[Long]),
    convertToPrimitive(classOf[Float]),
    convertToPrimitive(classOf[Double]))

}