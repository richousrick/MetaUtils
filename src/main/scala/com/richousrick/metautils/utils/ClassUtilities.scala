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
	 * @param c1               the first class to compare
	 * @param c2               the second class to compare
	 * @param fuzzyPrimitive   if primitives should be treated as equal to their wrapper classes.
	 *                       i.e. what should the result of Class[Integer] == Class[int] be?
	 * @param primitiveInherit if primitive classes should inherit their wrapper classes supertypes.
	 *                         If true primitive types will be considered subtypes of Serializable, Comparable
	 *                         and in the case of numeric primitives, Number.
	 * @return
	 * 1 if c2 <: c1
	 * 0 if c1 == c2
	 * -1 if c1 <: c2
	 * -2 otherwise
	 */
	/*
		TODO: figure out what to do with abstract supertypes always being assignable irrespective of the type parameter
		class SuperType[T]
		class SubType extends SuperType[Int]

		classOf[SuperType[SubType]].isAssignableFrom(classOf[SubType]) // == true, expect true
		classOf[SuperType[_]].isAssignableFrom(classOf[SubType])       // == true, expect false as scala defaults to invariance
		classOf[SuperType[Boolean]].isAssignableFrom(classOf[SubType]) // == true, expect false
	 */
	@scala.annotation.tailrec
	def compareClass(c1: Class[_], c2: Class[_], fuzzyPrimitive: Boolean = true, primitiveInherit: Boolean = true): Int =
	(fuzzyPrimitive, c1.isPrimitive, c2.isPrimitive) match {
		// fuzzy and first param is primitive, attempt to get second param as primitive and compare
		case (true, true, false) => compareClass(c1, convertToPrimitive(c2), false, primitiveInherit)
		// fuzzy and second param is primitive, attempt to get first param as primitive and compare
		case (true, false, true) => compareClass(convertToPrimitive(c1), c2, false, primitiveInherit)
		case _ => // if no need to convert param to primitive then compare c1 to c2
			if (c1 == c2)
				0
			else if (c1.isAssignableFrom(c2))
				1
			else if (c2.isAssignableFrom(c1))
				-1
			else if (!primitiveInherit)
			// if not equals or assignable it may be the case that a primitive number may be compared with an object
				-2
			else if (c1.isPrimitive && !c2.isPrimitive &&
				c2 == classOf[Serializable] || c2 == classOf[Comparable[_]] ||
				(c2 == classOf[Number] && numericPrimitives.contains(c1)))
			// if c1 is primitive and c2 is not check if c2 is an interface used by c1's wrapper class
				-1
			else if (c2.isPrimitive && !c1.isPrimitive &&
				c1 == classOf[Serializable] || c1 == classOf[Comparable[_]] ||
				(c1 == classOf[Number] && numericPrimitives.contains(c2)))
			// if c2 is primitive and c1 is not check if c1 is an interface used by c2's wrapper class
				1
			else
				-2
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