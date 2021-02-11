package com.richousrick.metautils.loader

import scala.collection.mutable

/**
 * Classloader that can handle Byte arrays.
 *
 * @param parent    the parent classloader
 * @param classDefs defined classes, stored as a map of [class name -> class content].
 *                  Note the full classname must be provided. e.g. "java.lang.Integer".
 */
class ByteArrayClassLoader private(var classDefs: mutable.Map[String, Array[Byte]], parent: ClassLoader) extends ClassLoader(parent) {

  /**
   * Creates a class loader that can handle the given byte arrays.
   *
   * @param classDefs defined classes, stored as a map of [class name -> class content].
   *                  Note the full classname must be provided. e.g. "java.lang.Integer".
   * @param parent    teh parent classloader
   */
  def this(classDefs: Map[String, Array[Byte]], parent: ClassLoader) = this(mutable.Map(classDefs.toSeq: _*), parent)

  /**
   * Classes that have already been defined via findClass
   */
  private val definedClasses: mutable.Map[String, Class[_]] = mutable.Map()

  override protected def findClass(name: String): Class[_] =
    if (definedClasses.contains(name))
      definedClasses(name)
    else
      classDefs.remove(name) match {
        case Some(cd) =>
          definedClasses += name -> defineClass(name, cd, 0, cd.length)
          definedClasses(name)

        case None => null
      }
}