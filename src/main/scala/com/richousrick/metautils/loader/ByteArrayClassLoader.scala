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


  override protected def findClass(name: String): Class[_] =
      classDefs.remove(name) match {
        case Some(cd) =>
          defineClass(name, cd, 0, cd.length)
        case None => null
      }
}