/*
 * Copyright (c) 2021. Rikkey Paal rikkeypaal@gmail.com
 * This file is part of MetaUtils
 */

package com.richousrick.metautils.utils

/**
 * Class with polymorphic methods
 */
class MetaUtilsTestExample {

  def isSubClass(superClass: SuperClass, targetID: Int): Boolean = superClass.ID == targetID

  def isSubClass(subClass: SubClass, targetID: Int): Boolean = targetID == 0 && subClass.secondCheck

  def isSubClass(subClass: SubClass): Boolean = subClass.secondCheck
}

/**
 * Class Used to test polymorphic classes with type shadowing
 */
abstract class SuperClass(val ID: Int)

/**
 * Class Used to test polymorphic classes with type shadowing
 */
class SubClass extends SuperClass(0) {
  val innerClass = new InnerClass
  val TYPE = 1

  def secondCheck: Boolean = innerClass.isValid

  class InnerClass {
    def isValid: Boolean = true
  }

}

/**
 * Class Used to test polymorphic classes with type shadowing
 */
class AnotherSubClass(ID: Int) extends SuperClass(ID) {
  val TYPE: Class[_] = Integer.TYPE
}