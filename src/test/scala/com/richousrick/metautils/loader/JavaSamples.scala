/*
 * Copyright (c) 2021. Rikkey Paal rikkeypaal@gmail.com
 * This file is part of MetaUtils
 */

package com.richousrick.metautils.loader

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

object JavaSamples {

  /**
   * Source for a small java class
   */
  val smallClassSrc: Array[Byte] = Files.readAllBytes(Paths.get("src\\example\\src\\SmallClass.class"))

  /**
   * Result of compiling smallClassSrc with javac
   */
  val smallClassComp: Array[Byte] = Files.readAllBytes(Paths.get("src\\example\\compiled\\SmallClass.class"))

  /**
   * Source code for a small program with a couple of features desirable for testing.
   * Such as: cyclic references, nested classes and static variables
   */
  val exampleProgSrc: Map[String, Array[Byte]] =
    Files.list(Paths.get("src\\example\\src\\exampleprog")).toScala(Set).map(p =>
      "com.richousrick.exampleprog." + p.toFile.getName.dropRight(6) -> Files.readAllBytes(p)
    ).toMap

  /**
   * Result of compiling exampleProgSrc with javac
   */
  val exampleProgComp: Map[String, Array[Byte]] =
    Files.list(Paths.get("src\\example\\compiled\\exampleprog")).toScala(Set).map(p =>
      "com.richousrick.exampleprog." + p.toFile.getName.dropRight(6) -> Files.readAllBytes(p)
    ).toMap
}
