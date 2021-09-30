/*
 * Copyright (c) 2021. Rikkey Paal rikkeypaal@gmail.com
 * This file is part of MetaUtils
 */

package com.richousrick.metautils.loader

import java.io.{ByteArrayOutputStream, PrintWriter}
import java.net.URI
import javax.tools.*
import javax.tools.JavaFileObject.Kind
import scala.jdk.CollectionConverters.*

/**
 * Files to enable compiling and loading Java files from strings.
 *
 * Implemented as cyclic references break classloader
 */
object JavaClassLoader {

  /**
   * Compiles and loads the classes.
   *
   * @param classes          Map of class name -> source code
   * @param superClassloader classloader that the generated classloader should inherit from.
   * @return Classes mapped to their classes, as well as the classloader.
   */
  def loadClassesWithLoader(classes: Map[String, String], superClassloader: ClassLoader = getClass.getClassLoader): (Map[String, Class[_]], ByteArrayClassLoader) = {
    val files: Map[String, Array[Byte]] = compileClasses(classes)
    val cl = new ByteArrayClassLoader(files, superClassloader)
    (files.keySet.map(e => e -> cl.loadClass(e)).toMap, cl)
  }

  /**
   * Compiles and loads the classes.
   *
   * @param classes Map of class name -> source code
   * @return className -> compiled class
   */
  def loadClasses(classes: Map[String, String]): Map[String, Class[_]] =
    loadClassesWithLoader(classes)._1

  /**
   * Compiles a set of strings storing classes to a set of byte arrays.
   * This works with full files including package declarations.
   * However, each file must contain exactly one top level declaration
   *
   * @param classes map of class Names -> class content
   * @return the compiled classes stored as byte arrays. Class Names -> Compiled Source
   * @throws IllegalArgumentException if no source files are provided
   */
  def compileClasses(classes: Map[String, String], verbose: Boolean = false): Map[String, Array[Byte]] = {
    if (classes.isEmpty)
      throw new IllegalArgumentException("No files provided")

    // Generate new mapper to store the generated .class files
    val mapper = new StreamMapper

    // Ensure the system has the java compiler installed
    val comp = ToolProvider.getSystemJavaCompiler match {
      case null => throw new Exception("Cannot find compiler")
      case c => c
    }

    // Invoke the compiler
    val success = comp.getTask(
      // Output debug info to System.out
      new PrintWriter(System.out),
      // Create custom file manager to capture compiled files instead of writing them to the file system
      new ForwardingJavaFileManager[StandardJavaFileManager](
        comp.getStandardFileManager(null, null, null)) {
        override def getJavaFileForOutput(location: JavaFileManager.Location,
                                          className: String,
                                          kind: Kind,
                                          sibling: FileObject): JavaFileObject = {
          if (verbose) println(s"Loaded Class $className")
          new JavaSourceFromString(className, mapper)
        }
      }, null,

      // The output should be captured, just in case it is not, output to project root.
      Iterable("-d", System.getProperty("user.dir")).asJava, null,

      // Generate sources for each java file, to mock system files
      classes.map(e => new JavaSourceFromString(e._1, e._2)).asJava).call()

    if (verbose) println("Success: " + success)

    // return the compiled (but not loaded) classes
    mapper.files
  }

  /**
   * A file object used to represent source coming from a string.
   *
   * @param name         the name of the compilation unit represented by this file object
   * @param code         the source code for the compilation unit represented by this file object
   * @param streamMapper the stream mapper that should be used to generate the stream this will write to.
   */
  private[loader] class JavaSourceFromString private(val name: String, val code: String, val streamMapper: Option[StreamMapper])
    extends SimpleJavaFileObject(URI.create("string:///" + name.replace('.', '/') + Kind.SOURCE.extension),
      Kind.SOURCE) {

    /**
     * Create a Java source to be used for writing to a stream
     *
     * @param name   of the file to write to the stream
     * @param mapper to be used to write the source to
     */
    def this(name: String, mapper: StreamMapper) = this(name, "", Some(mapper))

    /**
     * Create a Java source to be used for reading from.
     *
     * @param name name of the file
     * @param code content of the file
     */
    def this(name: String, code: String) = this(name, code, None)

    override def getCharContent(ignoreEncodingErrors: Boolean): CharSequence = code

    /**
     * Trys to get a new stream from the classes StreamMapper instance, if one exists.
     * Otherwise an UnsupportedOperationException is thrown
     *
     * @return the stream, if one was generated
     */
    override def openOutputStream(): ByteArrayOutputStream = streamMapper match {
      case None => throw new UnsupportedOperationException("openOutputStream is not supported unless a streammapper is supplied")
      case Some(mapper) => mapper.newStream(name) match {
        case Some(stream) => stream
        case None => throw new UnsupportedOperationException("Overwriting files in the same mapper is unsupported")
      }
    }
  }

}