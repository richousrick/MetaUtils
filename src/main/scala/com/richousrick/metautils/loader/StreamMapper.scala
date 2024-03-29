/*
 * Copyright (c) 2021. Rikkey Paal rikkeypaal@gmail.com
 * This file is part of MetaUtils
 */

package com.richousrick.metautils.loader

import java.io.ByteArrayOutputStream

/**
 * Class to generate byte streams and map their contents to keys.
 * The primary use of this is to help store and read the files generated by [[com.richousrick.metautils.loader.JavaClassLoader]]
 * Note: This is not thread safe.
 */
class StreamMapper {

  /** Generated Streams */
  private var streams = Map[String, ByteArrayOutputStream]()

  /** Boolean used to ensure no new streams are generated after they have started being read */
  private var locked: Boolean = false

  /**
   * Trys to generate a new stream mapped to the specified key.<br>
   * Overwriting is not supported. If a stream has already been generated using the provided key, then None will be returned.
   *
   * @param name key used to identify the stream
   * @return a new stream mapped to the specified name, if none exists
   */
  def newStream(name: String): Option[ByteArrayOutputStream] = if (locked || streams.contains(name)) None else {
    val newStream = new ByteArrayOutputStream()
    streams += name -> newStream
    Some(newStream)
  }

  /** Maps the keys used to identify the streams to the contents of said streams */
  lazy val files: Map[String, Array[Byte]] = {
    locked = true
    streams.map(e => e._1 -> e._2.toByteArray)
  }

}
