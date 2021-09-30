/*
 * Copyright (c) 2021. Rikkey Paal rikkeypaal@gmail.com
 * This file is part of MetaUtils
 */

package com.richousrick.metautils.loader

import org.scalatest.funsuite.AnyFunSuite

class StreamMapperTest extends AnyFunSuite {

  test("Can create streams for unique keys") {
    val mapper = new StreamMapper
    assert(mapper.newStream("Key A").isDefined)
    assert(mapper.newStream("Key B").isDefined)
    assert(mapper.newStream("Key C").isDefined)
  }

  test("Cannot create streams for duplicate keys") {
    val mapper = new StreamMapper
    val a = mapper.newStream("Key A").getOrElse(fail())

    val aByte = "Hello".getBytes("UTF-8")
    a.write(aByte)
    assert(a.toByteArray sameElements aByte)
    assert(mapper.newStream("Key A").isEmpty)
    assert(a.toByteArray sameElements aByte)

    assert(new StreamMapper().newStream("Key A").isDefined)
    assert(a.toByteArray sameElements aByte)
  }

  test("Can read data written to streams") {
    val mapper = new StreamMapper
    val a = mapper.newStream("Stream 1").getOrElse(fail())
    val b = mapper.newStream("Stream 2").getOrElse(fail())

    val example1 = "Hello this is an example set of text".getBytes("UTF-8")
    val example2 = "This is a different set of text".getBytes("UTF-8")

    a.write(example1)
    b.write(example2)

    assert(mapper.files.getOrElse("Stream 1", fail()) sameElements example1)

    assert(mapper.files.getOrElse("Stream 2", fail()) sameElements example2)

    assert(!(mapper.files.getOrElse("Stream 1", fail()) sameElements mapper.files.getOrElse("Stream 2", fail())))


  }

  test("Cannot generate new streams after reading") {
    val mapper = new StreamMapper
    mapper.newStream("Stream 1").getOrElse(fail())
    mapper.newStream("Stream 2").getOrElse(fail())

    val mapper2 = new StreamMapper
    mapper2.newStream("Stream 1").getOrElse(fail())
    mapper2.files
    assert(mapper2.newStream("Stream2").isEmpty)
  }
}
