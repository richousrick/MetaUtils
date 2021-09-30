package com.richousrick.metautils.utils

import com.richousrick.metautils.utils.ClassUtilities.compareClass

import java.lang.reflect.{Constructor, Executable, Method}
import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag
import scala.util.Try

/**
 * A collection of utilities focusing around invoking methods and constructors of types not known at compile time.
 */
object InvocationUtilities {

  /**
   * Attempts to get a public field from a given instance object.
   *
   * @param instance object to get the field of.
   *                 If referencing a static field the class may be supplied instead of an instance.
   * @param name     name of the desired field
   * @param rt       type tag representing the desired return type
   * @tparam R expected datatype of the field
   * @throws NoSuchFieldError if the desired field does not exsist or is not publicly visible
   * @return the value of the field in the given instance if it is publicly accessible
   */
  def get[R](instance: Any, name: String)(implicit rt: ClassTag[R]): R =
    getOpt[R](instance, name)(rt) match {
      case Some(r) => r
      case None => throw new NoSuchFieldError()
    }

  /**
   * Attempts to get a public field from a given instance object.
   *
   * @param clazz    class referencing the return type R
   * @param instance object to get the field of.
   *                 If referencing a static field the class may be supplied instead of an instance.
   * @param name     name of the desired field
   * @tparam R expected datatype of the field
   * @throws NoSuchFieldError if the desired field does not exsist or is not publicly visible
   * @return the value of the field in the given instance if it is publicly accessible
   */
  def get[R](clazz: Class[R], instance: Any, name: String): R =
    getOpt[R](clazz, instance, name) match {
      case Some(r) => r
      case None => throw new NoSuchFieldError()
    }

  /**
   * Attempts to get a public field from a given instance object.
   *
   * @param instance object to get the field of.
   *                 If referencing a static field the class may be supplied instead of an instance.
   * @param name     name of the desired field
   * @param rt       type tag representing the desired return type
   * @tparam R expected datatype of the field
   * @return the value of the field in the given instance if it is publicly accessible, None otherwise
   */
  def getOpt[R](instance: Any, name: String)(implicit rt: ClassTag[R]): Option[R] =
    Try((instance match {
      case value: Class[_] => value
      case _ => instance.getClass
    }).getField(name)).toOption.map(_.get(instance).asInstanceOf[R])

  /**
   * Attempts to get a public field from a given instance object.
   *
   * @param clazz    class referencing the return type R
   * @param instance object to get the field of.
   *                 If referencing a static field the class may be supplied instead of an instance.
   * @param name     name of the desired field
   * @tparam R expected datatype of the field
   * @return the value of the field in the given instance if it is publicly accessible, None otherwise
   */
  def getOpt[R](clazz: Class[R], instance: Any, name: String): Option[R] =
    Try((instance match {
      case value: Class[_] => value
      case _ => instance.getClass
    }).getField(name)).toOption.map(_.get(instance).asInstanceOf[R])

  /**
   * Attempts to call 'new C(params)', and get the created object.<br>
   * Note: This function requires the type parameter to be specified. See
   * [[com.richousrick.metautils.utils.InvocationUtilities#build(java.lang.Class, scala.collection.immutable.Seq) build]]
   * for a version that does not require the type parameters be specified.
   *
   * @param params parameters for the constructor
   * @param rt     class tag for type C
   * @tparam C type of object to instantiate with the constructor
   * @throws InstantiationError if the desired constructor cannot be found
   * @return the created object
   */
  def build[C](params: Any*)(implicit rt: ClassTag[C]): C =
    buildOpt[C](params: _*)(rt) match {
      case Some(c) => c
      case None => throw new InstantiationException("Constructor not found")
    }

  /**
   * Attempts to call 'new clazz(params)', and get the created object.
   *
   * @param clazz  class to be instantiated
   * @param params parameters for the constructor
   * @tparam C type of object to instantiate with the constructor
   * @throws InstantiationError if the desired constructor cannot be found
   * @return the created object
   */
  def build[C](clazz: Class[C], params: Any*): C =
    buildOpt[C](clazz, params: _*) match {
      case Some(c) => c
      case None => throw new InstantiationException("Constructor not found")
    }

  /**
   * Attempts to call 'new C(params)', and get the created object.<br>
   * Note: This function requires the type parameter to be specified. See
   * [[com.richousrick.metautils.utils.InvocationUtilities#buildOpt(scala.collection.immutable.Seq, richousrick.lib.NotNothing, scala.reflect.ClassTag) buildOpt]]
   * for a version that does not require the type parameters be specified.
   *
   * @param params parameters used by the constructor
   * @param rt     class tag for type C
   * @tparam C type of object to instantiate with the constructor
   * @return the created object if successful, None otherwise.
   */
  def buildOpt[C](params: Any*)(implicit rt: ClassTag[C]): Option[C] =
  // As type specification must be present, the class can be extracted from it.
  // This is not the case with other functions that allow their types to be inferred
    buildOpt[C](rt.runtimeClass.asInstanceOf[Class[C]], params: _*)

  /**
   * Attempts to call 'new clazz(params)', and get the created object.<br>
   *
   * @param clazz  class to attempt to instantiate
   * @param params parameters used by the constructor
   * @tparam C type of object to instantiate with the constructor
   * @return the created object if successful, None otherwise.
   */
  def buildOpt[C](clazz: Class[C], params: Any*): Option[C] =
    findConstructor(clazz, toTypes(params): _*).map(_.newInstance(params: _*))

  /**
   * Attempts to find the desired constructor
   *
   * @param clazz  object the desired constructor is for
   * @param params parameters for the constructor
   * @tparam C type of the desired object
   * @return the desired constructor if one was found
   */
  //TODO: Search companion apply
  def findConstructor[C](clazz: Class[C], params: Class[_]*): Option[Constructor[C]] =
    filterExecutables[Constructor[C]](clazz.getConstructors.map(_.asInstanceOf[Constructor[C]]), params: _*)()


  /**
   * Attempts to run the specified method.
   *
   * @param clazz    the type to return
   * @param instance , Object to call the desired method on
   * @param funcName , name of the function to call
   * @param params   , parameters of the desired function
   * @tparam R , return type
   * @throws NoSuchMethodError , if the desired method cannot be found.
   * @return the result of calling the desired method
   * @see [[com.richousrick.metautils.utils.InvocationUtilities#runOption]]
   */
  def run[R](clazz: Class[R], instance: Any, funcName: String, params: Any*): R =
    runOpt[R](clazz, instance, funcName, params: _*) match {
      case Some(r) => r
      case None => throw new NoSuchMethodError()
    }

  /**
   * Attempts to run the specified method.<br>
   * Note: Generic mode will be enabled if the type is not specified
   * i.e.
   * {{{	runFunc[returnType](...)
   * 	or
   * 	runFunc(...)(implicitly[ClassTag[returnType]])}}}
   * This means return type will not be used in filtering methods, and may lead to ClassCastExceptions on use
   *
   * @param instance , Object to call the desired method on
   * @param funcName , name of the function to call
   * @param params   , parameters of the desired function
   * @param rt       , ClassTag for type R
   * @tparam R , return type
   * @throws NoSuchMethodError , if the desired method cannot be found.
   * @return the result of calling the desired method
   * @see [[com.richousrick.metautils.utils.InvocationUtilities#runOption]]
   */
  def run[R](instance: Any, funcName: String, params: Any*)(implicit rt: ClassTag[R]): R =
    runOpt[R](instance, funcName, params: _*)(rt) match {
      case Some(r) => r
      case None => throw new NoSuchMethodError()
    }

  /**
   * Attempts to run the specified method.
   *
   * @param clazz    the type to return
   * @param instance , Object to call the desired method on
   * @param funcName , name of the function to call
   * @param params   , parameters of the desired function
   * @tparam R , return type
   * @return Some(instance.funcName(params)), if such a function exists. None otherwise
   */
  def runOpt[R](clazz: Class[R], instance: Any, funcName: String, params: Any*): Option[R] =
    getGenericFunction(instance,
      funcName,
      clazz,
      toTypes(params): _*)(allowGenericReturns = false).map(_ (params))

  /**
   * Attempts to run the specified method.<br>
   * Note: Generic mode will be enabled if the type is not specified
   * i.e.
   * {{{	runFunc[returnType](...)
   * 	or
   * 	runFunc(...)(implicitly[ClassTag[returnType]])}}}
   * This means return type will not be used in filtering methods, and may lead to ClassCastExceptions on use
   *
   * @param instance , Object to call the desired method on
   * @param funcName , name of the function to call
   * @param params   , parameters of the desired function
   * @param rt       , ClassTag for type R
   * @tparam R , return type
   * @return Some(instance.funcName(params)), if such a function exists. None otherwise
   */
  def runOpt[R](instance: Any, funcName: String, params: Any*)(implicit rt: ClassTag[R]): Option[R] =
    getGenericFunction(instance,
      funcName,
      rt.runtimeClass.asInstanceOf[Class[R]],
      toTypes(params): _*)(allowGenericReturns = true).map(_ (params))


  /**
   * Attempts run a collection of functions on an object.<br>
   * i.e. runChainOpt(instance, "f1.f2.f3", param1, param2) => instance.f1().f2().f3(param1, param2)
   *
   * @param clazz    the return type
   * @param instance object to start running the functions on
   * @param funcList list of parameterless functions to run in series, separated by full stops
   * @param params   parameters for the last function, if any.
   * @tparam R return type
   * @throws NoSuchMethodError if any of the desired methods cannot be found.
   * @return Some(instance.funcList(params)), if the functions in funcList exist and are applicable to the previous's return type.
   *         None otherwise
   */
  def runChain[R](clazz: Class[R], instance: Any, funcList: String, params: Any*): R =
    runChainOpt[R](clazz, instance, funcList, params: _*) match {
      case Some(r) => r
      case None => throw new NoSuchMethodError()
    }

  /**
   * Attempts run a collection of functions on an object.<br>
   * i.e. runChainOpt(instance, "f1.f2.f3", param1, param2) => instance.f1().f2().f3(param1, param2)
   *
   * @param instance object to start running the functions on
   * @param funcList list of parameterless functions to run in series, separated by full stops
   * @param params   parameters for the last function, if any.
   * @param rt       ClassTag for type R
   * @tparam R return type
   * @throws NoSuchMethodError if any of the desired methods cannot be found.
   * @return Some(instance.funcList(params)), if the functions in funcList exist and are applicable to the previous's return type.
   *         None otherwise
   */
  def runChain[R](instance: Any, funcList: String, params: Any*)(implicit rt: ClassTag[R]): R =
    runChainOpt[R](instance, funcList, params: _*)(rt) match {
      case Some(r) => r
      case None => throw new NoSuchMethodError()
    }

  /**
   * Attempts run a collection of functions on an object.<br>
   * i.e. runChainOpt(instance, "f1.f2.f3", param1, param2) => instance.f1().f2().f3(param1, param2)
   *
   * @param clazz    return type of the function
   * @param instance object to start running the functions on
   * @param funcList list of parameterless functions to run in series, separated by full stops
   * @param params   parameters for the last function, if any.
   * @tparam R return type
   * @return Some(instance.funcList(params)), if the functions in funcList exist and are applicable to the previous's return type.
   *         None otherwise
   */
  def runChainOpt[R](clazz: Class[R], instance: Any, funcList: String, params: Any*): Option[R] =
    params match {
      // if no params are supplied then all functions are parameterless, so analogous to internal chainRun.
      case Seq() => chainRun[R](instance, funcList.split('.'): _*)(ClassTag(clazz))
      case _ =>
        // if params exist then the last function is not parameterless, so call chainRun using all but the last function call.
        val funcs = funcList.split('.')
        chainRun[Any](instance, funcs.dropRight(1): _*) match {
          case Some(r) =>
            // Attempt to call runOpt on the result calling the last specified method using the parameters provided
            runOpt[R](clazz, r, funcs.last, params: _*)
          case None => None
        }
    }

  /**
   * Attempts run a collection of functions on an object.<br>
   * i.e. runChainOpt(instance, "f1.f2.f3", param1, param2) => instance.f1().f2().f3(param1, param2)
   *
   * @param instance object to start running the functions on
   * @param funcList list of parameterless functions to run in series, separated by full stops
   * @param params   parameters for the last function, if any.
   * @param rt       ClassTag for type R
   * @tparam R return type
   * @return Some(instance.funcList(params)), if the functions in funcList exist and are applicable to the previous's return type.
   *         None otherwise
   */
  def runChainOpt[R](instance: Any, funcList: String, params: Any*)(implicit rt: ClassTag[R]): Option[R] =
    params match {
      // if no params are supplied then all functions are parameterless, so analogous to internal chainRun.
      case Seq() => chainRun[R](instance, funcList.split('.'): _*)(rt)
      case _ =>
        // if params exist then the last function is not parameterless, so call chainRun using all but the last function call.
        val funcs = funcList.split('.')
        chainRun[Any](instance, funcs.dropRight(1): _*) match {
          case Some(r) =>
            // Attempt to call runOpt on the result calling the last specified method using the parameters provided
            runOpt[R](r, funcs.last, params: _*)
          case None => None
        }
    }

  /**
   * Internal method,<br>
   * Attempts to run a series of functions on an object. <br>
   * i.e. chainRun(instance, "f1", "f2", "f3") => instance.f1().f2().f3()
   *
   * @param instance object to start running the functions on
   * @param funcs    list of functions to run in order
   * @param rt       type tag for return type
   * @tparam R type of the desired return type
   * @return the result of running the collection of functions if possible
   * @see [[com.richousrick.metautils.utils.InvocationUtilities#runChainOpt(java.lang.Object, java.lang.String, scala.collection.immutable.Seq, scala.reflect.ClassTag) runChainOpt]]
   *      the publicly visible version of this function
   */
  private def chainRun[R](instance: Any, funcs: String*)(implicit rt: ClassTag[R]): Option[R] =
    Some(funcs.foldLeft[Any](instance) { (currRet: Any, func: String) =>
      // try to find and run the desired function on the current object.
      // If said function was ran succefully then update the current object with its return.
      // If the function could not be found then return None.
      runOpt[Any](currRet, func) match {
        case Some(v) => v
        case None => return None
      }
      // If the function list reduced to an object, then all function calls were successful, so return the result.
    }.asInstanceOf[R])


  /**
   * Attempts to create a function literal representing calling the desired function the the specified instance
   *
   * @param instance            the object the desired function should be called on
   * @param funcName            name of the desired function
   * @param returnType          return type of the desired function
   * @param params              arguments the desired function takes
   * @param allowGenericReturns if return type should be ignored when searching for a function.
   * @tparam C type of the object the function is called on
   * @tparam R type of data the function should return
   * @tparam P base type of data the parameters take.
   * @return a function literal representing the desired function, if one was found
   */
  def getGenericFunction[C, R, P](instance: C, funcName: String, returnType: Class[R], params: Class[P]*)
                                 (allowGenericReturns: Boolean): Option[Seq[P] => R] =
    buildGenericFunction[C, R, P](instance.getClass.asInstanceOf[Class[C]], funcName, returnType, params: _*)(
      allowGenericReturns).map(f =>
      (p: Seq[P]) => f(instance, p)
    )

  /**
   * Attempts to create a function literal that will allow the desired function to be invoked
   *
   * @param clazz               instance the method is called on
   * @param funcName            name of the desired function
   * @param returnType          return type of the desired function
   * @param params              arguments the desired function takes
   * @param allowGenericReturns , if true then return type will not be checked during search.
   *                            This is necessary as calling runFunction() without implicitly specifying type parameters will lead to returnType being an instance of Class[Nothing]
   * @tparam C type of the object the function is called on
   * @tparam R type of data the function should return
   * @tparam P base type of data the parameters take.
   * @return a function literal that allows the desired function to be invoked, if such a function was found.
   */
  //TODO: Improve handling of parameters to ensure the interface persists.
  // i.e. make the resulting function require a strongly typed tuple instead of a weakly typed seq
  def buildGenericFunction[C, R, P](clazz: Class[C], funcName: String, returnType: Class[R], params: Class[P]*)
                                   (allowGenericReturns: Boolean): Option[(C, Seq[P]) => R] =
    findGenericFunction(clazz,
      funcName,
      returnType,
      params: _*)(allowGenericReturns).map(m => (instance: C, args: Seq[P]) => m
      .invoke(instance, args: _*)
      .asInstanceOf[R])


  /**
   * Finds an instance of Method that works with the given parameters
   *
   * @param clazz      instance the method is called on
   * @param funcName   name of the desired function
   * @param returnType return type of the desired function
   * @param params     arguments the desired function takes
   * @tparam C type of the object the function is called on
   * @tparam R type of data the function should return
   * @return the desired function, if found
   */
  def findFunction[C, R](clazz: Class[C],
                         funcName: String,
                         returnType: Class[R],
                         params: Class[_]*): Option[Method] =
    findGenericFunction(clazz, funcName, returnType, params: _*)(allowGenericReturns = false)

  /**
   * Finds an instance of Method that works with the given parameters
   *
   * @param clazz               instance the method is called on
   * @param funcName            name of the desired function
   * @param returnType          return type of the desired function
   * @param params              arguments the desired function takes
   * @param allowGenericReturns , if true then return type will not be checked during search.
   *                            This is necessary as calling runFunction() without implicitly specifying type parameters will lead to returnType being an instance of Class[Nothing]
   * @tparam C type of the object the function is called on
   * @tparam R type of data the function should return
   * @return the desired function, if found
   */
  def findGenericFunction[C, R](clazz: Class[C], funcName: String, returnType: Class[R], params: Class[_]*)
                               (allowGenericReturns: Boolean): Option[Method] =
  // call search functions on the classes methods, also filtering by their name and returns
    filterExecutables(clazz.getMethods, params: _*)(Seq[Method => Boolean](_.getName == funcName) ++ (
      if (!allowGenericReturns)
        Some[Method => Boolean](f => compareClass(f.getReturnType, returnType) >= 0)
      else None))


  /**
   * Searches an array of executables for those that match the desired interface
   *
   * @param executables list of executables to filter
   * @param params      parameters of the desired function
   * @param miscFilters additional filters to run on the executables before testing the parameters match
   * @tparam C type of executable the function is searching
   * @return the executable that best matches the desired parameters and additional filters.
   *         Or None if no given executable both matches the given filters, and is callable with the given parameters.
   */
  def filterExecutables[C <: Executable](executables: Array[C], params: Class[_]*)
                                        (miscFilters: Seq[C => Boolean] = Seq()): Option[C] = {
    val filter: ListBuffer[C => Boolean] = miscFilters ++: ListBuffer[Executable => Boolean](
      // same num of params
      _.getParameterCount == params.size,
      // params types match
      f =>
        f.getParameterTypes.zip(params).forall {
          case (c1: Class[_], c2: Class[_]) => compareClass(c1, c2) >= 0
          case (_, null) => true // one of the target types are null, this means the type is not known at runtime.
          case (c1, c2) => throw new RuntimeException(s"This should not be reachable, $c1, $c2")
        }
    )
    // check over the number of matching methods
    executables.filter(m => filter.forall(_ (m))) match {
      case Array() => None // no matches found
      case Array(e1) => Some(e1) // one match found
      case arr => // multiple matches found, find one that has the most specific parameter types
        // go over very method and find those that have the most specific subtype
        var mostSpecificMethod: C = arr.head
        arr.tail.foreach {
          // if method is more specific then update current target
          case meth if isBetterMatch(mostSpecificMethod.getParameterTypes, meth) =>
            mostSpecificMethod = meth
          case _ =>
        }
        Some(mostSpecificMethod)
    }
  }

  /**
   * Tests if the given method is a more specific version of the current best method <br>
   * e.g. A <&lt;: B <br>
   * List(B, C), Method(A, C) => true,
   * List(A, C), Method(B, C) => false <br>
   * Im not sure if its possible, but in cases such as <br>
   * A &lt;: B, C &lt;: D, with functions F(A, D), F(B,C) <br>
   * calling F(A,C) would not be resolvable.
   *
   * @param currentBest interface of the current most specific method
   * @param method      method to test
   * @return false if any of the parameters in the given method are superclasses of those in currentBest
   */
  private def isBetterMatch(currentBest: Array[Class[_]], method: Executable): Boolean = {
    method.getParameterTypes.zipWithIndex.foreach(p =>
      compareClass(p._1, currentBest(p._2)) match {
        case 1 => return false // parameter in current best is more specific than the one in the method
        case -1 => return true // parameter in method is more specific than the one in the current best
        case 0 => // parameters are identical
        case -2 => // this should never be the case
          if (p._1.isPrimitive)
            return true
          else if (currentBest(p._2).isPrimitive)
            return false
          else
            throw new Exception("Comparing two methods that have incomparable parameters")
      })
    true
  }

  /**
   * Converts a list of vars into a list of thier types
   *
   * @param vars list of variables to get the types of
   * @return the types of the input vars
   */
  private def toTypes(vars: Seq[Any]): Seq[Class[Any]] =
    vars.map {
      case null => null
      case p => p.getClass.asInstanceOf[Class[Any]]
    }
}
