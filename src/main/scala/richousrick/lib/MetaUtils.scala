package richousrick.lib

import java.lang.reflect.{Constructor, Executable, Method}

import scala.collection.mutable.ListBuffer
import scala.reflect.ClassTag
import scala.util.Try

/**
 * A collection of helper functions to make calling methods and constructors via reflection easier.
 *
 * @author richousrick
 */
object MetaUtils {

	/**
	 * Attempts to call 'new C(params)', and get the created object.<br>
	 * Note: This function requires the type parameter to be specified. See
	 * [[richousrick.lib.MetaUtils#build(java.lang.Class, scala.collection.immutable.Seq) build]]
	 * for a version that does not require the type parameters be specified.
	 *
	 * @param params parameters for the constructor
	 * @param rt     class tag for type C
	 * @tparam C type of object to instantiate with the constructor
	 * @throws InstantiationError if the desired constructor cannot be found
	 * @return the created object
	 */
	def build[C](params: Any*)(implicit rt: ClassTag[C]): C =
		buildOpt[C](params: _*) match {
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
	 * [[richousrick.lib.MetaUtils#buildOpt(scala.collection.immutable.Seq, richousrick.lib.NotNothing, scala.reflect.ClassTag) buildOpt]]
	 * for a version that does not require the type parameters be specified.
	 *
	 * @param params parameters used by the constructor
	 * @param rt     class tag for type C
	 * @tparam C type of object to instantiate with the constructor
	 * @return the created object if successful, None otherwise.
	 */
	def buildOpt[C](params: Any*)(implicit rt: ClassTag[C]): Option[C] =
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
		findConstructor(clazz, params.map(p => p.getClass.asInstanceOf[Class[Any]]): _*).map(_.newInstance(params: _*))

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
	 * @see [[richousrick.lib.MetaUtils#runOption]]
	 */
	def run[R](instance: AnyRef, funcName: String, params: Any*)(implicit rt: ClassTag[R]): R =
		runOpt[R](instance, funcName, params: _*) match {
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
	 * @return Some(instance.funcName(params)), if such a function exists. None otherwise
	 */
	def runOpt[R](instance: AnyRef, funcName: String, params: Any*)(implicit rt: ClassTag[R]): Option[R] =
		getGenericFunction(instance,
			funcName,
			rt.runtimeClass.asInstanceOf[Class[R]],
			params.map(p => p.getClass.asInstanceOf[Class[Any]]): _*)(allowGenericReturns = true).map(_ (params))


	/**
	 * Attempts to create a function literal representing calling the desired function the the specified instance
	 *
	 * @param instance   the object the desired function should be called on
	 * @param funcName   name of the desired function
	 * @param returnType return type of the desired function
	 * @param params     arguments the desired function takes
	 * @tparam C type of the object the function is called on
	 * @tparam R type of data the function should return
	 * @tparam P base type of data the parameters take.
	 * @return a function literal representing the desired function, if one was found
	 */
	def getFunction[C, R, P](instance: C,
													 funcName: String,
													 returnType: Class[R],
													 params: Class[P]*): Option[Seq[P] => R] =
		getGenericFunction(instance, funcName, returnType, params: _*)(allowGenericReturns = false)

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
	 * @param clazz      instance the method is called on
	 * @param funcName   name of the desired function
	 * @param returnType return type of the desired function
	 * @param params     arguments the desired function takes
	 * @tparam C type of the object the function is called on
	 * @tparam R type of data the function should return
	 * @tparam P base type of data the parameters take.
	 * @return a function literal that allows the desired function to be invoked, if such a function was found.
	 */
	def buildFunction[C, R, P](clazz: Class[C],
														 funcName: String,
														 returnType: Class[R],
														 params: Class[P]*): Option[(C, Seq[P]) => R] =
		buildGenericFunction(clazz, funcName, returnType, params: _*)(allowGenericReturns = false)

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
				Some[Method => Boolean](_.getReturnType.isAssignableFrom(returnType))
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
					case _ => throw new RuntimeException("This should not be reachable")
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
					case meth if isBetter(mostSpecificMethod.getParameterTypes, meth) =>
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
	private def isBetter(currentBest: Array[Class[_]], method: Executable): Boolean = {
		method.getParameterTypes.zipWithIndex.foreach(p =>
			compareClass(p._1, currentBest(p._2)) match {
				case 1 => return false // parameter in current best is more specific than the one in the method
				case -1 => return true // parameter in method is more specific than the one in the current best
				case 0 => // parameters are identical
				case -2 => // this should never be the case
					throw new Exception("Comparing two methods that have incomparable parameters")
			})
		true
	}

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