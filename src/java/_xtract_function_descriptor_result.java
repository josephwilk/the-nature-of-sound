/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.10
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */


public class _xtract_function_descriptor_result {
  private transient long swigCPtr;
  protected transient boolean swigCMemOwn;

  protected _xtract_function_descriptor_result(long cPtr, boolean cMemoryOwn) {
    swigCMemOwn = cMemoryOwn;
    swigCPtr = cPtr;
  }

  protected static long getCPtr(_xtract_function_descriptor_result obj) {
    return (obj == null) ? 0 : obj.swigCPtr;
  }

  protected void finalize() {
    delete();
  }

  public synchronized void delete() {
    if (swigCPtr != 0) {
      if (swigCMemOwn) {
        swigCMemOwn = false;
        xtractJNI.delete__xtract_function_descriptor_result(swigCPtr);
      }
      swigCPtr = 0;
    }
  }

  public _xtract_function_descriptor_result_scalar getScalar() {
    long cPtr = xtractJNI._xtract_function_descriptor_result_scalar_get(swigCPtr, this);
    return (cPtr == 0) ? null : new _xtract_function_descriptor_result_scalar(cPtr, false);
  }

  public _xtract_function_descriptor_result_vector getVector() {
    long cPtr = xtractJNI._xtract_function_descriptor_result_vector_get(swigCPtr, this);
    return (cPtr == 0) ? null : new _xtract_function_descriptor_result_vector(cPtr, false);
  }

  public _xtract_function_descriptor_result() {
    this(xtractJNI.new__xtract_function_descriptor_result(), true);
  }

}
