/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.10
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */


public final class xtract_subband_scales_ {
  public final static xtract_subband_scales_ XTRACT_OCTAVE_SUBBANDS = new xtract_subband_scales_("XTRACT_OCTAVE_SUBBANDS");
  public final static xtract_subband_scales_ XTRACT_LINEAR_SUBBANDS = new xtract_subband_scales_("XTRACT_LINEAR_SUBBANDS");

  public final int swigValue() {
    return swigValue;
  }

  public String toString() {
    return swigName;
  }

  public static xtract_subband_scales_ swigToEnum(int swigValue) {
    if (swigValue < swigValues.length && swigValue >= 0 && swigValues[swigValue].swigValue == swigValue)
      return swigValues[swigValue];
    for (int i = 0; i < swigValues.length; i++)
      if (swigValues[i].swigValue == swigValue)
        return swigValues[i];
    throw new IllegalArgumentException("No enum " + xtract_subband_scales_.class + " with value " + swigValue);
  }

  private xtract_subband_scales_(String swigName) {
    this.swigName = swigName;
    this.swigValue = swigNext++;
  }

  private xtract_subband_scales_(String swigName, int swigValue) {
    this.swigName = swigName;
    this.swigValue = swigValue;
    swigNext = swigValue+1;
  }

  private xtract_subband_scales_(String swigName, xtract_subband_scales_ swigEnum) {
    this.swigName = swigName;
    this.swigValue = swigEnum.swigValue;
    swigNext = this.swigValue+1;
  }

  private static xtract_subband_scales_[] swigValues = { XTRACT_OCTAVE_SUBBANDS, XTRACT_LINEAR_SUBBANDS };
  private static int swigNext = 0;
  private final int swigValue;
  private final String swigName;
}

