/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 3.0.10
 *
 * Do not make changes to this file unless you know what you are doing--modify
 * the SWIG interface file instead.
 * ----------------------------------------------------------------------------- */


public final class xtract_unit_t {
  public final static xtract_unit_t XTRACT_HERTZ = new xtract_unit_t("XTRACT_HERTZ", xtractJNI.XTRACT_HERTZ_get());
  public final static xtract_unit_t XTRACT_ANY_AMPLITUDE_HERTZ = new xtract_unit_t("XTRACT_ANY_AMPLITUDE_HERTZ");
  public final static xtract_unit_t XTRACT_DBFS = new xtract_unit_t("XTRACT_DBFS");
  public final static xtract_unit_t XTRACT_DBFS_HERTZ = new xtract_unit_t("XTRACT_DBFS_HERTZ");
  public final static xtract_unit_t XTRACT_PERCENT = new xtract_unit_t("XTRACT_PERCENT");
  public final static xtract_unit_t XTRACT_BINS = new xtract_unit_t("XTRACT_BINS");
  public final static xtract_unit_t XTRACT_SONE = new xtract_unit_t("XTRACT_SONE");
  public final static xtract_unit_t XTRACT_MIDI_CENT = new xtract_unit_t("XTRACT_MIDI_CENT");

  public final int swigValue() {
    return swigValue;
  }

  public String toString() {
    return swigName;
  }

  public static xtract_unit_t swigToEnum(int swigValue) {
    if (swigValue < swigValues.length && swigValue >= 0 && swigValues[swigValue].swigValue == swigValue)
      return swigValues[swigValue];
    for (int i = 0; i < swigValues.length; i++)
      if (swigValues[i].swigValue == swigValue)
        return swigValues[i];
    throw new IllegalArgumentException("No enum " + xtract_unit_t.class + " with value " + swigValue);
  }

  private xtract_unit_t(String swigName) {
    this.swigName = swigName;
    this.swigValue = swigNext++;
  }

  private xtract_unit_t(String swigName, int swigValue) {
    this.swigName = swigName;
    this.swigValue = swigValue;
    swigNext = swigValue+1;
  }

  private xtract_unit_t(String swigName, xtract_unit_t swigEnum) {
    this.swigName = swigName;
    this.swigValue = swigEnum.swigValue;
    swigNext = this.swigValue+1;
  }

  private static xtract_unit_t[] swigValues = { XTRACT_HERTZ, XTRACT_ANY_AMPLITUDE_HERTZ, XTRACT_DBFS, XTRACT_DBFS_HERTZ, XTRACT_PERCENT, XTRACT_BINS, XTRACT_SONE, XTRACT_MIDI_CENT };
  private static int swigNext = 0;
  private final int swigValue;
  private final String swigName;
}

