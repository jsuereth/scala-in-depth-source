package booleans

/**
 * Encoding of Boolean types into the type system.
 */
sealed trait TBool {
  /**The 'if' function as a type constructor.
   * TrueType: The type to return if the current boolean is true.
   * FalseType: The type to return if the current boolean is false.
   * Up: The upper bound type for all values to prevent too many compiler acrobatics.
   */
  type If[TrueType <: Up, FalseType <: Up, Up] <: Up
}

/**
 * Encoding of the value 'true' into the type system.
 */
class TTrue extends TBool {
  type If[TrueType <: Up, FalseType <: Up, Up] = TrueType
}

/**
 * Encoding of the value 'false' into the type system.
 */
class TFalse extends TBool {
  type If[TrueType <: Up, FalseType <: Up, Up] = FalseType
}