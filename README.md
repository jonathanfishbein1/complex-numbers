# Complex Numbers

## Summary

A module to define complex number and basic arithmetic operations on them

Examples of complex numbers

3 + 4i a compolex number with both real and imaginary components

3 a complex number with no imaginary component

i a complex number with no real component

The above examples are complex numbers using the Cartesian representation they can also be represented
using polar coordinates in that case the magnitude of the real and imaginary components is the first component called the modulus.
The angle of the modulus when graphed on the complex plane is the second component.

Complex numbers have arithmetic operations associated with them including add, multiply, subtract, divide, modulus and conjugate.
The majority of the operations are familiar but encapsulate the unique procedures required to do so with complex numbers.

The conjugate of a complex number is the complex number with the sign of its imaginary portion flipped.  For example 
the conjugate of 3 + 4i would be 3 - 4i

There are two functions convertFromCartesianToPolar and convertFromPolarToCartesian for converting between Cartesian and polar and polar to 
Cartesian representations respectivly.

Complex numbers have two monoid instances sum and product.
