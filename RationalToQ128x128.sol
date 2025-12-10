// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;

/// @author Vibed by ChatGPT 5.1 improved by Tim Olson 1.0
library RationalToQ128x128 {
    struct Rational {
        int256 numerator;
        int256 denominator;
    }

    struct UnsignedRational {
        uint256 numerator;
        uint256 denominator;
    }

    /// @notice Converts a signed rational struct into signed Q128.128 fixed point
    function toQ128x128S(Rational memory rational) internal pure returns (int256 result) {
        return toQ128x128(rational.numerator, rational.denominator);
    }

    /// @notice Converts a signed rational `numerator / denominator` into signed Q128.128 fixed point
    function toQ128x128S(int256 numerator, int256 denominator) internal pure returns (int256 result) {
        return toQ128x128(numerator, denominator);
    }

    /// @notice Converts an unsigned rational struct into unsigned Q128.128 fixed point
    function toQ128x128U(UnsignedRational memory rational) internal pure returns (uint256 result) {
        return toQ128x128(rational.numerator, rational.denominator);
    }

    /// @notice Converts an unsigned rational `numerator / denominator` into unsigned Q128.128 fixed point
    function toQ128x128U(uint256 numerator, uint256 denominator) internal pure returns (uint256 result) {
        return toQ128x128(numerator, denominator);
    }

    /// @notice Converts a signed rational `numerator / denominator`
    ///         into Q128.128 (signed 128.128 fixed point),
    function toQ128x128(Rational memory rational) internal pure returns (int256 result) {
        return toQ128x128(rational.numerator, rational.denominator);
    }

    /// @notice Converts a signed rational `numerator / denominator`
    ///         into signed Q128.128 (signed 128.128 fixed point),
    ///         rounding toward zero.
    ///
    /// @dev Reverts if:
    ///      - `denominator == 0`, or
    ///      - the exact result overflows int256 range.
    ///
    ///      This computes floor(numerator * 2^128 / denominator)
    ///      for positive results, and ceil(numerator * 2^128 / denominator)
    ///      for negative results (rounding toward zero in both cases).
    function toQ128x128(UnsignedRational memory rational) internal pure returns (uint256 result) {
        return toQ128x128(rational.numerator, rational.denominator);
    }

    /// @notice Converts a signed rational `numerator / denominator`
    ///         into signed Q128.128 (signed 128.128 fixed point),
    ///         rounding toward zero.
    ///
    /// @dev Reverts if:
    ///      - `denominator == 0`, or
    ///      - the exact result overflows int256 range.
    ///
    ///      This computes floor(numerator * 2^128 / denominator)
    ///      for positive results, and ceil(numerator * 2^128 / denominator)
    ///      for negative results (rounding toward zero in both cases).
    function toQ128x128(int256 numerator, int256 denominator) internal pure returns (int256 result) {
        require(denominator != 0, "toQ128x128: div by zero");

        // Determine the sign of the result
        bool resultNegative = (numerator < 0) != (denominator < 0);

        // Work with absolute values
        // Special case: type(int256).min cannot be negated without overflow
        uint256 absNumerator;
        if (numerator == type(int256).min) {
            absNumerator = uint256(type(int256).max) + 1;
        } else {
            absNumerator = numerator >= 0 ? uint256(numerator) : uint256(-numerator);
        }

        uint256 absDenominator;
        if (denominator == type(int256).min) {
            absDenominator = uint256(type(int256).max) + 1;
        } else {
            absDenominator = denominator >= 0 ? uint256(denominator) : uint256(-denominator);
        }

        // Delegate to the unsigned version
        uint256 unsignedResult = toQ128x128(absNumerator, absDenominator);

        // Ensure the result fits in int256 range before casting
        require(unsignedResult <= uint256(type(int256).max), "toQ128x128: signed overflow");

        // Apply sign
        result = resultNegative ? -int256(unsignedResult) : int256(unsignedResult);
    }

    /// @notice Converts an unsigned rational `numerator / denominator`
    ///         into Q128.128 (unsigned 128.128 fixed point),
    ///         rounding toward zero (floor for positive inputs).
    ///
    /// @dev Reverts if:
    ///      - `denominator == 0`, or
    ///      - the exact result >= 2^256 (i.e. overflow of uint256).
    ///
    ///      This computes floor(numerator * 2^128 / denominator)
    ///      using a full 512-bit intermediate to avoid precision loss.
    function toQ128x128(uint256 numerator, uint256 denominator) internal pure returns (uint256 result) {
        require(denominator != 0, "toQ128x128: div by zero");

        // We want (numerator * 2^128) / denominator using full precision,
        // so we implement a 512-bit muldiv.
        //
        // Let:
        //   prod = numerator * 2^128
        //
        // Since 2^128 is a power of two, the 512-bit product is easy:
        //   prod0 = (numerator << 128) mod 2^256  (low 256 bits)
        //   prod1 = (numerator >> 128)            (high 256 bits)
        //
        // So prod = (prod1 * 2^256 + prod0).
        uint256 prod0;
        uint256 prod1;
        unchecked {
            prod0 = numerator << 128;
            prod1 = numerator >> 128;
        }

        // If the high 256 bits are zero, the product fits in 256 bits.
        // This is the cheap path: just do a normal 256-bit division.
        if (prod1 == 0) {
            unchecked {
                // denominator was already checked for 0.
                return prod0 / denominator;
            }
        }

        // At this point prod1 > 0, so the 512-bit product does not fit in a uint256.
        // We need a full-precision 512/256 division:
        //
        //   result = floor((prod1 * 2^256 + prod0) / denominator)
        //
        // and we must ensure the final result fits in uint256.

        // Ensure result < 2^256. This is equivalent to requiring:
        //   denominator > prod1
        // because if denominator <= prod1, then:
        //   (prod1 * 2^256) / denominator >= 2^256.
        require(denominator > prod1, "Q128x128: overflow");

        // Make division exact by subtracting the remainder from [prod1 prod0].
        uint256 remainder;
        assembly {
            // remainder = (prod1 * 2^256 + prod0) % denominator
            // Since we can only directly mod 256-bit values, we first mod `prod0`,
            // then adjust using the high word.
            remainder := mulmod(numerator, shl(128, 1), denominator)
        }

        // Now subtract `remainder` from the 512-bit product [prod1 prod0].
        assembly {
            // Subtract remainder from the low part; if it underflows, borrow 1 from the high part.
            let borrow := lt(prod0, remainder)
            prod0 := sub(prod0, remainder)
            prod1 := sub(prod1, borrow)
        }

        // Factor powers of two out of denominator to simplify the division.
        //
        // Let denominator = d * 2^shift, with d odd.
        // We can divide prod0 by 2^shift cheaply (bit shift),
        // then do an exact division by the odd d using modular inverse.
        uint256 twos;
        unchecked {
            twos = denominator & (~denominator + 1); // largest power of two divisor of denominator
        }

        assembly {
            // Divide denominator by twos.
            denominator := div(denominator, twos)

            // Divide the low word by twos.
            prod0 := div(prod0, twos)

            // Adjust the high word so that the full 512-bit number is shifted by `twos`.
            // twos = 2^k, so:
            //   combined = prod1 * 2^256 + prod0
            //   combined / twos =
            //     prod1 * 2^256 / twos + prod0 / twos
            // and 2^256 / twos = 2^(256-k).
            //
            // Here we compute:
            //   twos = 2^256 / twos
            twos := add(div(sub(0, twos), twos), 1)

            // Now add the shifted high bits into prod0:
            prod0 := or(prod0, mul(prod1, twos))
        }

        // At this point, denominator is odd and the 512-bit value
        // has been squeezed into prod0 (prod1 is effectively 0).

        // Compute the modular inverse of denominator modulo 2^256.
        // This uses Newton-Raphson iteration:
        //
        //   inv ≡ denominator^{-1} (mod 2^256)
        //
        // Starting from a seed for odd denominator:
        // All operations must be unchecked as they rely on modular arithmetic.
        unchecked {
            uint256 inv = (3 * denominator) ^ 2;

            // Perform Newton-Raphson iterations to refine the inverse.
            // Starting from inv which is correct modulo 2^4, then each
            // Newton-Raphson step doubles the number of correct bits:
            // 2⁴ → 2⁸ → 2¹⁶ → 2³² → 2⁶⁴ → 2¹²⁸ → 2²⁵⁶
            // Requiring six iterations for 256-bit precision:
            inv *= 2 - denominator * inv;
            inv *= 2 - denominator * inv;
            inv *= 2 - denominator * inv;
            inv *= 2 - denominator * inv;
            inv *= 2 - denominator * inv;
            inv *= 2 - denominator * inv;

            // Now inv is the modular inverse of denominator mod 2^256.
            // The exact division result is then:
            //
            //   result = (prod0 * inv) mod 2^256
            //
            // which is just ordinary 256-bit multiplication.
            result = prod0 * inv;
        }
    }

    //
    // General muldiv using Rationals
    //

    function mul(Rational memory rational, int256 y) internal pure returns (int256) {
        return mulDiv(rational.numerator, y, rational.denominator);
    }

    function mul(UnsignedRational memory rational, uint256 y) internal pure returns (uint256) {
        return mulDiv(rational.numerator, y, rational.denominator);
    }

    function mulDivS(int256 x, int256 y, int256 denominator) internal pure returns (int256 result) {
        return mulDiv(x, y, denominator);
    }

    function mulDivU(uint256 x, uint256 y, uint256 denominator) internal pure returns (uint256 result) {
        return mulDiv(x, y, denominator);
    }

    /// @notice Signed wrapper for full-precision mulDiv.
    ///         Computes trunc(x * y / denominator) toward zero with 512-bit precision.
    ///
    /// @dev Mirrors `toQ128x128(int256,int256)` sign handling.
    ///      Reverts if `denominator == 0` or magnitude overflows int256.
    function mulDiv(int256 x, int256 y, int256 denominator) internal pure returns (int256 result) {
        require(denominator != 0, "mulDiv: div by zero");

        // Determine the sign of the result
        bool negative = (x < 0) != (y < 0) != (denominator < 0);

        // Absolute values with care for MIN int
        uint256 ax;
        if (x == type(int256).min) {
            ax = uint256(type(int256).max) + 1;
        } else {
            ax = x >= 0 ? uint256(x) : uint256(-x);
        }

        uint256 ay;
        if (y == type(int256).min) {
            ay = uint256(type(int256).max) + 1;
        } else {
            ay = y >= 0 ? uint256(y) : uint256(-y);
        }

        uint256 ad;
        if (denominator == type(int256).min) {
            ad = uint256(type(int256).max) + 1;
        } else {
            ad = denominator >= 0 ? uint256(denominator) : uint256(-denominator);
        }

        uint256 unsignedResult = mulDiv(ax, ay, ad);

        // Ensure it fits in int256 to apply sign
        require(unsignedResult <= uint256(type(int256).max), "mulDiv: signed overflow");

        result = negative ? -int256(unsignedResult) : int256(unsignedResult);
    }

    /// @notice Full-precision mulDiv: computes floor(x * y / denominator)
    ///         with 512-bit intermediate precision to avoid overflow.
    ///
    /// @dev Reverts if `denominator == 0` or the exact result >= 2^256.
    ///      The implementation mirrors the 512/256 division flow used by
    ///      `toQ128x128(uint256,uint256)`, but with a general multiplicand `y`
    ///      instead of the fixed 2^128 shift.
    function mulDiv(uint256 x, uint256 y, uint256 denominator) internal pure returns (uint256 result) {
        require(denominator != 0, "mulDiv: div by zero");

        // Compute the 512-bit product [prod1 prod0] = x * y.
        // mm = (x * y) mod (2^256 - 1)
        // prod0 = (x * y) mod 2^256
        // prod1 = (x * y - prod0 - (mm < prod0 ? 1 : 0)) / 2^256
        uint256 prod0;
        uint256 prod1;
        assembly {
            let mm := mulmod(x, y, not(0))
            prod0 := mul(x, y)
            prod1 := sub(sub(mm, prod0), lt(mm, prod0))
        }

        // If the high 256 bits are zero, we can do a simple 256-bit division.
        if (prod1 == 0) {
            unchecked {
                return prod0 / denominator;
            }
        }

        // Ensure result < 2^256. This is equivalent to requiring denominator > prod1.
        require(denominator > prod1, "mulDiv: overflow");

        // Make division exact by subtracting the remainder from [prod1 prod0].
        uint256 remainder;
        assembly {
            remainder := mulmod(x, y, denominator)
            // Subtract remainder from the low part; if it underflows, borrow 1 from the high part.
            let borrow := lt(prod0, remainder)
            prod0 := sub(prod0, remainder)
            prod1 := sub(prod1, borrow)
        }

        // Factor powers of two out of denominator to simplify the division.
        uint256 twos;
        unchecked {
            twos = denominator & (~denominator + 1); // largest power of two divisor of denominator
        }

        assembly {
            // Divide denominator by twos.
            denominator := div(denominator, twos)

            // Divide the low word by twos.
            prod0 := div(prod0, twos)

            // Compute twos = 2^256 / twos.
            twos := add(div(sub(0, twos), twos), 1)

            // Shift bits from the high word into the low word.
            prod0 := or(prod0, mul(prod1, twos))
        }

        // Compute modular inverse of the (now odd) denominator modulo 2^256
        // via Newton-Raphson iterations.
        unchecked {
            uint256 inv = (3 * denominator) ^ 2;
            inv *= 2 - denominator * inv; // 2^8
            inv *= 2 - denominator * inv; // 2^16
            inv *= 2 - denominator * inv; // 2^32
            inv *= 2 - denominator * inv; // 2^64
            inv *= 2 - denominator * inv; // 2^128
            inv *= 2 - denominator * inv; // 2^256

            // Exact division: result = prod0 * inv mod 2^256
            result = prod0 * inv;
        }
    }
}
